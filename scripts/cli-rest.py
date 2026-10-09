#!/usr/bin/env python3
"""cli-rest: expose any CLI that speaks the mason completions protocol
as a REST API.

See scripts/gen-completions.bash for the protocol.  URL paths map onto command
words, so /foo/bar runs `<tool> foo bar` if each word was offered as a
completion.

Endpoints:

  GET  /                    discovery
  POST /_reload             re-run `<tool> completions`
  GET|POST /<word>[/<word>...]   run the command

Request bodies are ignored.  Commands return JSON:

  {"exit_code": N, "stdout": "...", "stderr": "...",
   "stdout_truncated": false, "stderr_truncated": false, "argv": [...]}

Status: 200 ran ok; 502 ran, exited non-zero; 400 bad argument; 404
unknown command; 405 wrong method; 500 server bug; 504 timed out.  The
server is single-threaded: commands run one at a time.  It binds to
127.0.0.1 by default because it executes real commands.

Usage:
  cli-rest.py --tool cli-tool [--host H] [--port P] [--timeout S]

"""

import argparse
import json
import shutil
import subprocess
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import quote, unquote, urlsplit

DEFAULT_MAX_OUTPUT = 1_000_000
SOCKET_TIMEOUT = 30.0


def parse_completions(text):
    """Parse completions output into {word-tuple: [next-word, ...]}.

    The empty tuple holds top-level completions.  Intermediate prefixes
    are registered so "item1,item1a:..." also makes ("item1",) reachable.
    """
    comps = {}

    def add(prefix, words):
        bucket = comps.setdefault(prefix, [])
        for word in words:
            if word and word not in bucket:
                bucket.append(word)

    for line in text.splitlines():
        line = line.strip()
        if not line:
            continue
        left, sep, right = line.partition(":")
        if not sep:
            add((), [w.strip() for w in line.split(",") if w.strip()])
            continue
        prefix = tuple(w.strip() for w in left.split(",") if w.strip())
        if not prefix:
            raise ValueError("completions line with empty prefix: %r" % line)
        for i in range(1, len(prefix) + 1):
            add(prefix[:i - 1], [prefix[i - 1]])
        add(prefix, [w.strip() for w in right.split(",") if w.strip()])

    if not comps:
        raise ValueError("no completions found in completions output")
    return comps


def truncate(data, limit):
    """Return (data[:limit], was_truncated) for bytes, or all of it if limit<=0."""
    if limit and len(data) > limit:
        return data[:limit], True
    return data, False


class CliApi:
    def __init__(self, tool, timeout, max_output, completions_timeout):
        self.tool = tool
        self.timeout = timeout
        self.max_output = max_output
        self.completions_timeout = completions_timeout
        self.completions = {}
        self.version = 0
        self.refresh()

    def refresh(self):
        try:
            p = subprocess.run([self.tool, "completions"], capture_output=True,
                               timeout=self.completions_timeout)
        except FileNotFoundError:
            raise RuntimeError("tool not found: %s" % self.tool)
        except subprocess.TimeoutExpired:
            raise RuntimeError("`%s completions` timed out" % self.tool)
        except OSError as e:
            raise RuntimeError("cannot run %s: %s" % (self.tool, e))
        if p.returncode != 0:
            raise RuntimeError("`%s completions` failed (exit %d): %s"
                               % (self.tool, p.returncode,
                                  p.stderr.decode("utf-8", "replace").strip()))
        self.completions = parse_completions(p.stdout.decode("utf-8", "replace"))
        self.version += 1

    def discovery(self):
        prefixes = []
        for prefix in sorted(self.completions, key=lambda p: (len(p), p)):
            path = "/" + "/".join(quote(w, safe="") for w in prefix) if prefix else "/"
            prefixes.append({"path": path, "next": self.completions[prefix]})
        return {
            "tool": self.tool,
            "version": self.version,
            "max_output_bytes": self.max_output,
            "prefixes": prefixes,
            "usage": "GET or POST /<word>[/<word>...]; POST /_reload",
        }

    def command(self, words):
        parent = ()
        for i, word in enumerate(words):
            choices = self.completions.get(parent, [])
            if word not in choices:
                if i == 0:
                    return 404, {"error": "unknown command %r" % word,
                                 "available": self.completions.get((), [])}
                return 400, {"error": "invalid argument %r after %s"
                                      % (word, " ".join(parent)),
                             "choices": choices}
            parent += (word,)
        return self.run(words)

    def run(self, words):
        argv = [self.tool] + list(words)
        try:
            p = subprocess.run(argv, capture_output=True, timeout=self.timeout)
        except subprocess.TimeoutExpired:
            return 504, {"error": "timed out after %ss" % self.timeout,
                         "argv": argv}
        except OSError as e:
            return 500, {"error": "cannot run %s: %s" % (self.tool, e),
                         "argv": argv}
        stdout, out_trunc = truncate(p.stdout, self.max_output)
        stderr, err_trunc = truncate(p.stderr, self.max_output)
        return (200 if p.returncode == 0 else 502), {
            "exit_code": p.returncode,
            "stdout": stdout.decode("utf-8", "replace"),
            "stderr": stderr.decode("utf-8", "replace"),
            "stdout_truncated": out_trunc,
            "stderr_truncated": err_trunc,
            "argv": argv,
        }


class Handler(BaseHTTPRequestHandler):
    server_version = "cli-rest"
    protocol_version = "HTTP/1.0"  # no keep-alive; request bodies are ignored
    timeout = SOCKET_TIMEOUT

    def do_GET(self):
        self.dispatch()

    def do_POST(self):
        self.dispatch()

    def send_json(self, code, obj, allow=None):
        body = json.dumps(obj, indent=2).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        if allow:
            self.send_header("Allow", allow)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def dispatch(self):
        try:
            self.route()
        except BrokenPipeError:
            pass
        except Exception as e:
            try:
                self.send_json(500, {"error": str(e)})
            except Exception:
                pass

    def route(self):
        api = self.server.api
        parts = [unquote(p) for p in urlsplit(self.path).path.split("/") if p]

        if not parts:
            return self.send_json(200, api.discovery())

        if parts[0] == "_reload":
            if len(parts) != 1:
                return self.send_json(404, {"error": "unknown path"})
            if self.command != "POST":
                return self.send_json(405, {"error": "use POST /_reload"},
                                      allow="POST")
            try:
                api.refresh()
            except (RuntimeError, ValueError) as e:
                return self.send_json(500, {"error": str(e)})
            return self.send_json(200, api.discovery())

        self.send_json(*api.command(parts))


def main():
    ap = argparse.ArgumentParser(
        description="Expose a CLI (mason completions protocol) as a REST API "
                    "using only the Python standard library.")
    ap.add_argument("--tool",
                    help="CLI to wrap; must support `<tool> completions`")
    ap.add_argument("--host", default="127.0.0.1",
                    help="bind address (default: 127.0.0.1)")
    ap.add_argument("--port", type=int, default=8777,
                    help="bind port (default: 8777; 0 picks a free port)")
    ap.add_argument("--timeout", type=float, default=120.0,
                    help="per-command timeout in seconds (default: 120)")
    ap.add_argument("--completions-timeout", type=float, default=30.0,
                    help="timeout for `<tool> completions` (default: 30)")
    ap.add_argument("--max-output", type=int, default=DEFAULT_MAX_OUTPUT,
                    help="max bytes per stream; 0 = unlimited (default: %d)"
                         % DEFAULT_MAX_OUTPUT)
    args = ap.parse_args()

    if "/" not in args.tool and shutil.which(args.tool) is None:
        sys.exit("cli-rest: tool not found in PATH: %s" % args.tool)

    try:
        api = CliApi(args.tool, args.timeout, args.max_output,
                     args.completions_timeout)
    except (RuntimeError, ValueError) as e:
        sys.exit("cli-rest: %s" % e)

    try:
        server = HTTPServer((args.host, args.port), Handler)
    except OSError as e:
        sys.exit("cli-rest: cannot bind %s:%d: %s" % (args.host, args.port, e))
    server.api = api

    host, port = server.server_address[0], server.server_address[1]
    n = len(api.completions.get((), []))
    print("cli-rest: %s -> http://%s:%d (%d top-level completion%s)"
          % (args.tool, host, port, n, "s" if n != 1 else ""), file=sys.stderr)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        server.server_close()


if __name__ == "__main__":
    main()
