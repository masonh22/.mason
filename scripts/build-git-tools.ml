#! /usr/bin/env ocaml

open Sys

let home = getenv "HOME"

(** process command line *)
let usage = "./build-git-tools.ml [--debug] [--only-setup] [--force-setup] [--do-pull] [--jobs n] [--download-dir d] [--install-dir d]  <packages>"

let debug = ref false
let only_setup = ref false
let force_setup = ref false
let do_pull = ref false
let jobs = ref "12"
let download_dir = ref "/opt/git"
let install_dir_prompt = ref ""
let packages = ref []

(* TODO make directories if they don't exist *)

let speclist = [
    ("--debug", Arg.Set debug, "List packages without installing them");
    ("--only-setup", Arg.Set only_setup, "Download and configure packages but do not compile or install them");
    ("--force-setup", Arg.Set force_setup, "Run the first time setup, even if the repo exists");
    ("--do-pull", Arg.Set do_pull, "Run 'git pull' before compiling");
    ("--jobs", Arg.Set_string jobs, "Number of jobs (for packages compiled with make)");
    ("--download-dir", Arg.Set_string download_dir, "Directory to download packages to");
    ("--install-dir", Arg.Set_string install_dir_prompt, "Directory to install packages in");
  ]

let anon_fun package = packages := package :: !packages

let _ = Arg.parse speclist anon_fun usage

let default_install = "/usr/local"
let local_install = home ^ "/.local"

type package = {
    name: string;
    repo: string;
    first_time: string -> string list; (* commands to run on the first time *)
    compile: string -> string list;    (* commands to compile and install *)
    (* TODO dependencies *)
  }

(* TODO sudo if can't install *)
let package_specs : package list = [
    {
      name = "ripgrep";
      repo = "git clone https://github.com/BurntSushi/ripgrep.git";
      first_time = (fun _ -> []);
      compile =
        (fun install_dir ->
          [
            "RUSTFLAGS='-C target-cpu=native' cargo +nightly build --features 'simd-accel' --release";
            "cargo +nightly install --path . --root " ^ install_dir; (* TODO test this *)
        ]);
    };
    {
      name = "fd";
      repo = "git clone https://github.com/sharkdp/fd.git";
      first_time = (fun _ -> []);
      compile =
        (fun install_dir ->
          [
            "RUSTFLAGS='-C target-cpu=native' cargo +nightly build --release";
            "cargo +nightly install --path . --root " ^ install_dir; (* TODO test this *)
        ]);
    };
    {
      name = "nghttp2";
      repo = "git clone https://github.com/nghttp2/nghttp2.git";
      first_time =
        (fun install_dir ->
          [
            "sudo apt install -y binutils autoconf automake autotools-dev \
             libtool pkg-config zlib1g-dev libcunit1-dev libssl-dev libxml2-dev \
             libev-dev libevent-dev libjansson-dev libc-ares-dev libjemalloc-dev \
             libsystemd-dev ruby-dev bison libelf-dev";
            "autoreconf -i";
            "automake";
            "autoconf";
            "CFLAGS='-march=native' CPPFLAGS='-march=native' ./configure --prefix=" ^ install_dir;
        ]);
      compile =
        (fun _ ->
          [
            "make -j" ^ !jobs;
            "make install";
        ]);
    };
    {
      name = "curl";
      repo = "git clone https://github.com/curl/curl.git";
      first_time =
        (fun install_dir ->
          [
            "autoreconf -fi";
            "CFLAGS='-march=native' CPPFLAGS='-march=native' ./configure --enable-optimize --disable-curldebug --enable-http --with-openssl --with-nghttp2=" ^ install_dir ^ " --prefix=" ^ install_dir;
        ]);
      compile =
        (fun _ ->
          [
            "make -j" ^ !jobs;
            "make install";
        ]);
    };
    {
      name = "emacs";
      repo = "git clone -b emacs-29 https://github.com/emacs-mirror/emacs.git";
      first_time =
        (fun install_dir ->
          [
            (* "sudo apt build-dep -y emacs"; *)
            "sudo apt install -y libgccjit0 libgccjit-11-dev libjansson4 \
             libjansson-dev gnutls-bin libtree-sitter-dev imagemagick \
             libmagick++-dev libwebp-dev webp libxft-dev libxft2 libgtk-3-dev \
             texinfo libgnutls28-dev libncurses-dev";
            "./autogen.sh";
            "CFLAGS='-march=native -O2 -g3' CPPFLAGS='-march=native' ./configure --with-native-compilation=aot --with-pgtk --with-imagemagick --prefix=" ^ install_dir;
        ]);
      compile =
        (fun _ ->
          [
            "make -j" ^ !jobs;
            "make install";
        ]);
    };
  ]

let run_commands cmds =
  List.map (fun x ->
      if command x <> 0 then
        failwith ("Failed to run command " ^ x))
    cmds |> ignore

let rec get_yes_no prompt default : bool =
  print_string prompt;
  let str = read_line () in
  if String.length str = 0 then default else
    match str.[0] with
    | 'y' | 'Y' -> true
    | 'n' | 'N' -> false
    | _ -> print_endline "Invalid option"; get_yes_no prompt default

let rec get_path prompt default alternate : string =
  (* TODO validate that the path exists or ask for confirmation to create it *)
  (* TODO this should make the prompt, not the caller *)
  print_string (prompt ^ "\n[1] " ^ default ^ "\n[2] " ^ alternate ^ "\nother (default [1]): ");
  let path = read_line () in
  if String.length path = 0 then default else
    match path.[0] with
    | '/' -> path
    | '1' -> default
    | '2' -> alternate
    | _ ->
       print_endline "This looks like an invalid path. Try again.";
       get_path prompt default alternate

exception Done

(* TODO pipe output to log file? *)
let install_package { name; repo; first_time; compile } =
  let dirname = !download_dir ^ "/" ^ name in
  print_endline ("Installing " ^ name ^ "...");
  if !debug then raise Done;
  chdir !download_dir;
  let needs_repo = not (file_exists name) || not (is_directory name) in
  if needs_repo then
    run_commands [repo];
  chdir dirname;
  (* TODO kinda bad because install is determined at install time for cargo, but during config for other packages *)
  let install =
    if String.length !install_dir_prompt <> 0 then !install_dir_prompt else
      get_path "Where should this be installed?" default_install local_install
  in
  if !force_setup || needs_repo then
    run_commands (first_time install);
  if !only_setup then raise Done;
  run_commands ((if !do_pull then List.cons "git pull" else Fun.id) (compile install))

let package_list =
  if !packages = [] then package_specs else
    List.filter (fun { name } ->
        List.exists (fun cli_name -> name = cli_name)
          !packages)
      package_specs

let _ =
  List.map (fun x ->
      if get_yes_no ("Install '" ^ x.name ^ "'? [y/N]: ") false then
        try install_package x with
        | Failure msg -> print_endline msg
        | Done -> ()
      else print_endline ("Skipping '" ^ x.name ^ "'")
    ) package_list
