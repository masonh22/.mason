if [ ! -z "${SSH_TTY}" ]; then
    export IS_SSH=true
fi

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;31:locus=01:quote=01'
export GREP_COLORS="mt=01;32" # Matching text in bold green.

if type emacs > /dev/null 2>&1; then
    export EDITOR='emacsclient --alternate-editor="" -nw'
else
    export EDITOR=nano
fi

# disable backup files for GNU utilities
export VERSION_CONTROL="never"

# tell cmake to make compile_commands.json for clangd
export CMAKE_EXPORT_COMPILE_COMMANDS=1

# This is probably (definitely) bad, but it is so annoying to configure 24-bit
# colors "correctly" so I'm just gonna assume that I always use a terminal that
# supports 24-bit colors to simplify things.
# export COLORTERM=truecolor

# only do next part once
if [ -n "${MASON_ENV_INIT_COMPLETE}" ]; then
    return 0
fi

# opam configuration
test -r ${HOME}/.opam/opam-init/init.sh && . ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

# cargo (rust) configuration
test -r ${HOME}/.cargo/env && . ${HOME}/.cargo/env
# this tells cargo to install stuff in this directory:
export CARGO_INSTALL_ROOT=${HOME}/.local

# elan (lean) configuration
test -r ${HOME}/.elan/env && . ${HOME}/.elan/env

# kubectl configuration (disabled because this is slow)
# [ -n "$INIT_TRACE" ] && echo "$(date +%s.%N) .mason/environment.sh kubectl"
# type kubectl > /dev/null 2>&1 && source <(kubectl completion bash)
# [ -n "$INIT_TRACE" ] && echo "$(date +%s.%N) .mason/environment.sh kubectl done"

# set PATH so it includes .mason bin if it exists
if [ -d "${MASON_HOME}/bin" ]; then
    PATH="${MASON_HOME}/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ]; then
    PATH="$HOME/bin:$PATH"
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/.local/bin" ]; then
    PATH="$HOME/.local/bin:$PATH"
fi

# GNU which, if available. This check is good enough for now...
if which -v > /dev/null 2>&1 \
        && ! alias which > /dev/null 2>&1 \
        && ! type 'which' 2> /dev/null | grep -q 'function'; then
    alias which="alias | $(which which) --tty-only --read-alias --show-dot --show-tilde"
fi

MASON_ENV_INIT_COMPLETE=true
