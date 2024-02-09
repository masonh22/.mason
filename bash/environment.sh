# only do this once
if [ -n "${MASON_ENV_INIT_COMPLETE}" ]; then
    return 0
fi

set -o ignoreeof

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize
# Append to the history file instead of overwriting it.
shopt -s histappend

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=100000000
HISTFILESIZE=100000000

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;31:locus=01:quote=01'
export GREP_COLORS="mt=01;32" # Matching text in bold green.

export EDITOR='emacsclient --alternate-editor="" -nw'

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

# disable backup files for GNU utilities
export VERSION_CONTROL="never"

MASON_ENV_INIT_COMPLETE=true
