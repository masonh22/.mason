set -o GLOB_COMPLETE # don't substitute glob patterns, but generate completions
set -o EXTENDED_HISTORY # timestamp/runtime in hist file
set -o APPEND_HISTORY # append to hist file
set -o INC_APPEND_HISTORY_TIME # write to the hist file after commands finish
set -o HIST_FIND_NO_DUPS # ignore dups when searching
set -o HIST_REDUCE_BLANKS # don't store blank lines
set -o HIST_IGNORE_DUPS # ignore consecutive commands in history
set -o HIST_IGNORE_SPACE # ignore lines that start with a space
set +o BEEP # disable bell

setopt prompt_subst # Enable prompt substitution
setopt ignore_eof # Don't exit on EOF (ctrl+d)

HISTFILE=~/.zsh_history
SAVEHIST=1000000000
HISTSIZE=1000000000

# bash/emacs style word selection
autoload -U select-word-style
select-word-style bash

bindkey '^[[1;5C' forward-word       # Ctrl+Right
bindkey '^[[1;5D' backward-word      # Ctrl+Left
bindkey '^[[3;5~' kill-word          # Ctrl+Delete
bindkey '^H' backward-kill-word      # Ctrl+Backspace
