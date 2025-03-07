set -o GLOB_COMPLETE # don't substitute glob patterns, but generate completions
set -o EXTENDED_HISTORY # timestamp/runtime in hist file
set -o SHARE_HISTORY # don't overwrite hist files on exit
set -o APPEND_HISTORY # append to hist file TODO diff between this and prev opt?
set -o HIST_FIND_NO_DUPS # ignore dups when searching
set -o HIST_REDUCE_BLANKS # don't store blank lines

setopt prompt_subst # Enable prompt substitution

HISTFILE=~/.zsh_history
SAVEHIST=1000000000
HISTSIZE=1000000000
