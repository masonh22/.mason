autoload -Uz compinit && compinit

zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' menu select interactive
zstyle ':completion:*' file-list all

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

ZLE_REMOVE_SUFFIX_CHARS=$' \t\n'
