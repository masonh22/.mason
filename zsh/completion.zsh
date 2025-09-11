fpath=(${MASON_HOME}/zsh/completion.zsh $fpath)
autoload -Uz compinit && compinit

zstyle ':completion:*' completer _expand _complete _ignored
zstyle ':completion:*' menu select interactive
zstyle ':completion:*' file-list all

zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

ZLE_REMOVE_SUFFIX_CHARS=$' \t\n'

# Better `make` completions
# TODO handle `-f <makefile> as well! zsh does this by default
_make_print_targets() {
    if [ -r Makefile ]; then
        make -pRrq : 2>/dev/null | awk -F: '/(^|\n)# Files(\n|$$)/,/(^|\n)# Finished Make data base/ {if ($$1 !~ "^[#. \t]") {print $1}}' | sort | grep -E -v -e '^[^[:alnum:]]' -e '^$@$$'
    fi
}

_my_make_targets() {
    compadd $(_make_print_targets)
}
compdef _my_make_targets make

_cdp_list() {
    if [ -z "$PROJECTS" ]; then
        return
    fi

    _arguments "1:subdir of Projects:_path_files -W '$PROJECTS'"
}
compdef _cdp_list cdp
