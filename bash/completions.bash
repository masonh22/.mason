# Completions for cdp
_cdp_list() {
    if [ -n "$PROJECTS" ]; then
        list=$(ls --color=never $PROJECTS)
    else
        list=
    fi
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$list" $cur) )
}
complete -o default -F _cdp_list cdp
