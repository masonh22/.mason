#!/usr/bin/env bash

# generate completions from an input program that outputs completion information
# that can be parsed by this program.
#
# The format is:
# item1,item2,item3
# item1:item1a,item1b,item1c
# item2:item2a,item2b
# item1,item1a:item1a1,item1a2
# item1,item1c:item1c1
# item2,item2b:item2b1
#
# If '-' is provided as the third positional argument, we will read the
# completions from stdin.

usage="$0 <zsh|bash> <program-name> [-]"

error() {
    echo "$@" 1>&2
}

gen_zsh() {
    echo "__${program_name}_completion() {"

    # start the if/else chain
    echo 'if false; then :;'

    for line in "${completions[@]}"; do
        prefix=$(echo "${line%%:*}" | tr ',' ' ')
        matches=$(echo "${line#*:}" | tr ',' ' ')
        if [ "$prefix" = "$matches" ]; then
            # no prefix in this case since there was no ':'
            echo 'elif (( CURRENT == 2 )); then'
            echo "_values 'action' $matches"
        else
            len=$(echo "$prefix" | wc -w)
            depth=$((len + 2))
            echo "elif (( CURRENT == $depth )) && [ \"\${words[*]:1:$len}\" = '$prefix' ]; then"
            echo "_values 'action' $matches"
        fi
    done

    echo 'fi'

    echo '}'

    echo "compdef __${program_name}_completion ${program_name}"
}

gen_bash() {
    echo "__${program_name}_completion() {"

    echo 'local cur prev words cword'
    echo '_init_completion || return'

    # start the if/else chain
    echo 'if false; then :;'

    for line in "${completions[@]}"; do
        prefix=$(echo "${line%%:*}" | tr ',' ' ')
        matches=$(echo "${line#*:}" | tr ',' ' ')
        if [ "$prefix" = "$matches" ]; then
            # no prefix in this case since there was no ':'
            echo 'elif [ "$cword" -eq 1 ]; then'
            echo "COMPREPLY=(\$(compgen -W \"$matches\" -- \"\$cur\"))"
        else
            len=$(echo "$prefix" | wc -w)
            depth=$((len + 1))
            echo "elif [ \"\$cword\" -eq $depth ] && [ \"\${words[*]:1:$len}\" = '$prefix' ]; then"
            echo "COMPREPLY=(\$(compgen -W \"$matches\" -- \"\$cur\"))"
        fi
    done

    echo 'fi'

    echo '}'

    echo "complete -F __${program_name}_completion ${program_name}"
}

main() {
    if [ $# -ne 2 ] && [ $# -ne 3 ]; then
        error "$usage"
        exit 1
    fi

    shell="$1"
    program_name="$2"
    use_stdin="$3"
    if [ "$use_stdin" = - ]; then
        completions=($(cat))
        ret=0
    else
        completions=($($program_name completions 2> /dev/null))
        ret=$?
    fi
    if [ $ret -ne 0 ]; then
        error "$program_name completions exited with code $ret"
        exit $ret
    fi

    case "$shell" in
        bash)
            gen_bash
            ;;
        zsh)
            gen_zsh
            ;;
        *)
            error "$usage"
            error "unknown shell '$shell'"
            exit 2
            ;;
    esac
}

main "$@"
