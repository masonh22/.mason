# To add one or more prompts, write a function to add prompt strings to
# [prompts] (and optionally bracket colors to [brackets]), then add this
# generating function to prompt_gen.

# stores prompt strings
declare -A prompts=()
# stores bracket colors
declare -A brackets=()
# stores functions for generating prompts, i.e. adding strings to [prompts]
# and [brackets]
declare -a prompt_gen=()

current_prompt_style=;

__BRACKET_INIT='\e[0m' # no formatting
__BRACKET_DEFAULT=${__BRACKET_INIT}

# wrapper to grab the exit code of the last command
__prompt_command() {
    __PROMPT_EXIT=$?

    # set terminal title
    if [ -z "$INSIDE_EMACS" ] || [ "$INSIDE_EMACS" = 'vterm' ]; then
        echo -ne "\033]0;${HOSTNAME}\007"
    fi

    # add a "Operating System Command" for the current directory
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html
    printf "\e]7;file://%s%s\e\\" "$HOSTNAME" "$PWD"
}
# We want our prompt command to go first so we can capture the return code of
# the last command
__MASON_PROMPT_COMMAND="__prompt_command${PROMPT_COMMAND:+;${PROMPT_COMMAND}}"
PROMPT_COMMAND="$__MASON_PROMPT_COMMAND"
unset __MASON_PROMPT_COMMAND

function prompt() {
    local usage='Usgae: prompt [help|list|preview|regen|switch] ...'
    case "$1" in
        'help')
            echo "$usage"
            return
            ;;
        'list'|'ls')
            # TODO: show all prompts, preview a prompt, list just the keys, etc.
            local p
            for p in "${!prompts[@]}"; do
                echo "$p:"
                printf '%s\n\n' "${prompts[$p]@P}"
            done
            return
            ;;
        'preview')
            shift
            if [ $# -lt 1 ]; then
                echo "$usage"
                return 1
            fi
            local preview_prompt="${prompts[$1]}"
            if [ -z "$preview_prompt" ]; then
                echo "Unknown prompt preset '$1'" 1>&2
                return 1
            fi
            printf '%s\n\n' "${preview_prompt@P}"
            return
            ;;
        'regen')
            if [ -z "${current_prompt_style}" ]; then
                >&2 echo 'Cannot rebuild prompt with previous style the first time prompt is called.'
                return 2
            fi
            echo "Regenerating prompt with style \"${current_prompt_style}\""
            prompt switch ${current_prompt_style}
            return
            ;;
        'switch')
            shift
            # fall through
            ;;
        '')
            echo "$usage"
            return 1
            ;;
        *)
            echo "$usage"
            echo "Unrecognized option '$1'"
            return 1
            ;;
    esac

    for gen_func in "${prompt_gen[@]}"; do
        $gen_func || return
    done

    local new_prompt="${prompts[$1]}"
    if [ -z "$new_prompt" ]; then
        >&2 echo "Unknown prompt preset '$1'"
        return 1
    fi
    current_prompt_style="$1"
    PS1="$new_prompt"
    local new_bracket="${brackets[$1]}"
    if [ -n "$new_bracket" ]; then
        __BRACKET_DEFAULT="$new_bracket"
    else
        __BRACKET_DEFAULT="${__BRACKET_INIT}"
    fi

    if [ "$INSIDE_EMACS" = 'vterm' ]; then
        PS1=${PS1}'\[$(vterm_prompt_end)\]'
    fi
}
function _prompt_list() {
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "${!prompts[*]}" $cur) )
}
complete -o default -F _prompt_list prompt

# useful things to include in prompts
gen_prompt_utils() {
    fancy_hostname="$(color_text @$(hostname | cut -d "." -f 1))"
    # only show hostname if this is an ssh session
    optional_hostname="$(if [ -n "${IS_SSH}" ]; then echo "${fancy_hostname}"; fi)"
    opam_switch='$(color_text "($(opam switch show))")'
    fancy_username="$(color_text $(whoami))"
    fancy_cursor='\e[?6;0;13;c'
    # change the bracket color depending on whether the last command failed
    bracket_color='if [ "$__PROMPT_EXIT" = "0" ]; then echo -e "${__BRACKET_DEFAULT}"; else echo -e "\e[0m\e[31m"; fi'
}
prompt_gen+=(gen_prompt_utils)

prompt_mason() {
    local bold='\[\e[1m\]'
    local unbold='\[\e[21m\]'
    local clear_formatting='\[\e[0m\]'
    local black='\[\e[30m\]'
    local bright_black='\[\e[90m\]'
    local red='\[\e[31m\]'
    local bright_red='\[\e[91m\]'
    local green='\[\e[32m\]'
    local bright_green='\[\e[92m\]'
    local yellow='\[\e[33m\]'
    local bright_yellow='\[\e[93m\]'
    local blue='\[\e[34m\]'
    local bright_blue='\[\e[94m\]'
    local magenta='\[\e[35m\]'
    local bright_magenta='\[\e[95m\]'
    local cyan='\[\e[36m\]'
    local bright_cyan='\[\e[96m\]'
    local white='\[\e[37m\]'
    local bright_white='\[\e[97m\]'

    local username='\u'
    local histname='\h'
    local current_time='\t'
    local current_dir='\w'

    prompts['colorless']="[${username} ${current_dir}] \$ "
    prompts['minimal']="${prompts['colorless']}"

    prompts['simple']="[${fancy_username} ${current_dir}]\n\$ "

    prompts['normal']="\$(${bracket_color})[${bright_magenta}${current_time} ${bright_cyan}${fancy_username}${optional_hostname} ${bright_cyan}${current_dir}\$(${bracket_color})]${clear_formatting}\n\$ "

    # TODO: maybe just condition git_prompt on whether git exists? that way it
    # can just be plugged into any other prompt (same with opam/ocaml)
    if [ -x "$(command -v git)" ]; then
        prompts['mason']="\$(${bracket_color})[${bright_magenta}${current_time} ${fancy_username}${optional_hostname} ${bright_cyan}${current_dir}\$(git_prompt)\$(${bracket_color})]${clear_formatting}\n\$ "
        prompts['git']="${prompts['mason']}"
        prompts['default']="${prompts['mason']}"

        if [ -x "$(command -v opam)" ]; then
            prompts['ocaml']="\$(${bracket_color})[${bright_magenta}${current_time} ${opam_switch} ${fancy_username}${optional_hostname} ${bright_cyan}${current_dir}\$(git_prompt)\$(${bracket_color})]${clear_formatting}\n\$ "

            # same as opam but with hostname
            prompts['full']="\$(${bracket_color})[${bright_magenta}${current_time} ${opam_switch} ${fancy_username}${fancy_hostname} ${bright_cyan}${current_dir}\$(git_prompt)\$(${bracket_color})]${clear_formatting}\n\$ "
            brackets['full']='\e[90m' # bright black
        fi # opam
    fi # git
}
prompt_gen+=(prompt_mason)

if [ -x "$(command -v git)" ]; then
    prompt switch git > /dev/null
else
    prompt switch simple > /dev/null
fi
