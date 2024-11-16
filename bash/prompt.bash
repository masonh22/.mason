current_prompt_style=""

__PS1=${PS1}
__BRACKET_DEFAULT='\e[0m' # no formatting

# wrapper to grab the exit code of the last command
__prompt_command() {
    __PROMPT_EXIT=$?

    # set terminal title
    if [ -z "$INSIDE_EMACS" ]; then
        echo -ne "\033]0;${HOSTNAME}\007"
    fi

    # add a "Operating System Command" for the current directory
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html
    printf "\e]7;file://%s%s\e\\" "$HOSTNAME" "$PWD"

    PS1=${__PS1}
}
PROMPT_COMMAND=__prompt_command

function prompt() {
    bold='\[\e[1m\]'
    unbold='\[\e[21m\]'
    clear_formatting='\[\e[0m\]'
    black='\[\e[30m\]'
    bright_black='\[\e[90m\]'
    red='\[\e[31m\]'
    bright_red='\[\e[91m\]'
    green='\[\e[32m\]'
    bright_green='\[\e[92m\]'
    yellow='\[\e[33m\]'
    bright_yellow='\[\e[93m\]'
    blue='\[\e[34m\]'
    bright_blue='\[\e[94m\]'
    magenta='\[\e[35m\]'
    bright_magenta='\[\e[95m\]'
    cyan='\[\e[36m\]'
    bright_cyan='\[\e[96m\]'
    white='\[\e[37m\]'
    bright_white='\[\e[97m\]'

    username='\u'
    histname='\h'
    current_time='\t'
    current_dir='\w'

    fancy_hostname="$(if [ -n "${IS_SSH}" ]; then echo "$(color_text @$(hostname | cut -d "." -f 1))"; fi)"
    opam_switch='$(color_text "($(opam switch show))")'
    fancy_username="$(color_text $(whoami))"
    fancy_cursor='\e[?6;0;13;c'
    # change the bracket color depending on whether the last command failed
    bracket_color='if [ "$__PROMPT_EXIT" = "0" ]; then echo -e "${__BRACKET_DEFAULT}"; else echo -e "\e[0m\e[31m"; fi'

    if [ -z "$1" ]; then
        if [ -z "${current_prompt_style}" ]; then
            >&2 echo "Cannot rebuild prompt with previous style the first time build_prompt is called."
            return 2
        fi
        echo "Regenerating prompt with style \"${current_prompt_style}\""
        build_prompt ${current_prompt_style}
        return
    fi

    case "$1" in
        colorless)
            __PS1="[${username} ${current_dir}] \$ "
            ;;
        simple)
            __PS1="[${fancy_username} ${current_dir}]\n\$ "
            ;;
        git|default|mason)
            __BRACKET_DEFAULT='\e[0m' # no formatting
            __PS1="\$(${bracket_color})[${bright_magenta}${current_time} ${fancy_username}${fancy_hostname} ${bright_cyan}${current_dir}\$(git_prompt)\$(${bracket_color})]${clear_formatting}\n\$ "
            ;;
        opam|ocaml)
            __BRACKET_DEFAULT='\e[90m' # bright black
            __PS1="\$(${bracket_color})[${bright_magenta}${current_time} ${opam_switch} ${fancy_username} ${bright_cyan}${current_dir}\$(git_prompt)\$(${bracket_color})]${clear_formatting}\n\$ "
            ;;
        full)
            # same as opam but with hostname
            __BRACKET_DEFAULT='\e[90m' # bright black
            __PS1="\$(${bracket_color})[${bright_magenta}${current_time} ${opam_switch} ${fancy_username}${fancy_hostname} ${bright_cyan}${current_dir}\$(git_prompt)\$(${bracket_color})]${clear_formatting}\n\$ "
            ;;
        *)
            >&2 echo "Unknown prompt preset \"$1\""
            return 1
            ;;
    esac

    current_promt_style=$1
}
function _prompt_list() {
    local cur=${COMP_WORDS[COMP_CWORD]}
    export prompts="colorless simple default ocaml full"
    COMPREPLY=( $(compgen -W "$prompts" $cur) )
}
complete -o default -F _prompt_list prompt

if [ -x "$(command -v git)" ]; then
    prompt git > /dev/null
else
    prompt simple > /dev/null
fi
