# Git integration
# https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Zsh
autoload -Uz vcs_info
function precmd_vcs_info { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '(%b)'

function prompt {
    local NEWLINE=$'\n'

    local bright_magenta="$(echo -e '\e[95m')"
    local bright_cyan="$(echo -e '\e[96m')"

    local username="$(color_text $(whoami))"
    local hostname="$(if [ -n "${IS_SSH}" ]; then echo "$(color_text @$(hostname | cut -d "." -f 1))"; fi)"
    local left_bracket='%(?..%F{red})[%f'
    local right_bracket='%(?..%F{red})]%f'
    local timestamp="${bright_magenta}%*%f"
    local dir="${bright_cyan}%~%f"

    PROMPT="${left_bracket}${timestamp} ${username}${hostname} ${dir}${right_bracket}${NEWLINE}%# "
    RPROMPT='${vcs_info_msg_0_} ${timestamp}'
}

prompt
