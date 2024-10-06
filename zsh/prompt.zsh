# Git integration
# https://git-scm.com/book/en/v2/Appendix-A%3A-Git-in-Other-Environments-Git-in-Zsh
autoload -Uz vcs_info
function precmd_vcs_info { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats '(%b)'

# TODO integrate colors.bash
function prompt {
    local NEWLINE=$'\n'

    local username='%F{green}%n%f'
    local hostname="$(if [ -n "${IS_SSH}" ]; then echo '%F{yellow}@%m%f'; fi)"
    local left_bracket='%(?..%F{red})[%f'
    local right_bracket='%(?..%F{red})]%f'
    local timestamp='%F{magenta}%*%f'
    local dir='%F{cyan}%~%f'

    PROMPT="${left_bracket}${timestamp} ${username}${hostname} ${dir}${right_bracket}${NEWLINE}%# "
    RPROMPT='${vcs_info_msg_0_} ${timestamp}'
}

prompt
