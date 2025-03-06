# Configure vcs_info
autoload -Uz vcs_info

__mason_vcs_unstagedstr='%F{red}x%f:'

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr "$__mason_vcs_unstagedstr"
zstyle ':vcs_info:*' stagedstr ':+'
zstyle ':vcs_info:git:*' formats '(%u%b%c)'
zstyle ':vcs_info:git:*' actionformats '(%a|%u%b%c)'

zstyle ':vcs_info:git*+set-message:*' hooks git-color-branch git-untracked

+vi-git-color-branch() {
    hook_com[branch]="$(color_text ${hook_com[branch]})"
}

+vi-git-untracked() {
    if [ -z "${hook_com[unstaged]}" ] && [ -n "$(git status --porcelain 2>&1)" ]; then
        hook_com[unstaged]="$__mason_vcs_unstagedstr"
    fi
}

precmd_functions+=( vcs_info )

# Enable prompt substitution
setopt prompt_subst

# Function to build the prompt
build_prompt() {
    local NEWLINE=$'\n'

    local bright_black='%F{8}'
    local bright_red='%F{9}'
    local bright_green='%F{10}'
    local bright_yellow='%F{11}'
    local bright_blue='%F{12}'
    local bright_magenta='%F{13}'
    local bright_cyan='%F{14}'
    local bright_white='%F{15}'

    # Brackets: red if last command exited with non-zero status
    local left_bracket='%(?..%F{red})[%f'
    local right_bracket='%(?..%F{red})]%f'

    local hostname='$(if [ -n "${IS_SSH}" ]; then echo "$(color_text @$(hostname | cut -d "." -f 1))"; fi)'

    # Prefix vcs_info_msg_0_ with a space if it is non-empty
    local vcs_info='${vcs_info_msg_0_:+ ${vcs_info_msg_0_}}'

    # Base prompt: user<@hostname> directory
    local base_prompt="${bright_green}%n%f${hostname} ${bright_cyan}%~%f"
    # Inner prompt: time user<@hostname> directory vcs_info
    local inner_prompt="${bright_magenta}%*%f ${base_prompt}${vcs_info}%f"
    # Full prompt: adds brackets
    local full_prompt="${left_bracket}${inner_prompt}${right_bracket}"
    PROMPT="${full_prompt}${NEWLINE}%# "

    if [ "$INSIDE_EMACS" = 'vterm' ]; then
        PROMPT=${PROMPT}'%{$(vterm_prompt_end)%}'
    fi
}

build_prompt
