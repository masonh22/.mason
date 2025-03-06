# Load required modules
autoload -Uz vcs_info

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '%F{red}x%f:'
zstyle ':vcs_info:*' stagedstr ':+'
zstyle ':vcs_info:git:*' formats '(%u%F{14}%b%f%c)'
zstyle ':vcs_info:git:*' actionformats '(%a|%u%F{14}%b%f%c)'

precmd_vcs_info() {
    vcs_info
}
precmd_functions+=( precmd_vcs_info )

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

    # Left prompt: [user<@hostname> directory]
    local left_prompt="${left_bracket}${bright_green}%n%f${hostname} ${bright_cyan}%~%f${right_bracket}"
    # Right prompt: time
    local right_prompt="${bright_magenta}%*%f "

    # Calculate spacing
    local zero='%([BSUbfksu]|([FK]|){*})'
    local left_size="${#${(S%%)left_prompt//$~zero/}}"
    local right_size="${#${(S%%)right_prompt//$~zero/}}"
    local space_size=$((COLUMNS - left_size - right_size))

    # Create the spacing string
    local spacing="${(l:$space_size:: :)}"

    print -rP "${left_prompt}${spacing}${right_prompt}"
}

PROMPT='%f%# '
RPROMPT='${vcs_info_msg_0_}%f'

precmd_functions+=( build_prompt )
