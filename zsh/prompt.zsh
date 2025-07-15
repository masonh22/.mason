#!/bin/zsh

# To add one or more prompts, write a function to add prompt strings to
# [prompts] (and optionally bracket colors to [brackets]), then add this
# generating function to prompt_gen.

# stores prompt strings
typeset -A prompts
# stores bracket colors (raw escape codes)
typeset -A brackets
# stores functions for generating prompts, i.e. adding strings to [prompts]
# and [brackets]
typeset -a prompt_gen

current_prompt_style=

__BRACKET_INIT='%f'
__BRACKET_DEFAULT=${__BRACKET_INIT}

# Set this to override the terminal title
MASON_TERM_TITLE="$(hostname -s)"

# Hook function executed before each prompt
__mason_prompt_precmd() {
    # Set terminal title
    if [[ -z "$INSIDE_EMACS" || "$INSIDE_EMACS" == 'vterm' ]]; then
        printf '\e]2;%s\a' "$MASON_TERM_TITLE"
    fi

    # Add a "Operating System Command" for the current directory (for Emacs)
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html
    printf '\e]7;file://%s%s\e\\' "$(hostname -s)" "${PWD}"
}

# Add to precmd_functions array if not already present
if (( ${#precmd_functions[@]} == 0 )) || (( ! ${precmd_functions[(Ie)__mason_prompt_precmd]} )); then
    precmd_functions+=(__mason_prompt_precmd)
fi

# Hook function executed before running a command
__mason_prompt_preexec() {
    local full_command="$1"
    local max_length=30
    local ellipsis="..."
    local truncated_cmd

    # If command is longer than max_length, truncate it
    if [[ ${#full_command} -gt $max_length ]]; then
        # Reserve space for ellipsis
        truncated_cmd="${full_command[1,$((max_length-${#ellipsis}))]}"
        truncated_cmd="${truncated_cmd}${ellipsis}"
    else
        truncated_cmd="$full_command"
    fi

    # Set the terminal title
    if [[ -z "$INSIDE_EMACS" || "$INSIDE_EMACS" == 'vterm' ]]; then
        printf '\e]2;%s %% %s\a' "$MASON_TERM_TITLE" "${truncated_cmd}"
    fi
}

# Add to preexec_functions array if not already present
if (( ${#preexec_functions[@]} == 0 )) || (( ! ${preexec_functions[(Ie)__mason_prompt_preexec]} )); then
    preexec_functions+=(__mason_prompt_preexec)
fi

_prompt_preview() {
    if (( $# < 1 )); then
        print -u2 "Usage: prompt preview <prompt name>"
        return 1
    fi
    local preview_prompt_name="$1"
    local preview_prompt="${prompts[$preview_prompt_name]}"

    if [[ -z "$preview_prompt" ]]; then
        print -u2 "Unknown prompt preset '$preview_prompt_name'"
        return 1
    fi

    local old_bracket="$__BRACKET_DEFAULT"
    local new_bracket="${brackets[$preview_prompt_name]}"

    if [[ -n "$new_bracket" ]]; then
        __BRACKET_DEFAULT="${new_bracket}"
    fi

    # print -P interprets Zsh prompt escape sequences like %n, %~, %{...%}
    # -r prevents backslash interpretation by print itself before -P processing.
    print -rP -- "${preview_prompt}"
    print

    __BRACKET_DEFAULT="$old_bracket"
}

prompt() {
    local usage='Usage: prompt [help|list|preview|regen|switch] ...'
    case "$1" in
        'help')
            print "$usage"
            return 0
            ;;
        'list'|'ls')
            local p
            # Iterate over keys of the associative array `prompts`
            for p in "${(@k)prompts}"; do
                print "$p:"
                _prompt_preview "$p"
            done
            return 0
            ;;
        'preview')
            shift
            _prompt_preview "$@" # Pass remaining arguments
            return $?
            ;;
        'regen')
            if [[ -z "${current_prompt_style}" ]]; then
                print -u2 'Cannot rebuild prompt: no previous style set.'
                return 2
            fi
            print "Regenerating prompt with style \"${current_prompt_style}\""
            # Recurse to 'prompt switch'
            prompt switch "${current_prompt_style}"
            return $?
            ;;
        'switch')
            shift
            # Fall through to main logic
            ;;
        '') # No arguments
            print "$usage"
            return 1
            ;;
        *) # Unknown option
            print "$usage"
            print -u2 "Unrecognized option '$1'"
            return 1
            ;;
    esac

    # Ensure all prompt generating functions are called
    local gen_func
    for gen_func in "${prompt_gen[@]}"; do
        # Execute the function stored in gen_func
        if ! "$gen_func"; then
            print -u2 "Prompt generator function '$gen_func' failed."
            return 1
        fi
    done

    local new_prompt_name="$1"
    if [[ -z "${prompts[$new_prompt_name]}" ]]; then
        print -u2 "Unknown prompt preset '$new_prompt_name'"
        if (( ${#prompts[@]} > 0 )); then
            print -u2 "Available presets: ${(@k)prompts}"
        else
            print -u2 "No presets are currently defined. Check your prompt generator functions."
        fi
        return 1
    fi

    current_prompt_style="$new_prompt_name"
    PROMPT="${prompts[$new_prompt_name]}" # Set the primary prompt string

    local new_bracket="${brackets[$new_prompt_name]}"
    if [[ -n "$new_bracket" ]]; then
        __BRACKET_DEFAULT="$new_bracket"
    else
        __BRACKET_DEFAULT="${__BRACKET_INIT}"
    fi

    if [[ "$INSIDE_EMACS" == 'vterm' ]]; then
        PROMPT=${PROMPT}'%{$(vterm_prompt_end)%}'
    fi
    return 0
}
_prompt_completion() {
    # `compstate` associative array holds completion context (e.g. current word index)
    # `words` array is 1-indexed, `CURRENT` is 1-based index of word to complete
    local -a actions_with_descriptions
    actions_with_descriptions=(
        'help:Show usage information'
        'list:List available prompt styles'
        'ls:Alias for list'
        'preview:Preview a prompt style'
        'regen:Regenerate the current prompt'
        'switch:Switch to a new prompt style'
    )

    # If completing the first argument to `prompt` (the action)
    if (( CURRENT == 2 )); then
        # _describe provides completion with descriptions
        _describe -t actions 'action' actions_with_descriptions
    # If completing the second argument (the prompt name)
    elif (( CURRENT == 3 )); then
        case "${words[2]}" in  # words[2] is the action (e.g., 'prompt switch <TAB>')
            'preview'|'switch')
                _values 'prompt style' ${(@k)prompts}
                ;;
        esac
    fi
    return 0 # Indicate success to completion system
}
compdef _prompt_completion prompt

# Utility function to generate common prompt components
gen_prompt_utils() {
    fancy_hostname="$(color_text "@$(hostname -s)")"

    if [[ -n "$IS_SSH" ]]; then
        optional_hostname="${fancy_hostname}"
    else
        optional_hostname=""
    fi

    if command -v opam >/dev/null 2>&1; then
      opam_switch='$(color_text "($(opam switch show 2>/dev/null || echo "no switch"))")'
    else
      opam_switch=''
    fi

    fancy_username="$(color_text "$(whoami)")"

    # TODO MASON use %j to detect background jobs!

    # Brackets: red if last command exited with non-zero status
    left_bracket='%(?.${__BRACKET_DEFAULT}.%F{red})[%f'
    right_bracket='%(?.${__BRACKET_DEFAULT}.%F{red})]%f'
}
prompt_gen+=(gen_prompt_utils) # Add function name to array

# Function to define Mason's prompts
prompt_mason() {
    local newline=$'\n'

    local clear_formatting='%f'
    local black='%F{black}'
    local bright_black='%F{8}'
    local red='%{red}'
    local bright_red='%F{9}'
    local green='%{green}'
    local bright_green='%F{10}'
    local yellow='%{yellow}'
    local bright_yellow='%F{11}'
    local blue='%{blue}'
    local bright_blue='%F{12}'
    local magenta='%{magenta}'
    local bright_magenta='%F{13}'
    local cyan='%{cyan}'
    local bright_cyan='%F{14}'
    local white='%{white}'
    local bright_white='%F{15}'

    local username='%n'
    local time_prompt='%D{%H:%M:%S}'  # Current time (HH:MM:SS)
    local dir_prompt='%~'   # Current directory (~ for $HOME, %d for full path)
    local prompt_char='%# ' # '#' for root, '%' for normal user (or $ if PROMPT_SP is unset)

    # Prefix vcs_info_msg_0_ with a space if it is non-empty
    local vcs_info='${vcs_info_msg_0_:+ ${vcs_info_msg_0_}}'

    prompts[colorless]="[${username} ${dir_prompt}]${prompt_char}" # No space before prompt_char
    prompts[minimal]="${prompts[colorless]}"

    prompts[simple]="[${fancy_username} ${dir_prompt}]${newline}${prompt_char}"

    prompts[normal]="${left_bracket}${bright_magenta}${time_prompt}${clear_formatting} ${fancy_username}${optional_hostname} ${bright_cyan}${dir_prompt}${clear_formatting}${right_bracket}${newline}${prompt_char}"

    prompts[mason]="${left_bracket}${bright_magenta}${time_prompt}${clear_formatting} ${fancy_username}${optional_hostname} ${bright_cyan}${dir_prompt}${clear_formatting}${vcs_info}${right_bracket}${newline}${prompt_char}"
    prompts[git]="${prompts[mason]}"
    prompts[default]="${prompts[mason]}" # Set 'default' to use this

    if command -v opam >/dev/null 2>&1; then
        prompts[ocaml]="${left_bracket}${bright_magenta}${time_prompt}${clear_formatting} ${opam_switch} ${fancy_username}${optional_hostname} ${bright_cyan}${dir_prompt}${clear_formatting}${vcs_info}${right_bracket}${newline}${prompt_char}"

        # 'full' prompt with fancy_hostname (which might be colored) instead of optional_hostname
        prompts[full]="${left_bracket}${bright_magenta}${time_prompt}${clear_formatting} ${opam_switch} ${fancy_username}${fancy_hostname} ${bright_cyan}${dir_prompt}${clear_formatting}${vcs_info}${right_bracket}${newline}${prompt_char}"
        brackets[full]="$bright_black"
    fi
}
prompt_gen+=(prompt_mason) # Add function name to array

################################################################################
# Configure vcs_info
autoload -Uz vcs_info

__mason_vcs_unstagedstr='%F{red}x%f:'

# Format the vcs_info_msg_0_ variable
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr "$__mason_vcs_unstagedstr"
zstyle ':vcs_info:*' stagedstr ':+'
zstyle ':vcs_info:git:*' formats '(%u%b%c%m)'
zstyle ':vcs_info:git:*' actionformats '(%a|%u%b%c%m)'

zstyle ':vcs_info:git*+set-message:*' hooks \
       git-st \
       git-color-branch \
       git-untracked \
       # intentionally blank

+vi-git-untracked() {
    if [ -z "${hook_com[unstaged]}" ] && [ -n "$(git status --porcelain 2>&1)" ]; then
        hook_com[unstaged]="$__mason_vcs_unstagedstr"
    fi
}

+vi-git-st() {
    local ahead behind
    local -a gitstatus

    # Exit early in case the worktree is on a detached HEAD
    git rev-parse ${hook_com[branch]}@{upstream} >/dev/null 2>&1 || return 0

    local -a ahead_and_behind=(
        $(git rev-list --left-right --count HEAD...${hook_com[branch]}@{upstream} 2>/dev/null)
    )

    ahead=${ahead_and_behind[1]}
    behind=${ahead_and_behind[2]}

    (( $ahead )) && gitstatus+=( ":%F{yellow}+${ahead}%f" )
    (( $behind )) && gitstatus+=( ":%F{yellow}-${behind}%f" )

    hook_com[misc]+="${(j:/:)gitstatus}"
}

+vi-git-color-branch() {
    hook_com[branch]="$(color_text ${hook_com[branch]})"
}

precmd_functions+=( vcs_info )
