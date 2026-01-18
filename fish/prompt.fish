# To add one or more prompts, write a function to add prompt strings to using
# [add_prompt] (and optionally bracket colors using [add_bracket]), then add
# this generating function to prompt_gen.

# stores prompt keys (emulating keys of an associative array)
set -g __prompt_keys
# stores bracket keys (emulating keys of an associative array)
set -g __bracket_keys

# Helper to register prompts
function add_prompt
    set -l key $argv[1]
    set -l val $argv[2]
    set -g __prompt_$key $val
    if not contains $key $__prompt_keys
        set -a __prompt_keys $key
    end
end

# Helper to register brackets
function add_bracket
    set -l key $argv[1]
    set -l val $argv[2]
    set -g __bracket_$key $val
    if not contains $key $__bracket_keys
        set -a __bracket_keys $key
    end
end

# stores functions for generating prompts, i.e. adding strings to [prompts]
# and [brackets]
set -g prompt_gen

set -g current_prompt_style

set -g __BRACKET_INIT '\e[0m' # no formatting
set -g __BRACKET_DEFAULT $__BRACKET_INIT

# wrapper to grab the exit code of the last command TODO MASON move this...
function __prompt_command
    set -g __PROMPT_EXIT $argv[1]

    # set terminal title
    if not set -q INSIDE_EMACS; or test "$INSIDE_EMACS" = 'vterm'
        echo -ne "\033]0;"(hostname)"\007"
    end

    # add a "Operating System Command" for the current directory
    # https://www.gnu.org/software/emacs/manual/html_node/emacs/Interactive-Shell.html
    printf "\e]7;file://%s%s\e\\" (hostname) "$PWD"
end

function _prompt_preview
    if test (count $argv) -lt 1
        echo "Usage: prompt preview <prompt name>"
        return 1
    end
    set -l target $argv[1]

    # Check if key exists
    if not contains $target $__prompt_keys
        echo "Unknown prompt preset '$target'" 1>&2
        return 1
    end

    set -l old_bracket "$__BRACKET_DEFAULT"
    set -l new_bracket __mason_brackets_$target
    if set -q $bracket_var
        set -g __BRACKET_DEFAULT $$bracket_var
    end

    # Emulate the prompt execution
    set -l prompt_var __prompt_$target
    printf '%s\n\n' "$$prompt_var"

    set -g __BRACKET_DEFAULT "$old_bracket"
end

function prompt
    set -l usage 'Usage: prompt [help|list|preview|regen|switch] ...'
    set -l cmd $argv[1]

    switch "$cmd"
        case 'help'
            echo "$usage"
            return
        case 'list' 'ls'
            for p in $__prompt_keys
                echo "$p:"
                _prompt_preview "$p"
            end
            return
        case 'preview'
            _prompt_preview $argv[2..-1]
            return $status
        case 'regen'
            if test -z "$current_prompt_style"
                echo 'Cannot rebuild prompt: no previous style set.' 1>&2
                return 2
            end
            echo "Regenerating prompt with style \"$current_prompt_style\""
            prompt switch $current_prompt_style
            return $status
        case 'switch'
            # fall through
        case ''
            echo "$usage"
            return 1
        case '*'
            echo "$usage"
            echo "Unrecognized option '$cmd'"
            return 1
    end

    set -l target $argv[2]

    # Run generators
    for gen_func in $prompt_gen
        eval $gen_func || return
    end

    # Check existence
    if not contains $target $__prompt_keys
        echo "Unknown prompt preset '$target'" 1>&2
        return 1
    end

    set -g current_prompt_style "$target"

    set -l bracket_var __mason_brackets_$target
    if set -q $bracket_var
        set -g __BRACKET_DEFAULT $$bracket_var
    else
        set -g __BRACKET_DEFAULT $__BRACKET_INIT
    end
end

# Fish completion for prompt command
function _prompt_list
    set -l verbs help list preview regen switch
    if test (count $argv) -eq 1
        printf "%s\n" $verbs
    else if contains $argv[1] preview switch
        printf "%s\n" $__prompt_keys
    end
end
complete -c prompt -f -a '(_prompt_list (commandline -opc)[2..-1])'

# useful things to include in prompts
function gen_prompt_utils
    set -g fancy_hostname (color_text "@"(hostname -s))

    if set -q SSH_TTY
        set -g optional_hostname "$fancy_hostname"
    else
        set -g optional_hostname ""
    end

    set -g opam_switch ""
    if command -v opam > /dev/null
        # set opam_switch (echo -n "("; color_text (opam switch show 2>/dev/null || echo "no switch"); echo -n ")")
        set opam_switch '(color_text "("(opam switch show 2>/dev/null || echo "no switch")")")'
    end

    set -g fancy_username (color_text $USER)

    set -g bracket_color 'if test "$__PROMPT_EXIT" = "0"; echo -e "$__BRACKET_DEFAULT"; else; echo -e "\e[0m\e[31m"; end'
end
set -a prompt_gen gen_prompt_utils

function prompt_mason
    set -l bold '\[\e[1m\]'
    set -l unbold '\[\e[21m\]'
    set -l clear_formatting '\[\e[0m\]'
    set -l black '\[\e[30m\]'
    set -l bright_black '\[\e[90m\]'
    set -l red '\[\e[31m\]'
    set -l bright_red '\[\e[91m\]'
    set -l green '\[\e[32m\]'
    set -l bright_green '\[\e[92m\]'
    set -l yellow '\[\e[33m\]'
    set -l bright_yellow '\[\e[93m\]'
    set -l blue '\[\e[34m\]'
    set -l bright_blue '\[\e[94m\]'
    set -l magenta '\[\e[35m\]'
    set -l bright_magenta '\[\e[95m\]'
    set -l cyan '\[\e[36m\]'
    set -l bright_cyan '\[\e[96m\]'
    set -l white '\[\e[37m\]'
    set -l bright_white '\[\e[97m\]'

    set -l username "$USER"
    set -l current_time '(date "+%H:%M:%S")'
    set -l current_dir '$PWD'

    add_prompt 'colorless' 'echo [$username $current_dir] > '
    add_prompt 'minimal' "$__prompt_colorless"

    add_prompt 'simple' "echo -n '['; echo -n \"$fancy_username\"; echo -n ' '; prompt_pwd; echo -n ']'; echo; echo -n '> '"

    add_prompt 'normal' "eval \$bracket_color; echo -n '['; $bright_magenta; $current_time; echo -n ' '; $clear_formatting; echo -n \"$fancy_username\$optional_hostname\"; echo -n ' '; $bright_cyan; prompt_pwd; $clear_formatting; eval \$bracket_color; echo -n ']'; $clear_formatting; echo; echo -n '> '"

    if command -v git > /dev/null
        set -l git_prompt 'fish_git_prompt'

        add_prompt 'mason' "eval \$bracket_color; echo -n '['; $bright_magenta; $current_time; echo -n ' '; $clear_formatting; echo -n \"$fancy_username\$optional_hostname\"; echo -n ' '; $bright_cyan; prompt_pwd; $clear_formatting; $git_prompt; eval \$bracket_color; echo -n ']'; $clear_formatting; echo; echo -n '> '"

        add_prompt 'git' 'eval $__prompt_mason'
        add_prompt 'default' 'eval $__prompt_mason'

        if command -v opam > /dev/null
            add_prompt 'ocaml' "eval \$bracket_color; echo -n '['; $bright_magenta; $current_time; echo -n ' '; $clear_formatting; echo -n \"$opam_switch\"; echo -n ' '; echo -n \"$fancy_username\$optional_hostname\"; echo -n ' '; $bright_cyan; prompt_pwd; $clear_formatting; $git_prompt; eval \$bracket_color; echo -n ']'; $clear_formatting; echo; echo -n '> '"

            add_prompt 'full' "eval \$bracket_color; echo -n '['; $bright_magenta; $current_time; echo -n ' '; $clear_formatting; echo -n \"$opam_switch\"; echo -n ' '; echo -n \"$fancy_username\$fancy_hostname\"; echo -n ' '; $bright_cyan; prompt_pwd; $clear_formatting; $git_prompt; eval \$bracket_color; echo -n ']'; $clear_formatting; echo; echo -n '> '"

            add_bracket 'full' \"$bright_black\"
        end
    end

    if not contains 'default' $__prompt_keys
         set -l simple_val $__prompt_simple
         add_prompt 'default' "$simple_val"
    end
end
set -a prompt_gen prompt_mason

# --- Main Fish Prompt Hook ---

function fish_prompt --description 'Write out the prompt'
    set -l last_status $status
    __prompt_command $last_status

    if test -z "$current_prompt_style"
        prompt switch simple
    end

    set -l prompt_var __prompt_$current_prompt_style
    if set -q $prompt_var
         eval $$prompt_var
    else
         echo -n "> "
    end

    if functions -q vterm_prompt_end
        vterm_prompt_end
    end
end
