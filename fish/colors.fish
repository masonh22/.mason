# Translated to fish from shell/colors.sh

# Returns 0 iff the color is good
function color_is_good_for_prompt
    set -l input_color $argv[1]
    set -l bad_colors 0 7 15 16 17 18 19 20 195 187 224 225 230 231 232 233 234 235 236 237 238 239 240 241 242 252 253 254 255
    if contains -- $input_color $bad_colors
        return 1 # hide it
    end
    return 0 # show it
end

function choose_good_color
    set -l text $argv[1]
    set -l texthash (echo "$text" | cksum | cut -d ' ' -f 1)
    set -l input_color (math "$texthash % 256")
    if color_is_good_for_prompt $input_color
        # the 38 (48) is for foreground (background), but not sure what the 5 is for
        echo -n "38;5;$input_color"
        return
    else
        # Try again by hashing the hash and the text.
        # This is deterministic, hacky, and different from the first color
        set -l new_hash (echo "$text$texthash" | cksum | cut -d ' ' -f 1)
        set -l new_color (math "$new_hash % 256")
        if color_is_good_for_prompt $new_color
            echo -n "38;5;$new_color"
        else
            # go back to the original hash just use a basic color
            echo -n (math "32 + $input_color % 5")
        end
    end
end

function color_number
    set -l text $argv[1]
    set -l texthash (echo "$text" | cksum | cut -d ' ' -f 1)
    echo (math "$texthash % 256")
end

function choose_color
    set -l text $argv[1]
    switch $text
        case laptop
            echo "38;5;146" # pale blue/purple
        case mason mhall
            echo "92" # bright green
        case root
            echo "38;5;31" # red
        case '*'
            choose_good_color "$text"
    end
end

function color_text
    if test -z "$argv[1]"
        echo ""
        return 0
    end
    printf "\e[%sm%s\e[0m" (choose_color $argv[1]) $argv[1]
end

function _show_color_with_name
    set -l color_name $argv[1]
    set -l number $argv[2]
    printf "%s => \e[%sm%s\e[0m\n" $number $number $color_name
end

function show_colors
    set -l mode $argv[1]
    switch $mode
        case basic
            set -l eight_bit_colors black red green yellow blue magenta cyan white
            for i in (seq 0 7)
                # Fish arrays are 1-based
                _show_color_with_name $eight_bit_colors[(math "$i + 1")] (math "30+$i")
            end
            set -l sixteen_bit_colors bright_black bright_red bright_green bright_yellow bright_blue bright_magenta bright_cyan bright_white
            for i in (seq 0 7)
                _show_color_with_name $sixteen_bit_colors[(math "$i + 1")] (math "90+$i")
            end
        case expanded
            for i in (seq 0 255)
                printf "\e[38;5;%dm %3d \e[0m" $i $i
                if test (math "$i % 10") -eq 9
                    echo
                end
            end
            echo
        case all
            echo "basic:"
            show_colors basic
            echo "expanded:"
            show_colors expanded
        case '*'
            echo "Usage: show_colors [basic|expanded|all]"
    end
end