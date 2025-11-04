# NOTE: I did not author this file. I borrowed it from a coworker.

# Returns 0 iff the color is good
color_is_good_for_prompt() {
    input_color="$1"
    bad_colors=(0 7 15 16 17 18 19 20 195 187 224 225 230 231 232 233 234 235 236 237 238 239 240 241 242 252 253 254 255)
    for bad_color in ${bad_colors}; do
        if [ "${input_color}" = "${bad_color}" ]; then
            return 1 # hide it
        fi
    done
    return 0 # show it
}

good_color_for_number() {
    input_color=$1
    if color_is_good_for_prompt $input_color; then
        # the 38 (48) is for foreground (background), but not sure what the 5 is for
        echo -n "38;5;${input_color}"
        return
    else
        # Try agian by hashing the hash and the text.
        # This is deterministic, hacky, and different from the first color
        new_hash=$(cksum <<< "${text}${texthash}" | cut -d ' ' -f 1)
        new_color=$((${new_hash} % 256))
        if color_is_good_for_prompt $new_color; then
            echo -n "38;5;${new_color}"
        else
            # go back to the original hash
            # just use a basic color
            echo -n "$((32 + ${input_color} % 5))"
        fi
    fi
}

color_number() {
    text="$1"
    texthash=$(cksum <<< "${text}" | cut -d ' ' -f 1)
    echo "$((${texthash} % 256))"
}

choose_color() {
    text="$1"
    case $text in # special cases
        laptop)
            echo "38;5;146" # pale blue/purple
            ;;
        mason|mhall)
            echo "92" # bright green
            ;;
        root)
            echo "38;5;31" # red
            ;;
        *)
            texthash=$(cksum <<< ${text} | cut -d ' ' -f 1)
            good_color_for_number $((${texthash} % 256))
            ;;
    esac
}

color_text() {
    if [ -z "$1" ] ; then
        echo ""
        return 0
    fi
    echo -e "\e[$(choose_color $1)m$1\e[0m"
}

_show_color_with_name() {
    color_name=$1
    number=$2
    printf "%s => \e[%sm%s\e[0m\n" \
           $number $number $color_name
}

show_colors() {
    case $1 in
        basic)
            eight_bit_colors=(black red green yellow blue magenta cyan white)
            for i in $(seq 0 7); do
                _show_color_with_name ${eight_bit_colors[$i]} $((30+i))
            done
            sixteen_bit_colors=(bright_black bright_red bright_green bright_yellow bright_blue bright_magenta bright_cyan bright_white)
            for i in $(seq 0 7); do
                _show_color_with_name ${sixteen_bit_colors[$i]} $((90+i))
            done
            ;;
        expanded)
            for i in $(seq 0 255); do
                printf "\e[38;5;%dm %3d \e[0m" $i $i
                if [ $(($i % 10)) -eq 9 ]; then echo; fi
            done
            echo
            ;;
        cleaned)
            for i in $(seq 0 255); do
                printf "\e[%sm %3d \e[0m" \
                       $(good_color_for_number $i) $i
                if [ $(($i % 10)) -eq 9 ]; then echo; fi
            done
            echo
            ;;
        all)
            echo "basic:"
            show_colors basic
            echo "expanded:"
            show_colors expanded
            echo "cleaned:"
            show_colors cleaned
            ;;
        *)
            echo "Usage: show_all_colors [basic|expanded|cleaned|all]"
            ;;
    esac
}
