#!/usr/bin/env bash

# This checks for whether common tools I use are installed. Ansible would
# probably be better for this, but it's pretty heavy-duty.

essentials=(
    alacritty
    curl
    emacs
    git
    htop
    zsh
)

extras=(
    bat
    fd
    fzf
    rg
    tailscale
)

languages=(
    python3
    pip3

    opam

    rustup
    cargo
    rustc
)

ret=0

for program in "${essentials[@]}" "${extras[@]}" "${languages[@]}"; do
    if [ ! -x "$(command -v "$program")" ]; then
        echo "Missing '$program'" >&2
        ret=1
    fi
done

# Check whether zsh is the default shell
if [ "$(basename $SHELL)"  != "zsh" ]; then
    echo "Default shell is $(basename $SHELL), change it to zsh with:" >&2
    echo '  chsh -s $(which zsh)' >&2
    ret=2
fi

exit $ret
