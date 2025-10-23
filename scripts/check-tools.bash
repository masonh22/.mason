#!/usr/bin/env bash

# This checks for whether common tools I use are installed. Ansible would
# probably be better for this, but it's pretty heavy-duty.

essentials=(
    curl
    emacs
    git
    htop
)

extras=(
    bat
    fd
    rg
    fzf
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

exit $ret
