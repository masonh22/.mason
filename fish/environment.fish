# Transpiled from shell/environment.sh

# SSH Check
if test -n "$SSH_TTY"
    set -gx IS_SSH true
    set -gx COLORTERM truecolor
end

set -gx GCC_COLORS 'error=01;31:warning=01;35:note=01;36:caret=01;31:locus=01:quote=01'
set -gx GREP_COLORS "mt=01;32"

if type -q emacs
    set -gx EDITOR 'emacsclient --alternate-editor="" -nw'
else
    set -gx EDITOR nano
end

set -gx VERSION_CONTROL "never"
set -gx CMAKE_EXPORT_COMPILE_COMMANDS 1

if test -d "$HOME/Projects"
    set -gx PROJECTS "$HOME/Projects"
end

# Only do next part once
if set -q MASON_ENV_INIT_COMPLETE
    exit
end

# Cargo (Rust)
if test -r "$HOME/.cargo/env.fish"
    source "$HOME/.cargo/env.fish"
else if test -d "$HOME/.cargo/bin"
    fish_add_path "$HOME/.cargo/bin"
end
set -gx CARGO_INSTALL_ROOT "$HOME/.local"

# Elan (Lean)
if test -d "$HOME/.elan/bin"
    fish_add_path "$HOME/.elan/bin"
end

# Opam (OCaml)
if type -q opam
    if test -r "$HOME/.opam/opam-init/init.fish"
        source "$HOME/.opam/opam-init/init.fish" > /dev/null 2>&1; or true
    end
end

# Kubectl
if type -q kubectl
    kubectl completion fish | source
end

# PATH handling
if test -d "$MASON_HOME/bin"
    fish_add_path "$MASON_HOME/bin"
end

if test -d "$HOME/bin"
    fish_add_path "$HOME/bin"
end

if test -d "$HOME/.local/bin"
    fish_add_path "$HOME/.local/bin"
end

set -gx MASON_ENV_INIT_COMPLETE true
