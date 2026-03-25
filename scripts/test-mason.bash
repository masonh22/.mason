#!/usr/bin/env bash
# Test suite for mason, mason-lib, and install-utils
# Runs in an isolated temp directory to avoid touching real config.

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REAL_MASON_HOME="$(cd "$SCRIPT_DIR/.." && pwd)"

# --- test framework ---

tests_run=0
tests_passed=0
tests_failed=0
current_test=""

pass() {
    tests_passed=$((tests_passed + 1))
    printf "  \033[00;32mPASS\033[0m %s\n" "$current_test"
}

fail_test() {
    tests_failed=$((tests_failed + 1))
    printf "  \033[0;31mFAIL\033[0m %s\n" "$current_test"
    printf "    %s\n" "$1"
}

run_test() {
    current_test="$1"
    tests_run=$((tests_run + 1))
}

assert_eq() {
    if [ "$1" = "$2" ]; then
        return 0
    else
        fail_test "expected '$2', got '$1'"
        return 1
    fi
}

assert_contains() {
    if echo "$1" | grep -qF "$2"; then
        return 0
    else
        fail_test "expected output to contain '$2'"
        return 1
    fi
}

assert_not_contains() {
    if ! echo "$1" | grep -qF "$2"; then
        return 0
    else
        fail_test "expected output NOT to contain '$2'"
        return 1
    fi
}

assert_file_exists() {
    if [ -e "$1" ]; then
        return 0
    else
        fail_test "expected '$1' to exist"
        return 1
    fi
}

assert_file_not_exists() {
    if [ ! -e "$1" ]; then
        return 0
    else
        fail_test "expected '$1' to not exist"
        return 1
    fi
}

assert_symlink_to() {
    local link="$1" expected_target="$2"
    if [ -L "$link" ]; then
        local actual
        actual="$(readlink "$link")"
        if [ "$actual" = "$expected_target" ]; then
            return 0
        else
            fail_test "'$link' points to '$actual', expected '$expected_target'"
            return 1
        fi
    else
        fail_test "'$link' is not a symlink"
        return 1
    fi
}

# --- test sandbox setup ---

SANDBOX="$(mktemp -d)"
FAKE_HOME="$SANDBOX/home"
FAKE_MASON="$SANDBOX/mason"
FAKE_CONFIG="$FAKE_HOME/.config"

setup_sandbox() {
    rm -rf "$SANDBOX"
    mkdir -p "$FAKE_HOME" "$FAKE_CONFIG" "$FAKE_MASON"

    # Copy scripts into fake mason home
    cp "$REAL_MASON_HOME/install-utils" "$FAKE_MASON/install-utils"
    cp "$REAL_MASON_HOME/mason" "$FAKE_MASON/mason"
    chmod +x "$FAKE_MASON/mason"

    # Create fake config directories with files
    mkdir -p "$FAKE_MASON/config/app1"
    echo "config1" > "$FAKE_MASON/config/app1/settings.conf"
    echo "config2" > "$FAKE_MASON/config/app1/theme.conf"

    mkdir -p "$FAKE_MASON/config/app2"
    echo "config3" > "$FAKE_MASON/config/app2/main.conf"

    # Create fake emacs dir
    mkdir -p "$FAKE_MASON/emacs"
    echo ";; early-init" > "$FAKE_MASON/emacs/early-init.el"

    # Create a .symlink.home file
    echo "symlink content" > "$FAKE_MASON/.testrc.symlink.home"
}

cleanup_sandbox() {
    rm -rf "$SANDBOX"
}

trap cleanup_sandbox EXIT

# --- install-utils shared function tests ---

printf "\n\033[1minstall-utils (shared functions)\033[0m\n"

setup_sandbox

run_test "installable_dst computes home destination"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; installable_dst "$MASON_HOME/.testrc.symlink.home" symlink')"
assert_eq "$result" "$FAKE_HOME/.testrc" && pass

run_test "installable_dst computes non-home destination"
# Create a fake file with a subdirectory target
touch "$FAKE_MASON/.foo.symlink.subdir"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; installable_dst "$MASON_HOME/.foo.symlink.subdir" symlink')"
assert_eq "$result" "$FAKE_HOME/subdir/.foo" && pass

run_test "installable_dst works for copy kind"
touch "$FAKE_MASON/.bashrc.copy.home"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; installable_dst "$MASON_HOME/.bashrc.copy.home" copy')"
assert_eq "$result" "$FAKE_HOME/.bashrc" && pass

run_test "find_installable finds symlink files"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; find_installable symlink')"
assert_contains "$result" ".testrc.symlink.home" && pass

run_test "find_installable finds copy files"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; find_installable copy')"
assert_contains "$result" ".bashrc.copy.home" && pass

run_test "get_emacs_home returns .config/emacs when it exists"
mkdir -p "$FAKE_HOME/.config/emacs"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; get_emacs_home')"
assert_eq "$result" "$FAKE_HOME/.config/emacs" && pass

run_test "get_emacs_home returns .emacs.d when only that exists"
setup_sandbox
mkdir -p "$FAKE_HOME/.emacs.d"
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; get_emacs_home')"
assert_eq "$result" "$FAKE_HOME/.emacs.d" && pass

run_test "get_emacs_home returns empty when neither exists"
setup_sandbox
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; get_emacs_home')"
assert_eq "$result" "" && pass

run_test "get_emacs_home --create creates .config/emacs"
setup_sandbox
result="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; get_emacs_home --create')"
assert_eq "$result" "$FAKE_HOME/.config/emacs" && assert_file_exists "$FAKE_HOME/.config/emacs" && pass

run_test "CONFIG_HOME defaults to \$HOME/.config"
result="$(unset XDG_CONFIG_HOME; HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; echo "$CONFIG_HOME"')"
assert_eq "$result" "$FAKE_HOME/.config" && pass

run_test "CONFIG_HOME respects XDG_CONFIG_HOME"
result="$(HOME="$FAKE_HOME" XDG_CONFIG_HOME="/custom/config" MASON_HOME="$FAKE_MASON" bash -c '. "$MASON_HOME/install-utils" "$MASON_HOME"; echo "$CONFIG_HOME"')"
assert_eq "$result" "/custom/config" && pass

run_test "install-utils requires an argument"
result="$(bash -c '. "'"$FAKE_MASON"'/install-utils"' 2>&1)" || true
assert_contains "$result" "must be given the root" && pass

# --- mason health tests ---

printf "\n\033[1mmason health\033[0m\n"

setup_sandbox
mkdir -p "$FAKE_HOME/.config/emacs"

# Install symlinks to simulate a working installation
mkdir -p "$FAKE_CONFIG/app1" "$FAKE_CONFIG/app2"
ln -s "$FAKE_MASON/config/app1/settings.conf" "$FAKE_CONFIG/app1/settings.conf"
ln -s "$FAKE_MASON/config/app1/theme.conf" "$FAKE_CONFIG/app1/theme.conf"
ln -s "$FAKE_MASON/config/app2/main.conf" "$FAKE_CONFIG/app2/main.conf"
ln -s "$FAKE_MASON/emacs/early-init.el" "$FAKE_HOME/.config/emacs/early-init.el"
ln -s "$FAKE_MASON/.testrc.symlink.home" "$FAKE_HOME/.testrc"

run_test "health reports all healthy when everything is linked"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health 2>&1)"
assert_contains "$output" "All healthy" && pass

run_test "health -v shows OK entries"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health -v 2>&1)"
assert_contains "$output" "OK" && assert_contains "$output" "settings.conf" && pass

run_test "health reports MISS for missing symlink"
rm "$FAKE_CONFIG/app1/settings.conf"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health 2>&1)"
assert_contains "$output" "MISS" && assert_contains "$output" "settings.conf" && pass

run_test "health reports WARN for overwritten symlink (regular file)"
rm "$FAKE_CONFIG/app1/theme.conf"
echo "overwritten" > "$FAKE_CONFIG/app1/theme.conf"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health 2>&1)"
assert_contains "$output" "!!" && assert_contains "$output" "theme.conf" && pass

run_test "health reports MISS for missing config directory"
setup_sandbox
mkdir -p "$FAKE_HOME/.config/emacs"
ln -s "$FAKE_MASON/emacs/early-init.el" "$FAKE_HOME/.config/emacs/early-init.el"
ln -s "$FAKE_MASON/.testrc.symlink.home" "$FAKE_HOME/.testrc"
# app1 and app2 config dirs don't exist in $FAKE_CONFIG
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health 2>&1)"
assert_contains "$output" "MISS" && assert_contains "$output" "config directory not installed" && pass

run_test "health reports DEAD for dead symlinks"
setup_sandbox
mkdir -p "$FAKE_CONFIG/app1"
ln -s "$FAKE_MASON/config/app1/settings.conf" "$FAKE_CONFIG/app1/settings.conf"
ln -s "$FAKE_MASON/config/app1/theme.conf" "$FAKE_CONFIG/app1/theme.conf"
mkdir -p "$FAKE_CONFIG/app2"
ln -s "$FAKE_MASON/config/app2/main.conf" "$FAKE_CONFIG/app2/main.conf"
mkdir -p "$FAKE_HOME/.config/emacs"
ln -s "$FAKE_MASON/emacs/early-init.el" "$FAKE_HOME/.config/emacs/early-init.el"
ln -s "$FAKE_MASON/.testrc.symlink.home" "$FAKE_HOME/.testrc"
# Create a dead symlink pointing into MASON_HOME
ln -s "$FAKE_MASON/config/app1/deleted.conf" "$FAKE_CONFIG/app1/deleted.conf"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health 2>&1)"
assert_contains "$output" "DEAD" && assert_contains "$output" "deleted.conf" && pass

run_test "health reports WARN for wrong symlink target"
setup_sandbox
mkdir -p "$FAKE_CONFIG/app1"
ln -s "/some/other/path" "$FAKE_CONFIG/app1/settings.conf"
ln -s "$FAKE_MASON/config/app1/theme.conf" "$FAKE_CONFIG/app1/theme.conf"
mkdir -p "$FAKE_CONFIG/app2"
ln -s "$FAKE_MASON/config/app2/main.conf" "$FAKE_CONFIG/app2/main.conf"
mkdir -p "$FAKE_HOME/.config/emacs"
ln -s "$FAKE_MASON/emacs/early-init.el" "$FAKE_HOME/.config/emacs/early-init.el"
ln -s "$FAKE_MASON/.testrc.symlink.home" "$FAKE_HOME/.testrc"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" health 2>&1)"
assert_contains "$output" "!!" && assert_contains "$output" "expected" && pass

# --- mason gc tests ---

printf "\n\033[1mmason gc\033[0m\n"

run_test "gc reports no dead symlinks when clean"
setup_sandbox
mkdir -p "$FAKE_CONFIG/app1" "$FAKE_CONFIG/app2"
ln -s "$FAKE_MASON/config/app1/settings.conf" "$FAKE_CONFIG/app1/settings.conf"
ln -s "$FAKE_MASON/config/app1/theme.conf" "$FAKE_CONFIG/app1/theme.conf"
ln -s "$FAKE_MASON/config/app2/main.conf" "$FAKE_CONFIG/app2/main.conf"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" gc 2>&1)"
assert_contains "$output" "No dead symlinks found" && pass

run_test "gc dry-run finds dead symlinks without removing"
ln -s "$FAKE_MASON/config/app1/gone.conf" "$FAKE_CONFIG/app1/gone.conf"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" gc 2>&1)"
assert_contains "$output" "DEAD" && assert_contains "$output" "gone.conf" && assert_contains "$output" "Run 'mason gc -f' to remove" && [ -L "$FAKE_CONFIG/app1/gone.conf" ] && pass

run_test "gc -f removes dead symlinks"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" gc -f 2>&1)"
assert_contains "$output" "Removed" && assert_contains "$output" "gone.conf" && assert_file_not_exists "$FAKE_CONFIG/app1/gone.conf" && pass


run_test "gc ignores symlinks not pointing into MASON_HOME"
setup_sandbox
mkdir -p "$FAKE_CONFIG/app1"
ln -s "/some/other/dead/link" "$FAKE_CONFIG/app1/unrelated.conf"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" XDG_CONFIG_HOME="$FAKE_CONFIG" "$FAKE_MASON/mason" gc 2>&1)"
assert_contains "$output" "No dead symlinks found" && pass

# --- mason CLI tests ---

printf "\n\033[1mmason CLI\033[0m\n"

run_test "mason with no args shows usage"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" "$FAKE_MASON/mason" 2>&1)"
assert_contains "$output" "Usage:" && pass

run_test "mason --help shows usage"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" "$FAKE_MASON/mason" --help 2>&1)"
assert_contains "$output" "Usage:" && pass

run_test "mason unknown-command exits with error"
output="$(HOME="$FAKE_HOME" MASON_HOME="$FAKE_MASON" "$FAKE_MASON/mason" bogus 2>&1)" || true
assert_contains "$output" "Unknown command" && pass

# --- install-utils install tests ---

printf "\n\033[1minstall-utils (install)\033[0m\n"

run_test "install-utils sources successfully with options"
output="$(HOME="$FAKE_HOME" bash -c '. "'"$REAL_MASON_HOME"'/install-utils" "'"$REAL_MASON_HOME"'" --no-prompt -d s; echo "sourced ok"' 2>&1)"
assert_contains "$output" "sourced ok" && pass

run_test "install_symlinks uses installable_dst correctly"
setup_sandbox
output="$(HOME="$FAKE_HOME" bash -c '. "'"$FAKE_MASON"'/install-utils" "'"$FAKE_MASON"'" --no-prompt -d s; install_symlinks' 2>&1)"
assert_contains "$output" "installing symlinks" && pass

run_test "install_copies uses installable_dst correctly"
setup_sandbox
touch "$FAKE_MASON/.bashrc.copy.home"
output="$(HOME="$FAKE_HOME" bash -c '. "'"$FAKE_MASON"'/install-utils" "'"$FAKE_MASON"'" --no-prompt -d o; install_copies' 2>&1)"
assert_contains "$output" "copying files" && assert_file_exists "$FAKE_HOME/.bashrc" && pass

# --- summary ---

printf "\n\033[1mResults: %d tests, %d passed, %d failed\033[0m\n\n" "$tests_run" "$tests_passed" "$tests_failed"

if [ "$tests_failed" -gt 0 ]; then
    exit 1
fi
