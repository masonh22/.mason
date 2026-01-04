# only do next part once
if [ -n "${MASON_ZSH_ENV_INIT_COMPLETE}" ]; then
    return 0
fi

# opam configuration
test -r ${HOME}/.opam/opam-init/init.zsh && . ${HOME}/.opam/opam-init/init.zsh > /dev/null 2>&1 || true

MASON_ZSH_ENV_INIT_COMPLETE=true
