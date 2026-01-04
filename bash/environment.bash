# only do next part once
if [ -n "${MASON_BASH_ENV_INIT_COMPLETE}" ]; then
    return 0
fi

# opam configuration
test -r ${HOME}/.opam/opam-init/init.sh && . ${HOME}/.opam/opam-init/init.sh > /dev/null 2>&1 || true

MASON_BASH_ENV_INIT_COMPLETE=true
