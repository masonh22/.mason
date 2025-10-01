# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto -I'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
elif [ "$(uname)" = 'Darwin' ]; then
    alias ls='ls --color=auto'

    alias grep='grep --color=auto -I'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# emacs
alias e='emacsclient --alternate-editor="" --create-frame'
alias eeval='emacsclient --eval'
alias eping='eeval "(+ 8 34)"'
alias estop='eeval "(save-buffers-kill-emacs)"'
alias ekill='eeval "(kill-emacs)"'
# git
alias gst='git status'
alias gc='git commit'
alias gd='git diff'
# kubectl
alias kcg="kubectl config get-contexts"
alias kcu="kubectl config use-context"
alias kg="kubectl get"
alias kgp="kubectl get pods"
alias kgd="kubectl get deployment"
alias kgs="kubectl get sts"
alias kd="kubectl describe"
alias kdp="kubectl describe pod"
alias kdd="kubectl describe deployment"
alias kds="kubectl describe sts"
alias k="kubectl"
alias kc="kubectl config set-context --current --namespace"
# commands
alias fd='fd --hidden'
alias rg='rg --with-filename --no-heading --hidden'
alias rm='rm -v'
# cd
alias '..'='cd ..'
alias '...'='cd ../..'

# bat/batcat
if [ -x "$(command -v batcat)" ]; then
   alias bat=batcat
fi

cdp() {
    if [ -n "$PROJECTS" ]; then
        cd "$PROJECTS/$1"
    else
        echo 'PROJECTS environment variable not set'
        exit 1
    fi
}

if [ -x "$(command -v aws)" ]; then
    # Refresh AWS SSO credentials if they're expired
    refresh-sso() {
        if [ -n "$1" ]; then
            __aws_profile="--profile '$1'"
        fi

        if ! aws sts get-caller-identity $__aws_profile > /dev/null 2>&1; then
            echo "Running 'aws sso login $__aws_profile'"
            aws sso login $__aws_profile
        else
            echo 'Credentials are valid'
        fi
    }
fi
