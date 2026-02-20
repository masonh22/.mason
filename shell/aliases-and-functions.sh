# enable color support of ls and also add handy aliases
if command -v dircolors > /dev/null 2>&1; then
    if [ -r ~/.dircolors ]; then
        eval "$(dircolors -b ~/.dircolors)"
    else
        eval "$(dircolors -b)"
    fi
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
alias ciaclean='git branch --merged | grep -vE "^\s*(\*|master|main|develop)" | xargs -n 1 git branch -d'
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
alias rg='rg --with-filename --no-heading'
alias rm='rm -v'
# cd
alias '..'='cd ..'
alias '...'='cd ../..'

# bat/batcat
if command -v batcat > /dev/null 2>&1; then
   alias bat=batcat
fi

# copy
if command -v wl-copy > /dev/null 2>&1; then
    alias copy='wl-copy'
    alias cpwd='pwd | tr -d "\n" | wl-copy'
elif command -v pbcopy > /dev/null 2>&1; then
    alias copy='pbcopy'
    alias cpwd='pwd | tr -d "\n" | pbcopy'
fi

# paste
if command -v wl-paste > /dev/null 2>&1; then
    alias paste='wl-paste'
elif command -v pbpaste > /dev/null 2>&1; then
    alias paste='pbpaste'
fi

mkcd() {
    mkdir -p "$1"
    cd "$1"
}

tempe() {
    cd "$(mktemp -d)"
    chmod -R 0700 .
}

cdp() {
    if [ -n "$PROJECTS" ]; then
        cd "$PROJECTS/$1"
    fi
}

# cd to the current directory within projects
cdh() {
    if [ -z "$PROJECTS" ]; then
        return
    fi

    if [[ "$PWD" == "$PROJECTS/"* ]]; then
        local relative_path="${PWD#"$PROJECTS/"}"
        if [ -n "$relative_path" ]; then
            local first_dir="${relative_path%%/*}"
            cd "$PROJECTS/$first_dir"
        fi
    fi
}

if [ -x "$(command -v aws)" ]; then
    # Refresh AWS SSO credentials if they're expired
    refresh-sso() {
        if [ -n "$1" ]; then
            __aws_profile=" --profile $1"
        fi

        if ! aws sts get-caller-identity $__aws_profile > /dev/null 2>&1; then
            echo "Running 'aws sso login$__aws_profile'"
            aws sso login $__aws_profile
        else
            echo 'Credentials are valid'
        fi
    }
fi
