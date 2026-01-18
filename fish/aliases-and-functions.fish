# Transpiled from shell/aliases-and-functions.sh and bash/aliases-and-functions.bash

# Colors
if command -v dircolors > /dev/null
    if test -r ~/.dircolors
        eval (dircolors -c ~/.dircolors)
    else
        eval (dircolors -c)
    end
    alias ls='ls --color=auto'
    alias grep='grep --color=auto -I'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
else if test (uname) = 'Darwin'
    alias ls='ls --color=auto'
    alias grep='grep --color=auto -I'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
end

# Emacs
alias e='emacsclient --alternate-editor="" --create-frame'
alias eeval='emacsclient --eval'
alias eping='eeval "(+ 8 34)"'
alias estop='eeval "(save-buffers-kill-emacs)"'
alias ekill='eeval "(kill-emacs)"'

# Git
alias gst='git status'
alias gc='git commit'
alias gd='git diff'

# Kubectl
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

# Commands
alias rg='rg --with-filename --no-heading'
alias rm='rm -v'

# cd
alias ..='cd ..'
alias ...='cd ../..'

# Bat
if command -v batcat > /dev/null
   alias bat=batcat
end

# Copy/Paste
if command -v wl-copy > /dev/null
    alias copy='wl-copy'
    alias cpwd='pwd | tr -d "\n" | wl-copy'
    alias paste='wl-paste'
else if command -v pbcopy > /dev/null
    alias copy='pbcopy'
    alias cpwd='pwd | tr -d "\n" | pbcopy'
    alias paste='pbpaste'
end

# Bash specific aliases
alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -CF'
alias bashrc='emacs -nw -Q ~/.bashrc -nw ; . ~/.bashrc'

# Functions

function mkcd
    mkdir -p $argv[1]
    cd $argv[1]
end

function tempe
    cd (mktemp -d)
    chmod -R 0700 .
end

function cdp
    if set -q PROJECTS
        cd "$PROJECTS/$argv[1]"
    end
end

function cdh
    if not set -q PROJECTS
        return
    end

    if string match -q "$PROJECTS/*" "$PWD"
        set -l relative_path (string replace "$PROJECTS/" "" "$PWD")
        if test -n "$relative_path"
            set -l first_dir (string split "/" -f 1 "$relative_path")
            cd "$PROJECTS/$first_dir"
        end
    end
end

if type -q aws
    function refresh-sso
        set -l profile_arg ""
        if test -n "$argv[1]"
            set profile_arg "--profile" "$argv[1]"
        end

        if not aws sts get-caller-identity $profile_arg > /dev/null 2>&1
            echo "Running 'aws sso login $profile_arg'"
            aws sso login $profile_arg
        else
            echo 'Credentials are valid'
        end
    end
end

function save-my-eyes
    if test -f /tmp/eyes_pid.txt
        set -l pid (head -n 1 /tmp/eyes_pid.txt)
        kill $pid
        rm /tmp/eyes_pid.txt > /dev/null
        return 0
    end
    every-n-minutes 20 -m 'It has been 20 minutes. Look away!' &
    set -l pid $last_pid
    echo $pid > /tmp/eyes_pid.txt
end
