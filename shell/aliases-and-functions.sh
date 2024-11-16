# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
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
# commands
alias fd='fd --hidden'
alias rg='rg --with-filename --no-heading --hidden'
alias rm='rm -v'
