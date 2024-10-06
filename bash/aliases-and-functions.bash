# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto -I'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias bashrc='emacs -nw -Q ~/.bashrc -nw ; . ~/.bashrc'
# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
# some more ls aliases
alias ll='ls -alhF'
alias la='ls -A'
alias l='ls -CF'

# display an alert every 20 minutes to look away
function save-my-eyes() {
    if [ -f /tmp/eyes_pid.txt ]; then
        pid=$(head -n 1 /tmp/eyes_pid.txt)
        kill $pid
        rm /tmp/eyes_pid.txt > /dev/null
        return 0
    fi
    every-n-minutes 20 -m 'It has been 20 minutes. Look away!' &
    pid=$!
    echo $pid > /tmp/eyes_pid.txt
}
