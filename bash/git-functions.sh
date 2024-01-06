function is_git {
    if [ $(git rev-parse --is-inside-work-tree 2> /dev/null) ]; then
        echo 'true'
        return 0
    else
        return 1
    fi
}

# Print the name of the current branch
function git_branch {
    git rev-parse --abbrev-ref HEAD 2> /dev/null
}

# Whether there are uncommitted changes
function git_clean {
    if [ -z "$(git status --porcelain)" ]; then
        echo 'true'
        return 0
    else
        return 1
    fi
}

# Print the number of unpushed changes for the current branch
function git_need_push {
    git log origin/$(git_branch)..HEAD --oneline --color=never 2> /dev/null | wc -l
}
