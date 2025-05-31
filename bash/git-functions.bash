is_git() {
    if [ $(git rev-parse --is-inside-work-tree 2> /dev/null) ]; then
        echo 'true'
        return 0
    else
        return 1
    fi
}

# Print the name of the current branch
git_branch() {
    git rev-parse --abbrev-ref HEAD 2> /dev/null
}

# Whether there are uncommitted changes
git_clean() {
    if [ -z "$(git status --porcelain)" ]; then
        echo 'true'
        return 0
    else
        return 1
    fi
}

# Print the number of unpushed changes for the current branch
git_need_push() {
    git log origin/$(git_branch)..HEAD --oneline --color=never 2> /dev/null | wc -l
}

git_prompt() {
    if [ -n "$(is_git)" ]; then
        branch=$(color_text $(git_branch))
        num_unpushed=$(git_need_push)
        if [ "$num_unpushed" != 0 ]; then
            num_unpushed=":\e[33m$num_unpushed\e[0m" # yellow
        else
            num_unpushed=
        fi
        dirty=
        if [ -z "$(git_clean)" ]; then
            dirty='\e[31mx\e[0m:' # red 'x'
        fi
        echo -e " \e[0m(${dirty}${branch}${num_unpushed})"
    fi
}
