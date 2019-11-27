#!/bin/bash
# Check for an interactive session
[ -z "$PS1" ] && return

type -f starship >/dev/null 2>&1 && eval "$(starship init bash)"

source ~/.bash/aliases
source ~/.bash/exports
source ~/.bash/function
if [ -f ~/.bash/local ]; then
    source ~/.bash/local
fi

