#!/bin/bash
# Check for an interactive session
[ -z "$PS1" ] && return

alias ls='ls --color=auto'

source ~/.bash/exports
source ~/.bash/function
source ~/.bash/prompt
if [ -f ~/.bash/local/ ]; then
    source ~/.bash/local
fi 

