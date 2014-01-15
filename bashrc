#!/bin/bash
# Check for an interactive session
[ -z "$PS1" ] && return

source ~/.bash/aliases
source ~/.bash/exports
source ~/.bash/function
source ~/.bash/prompt
if [ -f ~/.bash/local ]; then
    source ~/.bash/local
fi 

