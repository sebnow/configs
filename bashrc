#!/bin/bash
# Check for an interactive session
[ -z "$PS1" ] && return

if [[ $(uname -s) = "Darwin" ]]; then
	alias ls='ls -G'
else
	alias ls='ls --color=auto'
fi


source ~/.bash/exports
source ~/.bash/function
source ~/.bash/prompt
if [ -f ~/.bash/local ]; then
    source ~/.bash/local
fi 

