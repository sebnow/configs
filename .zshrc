[ -z "$PS1" ] && return

source ~/.zsh/aliases
source ~/.zsh/bindings
source ~/.zsh/functions
source ~/.zsh/options
source ~/.zsh/prompt
if [ -f ~/.zsh/local ]; then
	source ~/.zsh/local
fi

## Exported variables
# Use emacs(client) if we can
if which emacs > /dev/null 2>&1; then
	export EDITOR=emacsclient
	export VISUAL=emacsclient
	export ALTERNATE_EDITOR=emacs
# Use TextMate on Mac OS X
elif [ -x "/usr/local/bin/mate" ]; then
	export EDITOR="mate -w"
# Default to vim
else
	export EDITOR=vim
fi

# Man page colours
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
