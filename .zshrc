[ -z "$PS1" ] && return

source ~/.zsh/aliases
source ~/.zsh/bindings
source ~/.zsh/functions
source ~/.zsh/options
source ~/.zsh/prompt

## Exported variables
if [ "x$(uname -s)" = "xDarwin" ]; then
	export EDITOR="mate -w"
else
	export EDITOR="vim"
fi

# Man page colours
export LESS_TERMCAP_mb=$'\E[01;31m'
export LESS_TERMCAP_md=$'\E[01;31m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;32m'
