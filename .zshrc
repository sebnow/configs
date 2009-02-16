[ -z "$PS1" ] && return

source ~/.zsh/aliases
source ~/.zsh/bindings
source ~/.zsh/functions
source ~/.zsh/options
source ~/.zsh/prompt
source ~/.zsh/exports
if [ -f ~/.zsh/local ]; then
	source ~/.zsh/local
fi

