#!/bin/bash
# Check for an interactive session
[ -z "$PS1" ] && return
if [ -f ~/.bash/local ]; then
    source ~/.bash/local
fi

type -f starship >/dev/null 2>&1 && eval "$(starship init bash)"

source ~/.bash/aliases
source ~/.bash/exports
source ~/.bash/function


# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION
