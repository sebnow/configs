#!/bin/sh
# Execute program if it exists
ifexec() {
	local prg="$1"; shift
	which "$prg" 2>/dev/null 2>&1 && "$prg" $*
}

ifexec unclutter --root --noevents &
ifexec numlockx &
[ -r ~/.fehbg ] && sh ~/.fehbg &
urxvtd -q -o -f
urxvtc
exec xmonad

