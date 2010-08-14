#!/bin/sh
echo "$TERM" | grep "screen" 2>&1 > /dev/null
if [ $? -gt 0 ]; then
    exit
fi
printf "\ekssh %s\e\\" $@

