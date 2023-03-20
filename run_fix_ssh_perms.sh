#!/bin/sh

FILE="$HOME/.ssh/github"
if [ -f "$FILE" ]; then
    if [ "$(stat -c %a "$FILE")" != "600" ] ; then
        chmod 600 "$FILE"
    fi
fi


