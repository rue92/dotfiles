#!/bin/bash

SSH_FILES=("$HOME/.ssh/github" "$HOME/.ssh/digital_ocean")
for FILE in ${SSH_FILES[@]}; do
    if [ -f "$FILE" ]; then
        if [ "$(stat -c %a "$FILE")" != "600" ] ; then
            chmod 600 "$FILE"
        fi
    fi
done


