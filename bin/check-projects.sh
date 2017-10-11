#!/bin/sh

# set -e

for DIR in *; do
    if [ -d "$DIR" ]; then
        echo "-------------------- ${DIR} --------------------"
        ../emilio -f summary $DIR
        echo ""
    fi
done
