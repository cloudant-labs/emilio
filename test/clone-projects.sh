#!/bin/sh

INFILE=${1:-'projects.txt'}

for URL in `cat $INFILE`; do
    git clone "$URL"
done
