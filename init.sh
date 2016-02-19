#!/bin/bash
set -o errexit
set -o pipefail

TARGET=/tmp/

if [[ ! -d $TARGET/.when-bunny-dies ]]; then
    git clone https://github.com/binarin/when-bunny-dies "$TARGET/.when-bunny-dies"
fi

make -C $TARGET/.when-bunny-dies
