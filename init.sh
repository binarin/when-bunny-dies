#!/bin/bash
set -errexit
set -pipefail

if [[ ! -d $HOME/.when-bunny-dies ]]; then
    git clone https://github.com/binarin/when-bunny-dies "$HOME/.when-bunny-dies"
fi

make -C $HOME/.when-bunny-dies