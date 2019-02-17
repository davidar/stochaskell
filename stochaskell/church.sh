#!/bin/sh

. $NVM_DIR/nvm.sh >&2
nvm use 0.10.26 >&2

cd "$(dirname "$0")/webchurch"
[ -d node_modules ] || ./compile.sh >&2
./church "$1"
