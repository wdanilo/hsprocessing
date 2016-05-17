#!/usr/bin/env bash

if [ "$(uname)" == "Darwin" ]; then
    JSEXE=.stack-work/dist/x86_64-osx/Cabal-1.22.4.0_ghcjs/build/hsprocessing-test/hsprocessing-test.jsexe
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    JSEXE=.stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/hsprocessing-test/hsprocessing-test.jsexe
fi

cd $JSEXE
python -m SimpleHTTPServer
