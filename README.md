# hsprocessing

Build instructions:

stack build
cp dist/index.html .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/hsprocessing/hsprocessing.jsexe
cp dist/gl-matrix-min.js .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/hsprocessing/hsprocessing.jsexe
cd .stack-work/dist/x86_64-linux/Cabal-1.22.4.0_ghcjs/build/hsprocessing/hsprocessing.jsexe
python -m SimpleHTTPServer
