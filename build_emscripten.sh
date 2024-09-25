export EMPATH=$(dirname $(which emcc))
emcmake cmake -B build -DC3_WITH_LLVM=OFF -DC3_USE_VXCC=ON -DCMAKE_BUILD_TYPE=Debug
cd build
make

# generated c3c.html, c3c.js, and c3c.wasm
