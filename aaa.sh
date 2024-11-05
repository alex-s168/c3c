set -e
cmake -B build -G Ninja -S . \
    -DLLD_COFF=/usr/lib64/liblldCOFF.so \
    -DLLD_COMMON=/usr/lib64/liblldCommon.so \
    -DLLD_ELF=/usr/lib64/liblldELF.so \
    -DLLD_MINGW=/usr/lib64/liblldMinGW.so \
    -DLLD_WASM=/usr/lib64/liblldWasm.so \
    -DLLD_MACHO= \
    -DC3_WITH_LLVM=OFF -DC3_USE_VXCC=ON
cmake --build build
