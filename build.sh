cmake -B build -G Ninja -DC3_WITH_LLVM=OFF -DC3_USE_VXCC=ON -DCMAKE_BUILD_TYPE=Debug 
cmake --build build --config Debug
lldb build/c3c -- compile-only test.c3 --use-stdlib=no --experimental-vxcc --emit-asm
