#!/bin/sh

cd asmzx
cobc \
    -Wall \
    -Wextra \
    -x \
    asmzx.cbl \
    -o asmzx || exit
rm program.def program.def.dd program.ins program.sym program.sym.dd program.bin
./asmzx \
    || exit
cd ..

verilator \
    -I../ \
    -cc top_cpu.sv \
    -exe main.cpp \
    -o Vtop_cpu \
    -CFLAGS "$(sdl2-config --cflags)" \
    -LDFLAGS "$(sdl2-config --libs)" \
    || exit

make \
    -C ./obj_dir \
    -f Vtop_cpu.mk \
    || exit

./obj_dir/Vtop_cpu \
    || exit
