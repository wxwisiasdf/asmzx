#include <cstdio>
#include <verilated.h>
#include "Vtop_cpu.h"

int main(int argc, char* argv[]) {
    Verilated::commandArgs(argc, argv);
    std::unique_ptr<Vtop_cpu> top{ new Vtop_cpu{} };

    static unsigned char membuf[128] = {0};
    std::unique_ptr<FILE, decltype(&std::fclose)> fp(std::fopen("asmzx/program.bin", "r"), &std::fclose);
    std::fread(membuf, 1, sizeof(membuf), fp.get());

    for(auto i = 0; i < sizeof(membuf); i++) {
        if(i == 0 || i % (80 / 5) == 0) putchar('\n');
        printf("%02x ", (unsigned char)membuf[i]);
    }
    putchar('\n');

    top->rst = 1;
    top->clk = 0;
    top->eval();
    top->clk = 1;
    top->eval();
    top->rst = 0;
    top->clk = 0;
    top->eval();

    for(auto i = 0; i < 250; i++) {
        top->clk = !top->clk;
        top->eval();
        if(top->clk && top->ce) {
            if(top->wr) {
                membuf[top->addr % sizeof(membuf)] = top->data;
                top->rdy = 1;
            } else {
                top->data = membuf[top->addr % sizeof(membuf)];
                top->rdy = 1;
            }
        }
    }
    top->final();
    return 0;
}
