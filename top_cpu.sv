module top_cpu(
    input wire clk,
    input wire rst,

    inout wire [7:0] data,
    output reg [63:0] addr,
    output reg wr, // 0 = read, 1 = write
    output reg ce,
    input wire rdy
);
    // General processor state
    reg [63:0] x_reg[15:0]; // X-register file
    reg [3:0] pc_index; // X-reg being used as a program pointer
    reg [3:0] sp_index; // X-reg being used as a stack pointer
    reg extended_rrrr; // Extended rrrr-rrrr byte preceding instructions

    reg [5:0] state; // State for decoder
    localparam
        STATE_DECODE_S_FETCH = 0, // Start fetch
        STATE_DECODE_A_FETCH = 1,
        STATE_DECODE_A = 2,
        STATE_DECODE_B_FETCH = 3,
        STATE_DECODE_B = 4,
        STATE_DECODE_C_FETCH = 5,
        STATE_DECODE_C = 6,
        STATE_DECODE_D_FETCH = 7,
        STATE_DECODE_D = 8,
        STATE_DECODE_E_FETCH = 9,
        STATE_DECODE_E = 10,
        STATE_STORE = 11,
        STATE_LOAD = 12,
        STATE_COPY_READ = 13,
        STATE_COPY_WRITE = 14,
        STATE_SCAN = 15,
        STATE_CMA_1 = 16,
        STATE_CMA_2 = 17;
    
    reg [63:0] exception_table[7:0];
    localparam
        EXCEPT_DEBUG = 0,
        EXCEPT_INVALID_INST = 1,
        EXCEPT_BREAKPOINT = 2,
        EXCEPT_DIVIDE_ZERO = 3,
        EXCEPT_OVERFLOW = 4,
        EXCEPT_UNDERFLOW = 5;
    
    // Memory I/O
    reg [7:0] data_buf;
    assign data = wr ? data_buf : 0;

    // Register specificator operand
    reg [3:0] op_rrrr[3:0];

    // Type-A state
    wire [3:0] a_insn = data[3:0];
    wire [3:0] a_rrrr = data[7:4];

    // Type-B state
    wire [7:0] b_insn = data[7:0];

    // Store state
    reg [3:0] store_size; // Size of store (0-255)
    reg [63:0] store_data; // Data to store
    reg [63:0] store_addr;
    reg store_dir; // 0 = increment, 1 = decrement

    // Load state
    reg [3:0] load_size; // Size of load
    reg [3:0] load_reg; // Register to load data into
    reg [63:0] load_addr;
    reg load_dir; // 0 = increment, 1 = decrement

    // Copy state
    reg [63:0] copy_src;
    reg [63:0] copy_dst;
    reg [63:0] copy_bytes;
    reg [7:0] copy_buf;

    // Scan state
    reg [63:0] scan_addr;
    reg [7:0] scan_cmp;
    reg [63:0] scan_len;
    reg [3:0] scan_len_reg;

    // CMA state
    reg [63:0] cma_addr_1;
    reg [63:0] cma_addr_2;
    reg [7:0] cma_buf;
    reg [3:0] cma_reg;

    always @(posedge clk) begin
        if(rst) begin
            state <= STATE_DECODE_S_FETCH;
            extended_rrrr <= 0;
            pc_index <= 0;
            sp_index <= 0;
            x_reg[pc_index] <= 0;
            x_reg[sp_index] <= 0;
        end
    end

    always @(posedge clk) begin
        if(state == STATE_DECODE_S_FETCH) begin
            addr <= x_reg[pc_index];
            wr <= 0;
            ce <= 1;
            if(rdy) begin
                if(extended_rrrr != 0) begin
                    x_reg[pc_index] <= x_reg[pc_index] + 1; // Skip rrrr-rrrr byte
                    // X(r + 0) is obtained when fetching Type-A instruction
                    // so we will delay it
                    op_rrrr[1] <= data[3:0]; // X(r + 1)
                    op_rrrr[2] <= data[7:4]; // X(r + 2)
                    $display("rrrr-rrrr insn %b @ %d", data, x_reg[pc_index]);
                end else begin
                    // Preconfigured rrrr operation, no byte consumed
                    op_rrrr[0] <= a_rrrr;
                    op_rrrr[1] <= a_rrrr + 1;
                    op_rrrr[2] <= a_rrrr + 2;
                end
                state <= STATE_DECODE_A_FETCH; // Advance state to Type-A
            end
        end if(state == STATE_DECODE_A_FETCH) begin
            addr <= x_reg[pc_index];
            wr <= 0;
            ce <= 1;
            if(rdy) begin
                state <= STATE_DECODE_A;
                if(extended_rrrr) begin
                    op_rrrr[0] <= a_rrrr;
                end
            end
        end else if(state == STATE_DECODE_B_FETCH) begin
            addr <= x_reg[pc_index];
            wr <= 0;
            ce <= 1;
            if(rdy) begin
                state <= STATE_DECODE_B;
            end
        end else if(state == STATE_DECODE_C_FETCH) begin
            addr <= x_reg[pc_index];
            wr <= 0;
            ce <= 1;
            if(rdy) begin
                state <= STATE_DECODE_C;
            end
        end else if(state == STATE_DECODE_D_FETCH) begin
            addr <= x_reg[pc_index];
            wr <= 0;
            ce <= 1;
            if(rdy) begin
                state <= STATE_DECODE_D;
            end
        end else if(state == STATE_DECODE_E_FETCH) begin
            addr <= x_reg[pc_index];
            wr <= 0;
            ce <= 1;
            if(rdy) begin
                state <= STATE_DECODE_E;
            end
        end
    end

    always @(posedge clk) begin
        if(state == STATE_DECODE_A) begin
            state <= STATE_DECODE_S_FETCH;
            x_reg[pc_index] <= x_reg[pc_index] + 1; // Goto next instruction
            $display("A instruction %b @ %d", a_insn, x_reg[pc_index]);
            casez(a_insn)
            4'b0000: begin // JMP
                x_reg[pc_index] <= x_reg[op_rrrr[0]];
                x_reg[op_rrrr[2]] <= x_reg[pc_index] + 1;
                end
            4'b0001: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] + 1; // INC
            4'b0010: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]]; // ADD
            4'b0011: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] - 1; // DEC
            4'b0100: x_reg[op_rrrr[2]] <= { ~x_reg[op_rrrr[0]][63:63], x_reg[op_rrrr[0]][62:0] }; // FLP
            4'b0101: x_reg[op_rrrr[2]] <= ~x_reg[op_rrrr[0]]; // NOT
            4'b0110: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] & x_reg[op_rrrr[1]]; // AND
            4'b0111: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] | x_reg[op_rrrr[1]]; // IOR
            4'b1000: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] ^ x_reg[op_rrrr[1]]; // XOR
            4'b1001: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] + 2; // IN2
            4'b1010: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] - 2; // DE2
            4'b1011: x_reg[op_rrrr[2]] <= $unsigned(x_reg[op_rrrr[0]]) - $unsigned(x_reg[op_rrrr[1]]); // SUS
            4'b1100: x_reg[op_rrrr[2]] <= $unsigned(x_reg[op_rrrr[0]]) + $unsigned(x_reg[op_rrrr[1]]); // AUS
            4'b1101: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] & 64'hFFFFFFFF; // HLF
            4'b1110: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] & ~64'hFFFFFFFF; // HIF
            4'b1111: begin // VVV
                state <= STATE_DECODE_B_FETCH;
                end
            endcase
        end
    end

    always @(posedge clk) begin
        if(state == STATE_DECODE_B) begin
            state <= STATE_DECODE_S_FETCH;
            x_reg[pc_index] <= x_reg[pc_index] + 1; // Goto next instruction
            $display("B instruction %b @ %d", b_insn, x_reg[pc_index]);
            casez(b_insn)
            8'b0000_0000: pc_index <= op_rrrr[0]; // SPC
            8'b0000_0001: x_reg[op_rrrr[2]] <= x_reg[pc_index]; // GPC
            8'b0000_0010: x_reg[op_rrrr[2]] <= { <<{ x_reg[op_rrrr[0]] } };  // BSQ
            8'b0000_0011: x_reg[op_rrrr[2]] <= { 32'h0, { <<{ x_reg[op_rrrr[0]][31:0] } } }; // BSW
            8'b0000_0100: x_reg[op_rrrr[2]] <= { 48'h0, { <<{ x_reg[op_rrrr[0]][15:0] } } }; // BSS
            8'b0000_0101: x_reg[op_rrrr[2]] <= $unsigned(x_reg[op_rrrr[0]]) * $unsigned(x_reg[op_rrrr[0]]);
            8'b0000_0110: x_reg[op_rrrr[2]] <= $unsigned(x_reg[op_rrrr[0]]) / $unsigned(x_reg[op_rrrr[0]]);
            8'b0000_0111: sp_index <= op_rrrr[0]; // SSP
            8'b0000_1000: x_reg[op_rrrr[2]] <= x_reg[sp_index]; // GSP
            8'b0000_1001: begin // PSB
                state <= STATE_STORE;
                store_size <= 0; // 1-byte
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] + 1;
                end
            8'b0000_1010: begin // PSS
                state <= STATE_STORE;
                store_size <= 1; // 2-bytes
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] + 2;
                end
            8'b0000_1011: begin // PSW
                state <= STATE_STORE;
                store_size <= 3; // 4-bytes
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] + 4;
                end
            8'b0000_1100: begin // PSQ
                state <= STATE_STORE;
                store_size <= 7; // 8-bytes
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] + 8;
                end
            8'b0000_1101: begin // QSB
                state <= STATE_LOAD;
                load_size <= 0; // 1-byte
                load_reg <= op_rrrr[0];
                load_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] - 1;
                end
            8'b0000_1110: begin // QSS
                state <= STATE_LOAD;
                load_size <= 1; // 2-bytes
                load_reg <= op_rrrr[0];
                load_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] - 2;
                end
            8'b0000_1111: begin // QSW
                state <= STATE_LOAD;
                load_size <= 3; // 4-bytes
                load_reg <= op_rrrr[0];
                load_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] - 4;
                end
            8'b0001_0000: begin // QSQ
                state <= STATE_LOAD;
                load_size <= 7; // 8-bytes
                load_reg <= op_rrrr[0];
                load_addr <= x_reg[sp_index];
                x_reg[sp_index] <= x_reg[sp_index] - 8;
                end
            8'b0001_0001: store_dir <= 1; // SDS
            8'b0001_0010: store_dir <= 0; // SDC
            8'b0001_0011: load_dir <= 1; // LDS
            8'b0001_0100: load_dir <= 0; // LDC
            8'b0001_0101: begin // SSB
                state <= STATE_STORE;
                store_size <= 0; // 1-byte
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_0110: begin // SSS
                state <= STATE_STORE;
                store_size <= 1; // 2-bytes
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_0111: begin // SSW
                state <= STATE_STORE;
                store_size <= 3; // 4-bytes
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_1000: begin // SSQ
                state <= STATE_STORE;
                store_size <= 7; // 8-bytes
                store_data <= x_reg[op_rrrr[0]];
                store_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_1001: begin // LSB
                state <= STATE_LOAD;
                load_size <= 0; // 1-byte
                load_reg <= op_rrrr[2];
                load_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_1010: begin // LSS
                state <= STATE_LOAD;
                load_size <= 1; // 2-bytes
                load_reg <= op_rrrr[2];
                load_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_1011: begin // LSW
                state <= STATE_LOAD;
                load_size <= 3; // 4-bytes
                load_reg <= op_rrrr[2];
                load_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_1100: begin // LSQ
                state <= STATE_LOAD;
                load_size <= 7; // 8-bytes
                load_reg <= op_rrrr[2];
                load_addr <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                end
            8'b0001_1101: begin // CPY
                state <= STATE_COPY_READ;
                copy_src <= x_reg[op_rrrr[0]];
                copy_dst <= x_reg[op_rrrr[2]];
                copy_bytes <= x_reg[op_rrrr[1]];
                end
            8'b0001_1111: x_reg[op_rrrr[2]] <= (~x_reg[op_rrrr[0]]) + (~x_reg[op_rrrr[1]]); // NXR
            8'b0010_0000: begin // JMR
                x_reg[pc_index] <= x_reg[op_rrrr[0]];
                x_reg[op_rrrr[2]] <= x_reg[pc_index] + 1;
                end
            8'b0010_0001: begin // JRR
                x_reg[pc_index] <= x_reg[op_rrrr[0]] + x_reg[op_rrrr[1]];
                x_reg[op_rrrr[2]] <= x_reg[pc_index] + 1;
                end
            8'b0010_0010: begin // BFR
                if(x_reg[op_rrrr[0]] == 0) begin
                    x_reg[pc_index] <= x_reg[pc_index] + x_reg[op_rrrr[2]];
                end
                end
            8'b0010_0011: begin // BNR
                if(x_reg[op_rrrr[0]] != 0) begin
                    x_reg[pc_index] <= x_reg[pc_index] + x_reg[op_rrrr[2]];
                end
                end
            8'b0010_0100: begin // BEQ
                if(x_reg[op_rrrr[0]] == x_reg[op_rrrr[1]]) begin
                    x_reg[pc_index] <= x_reg[pc_index] + x_reg[op_rrrr[2]];
                end
                end
            8'b0010_0101: begin // BNE
                if(x_reg[op_rrrr[0]] != x_reg[op_rrrr[1]]) begin
                    x_reg[pc_index] <= x_reg[pc_index] + x_reg[op_rrrr[2]];
                end
                end
            8'b0010_0111: begin // DEB
                x_reg[pc_index] <= exception_table[EXCEPT_DEBUG];
                end
            8'b0010_1000: begin // SCN
                state <= STATE_SCAN;
                scan_addr <= x_reg[op_rrrr[0]];
                scan_cmp <= x_reg[op_rrrr[1]][7:0];
                scan_len_reg <= op_rrrr[2];
                scan_len <= x_reg[op_rrrr[2]];
                x_reg[op_rrrr[2]] <= 0;
                end
            8'b0010_1001: begin // CMA
                state <= STATE_CMA_1;
                cma_addr_1 <= x_reg[op_rrrr[0]];
                cma_addr_2 <= x_reg[op_rrrr[1]];
                cma_reg <= op_rrrr[2];
                end
            8'b0010_1010: begin // SEX
                exception_table[x_reg[op_rrrr[0]][2:0]] <= x_reg[op_rrrr[1]];
                x_reg[op_rrrr[2]] <= exception_table[x_reg[op_rrrr[0]][2:0]];
                end
            8'b0010_1011: extended_rrrr <= 1; // RRS
            8'b0010_1100: extended_rrrr <= 0; // RRC
            8'b0010_1101: // DDD
                $display("%m: X0=%x X1=%x X2=%x SP=%x PC=%x",
                    x_reg[op_rrrr[0]], x_reg[op_rrrr[1]], x_reg[op_rrrr[2]],
                    x_reg[pc_index], x_reg[sp_index]);
            8'b0010_1110: x_reg[op_rrrr[2]] <= 0; // CCC
            8'b0010_1111: begin // CC2
                x_reg[op_rrrr[2]] <= 0;
                x_reg[op_rrrr[0]] <= ~64'h0;
                end
            8'b0011_0000: begin // CC3
                x_reg[op_rrrr[2]] <= 0;
                x_reg[op_rrrr[0]] <= ~64'h0;
                x_reg[op_rrrr[1]] <= 64'h3F3F3F3F3F3F3F3F;
                end
            8'b0011_0001: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]]; // PW3
            8'b0011_0010: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]]; // PW4
            8'b0011_0011: x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]] * x_reg[op_rrrr[0]]; // PW5
            8'b0011_0100: begin // SE0
                exception_table[EXCEPT_INVALID_INST] <= x_reg[op_rrrr[0]];
                end
            8'b0011_0101: begin // IMM
                x_reg[op_rrrr[2]] <= ({ 60'h0, op_rrrr[0] } << 4) | { 60'h0, op_rrrr[1] };
                end
            8'b0011_0110: begin // LS8
                x_reg[op_rrrr[2]] <= x_reg[op_rrrr[0]] << 8;
                end
            8'b0011_0111: begin // CLR
                x_reg[op_rrrr[0]] <= 0;
                x_reg[op_rrrr[1]] <= 0;
                x_reg[op_rrrr[2]] <= 0;
                end
            default: begin
                x_reg[pc_index] <= exception_table[EXCEPT_INVALID_INST];
                end
            endcase
        end
    end

    always @(posedge clk) begin
        if(state == STATE_STORE) begin
            wr <= 1; // Write
            ce <= 1;
            data_buf <= store_data[7:0];
            addr <= store_addr;
            if(rdy) begin
                if(store_size == 0) state <= STATE_DECODE_S_FETCH;
                store_size <= store_size - 1;
                store_data <= store_data >> 8;
                // Take in account direction
                if(store_dir == 0) store_addr <= store_addr + 1;
                else store_addr <= store_addr - 1;
            end
        end
    end

    always @(posedge clk) begin
        if(state == STATE_LOAD) begin
            wr <= 0; // Read
            ce <= 1;
            addr <= load_addr;
            if(rdy) begin
                if(load_size == 0) state <= STATE_DECODE_S_FETCH;
                load_size <= load_size - 1;
                x_reg[load_reg] <= (x_reg[load_reg] << 8) | { 56'h0, data };
                // Take in account direction
                if(load_dir == 0) load_addr <= load_addr - 1;
                else load_addr <= load_addr + 1;
            end
        end
    end

    always @(posedge clk) begin
        if(state == STATE_COPY_READ) begin
            wr <= 0; // Read
            ce <= 1;
            addr <= copy_src;
            if(rdy) begin
                state <= STATE_COPY_WRITE;
                copy_buf <= data;
                copy_src <= copy_src + (load_dir == 0 ? 1 : -1);
            end
        end else if(state == STATE_COPY_WRITE) begin
            wr <= 1; // Write
            ce <= 1;
            data_buf <= copy_buf;
            addr <= copy_dst;
            if(rdy) begin
                state <= (copy_bytes == 0) ? STATE_DECODE_S_FETCH : STATE_COPY_READ;
                copy_bytes <= copy_bytes - 1;
                copy_dst <= copy_src + (copy_dst == 0 ? 1 : -1);
            end
        end
    end

    always @(posedge clk) begin
        if(state == STATE_SCAN) begin
            wr <= 0; // Read
            ce <= 1;
            addr <= scan_addr;
            if(rdy) begin
                state <= (scan_len == 0 || data == scan_cmp) ? STATE_DECODE_S_FETCH : STATE_SCAN;
                scan_len <= scan_len - 1;
                scan_addr <= scan_addr + (load_dir == 0 ? 1 : -1);
            end
        end
    end

    always @(posedge clk) begin
        if(state == STATE_CMA_1) begin
            wr <= 0; // Read
            ce <= 1;
            addr <= cma_addr_1;
            if(rdy) begin
                state <= STATE_CMA_2;
                cma_buf <= data;
            end
        end else if(state == STATE_CMA_2) begin
            wr <= 0; // Read
            ce <= 1;
            addr <= cma_addr_2;
            if(rdy) begin
                state <= STATE_DECODE_S_FETCH;
                x_reg[cma_reg] <= { 56'h0, $unsigned(cma_buf - data) };
            end
        end
    end
endmodule
