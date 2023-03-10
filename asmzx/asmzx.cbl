       identification division.
       program-id. asmzx.
       environment division.
       input-output section.
       file-control.
      *Input
           select fs-inputs assign to "program.asm"
           organization is line sequential.
      *Define list
           select optional fs-defines assign to "program.def"
           organization is indexed
           access is sequential
           record key is fs-define-name.
      *Symbol list
           select optional fs-symbols assign to "program.sym"
           organization is indexed
           access is sequential
           record key is fs-symbol-name.
      *Instruction list
           select optional fs-insns assign to "program.ins"
           organization is line sequential.
       data division.
       file section.
       fd  fs-inputs.
       01  fs-input.
           05 fs-input-section-a pic x(8).
           05 fs-input-section-b pic x(8).
           05 fs-input-section-c pic x(20).
           05 fs-input-section-d pic x(44).
       fd  fs-defines.
       copy "asdefn.cpy" replacing ==:pref:== BY ==fs==.
       fd  fs-symbols.
       copy "assymb.cpy" replacing ==:pref:== BY ==fs==.
       fd  fs-insns.
       copy "asinsn.cpy" replacing ==:pref:== BY ==fs==.
       working-storage section.
       01  i pic 9(4) comp.
       01  j pic 9(4) comp.
       01  ws-input.
           05 ws-input-section-a pic x(8).
           05 ws-input-section-b pic x(8).
           05 ws-input-section-c pic x(20).
           05 ws-input-section-d pic x(44).
       copy "asdefn.cpy" replacing ==:pref:== BY ==ws==.
       copy "assymb.cpy" replacing ==:pref:== BY ==ws==.
       copy "asinsn.cpy" replacing ==:pref:== BY ==ws==.
       copy "asopcp.cpy" replacing ==:pref:== BY ==ws==.
       01  ws-cur-addr pic 9(8).
       01  ws-80-line pic x(80).
       01  ws-expand-data.
           05 ws-expand-input pic x(80).
           05 ws-expand-output pic x(80).
       01  ws-oplen-data.
           05 ws-oplen-name pic x(12).
           05 ws-oplen-length pic 9(4).
       01  ws-rrrr pic x.
           88 ws-rrrr-set value 'S'.
           88 ws-rrrr-clear value 'C'.
       procedure division.
           open extend fs-defines.
      *Add default defines
           move "NULL" to ws-define-name.
           move "0" to ws-define-body.
           write fs-define from ws-define end-write.
           close fs-defines.
      *
           open input sharing with all fs-inputs.
           open extend fs-symbols, fs-insns.
           perform forever
               read fs-inputs
                   at end exit perform
                   not at end perform found-line
               end-read
           end-perform.
      *
           close fs-symbols, fs-inputs, fs-insns.
           call "asmxbin" end-call.
           goback.
       found-line.
      *Uppercase everything in the line
      *Section-A is never expanded because it's the place for directives
           initialize ws-80-line.
           move fs-input-section-a to ws-80-line.
           call "asmxcasu" using ws-80-line end-call.
           move ws-80-line to ws-input-section-a.
      *
           initialize ws-80-line.
           move fs-input-section-b to ws-80-line.
           call "asmxcasu" using ws-80-line end-call.
           move ws-80-line to ws-expand-input.
           call "asmxexpn" using ws-expand-data end-call.
           move ws-expand-output to ws-80-line.
           move ws-80-line to ws-input-section-b.
      *
           initialize ws-80-line.
           move fs-input-section-c to ws-80-line.
           call "asmxcasu" using ws-80-line end-call.
           move ws-80-line to ws-expand-input.
           call "asmxexpn" using ws-expand-data end-call.
           move ws-expand-output to ws-80-line.
           move ws-80-line to ws-input-section-c.
      *Section D is ignored and reserved for comments
           display ws-cur-addr ">>" ws-input-section-a
           ws-input-section-b ws-input-section-c ws-input-section-d
           end-display.
      *Directives
           evaluate ws-input-section-a(1:1)
               when '$' perform parse-directive
               when other perform parse-non-directive
           end-evaluate.
       parse-directive.
           evaluate ws-input-section-a(2:1)
               when 'D' perform parse-define-directive
               when 'R' perform parse-rset-directive
               when other perform parse-other-directive
           end-evaluate.
       parse-define-directive.
      *Defines name
           initialize ws-define-name.
           move 1 to j.
           perform varying i from 1 by 1
           until ws-input-section-b(i:1) = ','
               move ws-input-section-b(i:1) to ws-define-name(j:1)
               add 1 to j giving j end-add
           end-perform.
      *Skip comma and obtain define body
           add 1 to i giving i end-add.
           initialize ws-define-body.
           move 1 to j.
           perform varying i from i by 1
           until ws-input-section-b(i:1) = ' '
               move ws-input-section-b(i:1) to ws-define-body(j:1)
               add 1 to j giving j end-add
           end-perform.
           open extend fs-defines.
           write fs-define from ws-define
               invalid key
                   display "invalid define " ws-define-name end-display
               not invalid key
                   display "+define " ws-define-name end-display
           end-write.
           close fs-defines.
       parse-rset-directive.
      *rrrr-clear directive
           evaluate ws-input-section-a(3:1)
               when 'C' perform parse-rclear-directive
               when other exit
           end-evaluate.
           set ws-rrrr-set to true.
           perform parse-non-directive.
       parse-rclear-directive.
           set ws-rrrr-clear to true.
           perform parse-non-directive.
       parse-other-directive.
           display "unknown directive " ws-input-section-a end-display.
       parse-non-directive.
      *Parsing instruction, make sure to collect label (if any)
           evaluate ws-input-section-a(1:1)
               when 'A' thru 'Z' perform parse-label
               when '@' perform parse-label
               when other exit
           end-evaluate.
           evaluate ws-input-section-b(1:1)
               when 'A' thru 'Z' perform parse-insn
               when other exit
           end-evaluate.
       parse-insn.
      *Obtain opcode to use
           move ws-input-section-b to ws-insn-opcode.
      *Obtain first operand
           initialize ws-insn-x0.
           move 1 to j, i.
      *Skip X for registers
           if ws-input-section-c(i:1) = 'X'
               add 1 to i giving i end-add
           end-if.
           perform varying i from i by 1
           until ws-input-section-c(i:1) = ','
               if j > length of ws-insn-x0 then exit perform end-if
               move ws-input-section-c(i:1) to ws-insn-x0(j:1)
               add 1 to j giving j end-add
           end-perform.
           if ws-input-section-c(i:1) = ',' then
               add 1 to i giving i end-add
           end-if.
      *Second operand
           initialize ws-insn-x1.
           move 1 to j.
      *Skip X for registers
           if ws-input-section-c(i:1) = 'X'
               add 1 to i giving i end-add
           end-if.
           perform varying i from i by 1
           until ws-input-section-c(i:1) = ','
               if j > length of ws-insn-x1 then exit perform end-if
               move ws-input-section-c(i:1) to ws-insn-x1(j:1)
               add 1 to j giving j end-add
           end-perform.
           if ws-input-section-c(i:1) = ',' then
               add 1 to i giving i end-add
           end-if.
      *Third (and final) operand
           initialize ws-insn-x2.
           move 1 to j.
      *Skip X for registers
           if ws-input-section-c(i:1) = 'X'
               add 1 to i giving i end-add
           end-if.
           perform varying i from i by 1
           until ws-input-section-c(i:1) = ' '
               if j > length of ws-insn-x2 then exit perform end-if
               move ws-input-section-c(i:1) to ws-insn-x2(j:1)
               add 1 to j giving j end-add
           end-perform.
      *Obtain length of opcode
           move ws-insn-opcode to ws-oplen-name.
           call "asmxglen" using ws-oplen-data end-call.
           add ws-oplen-length to ws-cur-addr
           giving ws-cur-addr end-add.
      *Set RRRR flexibility considerations
           evaluate true
               when ws-rrrr-set
                   set ws-insn-rrrr-set to true
                   add 1 to ws-cur-addr giving ws-cur-addr end-add
               when ws-rrrr-clear set ws-insn-rrrr-clear to true
           end-evaluate.
      *Write-out the instruction
           write fs-insn from ws-insn end-write.
       parse-label.
           move ws-input-section-a to ws-symbol-name.
           move ws-cur-addr to ws-symbol-addr.
           set ws-symbol-type-local to true.
           write fs-symbol from ws-symbol
               invalid key
                   display "invalid symbol " ws-symbol-name end-display
               not invalid key
                   display "+symbol " ws-symbol-name end-display
           end-write.
       end program asmzx.
      ******************************************************************
      *Encode instructions into binary formats
       identification division.
       program-id. asmxbin.
       environment division.
       input-output section.
       file-control.
      *Binary output
           select optional fs-binary assign to "program.bin"
           organization is sequential.
      *Instruction list
           select optional fs-insns assign to "program.ins"
           organization is line sequential.
       data division.
       file section.
       fd  fs-binary.
       01  fs-binary-char usage binary-char.
       fd  fs-insns.
       copy "asinsn.cpy" replacing ==:pref:== BY ==fs==.
       working-storage section.
       01  ws-binary-char usage binary-char.
       copy "asinsn.cpy" replacing ==:pref:== BY ==ws==.
       copy "asopcp.cpy" replacing ==:pref:== BY ==ws==.
       01  i pic 9(4) comp.
       01  ws-code usage binary-char.
       01  ws-reg-x0 pic 9(2).
       01  ws-reg-x1 pic 9(2).
       01  ws-reg-x2 pic 9(2).
       linkage section.
       procedure division.
      *Output binary
           open input sharing with all fs-insns.
           perform write-binary.
           close fs-insns.
           goback.
       write-binary.
           open extend fs-binary.
           perform forever
               read fs-insns
                   at end exit perform
                   not at end perform write-binary-insn
               end-read
           end-perform.
           close fs-binary.
       write-binary-insn.
      *Get the opcode information for this specific instruction
           move fs-insn to ws-insn.
           move ws-insn-opcode to ws-opcode-name.
           call "asmxopcd" using ws-opcode end-call.
      *
           move ws-insn-x0 to ws-reg-x0.
           move ws-insn-x1 to ws-reg-x1.
           move ws-insn-x2 to ws-reg-x2.
           evaluate true
               when ws-insn-rrrr-set
      *Higher half
                   move ws-reg-x2 to ws-binary-char
                   multiply ws-binary-char by 16 giving ws-binary-char
                   end-multiply
      *Lower half
                   add ws-reg-x1 to ws-binary-char giving ws-binary-char
                   end-add
                   write fs-binary-char from ws-binary-char end-write
           end-evaluate.
      *We live in a society
           perform code-to-number
           evaluate true
               when ws-opcode-mode-a
      *X0 is located on the higher half, the code is on the lower
                   move ws-reg-x0 to ws-binary-char
                   multiply ws-binary-char by 16 giving ws-binary-char
                   end-multiply
      *Lower half
                   add ws-code to ws-binary-char giving ws-binary-char
                   end-add
                   write fs-binary-char from ws-binary-char end-write
               when ws-opcode-mode-b
      *X0 is located on the higher half, the code is on the lower
                   move ws-reg-x0 to ws-binary-char
                   multiply ws-binary-char by 16 giving ws-binary-char
                   end-multiply
      *Lower half
                   add 15 to ws-binary-char giving ws-binary-char
                   end-add
                   write fs-binary-char from ws-binary-char end-write
                   move ws-code to ws-binary-char
                   write fs-binary-char from ws-binary-char end-write
           end-evaluate.
      *Convert the binary code into a number we can use
       code-to-number.
           initialize ws-code.
           perform varying i from 1 by 1
           until i > length of ws-opcode-code
               evaluate ws-opcode-code(i:1)
                   when '0'
                       multiply ws-code by 2 giving ws-code end-multiply
                   when '1'
                       multiply ws-code by 2 giving ws-code end-multiply
                       add 1 to ws-code giving ws-code end-add
                   when 'a' thru 'z'
                       multiply ws-code by 2 giving ws-code end-multiply
                   when 'A' thru 'Z'
                       multiply ws-code by 2 giving ws-code end-multiply
               end-evaluate
           end-perform.
       end program asmxbin.
      ******************************************************************
      *Obtain information of a given instruction
       identification division.
       program-id. asmxopcd.
       environment division.
       input-output section.
       file-control.
      *Define list
           select fs-opcodes assign to "opcodes.def"
           organization is line sequential.
       data division.
       file section.
       fd  fs-opcodes.
       copy "asopcp.cpy" replacing ==:pref:== BY ==fs==.
       working-storage section.
       copy "asopcp.cpy" replacing ==:pref:== BY ==ws==.
       linkage section.
       copy "asopcp.cpy" replacing ==:pref:== BY ==ls==.
       procedure division using ls-opcode.
           open input fs-opcodes.
           perform forever
               read fs-opcodes into ws-opcode
                   at end exit perform
                   not at end 
                       if ws-opcode-mode not = '*'
                       and ws-opcode-name = ls-opcode-name then
                           move ws-opcode to ls-opcode
                           exit perform
                       end-if
               end-read
           end-perform.
           close fs-opcodes.
           goback.
       end program asmxopcd.
      ******************************************************************
      *Obtain legnth of instruction
       identification division.
       program-id. asmxglen.
       data division.
       working-storage section.
       copy "asopcp.cpy" replacing ==:pref:== BY ==ws==.
       linkage section.
       01  ls-data.
           05  ls-name pic x(12).
           05  ls-length pic 9(4).
       procedure division using ls-data.
           move ls-name to ws-opcode-name.
           call "asmxopcd" using ws-opcode end-call.
           evaluate true
               when ws-opcode-mode-a move 1 to ls-length
               when ws-opcode-mode-b move 2 to ls-length
               when ws-opcode-mode-c move 3 to ls-length
               when ws-opcode-mode-d move 4 to ls-length
               when ws-opcode-mode-e move 5 to ls-length
           end-evaluate.
           goback.
       end program asmxglen.
      ******************************************************************
      *Convert to uppercase program
       identification division.
       program-id. asmxcasu.
       data division.
       working-storage section.
       01  i pic 9(4) comp.
       01  j pic 9(4) comp.
       01  ws-lcas pic x(27) value "qwertyuiopasdfghjklzxcvbnm".
       01  ws-ucas pic x(27) value "QWERTYUIOPASDFGHJKLZXCVBNM".
       linkage section.
       77  ls-line pic x(80).
       procedure division using ls-line.
           perform varying i from 1 by 1 until i > length of ls-line
               perform varying j from 1 by 1 until j > length of ws-ucas
                  if ls-line(i:1) is = ws-lcas(j:1) then
                      move ws-ucas(j:1) to ls-line(i:1)
                  end-if
               end-perform
           end-perform.
           goback.
       end program asmxcasu.
      ******************************************************************
      *Macro expander program
       identification division.
       program-id. asmxexpn.
       environment division.
       input-output section.
       file-control.
      *Define list
           select optional fs-defines assign to "program.def"
           organization is indexed
           access is sequential
           record key is fs-define-name.
       data division.
       file section.
       fd  fs-defines.
       copy "asdefn.cpy" replacing ==:pref:== BY ==fs==.
       working-storage section.
       01  i pic 9(4) comp.
       01  j pic 9(4) comp.
       01  k pic 9(4) comp.
       01  l pic 9(4) comp.
       copy "asdefn.cpy" replacing ==:pref:== BY ==ws==.
       linkage section.
       01  ls-data.
           05  ls-data-input pic x(80).
           05  ls-data-output pic x(80).
       procedure division using ls-data.
           open input sharing with all fs-defines.
      *
           move 1 to j.
           perform varying i from 1 by 1
           until i > length of ls-data-input
           or j > length of ls-data-output
               if ls-data-input(i:1) = '$' then
      *Skip the dollar sign
                   add 1 to i giving i end-add
                   perform start-define
               else
                   move ls-data-input(i:1) to ls-data-output(j:1)
                   add 1 to j end-add
               end-if
           end-perform.
      *
           close fs-defines.
           goback.
       start-define.
           initialize ws-define.
      *Obtain the name of the define
           move 1 to k.
           perform varying i from i by 1 until ls-data-input(i:1) = ' '
               move ls-data-input(i:1) to ws-define-name(k:1)
               add 1 to k end-add
           end-perform.
           perform forever
               read fs-defines
                   at end exit perform
                   not at end
                      if ws-define-name = fs-define-name
                          perform found-define
                          exit perform
                      end-if
               end-read
           end-perform.
       found-define.
           move fs-define to ws-define.
           perform varying k from 1 by 1
           until k > length of ws-define-body
           or j > length of ls-data-output
               move ws-define-body(k:1) to ls-data-output(j:1)
               add 1 to j end-add
           end-perform.
       end program asmxexpn.
