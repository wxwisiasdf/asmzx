$ORG    X'0000F000'

$DEFINE ZERO,X0

$DEFINE T0,X1
$DEFINE T1,X2
$DEFINE T2,X3
$DEFINE T4,X4

$DEFINE SP,X14
$DEFINE PC,X15

@START  LDC                         Clear load direction flag
        SDC                         Clear store direction flag
        CLR     X0,X1,X2            Clear registers
        CLR     X3,X4,X5            In non-flexible mode the assembler
        CLR     X6,X7,X8            will ignore the 2 last operands
        CLR     X9,X10,X11
        CLR     X12,X13,X14
        CLR     X15
        SSP     $SP                 Set stack pointer to %SP
        SPC     $PC                 Set program pointer to %PC
        RRS                         Enable flexible registers
$RSET   XOR     $SP,$SP,$SP
        DDD     $SP,$PC,X0
@LOOP   GPC     $T0
        JMP     $T0

        RRC
$RCLR                               Clear flexible RRRR-RRRR mode for
                                    assembler
