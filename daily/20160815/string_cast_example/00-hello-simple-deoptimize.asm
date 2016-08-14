TEXT	"".main(SB), $144-0
MOVQ	(TLS), CX
LEAQ	-16(SP), AX
CMPQ	AX, 16(CX)
JLS	299
SUBQ	$144, SP
FUNCDATA	$0, gclocals·7d2d5fca80364273fb07d5820a76fef4(SB)
FUNCDATA	$1, gclocals·ac3ef59631d389f8690304bed09d23c2(SB)
LEAQ	go.string."hello world"(SB), BX
MOVQ	BX, "".s+56(SP)
MOVQ	$11, "".s+64(SP)
MOVQ	"".s+56(SP), BX
MOVQ	BX, "".autotmp_0000+104(SP)
MOVQ	"".s+64(SP), BX
MOVQ	BX, "".autotmp_0000+112(SP)
MOVQ	$0, BX
MOVQ	BX, "".autotmp_0004+88(SP)
MOVQ	BX, "".autotmp_0004+96(SP)
LEAQ	"".autotmp_0004+88(SP), BX
MOVQ	BX, "".autotmp_0002+48(SP)
MOVQ	"".autotmp_0002+48(SP), BX
CMPQ	BX, $0
JEQ	$1, 292
MOVQ	$1, "".autotmp_0001+128(SP)
MOVQ	$1, "".autotmp_0001+136(SP)
MOVQ	BX, "".autotmp_0001+120(SP)
LEAQ	type.string(SB), BX
MOVQ	BX, (SP)
LEAQ	"".autotmp_0000+104(SP), BX
MOVQ	BX, 8(SP)
MOVQ	$0, 16(SP)
PCDATA	$0, $1
CALL	runtime.convT2E(SB)
MOVQ	24(SP), BX
MOVQ	BX, "".autotmp_0005+72(SP)
MOVQ	32(SP), BX
MOVQ	BX, "".autotmp_0005+80(SP)
MOVQ	"".autotmp_0001+120(SP), BX
MOVQ	"".autotmp_0005+72(SP), BP
MOVQ	BP, (BX)
MOVQ	"".autotmp_0005+80(SP), BP
CMPB	runtime.writeBarrier(SB), $0
JNE	$0, 272
MOVQ	BP, 8(BX)
MOVQ	"".autotmp_0001+120(SP), BX
MOVQ	BX, (SP)
MOVQ	"".autotmp_0001+128(SP), BX
MOVQ	BX, 8(SP)
MOVQ	"".autotmp_0001+136(SP), BX
MOVQ	BX, 16(SP)
PCDATA	$0, $2
CALL	fmt.Println(SB)
ADDQ	$144, SP
RET
LEAQ	8(BX), R8
MOVQ	R8, (SP)
MOVQ	BP, 8(SP)
PCDATA	$0, $1
CALL	runtime.writebarrierptr(SB)
JMP	224
MOVL	AX, (BX)
JMP	109
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
TEXT	"".init(SB), $0-0
MOVQ	(TLS), CX
CMPQ	SP, 16(CX)
JLS	65
NOP
NOP
FUNCDATA	$0, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
MOVB	"".initdone·(SB), BL
CMPB	BL, $0
JEQ	45
MOVB	"".initdone·(SB), BL
CMPB	BL, $2
JNE	38
RET
PCDATA	$0, $0
CALL	runtime.throwinit(SB)
UNDEF
MOVB	$1, "".initdone·(SB)
PCDATA	$0, $0
CALL	fmt.init(SB)
MOVB	$2, "".initdone·(SB)
RET
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
TEXT	type..hash.[1]interface {}(SB), $48-24
MOVQ	(TLS), CX
CMPQ	SP, 16(CX)
JLS	175
SUBQ	$48, SP
FUNCDATA	$0, gclocals·0b86ef39f3fed835f14ba5f4d7c62fa2(SB)
FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
MOVQ	$0, "".~r2+72(FP)
MOVQ	$0, "".autotmp_0006+40(SP)
MOVQ	$1, "".autotmp_0007+32(SP)
MOVQ	"".autotmp_0006+40(SP), BX
MOVQ	"".autotmp_0007+32(SP), BP
CMPQ	BX, BP
JGE	$0, 156
MOVQ	"".autotmp_0006+40(SP), BX
MOVQ	BX, "".i+24(SP)
MOVQ	"".p+56(FP), BX
CMPQ	BX, $0
JEQ	$1, 171
MOVQ	"".i+24(SP), BP
SHLQ	$4, BP
ADDQ	BP, BX
MOVQ	BX, (SP)
MOVQ	"".h+64(FP), BX
MOVQ	BX, 8(SP)
PCDATA	$0, $0
CALL	runtime.nilinterhash(SB)
MOVQ	16(SP), BX
MOVQ	BX, "".h+64(FP)
MOVQ	"".autotmp_0006+40(SP), BX
ADDQ	$1, BX
MOVQ	BX, "".autotmp_0006+40(SP)
MOVQ	"".autotmp_0006+40(SP), BX
MOVQ	"".autotmp_0007+32(SP), BP
CMPQ	BX, BP
JLT	$0, 65
MOVQ	"".h+64(FP), BX
MOVQ	BX, "".~r2+72(FP)
ADDQ	$48, SP
RET
MOVL	AX, (BX)
JMP	86
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
TEXT	type..eq.[1]interface {}(SB), $96-24
MOVQ	(TLS), CX
CMPQ	SP, 16(CX)
JLS	298
SUBQ	$96, SP
FUNCDATA	$0, gclocals·3bb21ca8fe1d99a3e492463bd711418a(SB)
FUNCDATA	$1, gclocals·a8eabfc4a4514ed6b3b0c61e9680e440(SB)
MOVB	$0, "".~r2+120(FP)
MOVQ	$0, "".autotmp_0008+56(SP)
MOVQ	$1, "".autotmp_0009+48(SP)
MOVQ	"".autotmp_0008+56(SP), BX
MOVQ	"".autotmp_0009+48(SP), BP
CMPQ	BX, BP
JGE	$0, 264
MOVQ	"".autotmp_0008+56(SP), BX
MOVQ	BX, "".i+40(SP)
MOVQ	"".q+112(FP), BX
CMPQ	BX, $0
JEQ	$1, 291
MOVQ	"".i+40(SP), BP
SHLQ	$4, BP
ADDQ	BP, BX
MOVQ	(BX), BP
MOVQ	BP, "".autotmp_0010+80(SP)
MOVQ	8(BX), BP
MOVQ	BP, "".autotmp_0010+88(SP)
MOVQ	"".p+104(FP), BX
CMPQ	BX, $0
JEQ	$1, 284
MOVQ	"".i+40(SP), BP
SHLQ	$4, BP
ADDQ	BP, BX
MOVQ	(BX), BP
MOVQ	BP, "".autotmp_0011+64(SP)
MOVQ	8(BX), BP
MOVQ	BP, "".autotmp_0011+72(SP)
MOVQ	"".autotmp_0011+64(SP), BX
MOVQ	"".autotmp_0010+80(SP), BP
CMPQ	BX, BP
JNE	274
MOVQ	"".autotmp_0011+64(SP), BP
MOVQ	BP, (SP)
MOVQ	"".autotmp_0011+72(SP), BP
MOVQ	BP, 8(SP)
MOVQ	"".autotmp_0010+80(SP), BP
MOVQ	BP, 16(SP)
MOVQ	"".autotmp_0010+88(SP), BP
MOVQ	BP, 24(SP)
PCDATA	$0, $0
CALL	runtime.efaceeq(SB)
MOVB	32(SP), BL
CMPB	BL, $0
JEQ	274
MOVQ	"".autotmp_0008+56(SP), BX
ADDQ	$1, BX
MOVQ	BX, "".autotmp_0008+56(SP)
MOVQ	"".autotmp_0008+56(SP), BX
MOVQ	"".autotmp_0009+48(SP), BP
CMPQ	BX, BP
JLT	$0, 65
MOVB	$1, "".~r2+120(FP)
ADDQ	$96, SP
RET
MOVB	$0, "".~r2+120(FP)
ADDQ	$96, SP
RET
MOVL	AX, (BX)
JMP	134
MOVL	AX, (BX)
JMP	90
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
