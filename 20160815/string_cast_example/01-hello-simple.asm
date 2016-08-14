TEXT	"".main(SB), $168-0
MOVQ	(TLS), CX
LEAQ	-40(SP), AX
CMPQ	AX, 16(CX)
JLS	342
SUBQ	$168, SP
FUNCDATA	$0, gclocals·7d2d5fca80364273fb07d5820a76fef4(SB)
FUNCDATA	$1, gclocals·60db3765efb333efb90976c2f8035659(SB)
LEAQ	go.string."hello world"(SB), CX
MOVQ	$11, AX
MOVQ	$0, (SP)
MOVQ	CX, "".s+48(SP)
MOVQ	CX, 8(SP)
MOVQ	AX, "".s+56(SP)
MOVQ	AX, 16(SP)
PCDATA	$0, $0
CALL	runtime.stringtoslicebyte(SB)
MOVQ	24(SP), DX
MOVQ	32(SP), CX
MOVQ	40(SP), AX
MOVQ	DX, "".casted+96(SP)
MOVQ	DX, "".autotmp_0000+144(SP)
MOVQ	CX, "".casted+104(SP)
MOVQ	CX, "".autotmp_0000+152(SP)
MOVQ	AX, "".casted+112(SP)
MOVQ	AX, "".autotmp_0000+160(SP)
MOVQ	$0, BX
MOVQ	BX, "".autotmp_0004+80(SP)
MOVQ	BX, "".autotmp_0004+88(SP)
LEAQ	"".autotmp_0004+80(SP), BX
CMPQ	BX, $0
JEQ	$1, 335
MOVQ	$1, "".autotmp_0001+128(SP)
MOVQ	$1, "".autotmp_0001+136(SP)
MOVQ	BX, "".autotmp_0001+120(SP)
LEAQ	type.[]uint8(SB), BX
MOVQ	BX, (SP)
LEAQ	"".autotmp_0000+144(SP), BX
MOVQ	BX, 8(SP)
MOVQ	$0, 16(SP)
PCDATA	$0, $1
CALL	runtime.convT2E(SB)
MOVQ	24(SP), CX
MOVQ	32(SP), AX
MOVQ	"".autotmp_0001+120(SP), BX
MOVQ	CX, "".autotmp_0005+64(SP)
MOVQ	CX, (BX)
MOVQ	AX, "".autotmp_0005+72(SP)
CMPB	runtime.writeBarrier(SB), $0
JNE	$0, 315
MOVQ	AX, 8(BX)
MOVQ	"".autotmp_0001+120(SP), BX
MOVQ	BX, (SP)
MOVQ	"".autotmp_0001+128(SP), BX
MOVQ	BX, 8(SP)
MOVQ	"".autotmp_0001+136(SP), BX
MOVQ	BX, 16(SP)
PCDATA	$0, $2
CALL	fmt.Println(SB)
ADDQ	$168, SP
RET
LEAQ	8(BX), R8
MOVQ	R8, (SP)
MOVQ	AX, 8(SP)
PCDATA	$0, $1
CALL	runtime.writebarrierptr(SB)
JMP	267
MOVL	AX, (BX)
JMP	159
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
TEXT	"".init(SB), $0-0
MOVQ	(TLS), CX
CMPQ	SP, 16(CX)
JLS	67
NOP
NOP
FUNCDATA	$0, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
MOVBQZX	"".initdone·(SB), BX
CMPB	BL, $0
JEQ	47
MOVBQZX	"".initdone·(SB), BX
CMPB	BL, $2
JNE	40
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
TEXT	type..hash.[1]interface {}(SB), $40-24
MOVQ	(TLS), CX
CMPQ	SP, 16(CX)
JLS	127
SUBQ	$40, SP
MOVQ	"".h+56(FP), CX
FUNCDATA	$0, gclocals·0b86ef39f3fed835f14ba5f4d7c62fa2(SB)
FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
MOVQ	$0, AX
MOVQ	$1, "".autotmp_0007+24(SP)
MOVQ	"".autotmp_0007+24(SP), BP
CMPQ	AX, BP
JGE	$0, 113
MOVQ	AX, "".autotmp_0006+32(SP)
MOVQ	"".p+48(FP), BX
CMPQ	BX, $0
JEQ	$1, 123
MOVQ	AX, BP
SHLQ	$4, BP
ADDQ	BP, BX
MOVQ	BX, (SP)
MOVQ	CX, "".h+56(FP)
MOVQ	CX, 8(SP)
PCDATA	$0, $0
CALL	runtime.nilinterhash(SB)
MOVQ	16(SP), CX
MOVQ	"".autotmp_0006+32(SP), AX
INCQ	AX
MOVQ	"".autotmp_0007+24(SP), BP
CMPQ	AX, BP
JLT	$0, 45
MOVQ	CX, "".~r2+64(FP)
ADDQ	$40, SP
RET
MOVL	AX, (BX)
JMP	61
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
TEXT	type..eq.[1]interface {}(SB), $88-24
MOVQ	(TLS), CX
CMPQ	SP, 16(CX)
JLS	225
SUBQ	$88, SP
FUNCDATA	$0, gclocals·3bb21ca8fe1d99a3e492463bd711418a(SB)
FUNCDATA	$1, gclocals·a8eabfc4a4514ed6b3b0c61e9680e440(SB)
MOVQ	$0, AX
MOVQ	$1, "".autotmp_0009+40(SP)
MOVQ	"".autotmp_0009+40(SP), BP
CMPQ	AX, BP
JGE	$0, 194
MOVQ	AX, "".autotmp_0008+48(SP)
MOVQ	"".q+104(FP), BX
CMPQ	BX, $0
JEQ	$1, 218
MOVQ	AX, BP
SHLQ	$4, BP
ADDQ	BP, BX
MOVQ	(BX), CX
MOVQ	8(BX), SI
MOVQ	"".p+96(FP), BX
CMPQ	BX, $0
JEQ	$1, 214
MOVQ	AX, BP
SHLQ	$4, BP
ADDQ	BP, BX
MOVQ	(BX), AX
MOVQ	8(BX), DX
CMPQ	AX, CX
JNE	204
MOVQ	AX, "".autotmp_0011+56(SP)
MOVQ	AX, (SP)
MOVQ	DX, "".autotmp_0011+64(SP)
MOVQ	DX, 8(SP)
MOVQ	CX, "".autotmp_0010+72(SP)
MOVQ	CX, 16(SP)
MOVQ	SI, "".autotmp_0010+80(SP)
MOVQ	SI, 24(SP)
PCDATA	$0, $0
CALL	runtime.efaceeq(SB)
MOVBQZX	32(SP), BX
CMPB	BL, $0
JEQ	204
MOVQ	"".autotmp_0008+48(SP), AX
INCQ	AX
MOVQ	"".autotmp_0009+40(SP), BP
CMPQ	AX, BP
JLT	$0, 48
MOVB	$1, "".~r2+112(FP)
ADDQ	$88, SP
RET
MOVB	$0, "".~r2+112(FP)
ADDQ	$88, SP
RET
MOVL	AX, (BX)
JMP	96
MOVL	AX, (BX)
JMP	68
NOP
CALL	runtime.morestack_noctxt(SB)
JMP	0
