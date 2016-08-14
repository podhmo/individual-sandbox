"".main t=1 size=416 value=0 args=0x0 locals=0xb0
	0x0000 00000 (01-hello.go:7)	TEXT	"".main(SB), $176-0
	0x0000 00000 (01-hello.go:7)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:7)	LEAQ	-48(SP), AX
	0x000e 00014 (01-hello.go:7)	CMPQ	AX, 16(CX)
	0x0012 00018 (01-hello.go:7)	JLS	393
	0x0018 00024 (01-hello.go:7)	SUBQ	$176, SP
	0x001f 00031 (01-hello.go:7)	FUNCDATA	$0, gclocals·7d2d5fca80364273fb07d5820a76fef4(SB)
	0x001f 00031 (01-hello.go:7)	FUNCDATA	$1, gclocals·decfa17fe033bd4c3f6d4e5d747ef904(SB)
	0x001f 00031 (01-hello.go:8)	LEAQ	go.string."hello world"(SB), BX
	0x0026 00038 (01-hello.go:8)	MOVQ	BX, "".s+56(SP)
	0x002b 00043 (01-hello.go:8)	MOVQ	$11, "".s+64(SP)
	0x0034 00052 (01-hello.go:9)	MOVQ	$0, (SP)
	0x003c 00060 (01-hello.go:9)	MOVQ	"".s+56(SP), BX
	0x0041 00065 (01-hello.go:9)	MOVQ	BX, 8(SP)
	0x0046 00070 (01-hello.go:9)	MOVQ	"".s+64(SP), BX
	0x004b 00075 (01-hello.go:9)	MOVQ	BX, 16(SP)
	0x0050 00080 (01-hello.go:9)	PCDATA	$0, $0
	0x0050 00080 (01-hello.go:9)	CALL	runtime.stringtoslicebyte(SB)
	0x0055 00085 (01-hello.go:9)	MOVQ	24(SP), BX
	0x005a 00090 (01-hello.go:9)	MOVQ	BX, "".casted+104(SP)
	0x005f 00095 (01-hello.go:9)	MOVQ	32(SP), BX
	0x0064 00100 (01-hello.go:9)	MOVQ	BX, "".casted+112(SP)
	0x0069 00105 (01-hello.go:9)	MOVQ	40(SP), BX
	0x006e 00110 (01-hello.go:9)	MOVQ	BX, "".casted+120(SP)
	0x0073 00115 (01-hello.go:10)	MOVQ	"".casted+104(SP), BX
	0x0078 00120 (01-hello.go:10)	MOVQ	BX, "".autotmp_0000+152(SP)
	0x0080 00128 (01-hello.go:10)	MOVQ	"".casted+112(SP), BX
	0x0085 00133 (01-hello.go:10)	MOVQ	BX, "".autotmp_0000+160(SP)
	0x008d 00141 (01-hello.go:10)	MOVQ	"".casted+120(SP), BX
	0x0092 00146 (01-hello.go:10)	MOVQ	BX, "".autotmp_0000+168(SP)
	0x009a 00154 (01-hello.go:10)	MOVQ	$0, BX
	0x009c 00156 (01-hello.go:10)	MOVQ	BX, "".autotmp_0004+88(SP)
	0x00a1 00161 (01-hello.go:10)	MOVQ	BX, "".autotmp_0004+96(SP)
	0x00a6 00166 (01-hello.go:10)	LEAQ	"".autotmp_0004+88(SP), BX
	0x00ab 00171 (01-hello.go:10)	MOVQ	BX, "".autotmp_0002+48(SP)
	0x00b0 00176 (01-hello.go:10)	MOVQ	"".autotmp_0002+48(SP), BX
	0x00b5 00181 (01-hello.go:10)	CMPQ	BX, $0
	0x00b9 00185 (01-hello.go:10)	JEQ	$1, 386
	0x00bf 00191 (01-hello.go:10)	MOVQ	$1, "".autotmp_0001+136(SP)
	0x00cb 00203 (01-hello.go:10)	MOVQ	$1, "".autotmp_0001+144(SP)
	0x00d7 00215 (01-hello.go:10)	MOVQ	BX, "".autotmp_0001+128(SP)
	0x00df 00223 (01-hello.go:10)	LEAQ	type.[]uint8(SB), BX
	0x00e6 00230 (01-hello.go:10)	MOVQ	BX, (SP)
	0x00ea 00234 (01-hello.go:10)	LEAQ	"".autotmp_0000+152(SP), BX
	0x00f2 00242 (01-hello.go:10)	MOVQ	BX, 8(SP)
	0x00f7 00247 (01-hello.go:10)	MOVQ	$0, 16(SP)
	0x0100 00256 (01-hello.go:10)	PCDATA	$0, $1
	0x0100 00256 (01-hello.go:10)	CALL	runtime.convT2E(SB)
	0x0105 00261 (01-hello.go:10)	MOVQ	24(SP), BX
	0x010a 00266 (01-hello.go:10)	MOVQ	BX, "".autotmp_0005+72(SP)
	0x010f 00271 (01-hello.go:10)	MOVQ	32(SP), BX
	0x0114 00276 (01-hello.go:10)	MOVQ	BX, "".autotmp_0005+80(SP)
	0x0119 00281 (01-hello.go:10)	MOVQ	"".autotmp_0001+128(SP), BX
	0x0121 00289 (01-hello.go:10)	MOVQ	"".autotmp_0005+72(SP), BP
	0x0126 00294 (01-hello.go:10)	MOVQ	BP, (BX)
	0x0129 00297 (01-hello.go:10)	MOVQ	"".autotmp_0005+80(SP), BP
	0x012e 00302 (01-hello.go:10)	CMPB	runtime.writeBarrier(SB), $0
	0x0135 00309 (01-hello.go:10)	JNE	$0, 366
	0x0137 00311 (01-hello.go:10)	MOVQ	BP, 8(BX)
	0x013b 00315 (01-hello.go:10)	MOVQ	"".autotmp_0001+128(SP), BX
	0x0143 00323 (01-hello.go:10)	MOVQ	BX, (SP)
	0x0147 00327 (01-hello.go:10)	MOVQ	"".autotmp_0001+136(SP), BX
	0x014f 00335 (01-hello.go:10)	MOVQ	BX, 8(SP)
	0x0154 00340 (01-hello.go:10)	MOVQ	"".autotmp_0001+144(SP), BX
	0x015c 00348 (01-hello.go:10)	MOVQ	BX, 16(SP)
	0x0161 00353 (01-hello.go:10)	PCDATA	$0, $2
	0x0161 00353 (01-hello.go:10)	CALL	fmt.Println(SB)
	0x0166 00358 (01-hello.go:11)	ADDQ	$176, SP
	0x016d 00365 (01-hello.go:11)	RET
	0x016e 00366 (01-hello.go:10)	LEAQ	8(BX), R8
	0x0172 00370 (01-hello.go:10)	MOVQ	R8, (SP)
	0x0176 00374 (01-hello.go:10)	MOVQ	BP, 8(SP)
	0x017b 00379 (01-hello.go:10)	PCDATA	$0, $1
	0x017b 00379 (01-hello.go:10)	CALL	runtime.writebarrierptr(SB)
	0x0180 00384 (01-hello.go:10)	JMP	315
	0x0182 00386 (01-hello.go:10)	MOVL	AX, (BX)
	0x0184 00388 (01-hello.go:10)	JMP	191
	0x0189 00393 (01-hello.go:10)	NOP
	0x0189 00393 (01-hello.go:7)	CALL	runtime.morestack_noctxt(SB)
	0x018e 00398 (01-hello.go:7)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 8d 44 24 d0 48 3b  eH..%....H.D$.H;
	0x0010 41 10 0f 86 71 01 00 00 48 81 ec b0 00 00 00 48  A...q...H......H
	0x0020 8d 1d 00 00 00 00 48 89 5c 24 38 48 c7 44 24 40  ......H.\$8H.D$@
	0x0030 0b 00 00 00 48 c7 04 24 00 00 00 00 48 8b 5c 24  ....H..$....H.\$
	0x0040 38 48 89 5c 24 08 48 8b 5c 24 40 48 89 5c 24 10  8H.\$.H.\$@H.\$.
	0x0050 e8 00 00 00 00 48 8b 5c 24 18 48 89 5c 24 68 48  .....H.\$.H.\$hH
	0x0060 8b 5c 24 20 48 89 5c 24 70 48 8b 5c 24 28 48 89  .\$ H.\$pH.\$(H.
	0x0070 5c 24 78 48 8b 5c 24 68 48 89 9c 24 98 00 00 00  \$xH.\$hH..$....
	0x0080 48 8b 5c 24 70 48 89 9c 24 a0 00 00 00 48 8b 5c  H.\$pH..$....H.\
	0x0090 24 78 48 89 9c 24 a8 00 00 00 31 db 48 89 5c 24  $xH..$....1.H.\$
	0x00a0 58 48 89 5c 24 60 48 8d 5c 24 58 48 89 5c 24 30  XH.\$`H.\$XH.\$0
	0x00b0 48 8b 5c 24 30 48 83 fb 00 0f 84 c3 00 00 00 48  H.\$0H.........H
	0x00c0 c7 84 24 88 00 00 00 01 00 00 00 48 c7 84 24 90  ..$........H..$.
	0x00d0 00 00 00 01 00 00 00 48 89 9c 24 80 00 00 00 48  .......H..$....H
	0x00e0 8d 1d 00 00 00 00 48 89 1c 24 48 8d 9c 24 98 00  ......H..$H..$..
	0x00f0 00 00 48 89 5c 24 08 48 c7 44 24 10 00 00 00 00  ..H.\$.H.D$.....
	0x0100 e8 00 00 00 00 48 8b 5c 24 18 48 89 5c 24 48 48  .....H.\$.H.\$HH
	0x0110 8b 5c 24 20 48 89 5c 24 50 48 8b 9c 24 80 00 00  .\$ H.\$PH..$...
	0x0120 00 48 8b 6c 24 48 48 89 2b 48 8b 6c 24 50 80 3d  .H.l$HH.+H.l$P.=
	0x0130 00 00 00 00 00 75 37 48 89 6b 08 48 8b 9c 24 80  .....u7H.k.H..$.
	0x0140 00 00 00 48 89 1c 24 48 8b 9c 24 88 00 00 00 48  ...H..$H..$....H
	0x0150 89 5c 24 08 48 8b 9c 24 90 00 00 00 48 89 5c 24  .\$.H..$....H.\$
	0x0160 10 e8 00 00 00 00 48 81 c4 b0 00 00 00 c3 4c 8d  ......H.......L.
	0x0170 43 08 4c 89 04 24 48 89 6c 24 08 e8 00 00 00 00  C.L..$H.l$......
	0x0180 eb b9 89 03 e9 36 ff ff ff e8 00 00 00 00 e9 6d  .....6.........m
	0x0190 fe ff ff cc cc cc cc cc cc cc cc cc cc cc cc cc  ................
	rel 5+4 t=14 +0
	rel 34+4 t=13 go.string."hello world"+0
	rel 81+4 t=6 runtime.stringtoslicebyte+0
	rel 226+4 t=13 type.[]uint8+0
	rel 257+4 t=6 runtime.convT2E+0
	rel 304+4 t=13 runtime.writeBarrier+-1
	rel 354+4 t=6 fmt.Println+0
	rel 380+4 t=6 runtime.writebarrierptr+0
	rel 394+4 t=6 runtime.morestack_noctxt+0
"".init t=1 size=80 value=0 args=0x0 locals=0x0
	0x0000 00000 (01-hello.go:11)	TEXT	"".init(SB), $0-0
	0x0000 00000 (01-hello.go:11)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:11)	CMPQ	SP, 16(CX)
	0x000d 00013 (01-hello.go:11)	JLS	65
	0x000f 00015 (01-hello.go:11)	NOP
	0x000f 00015 (01-hello.go:11)	NOP
	0x000f 00015 (01-hello.go:11)	FUNCDATA	$0, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
	0x000f 00015 (01-hello.go:11)	FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
	0x000f 00015 (01-hello.go:11)	MOVB	"".initdone·(SB), BL
	0x0015 00021 (01-hello.go:11)	CMPB	BL, $0
	0x0018 00024 (01-hello.go:11)	JEQ	45
	0x001a 00026 (01-hello.go:11)	MOVB	"".initdone·(SB), BL
	0x0020 00032 (01-hello.go:11)	CMPB	BL, $2
	0x0023 00035 (01-hello.go:11)	JNE	38
	0x0025 00037 (01-hello.go:11)	RET
	0x0026 00038 (01-hello.go:11)	PCDATA	$0, $0
	0x0026 00038 (01-hello.go:11)	CALL	runtime.throwinit(SB)
	0x002b 00043 (01-hello.go:11)	UNDEF
	0x002d 00045 (01-hello.go:11)	MOVB	$1, "".initdone·(SB)
	0x0034 00052 (01-hello.go:11)	PCDATA	$0, $0
	0x0034 00052 (01-hello.go:11)	CALL	fmt.init(SB)
	0x0039 00057 (01-hello.go:11)	MOVB	$2, "".initdone·(SB)
	0x0040 00064 (01-hello.go:11)	RET
	0x0041 00065 (01-hello.go:11)	NOP
	0x0041 00065 (01-hello.go:11)	CALL	runtime.morestack_noctxt(SB)
	0x0046 00070 (01-hello.go:11)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 3b 61 10 76 32 8a  eH..%....H;a.v2.
	0x0010 1d 00 00 00 00 80 fb 00 74 13 8a 1d 00 00 00 00  ........t.......
	0x0020 80 fb 02 75 01 c3 e8 00 00 00 00 0f 0b c6 05 00  ...u............
	0x0030 00 00 00 01 e8 00 00 00 00 c6 05 00 00 00 00 02  ................
	0x0040 c3 e8 00 00 00 00 eb b8 cc cc cc cc cc cc cc cc  ................
	rel 5+4 t=14 +0
	rel 17+4 t=13 "".initdone·+0
	rel 28+4 t=13 "".initdone·+0
	rel 39+4 t=6 runtime.throwinit+0
	rel 47+4 t=13 "".initdone·+-1
	rel 53+4 t=6 fmt.init+0
	rel 59+4 t=13 "".initdone·+-1
	rel 66+4 t=6 runtime.morestack_noctxt+0
type..hash.[1]interface {} t=1 dupok size=192 value=0 args=0x18 locals=0x30
	0x0000 00000 (01-hello.go:1)	TEXT	type..hash.[1]interface {}(SB), $48-24
	0x0000 00000 (01-hello.go:1)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:1)	CMPQ	SP, 16(CX)
	0x000d 00013 (01-hello.go:1)	JLS	175
	0x0013 00019 (01-hello.go:1)	SUBQ	$48, SP
	0x0017 00023 (01-hello.go:1)	FUNCDATA	$0, gclocals·0b86ef39f3fed835f14ba5f4d7c62fa2(SB)
	0x0017 00023 (01-hello.go:1)	FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
	0x0017 00023 (01-hello.go:1)	MOVQ	$0, "".~r2+72(FP)
	0x0020 00032 (01-hello.go:1)	MOVQ	$0, "".autotmp_0006+40(SP)
	0x0029 00041 (01-hello.go:1)	MOVQ	$1, "".autotmp_0007+32(SP)
	0x0032 00050 (01-hello.go:1)	MOVQ	"".autotmp_0006+40(SP), BX
	0x0037 00055 (01-hello.go:1)	MOVQ	"".autotmp_0007+32(SP), BP
	0x003c 00060 (01-hello.go:1)	CMPQ	BX, BP
	0x003f 00063 (01-hello.go:1)	JGE	$0, 156
	0x0041 00065 (01-hello.go:1)	MOVQ	"".autotmp_0006+40(SP), BX
	0x0046 00070 (01-hello.go:1)	MOVQ	BX, "".i+24(SP)
	0x004b 00075 (01-hello.go:1)	MOVQ	"".p+56(FP), BX
	0x0050 00080 (01-hello.go:1)	CMPQ	BX, $0
	0x0054 00084 (01-hello.go:1)	JEQ	$1, 171
	0x0056 00086 (01-hello.go:1)	MOVQ	"".i+24(SP), BP
	0x005b 00091 (01-hello.go:1)	SHLQ	$4, BP
	0x005f 00095 (01-hello.go:1)	ADDQ	BP, BX
	0x0062 00098 (01-hello.go:1)	MOVQ	BX, (SP)
	0x0066 00102 (01-hello.go:1)	MOVQ	"".h+64(FP), BX
	0x006b 00107 (01-hello.go:1)	MOVQ	BX, 8(SP)
	0x0070 00112 (01-hello.go:1)	PCDATA	$0, $0
	0x0070 00112 (01-hello.go:1)	CALL	runtime.nilinterhash(SB)
	0x0075 00117 (01-hello.go:1)	MOVQ	16(SP), BX
	0x007a 00122 (01-hello.go:1)	MOVQ	BX, "".h+64(FP)
	0x007f 00127 (01-hello.go:1)	MOVQ	"".autotmp_0006+40(SP), BX
	0x0084 00132 (01-hello.go:1)	ADDQ	$1, BX
	0x0088 00136 (01-hello.go:1)	MOVQ	BX, "".autotmp_0006+40(SP)
	0x008d 00141 (01-hello.go:1)	MOVQ	"".autotmp_0006+40(SP), BX
	0x0092 00146 (01-hello.go:1)	MOVQ	"".autotmp_0007+32(SP), BP
	0x0097 00151 (01-hello.go:1)	CMPQ	BX, BP
	0x009a 00154 (01-hello.go:1)	JLT	$0, 65
	0x009c 00156 (01-hello.go:1)	MOVQ	"".h+64(FP), BX
	0x00a1 00161 (01-hello.go:1)	MOVQ	BX, "".~r2+72(FP)
	0x00a6 00166 (01-hello.go:1)	ADDQ	$48, SP
	0x00aa 00170 (01-hello.go:1)	RET
	0x00ab 00171 (01-hello.go:1)	MOVL	AX, (BX)
	0x00ad 00173 (01-hello.go:1)	JMP	86
	0x00af 00175 (01-hello.go:1)	NOP
	0x00af 00175 (01-hello.go:1)	CALL	runtime.morestack_noctxt(SB)
	0x00b4 00180 (01-hello.go:1)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 3b 61 10 0f 86 9c  eH..%....H;a....
	0x0010 00 00 00 48 83 ec 30 48 c7 44 24 48 00 00 00 00  ...H..0H.D$H....
	0x0020 48 c7 44 24 28 00 00 00 00 48 c7 44 24 20 01 00  H.D$(....H.D$ ..
	0x0030 00 00 48 8b 5c 24 28 48 8b 6c 24 20 48 39 eb 7d  ..H.\$(H.l$ H9.}
	0x0040 5b 48 8b 5c 24 28 48 89 5c 24 18 48 8b 5c 24 38  [H.\$(H.\$.H.\$8
	0x0050 48 83 fb 00 74 55 48 8b 6c 24 18 48 c1 e5 04 48  H...tUH.l$.H...H
	0x0060 01 eb 48 89 1c 24 48 8b 5c 24 40 48 89 5c 24 08  ..H..$H.\$@H.\$.
	0x0070 e8 00 00 00 00 48 8b 5c 24 10 48 89 5c 24 40 48  .....H.\$.H.\$@H
	0x0080 8b 5c 24 28 48 83 c3 01 48 89 5c 24 28 48 8b 5c  .\$(H...H.\$(H.\
	0x0090 24 28 48 8b 6c 24 20 48 39 eb 7c a5 48 8b 5c 24  $(H.l$ H9.|.H.\$
	0x00a0 40 48 89 5c 24 48 48 83 c4 30 c3 89 03 eb a7 e8  @H.\$HH..0......
	0x00b0 00 00 00 00 e9 47 ff ff ff cc cc cc cc cc cc cc  .....G..........
	rel 5+4 t=14 +0
	rel 113+4 t=6 runtime.nilinterhash+0
	rel 176+4 t=6 runtime.morestack_noctxt+0
type..eq.[1]interface {} t=1 dupok size=320 value=0 args=0x18 locals=0x60
	0x0000 00000 (01-hello.go:1)	TEXT	type..eq.[1]interface {}(SB), $96-24
	0x0000 00000 (01-hello.go:1)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:1)	CMPQ	SP, 16(CX)
	0x000d 00013 (01-hello.go:1)	JLS	298
	0x0013 00019 (01-hello.go:1)	SUBQ	$96, SP
	0x0017 00023 (01-hello.go:1)	FUNCDATA	$0, gclocals·3bb21ca8fe1d99a3e492463bd711418a(SB)
	0x0017 00023 (01-hello.go:1)	FUNCDATA	$1, gclocals·a8eabfc4a4514ed6b3b0c61e9680e440(SB)
	0x0017 00023 (01-hello.go:1)	MOVB	$0, "".~r2+120(FP)
	0x001c 00028 (01-hello.go:1)	MOVQ	$0, "".autotmp_0008+56(SP)
	0x0025 00037 (01-hello.go:1)	MOVQ	$1, "".autotmp_0009+48(SP)
	0x002e 00046 (01-hello.go:1)	MOVQ	"".autotmp_0008+56(SP), BX
	0x0033 00051 (01-hello.go:1)	MOVQ	"".autotmp_0009+48(SP), BP
	0x0038 00056 (01-hello.go:1)	CMPQ	BX, BP
	0x003b 00059 (01-hello.go:1)	JGE	$0, 264
	0x0041 00065 (01-hello.go:1)	MOVQ	"".autotmp_0008+56(SP), BX
	0x0046 00070 (01-hello.go:1)	MOVQ	BX, "".i+40(SP)
	0x004b 00075 (01-hello.go:1)	MOVQ	"".q+112(FP), BX
	0x0050 00080 (01-hello.go:1)	CMPQ	BX, $0
	0x0054 00084 (01-hello.go:1)	JEQ	$1, 291
	0x005a 00090 (01-hello.go:1)	MOVQ	"".i+40(SP), BP
	0x005f 00095 (01-hello.go:1)	SHLQ	$4, BP
	0x0063 00099 (01-hello.go:1)	ADDQ	BP, BX
	0x0066 00102 (01-hello.go:1)	MOVQ	(BX), BP
	0x0069 00105 (01-hello.go:1)	MOVQ	BP, "".autotmp_0010+80(SP)
	0x006e 00110 (01-hello.go:1)	MOVQ	8(BX), BP
	0x0072 00114 (01-hello.go:1)	MOVQ	BP, "".autotmp_0010+88(SP)
	0x0077 00119 (01-hello.go:1)	MOVQ	"".p+104(FP), BX
	0x007c 00124 (01-hello.go:1)	CMPQ	BX, $0
	0x0080 00128 (01-hello.go:1)	JEQ	$1, 284
	0x0086 00134 (01-hello.go:1)	MOVQ	"".i+40(SP), BP
	0x008b 00139 (01-hello.go:1)	SHLQ	$4, BP
	0x008f 00143 (01-hello.go:1)	ADDQ	BP, BX
	0x0092 00146 (01-hello.go:1)	MOVQ	(BX), BP
	0x0095 00149 (01-hello.go:1)	MOVQ	BP, "".autotmp_0011+64(SP)
	0x009a 00154 (01-hello.go:1)	MOVQ	8(BX), BP
	0x009e 00158 (01-hello.go:1)	MOVQ	BP, "".autotmp_0011+72(SP)
	0x00a3 00163 (01-hello.go:1)	MOVQ	"".autotmp_0011+64(SP), BX
	0x00a8 00168 (01-hello.go:1)	MOVQ	"".autotmp_0010+80(SP), BP
	0x00ad 00173 (01-hello.go:1)	CMPQ	BX, BP
	0x00b0 00176 (01-hello.go:1)	JNE	274
	0x00b2 00178 (01-hello.go:1)	MOVQ	"".autotmp_0011+64(SP), BP
	0x00b7 00183 (01-hello.go:1)	MOVQ	BP, (SP)
	0x00bb 00187 (01-hello.go:1)	MOVQ	"".autotmp_0011+72(SP), BP
	0x00c0 00192 (01-hello.go:1)	MOVQ	BP, 8(SP)
	0x00c5 00197 (01-hello.go:1)	MOVQ	"".autotmp_0010+80(SP), BP
	0x00ca 00202 (01-hello.go:1)	MOVQ	BP, 16(SP)
	0x00cf 00207 (01-hello.go:1)	MOVQ	"".autotmp_0010+88(SP), BP
	0x00d4 00212 (01-hello.go:1)	MOVQ	BP, 24(SP)
	0x00d9 00217 (01-hello.go:1)	PCDATA	$0, $0
	0x00d9 00217 (01-hello.go:1)	CALL	runtime.efaceeq(SB)
	0x00de 00222 (01-hello.go:1)	MOVB	32(SP), BL
	0x00e2 00226 (01-hello.go:1)	CMPB	BL, $0
	0x00e5 00229 (01-hello.go:1)	JEQ	274
	0x00e7 00231 (01-hello.go:1)	MOVQ	"".autotmp_0008+56(SP), BX
	0x00ec 00236 (01-hello.go:1)	ADDQ	$1, BX
	0x00f0 00240 (01-hello.go:1)	MOVQ	BX, "".autotmp_0008+56(SP)
	0x00f5 00245 (01-hello.go:1)	MOVQ	"".autotmp_0008+56(SP), BX
	0x00fa 00250 (01-hello.go:1)	MOVQ	"".autotmp_0009+48(SP), BP
	0x00ff 00255 (01-hello.go:1)	CMPQ	BX, BP
	0x0102 00258 (01-hello.go:1)	JLT	$0, 65
	0x0108 00264 (01-hello.go:1)	MOVB	$1, "".~r2+120(FP)
	0x010d 00269 (01-hello.go:1)	ADDQ	$96, SP
	0x0111 00273 (01-hello.go:1)	RET
	0x0112 00274 (01-hello.go:1)	MOVB	$0, "".~r2+120(FP)
	0x0117 00279 (01-hello.go:1)	ADDQ	$96, SP
	0x011b 00283 (01-hello.go:1)	RET
	0x011c 00284 (01-hello.go:1)	MOVL	AX, (BX)
	0x011e 00286 (01-hello.go:1)	JMP	134
	0x0123 00291 (01-hello.go:1)	MOVL	AX, (BX)
	0x0125 00293 (01-hello.go:1)	JMP	90
	0x012a 00298 (01-hello.go:1)	NOP
	0x012a 00298 (01-hello.go:1)	CALL	runtime.morestack_noctxt(SB)
	0x012f 00303 (01-hello.go:1)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 3b 61 10 0f 86 17  eH..%....H;a....
	0x0010 01 00 00 48 83 ec 60 c6 44 24 78 00 48 c7 44 24  ...H..`.D$x.H.D$
	0x0020 38 00 00 00 00 48 c7 44 24 30 01 00 00 00 48 8b  8....H.D$0....H.
	0x0030 5c 24 38 48 8b 6c 24 30 48 39 eb 0f 8d c7 00 00  \$8H.l$0H9......
	0x0040 00 48 8b 5c 24 38 48 89 5c 24 28 48 8b 5c 24 70  .H.\$8H.\$(H.\$p
	0x0050 48 83 fb 00 0f 84 c9 00 00 00 48 8b 6c 24 28 48  H.........H.l$(H
	0x0060 c1 e5 04 48 01 eb 48 8b 2b 48 89 6c 24 50 48 8b  ...H..H.+H.l$PH.
	0x0070 6b 08 48 89 6c 24 58 48 8b 5c 24 68 48 83 fb 00  k.H.l$XH.\$hH...
	0x0080 0f 84 96 00 00 00 48 8b 6c 24 28 48 c1 e5 04 48  ......H.l$(H...H
	0x0090 01 eb 48 8b 2b 48 89 6c 24 40 48 8b 6b 08 48 89  ..H.+H.l$@H.k.H.
	0x00a0 6c 24 48 48 8b 5c 24 40 48 8b 6c 24 50 48 39 eb  l$HH.\$@H.l$PH9.
	0x00b0 75 60 48 8b 6c 24 40 48 89 2c 24 48 8b 6c 24 48  u`H.l$@H.,$H.l$H
	0x00c0 48 89 6c 24 08 48 8b 6c 24 50 48 89 6c 24 10 48  H.l$.H.l$PH.l$.H
	0x00d0 8b 6c 24 58 48 89 6c 24 18 e8 00 00 00 00 8a 5c  .l$XH.l$.......\
	0x00e0 24 20 80 fb 00 74 2b 48 8b 5c 24 38 48 83 c3 01  $ ...t+H.\$8H...
	0x00f0 48 89 5c 24 38 48 8b 5c 24 38 48 8b 6c 24 30 48  H.\$8H.\$8H.l$0H
	0x0100 39 eb 0f 8c 39 ff ff ff c6 44 24 78 01 48 83 c4  9...9....D$x.H..
	0x0110 60 c3 c6 44 24 78 00 48 83 c4 60 c3 89 03 e9 63  `..D$x.H..`....c
	0x0120 ff ff ff 89 03 e9 30 ff ff ff e8 00 00 00 00 e9  ......0.........
	0x0130 cc fe ff ff cc cc cc cc cc cc cc cc cc cc cc cc  ................
	rel 5+4 t=14 +0
	rel 218+4 t=6 runtime.efaceeq+0
	rel 299+4 t=6 runtime.morestack_noctxt+0
go.string.hdr."hello world" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 0b 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."hello world"+0
go.string."hello world" t=8 dupok size=16 value=0
	0x0000 68 65 6c 6c 6f 20 77 6f 72 6c 64 00              hello world.
gclocals·decfa17fe033bd4c3f6d4e5d747ef904 t=8 dupok size=20 value=0
	0x0000 03 00 00 00 10 00 00 00 00 00 00 00 60 24 00 00  ............`$..
	0x0010 60 20 00 00                                      ` ..
gclocals·7d2d5fca80364273fb07d5820a76fef4 t=8 dupok size=8 value=0
	0x0000 03 00 00 00 00 00 00 00                          ........
gclocals·33cdeccccebe80329f1fdbee7f5874cb t=8 dupok size=8 value=0
	0x0000 01 00 00 00 00 00 00 00                          ........
gclocals·33cdeccccebe80329f1fdbee7f5874cb t=8 dupok size=8 value=0
	0x0000 01 00 00 00 00 00 00 00                          ........
"".initdone· t=31 size=1 value=0
"".main·f t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 "".main+0
"".init·f t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 "".init+0
runtime.gcbits.01 t=8 dupok size=1 value=0
	0x0000 01                                               .
go.string.hdr."[]uint8" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 07 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."[]uint8"+0
go.string."[]uint8" t=8 dupok size=8 value=0
	0x0000 5b 5d 75 69 6e 74 38 00                          []uint8.
type.[]uint8 t=8 dupok size=72 value=0
	0x0000 18 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00  ................
	0x0010 df 7e 2e 38 00 08 08 17 00 00 00 00 00 00 00 00  .~.8............
	0x0020 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0040 00 00 00 00 00 00 00 00                          ........
	rel 24+8 t=1 runtime.algarray+272
	rel 32+8 t=1 runtime.gcbits.01+0
	rel 40+8 t=1 go.string.hdr."[]uint8"+0
	rel 56+8 t=1 go.weak.type.*[]uint8+0
	rel 64+8 t=1 type.uint8+0
go.typelink.[]uint8	[]uint8 t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type.[]uint8+0
runtime.gcbits.03 t=8 dupok size=1 value=0
	0x0000 03                                               .
go.string.hdr."interface {}" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 0c 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."interface {}"+0
go.string."interface {}" t=8 dupok size=16 value=0
	0x0000 69 6e 74 65 72 66 61 63 65 20 7b 7d 00           interface {}.
type.interface {} t=8 dupok size=88 value=0
	0x0000 10 00 00 00 00 00 00 00 10 00 00 00 00 00 00 00  ................
	0x0010 e7 57 a0 18 00 08 08 14 00 00 00 00 00 00 00 00  .W..............
	0x0020 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0040 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0050 00 00 00 00 00 00 00 00                          ........
	rel 24+8 t=1 runtime.algarray+256
	rel 32+8 t=1 runtime.gcbits.03+0
	rel 40+8 t=1 go.string.hdr."interface {}"+0
	rel 56+8 t=1 go.weak.type.*interface {}+0
	rel 64+8 t=1 type.interface {}+88
go.string.hdr."[]interface {}" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 0e 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."[]interface {}"+0
go.string."[]interface {}" t=8 dupok size=16 value=0
	0x0000 5b 5d 69 6e 74 65 72 66 61 63 65 20 7b 7d 00     []interface {}.
type.[]interface {} t=8 dupok size=72 value=0
	0x0000 18 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00  ................
	0x0010 70 93 ea 2f 00 08 08 17 00 00 00 00 00 00 00 00  p../............
	0x0020 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0040 00 00 00 00 00 00 00 00                          ........
	rel 24+8 t=1 runtime.algarray+272
	rel 32+8 t=1 runtime.gcbits.01+0
	rel 40+8 t=1 go.string.hdr."[]interface {}"+0
	rel 56+8 t=1 go.weak.type.*[]interface {}+0
	rel 64+8 t=1 type.interface {}+0
go.typelink.[]interface {}	[]interface {} t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type.[]interface {}+0
gclocals·33cdeccccebe80329f1fdbee7f5874cb t=8 dupok size=8 value=0
	0x0000 01 00 00 00 00 00 00 00                          ........
gclocals·0b86ef39f3fed835f14ba5f4d7c62fa2 t=8 dupok size=12 value=0
	0x0000 01 00 00 00 03 00 00 00 01 00 00 00              ............
gclocals·a8eabfc4a4514ed6b3b0c61e9680e440 t=8 dupok size=12 value=0
	0x0000 01 00 00 00 04 00 00 00 00 00 00 00              ............
gclocals·3bb21ca8fe1d99a3e492463bd711418a t=8 dupok size=12 value=0
	0x0000 01 00 00 00 03 00 00 00 03 00 00 00              ............
type..hashfunc.[1]interface {} t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type..hash.[1]interface {}+0
type..eqfunc.[1]interface {} t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type..eq.[1]interface {}+0
type..alg.[1]interface {} t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 type..hashfunc.[1]interface {}+0
	rel 8+8 t=1 type..eqfunc.[1]interface {}+0
go.string.hdr."[1]interface {}" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 0f 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."[1]interface {}"+0
go.string."[1]interface {}" t=8 dupok size=16 value=0
	0x0000 5b 31 5d 69 6e 74 65 72 66 61 63 65 20 7b 7d 00  [1]interface {}.
type.[1]interface {} t=8 dupok size=88 value=0
	0x0000 10 00 00 00 00 00 00 00 10 00 00 00 00 00 00 00  ................
	0x0010 50 91 5b fa 00 08 08 11 00 00 00 00 00 00 00 00  P.[.............
	0x0020 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0040 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0050 01 00 00 00 00 00 00 00                          ........
	rel 24+8 t=1 type..alg.[1]interface {}+0
	rel 32+8 t=1 runtime.gcbits.03+0
	rel 40+8 t=1 go.string.hdr."[1]interface {}"+0
	rel 56+8 t=1 go.weak.type.*[1]interface {}+0
	rel 64+8 t=1 type.interface {}+0
	rel 72+8 t=1 type.[]interface {}+0
go.typelink.[1]interface {}	[1]interface {} t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type.[1]interface {}+0
go.string.hdr."*[1]interface {}" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 10 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."*[1]interface {}"+0
go.string."*[1]interface {}" t=8 dupok size=24 value=0
	0x0000 2a 5b 31 5d 69 6e 74 65 72 66 61 63 65 20 7b 7d  *[1]interface {}
	0x0010 00                                               .
type.*[1]interface {} t=8 dupok size=72 value=0
	0x0000 08 00 00 00 00 00 00 00 08 00 00 00 00 00 00 00  ................
	0x0010 bf 03 a8 35 00 08 08 36 00 00 00 00 00 00 00 00  ...5...6........
	0x0020 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0030 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00  ................
	0x0040 00 00 00 00 00 00 00 00                          ........
	rel 24+8 t=1 runtime.algarray+80
	rel 32+8 t=1 runtime.gcbits.01+0
	rel 40+8 t=1 go.string.hdr."*[1]interface {}"+0
	rel 56+8 t=1 go.weak.type.**[1]interface {}+0
	rel 64+8 t=1 type.[1]interface {}+0
go.string.hdr."fmt" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 03 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."fmt"+0
go.string."fmt" t=8 dupok size=8 value=0
	0x0000 66 6d 74 00                                      fmt.
go.importpath.fmt. t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 03 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."fmt"+0
type..hash.[1]interface {}·f t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type..hash.[1]interface {}+0
type..eq.[1]interface {}·f t=8 dupok size=8 value=0
	0x0000 00 00 00 00 00 00 00 00                          ........
	rel 0+8 t=1 type..eq.[1]interface {}+0
