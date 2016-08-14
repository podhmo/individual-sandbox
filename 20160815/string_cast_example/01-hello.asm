"".main t=1 size=352 value=0 args=0x0 locals=0xa8
	0x0000 00000 (01-hello.go:7)	TEXT	"".main(SB), $168-0
	0x0000 00000 (01-hello.go:7)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:7)	LEAQ	-40(SP), AX
	0x000e 00014 (01-hello.go:7)	CMPQ	AX, 16(CX)
	0x0012 00018 (01-hello.go:7)	JLS	342
	0x0018 00024 (01-hello.go:7)	SUBQ	$168, SP
	0x001f 00031 (01-hello.go:7)	FUNCDATA	$0, gclocals·7d2d5fca80364273fb07d5820a76fef4(SB)
	0x001f 00031 (01-hello.go:7)	FUNCDATA	$1, gclocals·60db3765efb333efb90976c2f8035659(SB)
	0x001f 00031 (01-hello.go:8)	LEAQ	go.string."hello world"(SB), CX
	0x0026 00038 (01-hello.go:8)	MOVQ	$11, AX
	0x002d 00045 (01-hello.go:9)	MOVQ	$0, (SP)
	0x0035 00053 (01-hello.go:9)	MOVQ	CX, "".s+48(SP)
	0x003a 00058 (01-hello.go:9)	MOVQ	CX, 8(SP)
	0x003f 00063 (01-hello.go:9)	MOVQ	AX, "".s+56(SP)
	0x0044 00068 (01-hello.go:9)	MOVQ	AX, 16(SP)
	0x0049 00073 (01-hello.go:9)	PCDATA	$0, $0
	0x0049 00073 (01-hello.go:9)	CALL	runtime.stringtoslicebyte(SB)
	0x004e 00078 (01-hello.go:9)	MOVQ	24(SP), DX
	0x0053 00083 (01-hello.go:9)	MOVQ	32(SP), CX
	0x0058 00088 (01-hello.go:9)	MOVQ	40(SP), AX
	0x005d 00093 (01-hello.go:10)	MOVQ	DX, "".casted+96(SP)
	0x0062 00098 (01-hello.go:10)	MOVQ	DX, "".autotmp_0000+144(SP)
	0x006a 00106 (01-hello.go:10)	MOVQ	CX, "".casted+104(SP)
	0x006f 00111 (01-hello.go:10)	MOVQ	CX, "".autotmp_0000+152(SP)
	0x0077 00119 (01-hello.go:10)	MOVQ	AX, "".casted+112(SP)
	0x007c 00124 (01-hello.go:10)	MOVQ	AX, "".autotmp_0000+160(SP)
	0x0084 00132 (01-hello.go:10)	MOVQ	$0, BX
	0x0086 00134 (01-hello.go:10)	MOVQ	BX, "".autotmp_0004+80(SP)
	0x008b 00139 (01-hello.go:10)	MOVQ	BX, "".autotmp_0004+88(SP)
	0x0090 00144 (01-hello.go:10)	LEAQ	"".autotmp_0004+80(SP), BX
	0x0095 00149 (01-hello.go:10)	CMPQ	BX, $0
	0x0099 00153 (01-hello.go:10)	JEQ	$1, 335
	0x009f 00159 (01-hello.go:10)	MOVQ	$1, "".autotmp_0001+128(SP)
	0x00ab 00171 (01-hello.go:10)	MOVQ	$1, "".autotmp_0001+136(SP)
	0x00b7 00183 (01-hello.go:10)	MOVQ	BX, "".autotmp_0001+120(SP)
	0x00bc 00188 (01-hello.go:10)	LEAQ	type.[]uint8(SB), BX
	0x00c3 00195 (01-hello.go:10)	MOVQ	BX, (SP)
	0x00c7 00199 (01-hello.go:10)	LEAQ	"".autotmp_0000+144(SP), BX
	0x00cf 00207 (01-hello.go:10)	MOVQ	BX, 8(SP)
	0x00d4 00212 (01-hello.go:10)	MOVQ	$0, 16(SP)
	0x00dd 00221 (01-hello.go:10)	PCDATA	$0, $1
	0x00dd 00221 (01-hello.go:10)	CALL	runtime.convT2E(SB)
	0x00e2 00226 (01-hello.go:10)	MOVQ	24(SP), CX
	0x00e7 00231 (01-hello.go:10)	MOVQ	32(SP), AX
	0x00ec 00236 (01-hello.go:10)	MOVQ	"".autotmp_0001+120(SP), BX
	0x00f1 00241 (01-hello.go:10)	MOVQ	CX, "".autotmp_0005+64(SP)
	0x00f6 00246 (01-hello.go:10)	MOVQ	CX, (BX)
	0x00f9 00249 (01-hello.go:10)	MOVQ	AX, "".autotmp_0005+72(SP)
	0x00fe 00254 (01-hello.go:10)	CMPB	runtime.writeBarrier(SB), $0
	0x0105 00261 (01-hello.go:10)	JNE	$0, 315
	0x0107 00263 (01-hello.go:10)	MOVQ	AX, 8(BX)
	0x010b 00267 (01-hello.go:10)	MOVQ	"".autotmp_0001+120(SP), BX
	0x0110 00272 (01-hello.go:10)	MOVQ	BX, (SP)
	0x0114 00276 (01-hello.go:10)	MOVQ	"".autotmp_0001+128(SP), BX
	0x011c 00284 (01-hello.go:10)	MOVQ	BX, 8(SP)
	0x0121 00289 (01-hello.go:10)	MOVQ	"".autotmp_0001+136(SP), BX
	0x0129 00297 (01-hello.go:10)	MOVQ	BX, 16(SP)
	0x012e 00302 (01-hello.go:10)	PCDATA	$0, $2
	0x012e 00302 (01-hello.go:10)	CALL	fmt.Println(SB)
	0x0133 00307 (01-hello.go:11)	ADDQ	$168, SP
	0x013a 00314 (01-hello.go:11)	RET
	0x013b 00315 (01-hello.go:10)	LEAQ	8(BX), R8
	0x013f 00319 (01-hello.go:10)	MOVQ	R8, (SP)
	0x0143 00323 (01-hello.go:10)	MOVQ	AX, 8(SP)
	0x0148 00328 (01-hello.go:10)	PCDATA	$0, $1
	0x0148 00328 (01-hello.go:10)	CALL	runtime.writebarrierptr(SB)
	0x014d 00333 (01-hello.go:10)	JMP	267
	0x014f 00335 (01-hello.go:10)	MOVL	AX, (BX)
	0x0151 00337 (01-hello.go:10)	JMP	159
	0x0156 00342 (01-hello.go:10)	NOP
	0x0156 00342 (01-hello.go:7)	CALL	runtime.morestack_noctxt(SB)
	0x015b 00347 (01-hello.go:7)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 8d 44 24 d8 48 3b  eH..%....H.D$.H;
	0x0010 41 10 0f 86 3e 01 00 00 48 81 ec a8 00 00 00 48  A...>...H......H
	0x0020 8d 0d 00 00 00 00 48 c7 c0 0b 00 00 00 48 c7 04  ......H......H..
	0x0030 24 00 00 00 00 48 89 4c 24 30 48 89 4c 24 08 48  $....H.L$0H.L$.H
	0x0040 89 44 24 38 48 89 44 24 10 e8 00 00 00 00 48 8b  .D$8H.D$......H.
	0x0050 54 24 18 48 8b 4c 24 20 48 8b 44 24 28 48 89 54  T$.H.L$ H.D$(H.T
	0x0060 24 60 48 89 94 24 90 00 00 00 48 89 4c 24 68 48  $`H..$....H.L$hH
	0x0070 89 8c 24 98 00 00 00 48 89 44 24 70 48 89 84 24  ..$....H.D$pH..$
	0x0080 a0 00 00 00 31 db 48 89 5c 24 50 48 89 5c 24 58  ....1.H.\$PH.\$X
	0x0090 48 8d 5c 24 50 48 83 fb 00 0f 84 b0 00 00 00 48  H.\$PH.........H
	0x00a0 c7 84 24 80 00 00 00 01 00 00 00 48 c7 84 24 88  ..$........H..$.
	0x00b0 00 00 00 01 00 00 00 48 89 5c 24 78 48 8d 1d 00  .......H.\$xH...
	0x00c0 00 00 00 48 89 1c 24 48 8d 9c 24 90 00 00 00 48  ...H..$H..$....H
	0x00d0 89 5c 24 08 48 c7 44 24 10 00 00 00 00 e8 00 00  .\$.H.D$........
	0x00e0 00 00 48 8b 4c 24 18 48 8b 44 24 20 48 8b 5c 24  ..H.L$.H.D$ H.\$
	0x00f0 78 48 89 4c 24 40 48 89 0b 48 89 44 24 48 80 3d  xH.L$@H..H.D$H.=
	0x0100 00 00 00 00 00 75 34 48 89 43 08 48 8b 5c 24 78  .....u4H.C.H.\$x
	0x0110 48 89 1c 24 48 8b 9c 24 80 00 00 00 48 89 5c 24  H..$H..$....H.\$
	0x0120 08 48 8b 9c 24 88 00 00 00 48 89 5c 24 10 e8 00  .H..$....H.\$...
	0x0130 00 00 00 48 81 c4 a8 00 00 00 c3 4c 8d 43 08 4c  ...H.......L.C.L
	0x0140 89 04 24 48 89 44 24 08 e8 00 00 00 00 eb bc 89  ..$H.D$.........
	0x0150 03 e9 49 ff ff ff e8 00 00 00 00 e9 a0 fe ff ff  ..I.............
	rel 5+4 t=14 +0
	rel 34+4 t=13 go.string."hello world"+0
	rel 74+4 t=6 runtime.stringtoslicebyte+0
	rel 191+4 t=13 type.[]uint8+0
	rel 222+4 t=6 runtime.convT2E+0
	rel 256+4 t=13 runtime.writeBarrier+-1
	rel 303+4 t=6 fmt.Println+0
	rel 329+4 t=6 runtime.writebarrierptr+0
	rel 343+4 t=6 runtime.morestack_noctxt+0
"".init t=1 size=80 value=0 args=0x0 locals=0x0
	0x0000 00000 (01-hello.go:11)	TEXT	"".init(SB), $0-0
	0x0000 00000 (01-hello.go:11)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:11)	CMPQ	SP, 16(CX)
	0x000d 00013 (01-hello.go:11)	JLS	67
	0x000f 00015 (01-hello.go:11)	NOP
	0x000f 00015 (01-hello.go:11)	NOP
	0x000f 00015 (01-hello.go:11)	FUNCDATA	$0, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
	0x000f 00015 (01-hello.go:11)	FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
	0x000f 00015 (01-hello.go:11)	MOVBQZX	"".initdone·(SB), BX
	0x0016 00022 (01-hello.go:11)	CMPB	BL, $0
	0x0019 00025 (01-hello.go:11)	JEQ	47
	0x001b 00027 (01-hello.go:11)	MOVBQZX	"".initdone·(SB), BX
	0x0022 00034 (01-hello.go:11)	CMPB	BL, $2
	0x0025 00037 (01-hello.go:11)	JNE	40
	0x0027 00039 (01-hello.go:11)	RET
	0x0028 00040 (01-hello.go:11)	PCDATA	$0, $0
	0x0028 00040 (01-hello.go:11)	CALL	runtime.throwinit(SB)
	0x002d 00045 (01-hello.go:11)	UNDEF
	0x002f 00047 (01-hello.go:11)	MOVB	$1, "".initdone·(SB)
	0x0036 00054 (01-hello.go:11)	PCDATA	$0, $0
	0x0036 00054 (01-hello.go:11)	CALL	fmt.init(SB)
	0x003b 00059 (01-hello.go:11)	MOVB	$2, "".initdone·(SB)
	0x0042 00066 (01-hello.go:11)	RET
	0x0043 00067 (01-hello.go:11)	NOP
	0x0043 00067 (01-hello.go:11)	CALL	runtime.morestack_noctxt(SB)
	0x0048 00072 (01-hello.go:11)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 3b 61 10 76 34 0f  eH..%....H;a.v4.
	0x0010 b6 1d 00 00 00 00 80 fb 00 74 14 0f b6 1d 00 00  .........t......
	0x0020 00 00 80 fb 02 75 01 c3 e8 00 00 00 00 0f 0b c6  .....u..........
	0x0030 05 00 00 00 00 01 e8 00 00 00 00 c6 05 00 00 00  ................
	0x0040 00 02 c3 e8 00 00 00 00 eb b6 cc cc cc cc cc cc  ................
	rel 5+4 t=14 +0
	rel 18+4 t=13 "".initdone·+0
	rel 30+4 t=13 "".initdone·+0
	rel 41+4 t=6 runtime.throwinit+0
	rel 49+4 t=13 "".initdone·+-1
	rel 55+4 t=6 fmt.init+0
	rel 61+4 t=13 "".initdone·+-1
	rel 68+4 t=6 runtime.morestack_noctxt+0
type..hash.[1]interface {} t=1 dupok size=144 value=0 args=0x18 locals=0x28
	0x0000 00000 (01-hello.go:1)	TEXT	type..hash.[1]interface {}(SB), $40-24
	0x0000 00000 (01-hello.go:1)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:1)	CMPQ	SP, 16(CX)
	0x000d 00013 (01-hello.go:1)	JLS	127
	0x000f 00015 (01-hello.go:1)	SUBQ	$40, SP
	0x0013 00019 (01-hello.go:1)	MOVQ	"".h+56(FP), CX
	0x0018 00024 (01-hello.go:1)	FUNCDATA	$0, gclocals·0b86ef39f3fed835f14ba5f4d7c62fa2(SB)
	0x0018 00024 (01-hello.go:1)	FUNCDATA	$1, gclocals·33cdeccccebe80329f1fdbee7f5874cb(SB)
	0x0018 00024 (01-hello.go:1)	MOVQ	$0, AX
	0x001a 00026 (01-hello.go:1)	MOVQ	$1, "".autotmp_0007+24(SP)
	0x0023 00035 (01-hello.go:1)	MOVQ	"".autotmp_0007+24(SP), BP
	0x0028 00040 (01-hello.go:1)	CMPQ	AX, BP
	0x002b 00043 (01-hello.go:1)	JGE	$0, 113
	0x002d 00045 (01-hello.go:1)	MOVQ	AX, "".autotmp_0006+32(SP)
	0x0032 00050 (01-hello.go:1)	MOVQ	"".p+48(FP), BX
	0x0037 00055 (01-hello.go:1)	CMPQ	BX, $0
	0x003b 00059 (01-hello.go:1)	JEQ	$1, 123
	0x003d 00061 (01-hello.go:1)	MOVQ	AX, BP
	0x0040 00064 (01-hello.go:1)	SHLQ	$4, BP
	0x0044 00068 (01-hello.go:1)	ADDQ	BP, BX
	0x0047 00071 (01-hello.go:1)	MOVQ	BX, (SP)
	0x004b 00075 (01-hello.go:1)	MOVQ	CX, "".h+56(FP)
	0x0050 00080 (01-hello.go:1)	MOVQ	CX, 8(SP)
	0x0055 00085 (01-hello.go:1)	PCDATA	$0, $0
	0x0055 00085 (01-hello.go:1)	CALL	runtime.nilinterhash(SB)
	0x005a 00090 (01-hello.go:1)	MOVQ	16(SP), CX
	0x005f 00095 (01-hello.go:1)	MOVQ	"".autotmp_0006+32(SP), AX
	0x0064 00100 (01-hello.go:1)	INCQ	AX
	0x0067 00103 (01-hello.go:1)	MOVQ	"".autotmp_0007+24(SP), BP
	0x006c 00108 (01-hello.go:1)	CMPQ	AX, BP
	0x006f 00111 (01-hello.go:1)	JLT	$0, 45
	0x0071 00113 (01-hello.go:1)	MOVQ	CX, "".~r2+64(FP)
	0x0076 00118 (01-hello.go:1)	ADDQ	$40, SP
	0x007a 00122 (01-hello.go:1)	RET
	0x007b 00123 (01-hello.go:1)	MOVL	AX, (BX)
	0x007d 00125 (01-hello.go:1)	JMP	61
	0x007f 00127 (01-hello.go:1)	NOP
	0x007f 00127 (01-hello.go:1)	CALL	runtime.morestack_noctxt(SB)
	0x0084 00132 (01-hello.go:1)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 3b 61 10 76 70 48  eH..%....H;a.vpH
	0x0010 83 ec 28 48 8b 4c 24 38 31 c0 48 c7 44 24 18 01  ..(H.L$81.H.D$..
	0x0020 00 00 00 48 8b 6c 24 18 48 39 e8 7d 44 48 89 44  ...H.l$.H9.}DH.D
	0x0030 24 20 48 8b 5c 24 30 48 83 fb 00 74 3e 48 89 c5  $ H.\$0H...t>H..
	0x0040 48 c1 e5 04 48 01 eb 48 89 1c 24 48 89 4c 24 38  H...H..H..$H.L$8
	0x0050 48 89 4c 24 08 e8 00 00 00 00 48 8b 4c 24 10 48  H.L$......H.L$.H
	0x0060 8b 44 24 20 48 ff c0 48 8b 6c 24 18 48 39 e8 7c  .D$ H..H.l$.H9.|
	0x0070 bc 48 89 4c 24 40 48 83 c4 28 c3 89 03 eb be e8  .H.L$@H..(......
	0x0080 00 00 00 00 e9 77 ff ff ff cc cc cc cc cc cc cc  .....w..........
	rel 5+4 t=14 +0
	rel 86+4 t=6 runtime.nilinterhash+0
	rel 128+4 t=6 runtime.morestack_noctxt+0
type..eq.[1]interface {} t=1 dupok size=240 value=0 args=0x18 locals=0x58
	0x0000 00000 (01-hello.go:1)	TEXT	type..eq.[1]interface {}(SB), $88-24
	0x0000 00000 (01-hello.go:1)	MOVQ	(TLS), CX
	0x0009 00009 (01-hello.go:1)	CMPQ	SP, 16(CX)
	0x000d 00013 (01-hello.go:1)	JLS	225
	0x0013 00019 (01-hello.go:1)	SUBQ	$88, SP
	0x0017 00023 (01-hello.go:1)	FUNCDATA	$0, gclocals·3bb21ca8fe1d99a3e492463bd711418a(SB)
	0x0017 00023 (01-hello.go:1)	FUNCDATA	$1, gclocals·a8eabfc4a4514ed6b3b0c61e9680e440(SB)
	0x0017 00023 (01-hello.go:1)	MOVQ	$0, AX
	0x0019 00025 (01-hello.go:1)	MOVQ	$1, "".autotmp_0009+40(SP)
	0x0022 00034 (01-hello.go:1)	MOVQ	"".autotmp_0009+40(SP), BP
	0x0027 00039 (01-hello.go:1)	CMPQ	AX, BP
	0x002a 00042 (01-hello.go:1)	JGE	$0, 194
	0x0030 00048 (01-hello.go:1)	MOVQ	AX, "".autotmp_0008+48(SP)
	0x0035 00053 (01-hello.go:1)	MOVQ	"".q+104(FP), BX
	0x003a 00058 (01-hello.go:1)	CMPQ	BX, $0
	0x003e 00062 (01-hello.go:1)	JEQ	$1, 218
	0x0044 00068 (01-hello.go:1)	MOVQ	AX, BP
	0x0047 00071 (01-hello.go:1)	SHLQ	$4, BP
	0x004b 00075 (01-hello.go:1)	ADDQ	BP, BX
	0x004e 00078 (01-hello.go:1)	MOVQ	(BX), CX
	0x0051 00081 (01-hello.go:1)	MOVQ	8(BX), SI
	0x0055 00085 (01-hello.go:1)	MOVQ	"".p+96(FP), BX
	0x005a 00090 (01-hello.go:1)	CMPQ	BX, $0
	0x005e 00094 (01-hello.go:1)	JEQ	$1, 214
	0x0060 00096 (01-hello.go:1)	MOVQ	AX, BP
	0x0063 00099 (01-hello.go:1)	SHLQ	$4, BP
	0x0067 00103 (01-hello.go:1)	ADDQ	BP, BX
	0x006a 00106 (01-hello.go:1)	MOVQ	(BX), AX
	0x006d 00109 (01-hello.go:1)	MOVQ	8(BX), DX
	0x0071 00113 (01-hello.go:1)	CMPQ	AX, CX
	0x0074 00116 (01-hello.go:1)	JNE	204
	0x0076 00118 (01-hello.go:1)	MOVQ	AX, "".autotmp_0011+56(SP)
	0x007b 00123 (01-hello.go:1)	MOVQ	AX, (SP)
	0x007f 00127 (01-hello.go:1)	MOVQ	DX, "".autotmp_0011+64(SP)
	0x0084 00132 (01-hello.go:1)	MOVQ	DX, 8(SP)
	0x0089 00137 (01-hello.go:1)	MOVQ	CX, "".autotmp_0010+72(SP)
	0x008e 00142 (01-hello.go:1)	MOVQ	CX, 16(SP)
	0x0093 00147 (01-hello.go:1)	MOVQ	SI, "".autotmp_0010+80(SP)
	0x0098 00152 (01-hello.go:1)	MOVQ	SI, 24(SP)
	0x009d 00157 (01-hello.go:1)	PCDATA	$0, $0
	0x009d 00157 (01-hello.go:1)	CALL	runtime.efaceeq(SB)
	0x00a2 00162 (01-hello.go:1)	MOVBQZX	32(SP), BX
	0x00a7 00167 (01-hello.go:1)	CMPB	BL, $0
	0x00aa 00170 (01-hello.go:1)	JEQ	204
	0x00ac 00172 (01-hello.go:1)	MOVQ	"".autotmp_0008+48(SP), AX
	0x00b1 00177 (01-hello.go:1)	INCQ	AX
	0x00b4 00180 (01-hello.go:1)	MOVQ	"".autotmp_0009+40(SP), BP
	0x00b9 00185 (01-hello.go:1)	CMPQ	AX, BP
	0x00bc 00188 (01-hello.go:1)	JLT	$0, 48
	0x00c2 00194 (01-hello.go:1)	MOVB	$1, "".~r2+112(FP)
	0x00c7 00199 (01-hello.go:1)	ADDQ	$88, SP
	0x00cb 00203 (01-hello.go:1)	RET
	0x00cc 00204 (01-hello.go:1)	MOVB	$0, "".~r2+112(FP)
	0x00d1 00209 (01-hello.go:1)	ADDQ	$88, SP
	0x00d5 00213 (01-hello.go:1)	RET
	0x00d6 00214 (01-hello.go:1)	MOVL	AX, (BX)
	0x00d8 00216 (01-hello.go:1)	JMP	96
	0x00da 00218 (01-hello.go:1)	MOVL	AX, (BX)
	0x00dc 00220 (01-hello.go:1)	JMP	68
	0x00e1 00225 (01-hello.go:1)	NOP
	0x00e1 00225 (01-hello.go:1)	CALL	runtime.morestack_noctxt(SB)
	0x00e6 00230 (01-hello.go:1)	JMP	0
	0x0000 65 48 8b 0c 25 00 00 00 00 48 3b 61 10 0f 86 ce  eH..%....H;a....
	0x0010 00 00 00 48 83 ec 58 31 c0 48 c7 44 24 28 01 00  ...H..X1.H.D$(..
	0x0020 00 00 48 8b 6c 24 28 48 39 e8 0f 8d 92 00 00 00  ..H.l$(H9.......
	0x0030 48 89 44 24 30 48 8b 5c 24 68 48 83 fb 00 0f 84  H.D$0H.\$hH.....
	0x0040 96 00 00 00 48 89 c5 48 c1 e5 04 48 01 eb 48 8b  ....H..H...H..H.
	0x0050 0b 48 8b 73 08 48 8b 5c 24 60 48 83 fb 00 74 76  .H.s.H.\$`H...tv
	0x0060 48 89 c5 48 c1 e5 04 48 01 eb 48 8b 03 48 8b 53  H..H...H..H..H.S
	0x0070 08 48 39 c8 75 56 48 89 44 24 38 48 89 04 24 48  .H9.uVH.D$8H..$H
	0x0080 89 54 24 40 48 89 54 24 08 48 89 4c 24 48 48 89  .T$@H.T$.H.L$HH.
	0x0090 4c 24 10 48 89 74 24 50 48 89 74 24 18 e8 00 00  L$.H.t$PH.t$....
	0x00a0 00 00 0f b6 5c 24 20 80 fb 00 74 20 48 8b 44 24  ....\$ ...t H.D$
	0x00b0 30 48 ff c0 48 8b 6c 24 28 48 39 e8 0f 8c 6e ff  0H..H.l$(H9...n.
	0x00c0 ff ff c6 44 24 70 01 48 83 c4 58 c3 c6 44 24 70  ...D$p.H..X..D$p
	0x00d0 00 48 83 c4 58 c3 89 03 eb 86 89 03 e9 63 ff ff  .H..X........c..
	0x00e0 ff e8 00 00 00 00 e9 15 ff ff ff cc cc cc cc cc  ................
	rel 5+4 t=14 +0
	rel 158+4 t=6 runtime.efaceeq+0
	rel 226+4 t=6 runtime.morestack_noctxt+0
go.string.hdr."hello world" t=8 dupok size=16 value=0
	0x0000 00 00 00 00 00 00 00 00 0b 00 00 00 00 00 00 00  ................
	rel 0+8 t=1 go.string."hello world"+0
go.string."hello world" t=8 dupok size=16 value=0
	0x0000 68 65 6c 6c 6f 20 77 6f 72 6c 64 00              hello world.
gclocals·60db3765efb333efb90976c2f8035659 t=8 dupok size=20 value=0
	0x0000 03 00 00 00 0f 00 00 00 00 00 00 00 30 12 00 00  ............0...
	0x0010 30 10 00 00                                      0...
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
