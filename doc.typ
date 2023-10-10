#import "@preview/tablex:0.0.5": tablex, colspanx, hlinex, vlinex
#import "template.typ": *

#set document(title: "ARMv7 Quick Reference Guide", author: ("Howard Miller", "Matthew Fellenz"))

#show: ref-card

#heading-group[
= ARMv7 Quick Reference Guide
== West Valley College
== Saratoga, California, 2023
]


#info-table("General Registers AAPCS", 3,
	"R0\u{2013}R3", "", "Arguments/Parameters and return values (OK to Use)",
	"R4\u{2013}R10", "", "General purpose registers (must be Saved/Restored)",
	"R11", "FP", "Frame Pointer",
	"R12", "IP", "Intra-Procedure-call scratch register",
	"R13", "SP", "Stack Pointer",
	"R14", "LR", "Return address",
	"R15", "PC", "Program counter",
)

#info-table("Assembly Directives for Program Labels", 3,
	"loop:", "Label", "All labels end with '.'",
	"MyLabel:", "Label", "Labels are case sensitive",
)

#info-table("Assembly Directives for Program Comments", 3,
	"//", "Line Comment", "Ignore all characters to end of line",
	"@", "Line Comment", "Ignore all characters to end of line",
	"/* ... */", "Block Comment", "Ignore all characters between /* and */",
)

#info-table("Assembly Directives for Control of Listing File", 3,
	".title", "\"My Program\"", "Title for each page",
	".sbttl", "\"Some Function\"", "Subtitle for each page",
	".psize", "50,100", "Create landscape orientation listing file",
	".eject", "", "Skip to next page",
)

#info-table("Assembly Storage Directives", 3,
	".data", "", "Enter the data section",
	".text", "", "Enter the text section",
	".balign", "4, 0x20", "Write 0x20 until a 4-byte boundary",
	".byte", "0x12", "Allocate a byte with hex 12",
	".hword", "0x3456", "Allocate 2 bytes with hex 5635", // Intentional error.
	".word", "0x6789abcd", "Allocate 4 bytes with hex cdab8967",
	".quad", "0x2345", "Allocate 8 bytes with hex 452300...00",
	".octa", "0x6789", "Allocate 16 bytes with hex 896700...00",
	".float", "3.1415927", "Allocate 4 bytes with 7 digits of pi",
	".single", "3.1415927", "Same as .float, but more explicit",
	".double", "3.141592654...", "Allocate 8 bytes with 15 digits of pi",
	".ascii", "\"ABC\"", "ASCII-encode string as 0x414243",
	".asciz", "\"ABC\"", ".ascii with zero byte added at end",
	".string", "\"ABC\"", "Same as .asciz",
	".org", ". + 0x100", "Fill 0x100 bytes with 0x00",
	".skip", "64", "Skip 64 bytes and fills each with 0x00",
	".fill", "16, 2 0x20", "Fill 16 bytes with 0x0020 repeated",
)

#info-table("C Functions", 3,
	"putchar", "Input", [R0_7:0 is the ASCII character to display],
	"puts", "Input", "R0 is the address of the ASCIZ string to display",
	"printf", "Input", "R0 is the address of the ASCIZ format string\nR1\u{2013}3 hold the optional 1st-3rd values in the format string\n%[parameter] [flags] [width] [.precision] [length] type",
	"scanf", "Input", "R0 is the address of the ASCIZ format string\nR1\u{2013}3 hold the addresses of the 1st\u{2013}3rd values in the format string",
	"Format", "String", "%c Char, %d %i Int, %f Double, %x %X Hex, %s String",
)

#let Rd = $upright("Rd")$
#let Rm = $upright("Rm")$
#let Rn = $upright("Rn")$
#let Rs = $upright("Rs")$
#let Rt = $upright("Rt")$
#let PC = $upright("PC")$
#let LR = $upright("LR")$
#let carry = $C_"(carry)"$
#let Mem = $"Mem"$
#let op2 = $upright("op2")$
#let shift = $upright("shift")$
#let asr = $macron(>>)$
// Logic and
#let land = $class("binary", "&")$
#let signed = $plus.minus$
#let unsigned = $diameter$
#let ifm(cond) = $"if"(cond) med$

#instruction-table("Bitwise and Move Instructions", extra-column: true,
	"AND{S}", $Rd, Rn, op2$, $Rd = Rn land op2$, "NZC",
	"ASR{S}", $Rd, Rn, \#shift_5$, $Rd = Rn asr shift$, "NZC",
	"ASR{S}", $Rd, Rn, Rs$, $Rd = Rn asr Rs$, "NZC",
	"BFC", $Rd, \#p, \#n$, $Rd_(p + n - 1:p) = 0_n$, "",
	"BFI", $Rd, Rn, \#p, \#n$, $Rd_(p + n - 1:p) = Rn_(n-1:0)$, "",
	"BIC{S}", $Rd, Rn, op2$, $Rd = Rn land space.med ~op2$, "NZC",
	"CLZ", $Rd, Rn$, $Rd = upright("CountLeadingZeros")(Rn)$, "",
	"EOR{S}", $Rd, Rn, op2$, $Rd = Rn xor op2$, "NZC",
	"LSL{S}", $Rd, Rn, \#shift_5$, $Rd = Rn << shift$, "NZC",
	"LSL{S}", $Rd, Rn, Rs$, $Rd = Rn << Rs$, "NZC",
	"LSR{S}", $Rd, Rn, \#shift_5$, $Rd = Rn >> shift$, "NZC",
	"LSR{S}", $Rd, Rn, Rs$, $Rd = Rn >> Rs$, "NZC",
	"MOV{S}", $Rd, op2$, $Rd = op2$, "NZC",
	"MOVT", $Rd, \#i_16$, $Rd_(31:16) = i$, "",
	"MOVW", $Rd, \#i_16$, $Rd_(31:16) = 0; Rd_(15:0) = i$, "",
	"MVN{S}", $Rd, op2$, $Rd = ~op2$, "NZC",
	"ORR{S}", $Rd, Rn, op2$, $Rd = Rn | op2$, "NZC",
	"RBIT", $Rd, Rn$, $Rd = upright("ReverseBits")(Rn)$, "",
	"REV", $Rd, Rn$, $Rd = Rn_"B0":Rn_"B1":Rn_"B2":Rn_"B3"$, "",
	"REV16", $Rd, Rn$, $Rd = Rn_"B2":Rn_"B3":Rn_"B0":Rn_"B1"$, "",
	"REVSH", $Rd, Rn$, $Rd = Rn^signed_"B0":Rn_"B1"$, "",
	"ROR{S}", $Rd, Rn, \#shift_5$, $Rd = Rn >>> shift$, "NZC",
	"ROR{S}", $Rd, Rn, Rs$, $Rd = Rn >>> Rs$, "NZC",
	"RRX{S}", $Rd, Rn$, $Rd = C:Rn_(31:1); carry = Rn_0$, "NZC",
	"SBFX", $Rd, Rn, \#p, \#n$, $Rd = Rn^signed_(p+n-1:p)$, "",
	"TEQ", $Rd, op2$, $Rd xor op2$, "NZ",
	"TST", $Rd, op2$, $Rd land op2$, "NZ",
	"UBFX", $Rd, Rn, \#p, \#n$, $Rd = Rn^unsigned_(p+n-1:p)$, "",
)

#info-table("Operand 2", 2,
	$\#i_32$, [$i_8 >>> i_4:0$ (aka ROR 4 bits \*2)],
	$Rm$, [$Rm$ (same as $Rm, "lsl" \#0$)],
	$Rm, "lsl" \#n$, $Rm << {1..31}$,
	$Rm, "lsr" \#n$, $Rm >> {1..32}$,
	$Rm, "asr" \#n$, $Rm asr {1..32}$,
	$Rm, "ror" \#n$, $Rm >>> {1..31}$,
	$Rm, "rrx"$, [$C:Rm_(31:1); carry = Rm_0$],
	$Rm, "lsl" Rs$, $Rm << Rs$,
	$Rm, "lsr" Rs$, $Rm >> Rs$,
	$Rm, "asr" Rs$, $Rm asr Rs$,
	$Rm, "ror" Rs$, $Rm >>> Rs$,
)

// TODO Clean this up to be more generic and show what parameters are allowed.
// TODO this table is in here twice??
#instruction-table("Load and Store Addressing Modes",
	"MOV{S}", $Rd, "#0b000111"$, $Rd = 000111_2$,
	"LDR", $Rt, "=some_label"$, [$Rt = upright("AddressOf")("some_label")$ \ #align(right, text(size: text-size - 1pt, "(Load 32-bit address from literal pool)"))],
	"LDR", $Rt, [Rn]$, $Rt = Mem[Rn]$,
	"LDR", $Rt, [Rn, \#4]$, $Rt = Mem[Rn + 4]$,
	"LDR", $Rt, [Rn], \#+4$, $"Post:" Rt = Mem[Rn_"Orig"]; Rn = Rn + 4$,
	"LDR", $Rt, [Rn, Rm]$, $Rt = Mem[Rn + Rm]$,
	"LDR", $Rt, [Rn, Rm, "lsl" #3]$, $Rt = Mem[Rn + Rm << 3]$,
	"STR", $Rt, [Rn]$, $Mem[Rn] = Rt$,
	"STR", $Rt, [Rn, \#4]$, $Mem[Rn + 4] = Rt$,
	"STR", $Rt, [Rn, \#4]!$, $"Pre:" Rn = Rn + 4, Mem[Rn_"New"] = Rt$,
	"STR", $Rt, [Rn, Rm]$, $Mem[Rn + Rm] = Rt$,
	"STR", $Rt, [Rn, Rm, "lsr" \#2]$, $Mem[Rn + Rm >> 2] = Rt$,
)

#instruction-table("Arithmetic Instructions", extra-column: true,
	"ADC{S}", $Rd, Rn, op2$, $Rd = Rn + op2 + carry$, "",
	"ADD{S}", $Rd, Rn, op2$, $Rd = Rn + op2$, "",
	"ADR", $Rd, signed "rel"_12$, $Rd = PC + "rel"^signed$, "",
	"CMN", $Rd, op2$, $"NZCV" <- Rd + op2$, "",
	"CMP", $Rd, op2$, $"NZCV" <- Rd - op2$, "",
	"MUL{S}", $Rd, Rn, Rm$, $Rd = Rn times Rm$, "",
	"QADD", $Rd, Rm, Rn$, $Rd = "Sat"^signed (Rm + Rn)$, "Q",
	"QDADD", $Rd, Rm, Rn$, $Rd = "Sat"^signed (Rm + "Sat"^signed(2 times Rn))$, "Q",
	"QSUB", $Rd, Rm, Rn$, $Rd = "Sat"^signed (Rm - Rn)$, "Q",
	"QDSUB", $Rd, Rm, Rn$, $Rd = "Sat"^signed (Rm - "Sat"^signed(2 times Rn))$, "Q",
	"RSB{S}", $Rd, Rn, op2$, $Rd = op2 - Rn$, "",
	"RSC{S}", $Rd, Rn, op2$, $Rd = op2 - (Rn + carry)$, "",
	"SBC{S}", $Rd, Rn, op2$, $Rd = Rn - (op2 + carry)$, "",
	"SDIV", $Rd, Rn, Rm$, $Rd = Rn macron(div) Rm$, "",
	"SSAT", $Rd, \#"st"_5, Rn{"slr"}$, $Rd = "Sat"^signed (Rn^signed, "st")$, "Q",
	"SSAT16", $Rd, \#"st"_4, Rn$, $Rd = "Sat"^signed (Rn^signed_"H1", "st"):"Sat"^signed (Rn^signed_"H0", "st")$, "Q",
	"SUB{S}", $Rd, Rn, op2$, $Rd = Rn - op2$, "",
	"UDIV", $Rd, Rn, Rm$, $Rd = Rn div Rm$, "",
	"USAT", $Rd, \#"st"_5, Rn{"slr"}$, $Rd = "Sat"^unsigned (Rn^signed, "st")$, "Q",
	"USAT16", $Rd, \#"st"_4, Rn$, $Rd = "Sat"^unsigned (Rn^signed_"H1", "st"):"Sat"^unsigned (Rn^signed_"H0", "st")$, "Q",
	colspanx(2, $"Sat"^x (v, "bits")$), [If $v$ is out of bounds, it is clamped to $"bits"$ bits. For $x$, $signed$ (signed) or $unsigned$ (unsigned) indicate which range it is clamped to. $v$ is always interpreted as signed.], "",
)

#let thumbstate = $upright(T)$

#instruction-table("Branch and Jump Instructions",
	"B", $"rel"_24$, $PC = PC + "rel"^signed_24 << 2$,
	"Bcc", $"rel"_24$, $ifm("cc") PC = PC + "rel"^signed_24 << 2$,
	"BL", $"rel"_24$, $LR = PC; PC = PC + "rel"^signed_24 << 2$,
	// The ARM website does not explicitly mention that the Thumb state is saved in `LR`, but this SE link says it does: <https://electronics.stackexchange.com/questions/293238/arm-mode-and-thumb-mode-makes-the-pcs-bit-0>
	"BLX", $"rel"_24$, [$LR = PC_(31:1):thumbstate_0; PC = PC + "rel"^signed_24 << 1$ (Can change mode)],
	"BLX", $Rm$, $LR = PC_(31:1):thumbstate_0; PC = Rm_(31:1):0; thumbstate = Rm_0$,
	"BX", $Rm$, [$PC = Rm_(31:1):0; upright(T) = Rm_0$],
)

#let fN = $upright(N)$
#let fC = $upright(C)$
#let fZ = $upright(Z)$
#let fV = $upright(V)$

#info-table("Condition Codes (cc): Negative, Zero, Carry, oVerflow", 3,
	"EQ", "Equal", $fZ$,
	"NE", "Not equal", $!fZ$,
	"CS/HS", "Carry set, Unsigned higher or same", $fC$,
	"CC/LO", "Carry clear, Unsigned lower", $!fC$,
	"MI", "Minus, Negative", $fN$,
	"PL", "Plus, Positive or zero", $!fN$,
	"VS", "Overflow", $fV$,
	"VC", "No overflow", $!fV$,
	"HI", "Unsigned higher", $fC land !fZ$,
	"LS", "Unsigned lower or same", $!fC | fZ$,
	"GE", "Signed greater than or equal", $fN = fV$,
	"LT", "Signed less than", $fN != fV$,
	"GT", "Signed greater than", $!fZ land fN = fV$,
	"LE", "Signed less than or equal", $fZ | fN != fV$,
	"AL", "Always (default)", $1$,
)

#info-table("Notes for Instruction Set", 2,
	"Q", "Sets the Q flag in APSR",
)

#ascii-table

#powers-hex-table

#gdb-table

#let rlist = $upright("rlist")$
#let cnt = $upright("cnt")$
#let addr = $upright("addr")$
#let SP = $upright("SP")$

#instruction-table("Load and Store Instructions",
	"LDMDA", $Rn{!}, rlist$, $rlist = [Rn - 4 times cnt + 4]; ifm(!) Rn "-=" 4 times cnt$,
	"LDMDB", $Rn{!}, rlist$, $rlist = [Rn - 4 times cnt]; ifm(!) Rn "-=" 4 times cnt$,
	"LDMIA", $Rn{!}, rlist$, $rlist = [Rn]; ifm(!) Rn "+=" 4 times cnt$,
	"LDMIB", $Rn{!}, rlist$, $rlist = [Rn + 4]; ifm(!) Rn "+=" 4 times cnt$,
	// Whenever there are instructions that vary by data size, they should be ordered like this:
	"LDR", $Rt, [addr]$, $Rt = [addr]$,
	"LDRB", $Rt, [addr]$, $Rt = [addr]^unsigned_8$,
	"LDRH", $Rt, [addr]$, $Rt = [addr]^unsigned_16$,
	"LDRD", $Rt_1, Rt_2, [addr]$, $Rt_2:Rt_1 = [addr]_64$,
	"LDRSB", $Rt, [addr]$, $Rt = [addr]^signed_8$,
	"LDRSH", $Rt, [addr]$, $Rt = [addr]^signed_16$,
	// Italic for pseudo-instruction.
	[_POP_], $rlist$, $rlist = [SP]; SP "+=" 4 times cnt$,
	[_PUSH_], $rlist$, $SP "-=" 4 times cnt; [SP] = rlist$,
	"STMDA", $Rn{!}, rlist$, $[Rn - 4 times cnt + 4] = rlist; ifm(!) Rn "-=" 4 times cnt$,
	"STMDB", $Rn{!}, rlist$, $[Rn - 4 times cnt] = rlist; ifm(!) Rn "-=" 4 times cnt$,
	"STMIA", $Rn{!}, rlist$, $[Rn] = rlist; ifm(!) Rn "+=" 4 times cnt$,
	"STMIB", $Rn{!}, rlist$, $[Rn + 4] = rlist; ifm(!) Rn "+=" 4 times cnt$,
	"STR", $Rt, [addr]$, $[addr] = Rt$,
	"STRB", $Rt, [addr]$, $[addr]_8 = Rt_(7:0)$,
	"STRH", $Rt, [addr]$, $[addr]_16 = Rt_(15:0)$,
	"STRD", $Rt_1, Rt_2, [addr]$, $[addr]_64 = Rt_2:Rt_1$,
)

#let AS = $upright("AS")$

// TODO reformat the first column. it's very confusing.
#instruction-table("ARM LDR/STR Addressing Modes",
	"non-T", $[Rn{, \#i_8}]{!}$, $addr = Rn + i^signed; ifm(!) Rn = addr$,
	"xxR{,B}", $[Rn{, \#i_12}]{!}$, $addr = Rn + i^signed; ifm(!) Rn = addr$,
	"any", $[Rn]{, \#i_8}$, $addr = Rn; Rn "+=" i^signed$,
	"xxR{,B}{T}", $[Rn], \#signed i_12$, $addr = Rn; Rn "+=" i^signed$,
	"non-T", $[Rn, Rm]{!}$, $addr = Rn + Rm; ifm(!) Rn = addr$,
	"xxR{,B}", $[Rn, Rm{AS}]{!}$, $addr = Rn + AS(Rm); ifm(!) Rn = addr$,
	"any", $[Rn], Rm$, $addr = Rn; Rn "+=" Rm$,
	"xxR{,B}{T}", $[Rn], Rm{AS}$, $addr = Rn; Rn "+=" AS(Rm)$,
	// TODO what are these even?
	"LD non-T", $signed "rel"_8$, $addr = PC + "rel"^signed$,
	"LDR{,B}", $signed "rel"_12$, $addr = PC + "rel"^signed$,
)

#let APSR = $"APSR"$

#instruction-table("Special Instructions",
	"BKPT", $\#i_16$, $"BreakPoint"(i)$,
	"DBG", $\#i_4$, $"DebugHint"(i)$,
	"DMB", [_option_], $"DataMemoryBarrier"("option")$,
	"DSB", [_option_], $"DataSynchronizationBarrier"("option")$,
	"ISB", "SY", $"InstructionSynchronizationBarrier"("SY")$,
	"NOP", "", "Do nothing (still an instruction)",
	"PLD{W}", $[addr]$, $"PreloadData"(addr)$,
	"PLI", $[addr]$, $"PreloadInstruction"(addr)$,
	"SETEND", ${"BE/LE"}$, $"EndianState" = {"BE/LE"}$,
	"SEV", "", $"SendEvent"()$,
	"SVC", $\#i_24$, $"CallSupervisor"(i)$,
	"UDF", $\#i_16$, $"UndefinedException"(i)$,
	"WFE", "", $"WaitForEvent"()$,
	"WFI", "", $"WaitForInterrupt"()$,
	"YIELD", "", $"HintYield"()$,
	"MSR", $APSR, op2$, $APSR = op2$,
	"MRS", $Rd, APSR$, $Rd = APSR$,
)

#let Ra = $upright("Ra")$
#let mtimes = $macron(times)$

#instruction-table("Multiplication Instructions",
	// Putting the addition first for consistency with MLS.
	"MLA{S}", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn times Rm$,
	"MLS", $Rd, Rn, Rm, Ra$, $Rd = Ra - Rn times Rm$,
	"MUL{S}", $Rd, Rn, Rm$, $Rd = Rn times Rm$,
	"SMLAxy", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn^signed_"Hx" mtimes Rm^signed_"Hy"$,
	"SMLaD", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn^signed_"H0" mtimes Rm^signed_"H0" signed Rn^signed_"H1" mtimes Rm^signed_"H1"$,
	"SMLaDX", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$,
	"SMLaLD", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$,
	"SMLaLDX", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn^signed_"H0" mtimes Rm^signed_"H1" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H0"$,
	"SMLAL{S}", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn mtimes Rm$,
	"SMLALxy", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn^signed_"Hx" mtimes Rm^signed_"Hy"$,
	"SMLAWy", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn mtimes Rm^signed_"Hy"$,
	"SMMLa", $Rd, Rn, Rm, Ra$, $Rd = Ra plus.minus (Rn mtimes Rm)_(63:32)$,
	"SMMLaR", $Rd, Rn, Rm, Ra$, $Rd = Ra plus.minus (Rm mtimes Rm + "0x8000" #thousand 0000)_(63:32)$,
	"SMMUL", $Rd, Rn, Rm$, $Rd = (Rn mtimes Rm)_(63:32)$,
	"SMMULR", $Rd, Rn, Rm$, $Rd = (Rn mtimes Rm + "0x8000" #thousand 0000)_(63:32)$,
	"SMUaD", $Rd, Rn, Rm$, $Rd = Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$,
	"SMUaDX", $Rd, Rn, Rm$, $Rd = Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$,
	"SMULxy", $Rd, Rn, Rm$, $Rd = Rn^signed_"Hx" mtimes Rm^signed_"Hy"$,
	"SMULL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rn mtimes Rm$,
	"SMULL{S}", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rn mtimes Rm$,
	"SMULWy", $Rd, Rn, Rm$, $Rd = (Rn mtimes Rm^signed_"Hy")_(47:16)$,
	"UMAAL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rd_1 + Rd_2 + Rn times Rm$,
	"UMLAL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn times Rm$,
	"UMULL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rn times Rm$,
)

#{
let setmon = $"SetExclusiveMonitor"()$
let pass = $"Pass?"$
let rdpass = $Rd = pass$
instruction-table("Exclusive Load and Store Instructions",
	"CLREX", "", "Remove any exclusive monitors",
	"LDREX", $Rt, [Rn]$, $Rt = [Rn]; setmon$,
	// This is not supported in ARM.
	// "LDREX", $Rt, [Rn, \#i_10]$, $Rt = [Rn + i^signed]; setmon$,
	"LDREXB", $Rt, [Rn]$, $Rt = [Rn]_(7:0); setmon$,
	"LDREXH", $Rt, [Rn]$, $Rt = [Rn]_(15:0); setmon$,
	"LDREXD", $Rt_1, Rt_2, [Rn]$, $Rt_2:Rt_1 = [Rn]_(127:0); setmon$,
	"STREX", $Rd, Rn, [Rt]$, $ifm(pass) [Rt] = Rn; rdpass$,
	"STREX", $Rd, Rn, [Rt, \#i_10]$, $ifm(pass) [Rt] = Rn; rdpass$,
	"STREXB", $Rd, Rn, [Rt]$, $ifm(pass) [Rt]_(7:0) = Rn_(7:0); rdpass$,
	"STREXH", $Rd, Rn, [Rt]$, $ifm(pass) [Rt]_(15:0) = Rn_(15:0); rdpass$,
	"STREXD", $Rd, Rn_1, Rn_2, [Rt]$, $ifm(pass) [Rt]_(127:0) = Rn_2:Rn_1; rdpass$,
)
}

#colbreak(weak: true)

#v(1em) // A slight visual break.
#heading-group[
= ARMv7 Floating Point Instructions
== Quick Reference Guide
]

#let Sd = $"Sd"$
#let Dd = $"Dd"$
#let Sn = $"Sn"$
#let Dn = $"Dn"$
// Perform a value-preserving cast.
#let Cast = $"Cast"$
// Reinterpret the same raw data as a new type.
#let Reinterpret = $"Reinterpret"$
#let imm = $"imm"$
#let FPSCR = $"FPSCR"$
#let nzcv = $"nzcv"$

#instruction-table("Floating-Point Register Transfer Instructions",
	"VMOV.F32", $Sd, \#signed imm$, $Sd = signed imm$,
	"VMOV.F64", $Sd, \#signed imm$, $Sd = signed imm$, // Intentional error.
	"VMOV.F32", $Sd, Sn$, $Sd = Sn$,
	"VMOV.F64", $Dd, Dn$, $Dd = Dn$,
	"VMOV", $Rd, Sn$, $Rd = Reinterpret(Sn)$,
	"VMOV", $Sd, Rn$, $Sd = Reinterpret(Rn)$,
	"VMOV", $Dd, Rn_1, Rn_2$, $Dd = Reinterpret(Rn_2:Rn_1)$,
	"VMOV", $Rd_1, Rd_2, Dn$, $Rd_2:Rd_1 = Reinterpret(Dn)$,
	// TODO What even are these two?
	"VMOV{.32}", $"Ddx", Rn$, $Dd_"Wx" = Rn$,
	"VMOV{.32}", $Rd, "Dnx"$, $Rd = Dn_"Wx"$,
	"VMRS", $Rd, "system-reg"$, $Rd = "system-reg"$,
	"VMRS", $"APSR_nzcv", FPSCR$, $"APSR"_nzcv = FPSCR_nzcv$,
	"VMSR", $"system-reg", Rn$, $"system-reg" = Rn$,
)

#let fx = $"fx"$
#let fy = $"fy"$
#let fz = $"fz"$
#let fd = $"fd"$
#let fn = $"fn"$
#let dz = $"fz"$
#let Compare = $"Compare"$
#let pmeq = sym.plus.minus + "="

#instruction-table("Floating-Point Instructions",
	"VABS.f", $fx, fy$, $fx = |fy|$,
	"VADD.f", $fd, fx, fy$, $fd = fx + fy$,
	"VCMP{E}.f", $fx, \#0.0$, $FPSCR_nzcv = Compare(fx, 0.0)$,
	"VCMP{E}.f", $fx, fy$, $FPSCR_nzcv = Compare(fx, fy)$,
	"VCVT.F32.F64", $Sd, Dn$, $Sd = "Float2Float"(Dn)$,
	"VCVT.F64.F32", $Dd, Sn$, $Dd = "Float2Float"(Sn)$,
	"VCVT.sz.f", $fd, fx, \#i_5$, $fd = "Float2Fixed"(fx, i)$,
	"VCVT.f.sz", $fd, fx, \#i_5$, $fd = "Fixed2Float"(fx, i)$,
	"VCVT{R}.s32.f", $Sd, fn$, $Sd = "Float2Int"(fn)$,
	"VCVT.f.s32", $fd, Sn$, $fd = "Int2Float"(Sn)$,
	"VCVTx.F16.F32", $Sd, Sn$, $Sd_"Hx" = "Float2Float"(Sn)$,
	"VCVTx.F32.F16", $Sd, Sn$, $Sd = "Float2Float"(Sn_"Hx")$,
	"VDIV.f", $fx, fy, dz$, $fx = fy div dz$, // Intentional error.
	"VFMa.f", $fx, fy, fz$, $fx pmeq fy times fz$,
	"VFMNa.f", $fx, fy, fz$, $fx = -fx plus.minus fy times fz$,
	// TODO why is it floored? Seems wrong.
	"VMLa.f", $fx, fy, fz$, $fx pmeq floor(fy times fz)$,
	"VMUL.f", $fx, fy, fz$, $fx = fy times fz$,
	"VNEG.f", $fx, fy$, $fx = -fy$,
	"VNMLa.f", $fx, fy, fz$, $fx = -fx plus.minus -floor(fy times fz)$,
	"VNMUL.f", $fx, fy, fz$, $fx = -floor(fy times fz)$,
	"VSQRT.f", $fx, fy$, $fx = sqrt(fy)$,
	"VSUB.f", $fx, fy, fz$, $fx = fy - fz$,
)

#let mx = $"mx"$
#let my = $"my"$
#let rx = $"rx"$
#let ry = $"ry"$
#let dx = $"dx"$
#let dy = $"dy"$

#instruction-table("SIMD Register Transfer Instructions",
	"VDUP.z", $mx, ry$, $mx_"z*" = ry_"z"$,
	"VDUP.z", $mx, dy[i]$, $mx_"z*" = dy_"zi"$,
	"VMOV.z", $dx[i], ry$, $dx_"zi" = ry_"z"$,
	// What does "^s" mean?
	"VMOV.sz", $rx, dy[i]$, $rx = dy_"zi"^"s"$,
	"VMOV", $mx, my$, $mx = my$,
)

#info-table("Floating-Point System Registers", 2,
	"FPEXC", "Floating-Point EXception Control",
	"FPSCR", "Floating-Point Status and Control Register",
	"FPSID", "Floating-Point System ID",
	"MVFR{0,1,2}", "Media and VFP feature registers",
)

#{
let bit(..xs) = text(font: monospace, "0x" + chunks(4, left-pad(8, "0", str(xs.pos().map(x => calc.pow(2, x)).sum(), base: 16))).join(thousand))
info-table("Floating-Point Status and Control Register (FPSCR)", 3,
	"Name", "Bit", "Meaning",
	"IOC", bit(0), "Invalid Operation cumulative exception",
	"DZC", bit(1), "Division by Zero cumulative exception",
	"OFC", bit(2), "Overflow cumulative exception",
	"UFC", bit(3), "Underflow cumulative exception",
	"IXC", bit(4), "Inexact cumulative exception",
	"IDC", bit(7), "Input Denormal cumulative exception",
	"IOE", bit(8), "Invalid Operation exception trap enable",
	"DZE", bit(9), "Division by Zero exception trap enable",
	"OFE", bit(10), "Overflow exception trap enable",
	"UFE", bit(11), "Underflow exception trap enable",
	"IXE", bit(12), "Inexact exception trap enable",
	"IDE", bit(15), "Input Denormal exception trap enable",
	"RMode", bit(23, 22), "Rounding mode (nearest, plus infinity, minus infinity, zero)",
	"FZ", bit(24), "Enable Flush-to-zero mode",
	"DN", bit(25), "Propagate NaN operands to output",
	"AHP", bit(26), "Use alternative half-precision format",
	"QC", bit(27), "SIMD saturation occurred",
	"V", bit(28), "Overflow condition flag",
	"C", bit(29), "Carry condition flag",
	"Z", bit(30), "Zero condition flag",
	"N", bit(31), "Negative condition flag",
	align: (x, y) => if y == 0 { left } else { (left, right, left).at(x) } + horizon,
)
}

#info-table("Floating-Point and SIMD Keys", 2,
	"s", "Operation signedness (S or U)",
	"z", "Data size (8, 16, or 32)",
	"f", "Floating-point size (F32 or F64)",
	"sx, dx, qx", "Single-/double-/quadword register",
	"fx, fy, fz", "Floating-point register (Sx or Dx depending on data size)",
	"mx, my, mz", "SIMD Register (Dx or Qx)",
	// TODO floor means round??
	$floor(x)$, "Value is rounded",
	"{E}", "Any NaN causes an exception",
	"{R}", "Use the rounding mode in FPSCR instead of round-to-zero",
)

#info-table("Notes for Floating-Point and SIMD Instructions", 2,
	"V3, V4", "Introduced in VFP v3, v4",
	"SH, S2", "Introduced in SIMD with half-precision floats, SIMDv2",
	"F", "Instruction with data type F32 also exists",
)

#info-table("Keys", 2,
	"{S}", "Optional suffix; if present, update NZCV",
	"{slr}", "An optional LSL #N or ASR #N",
	"op2", "Immediate or shifted register",
	"a", [A or S to make $plus.minus$ add or subtract],
	"i, j", "Immediate operand, range 0..max or 1..max+1",
	"rx, ry, rz, rw", "General registers",
	"rlist", "Comma-separated registers within { }",
	"{rb}", "Optional rotate by multiple of 8 bits",
	"{sl}", "Optional left shift",
	"{sr}", "Optional right shift",
	"{AS}", [Arm shift or rotate (LSL, ROR, LSR, ASR, RRX). $AS(x)$ uses it.],
	$"value"^signed, "value"^unsigned$, "Value is sign/zero extended",
	$macron(times), macron(div), macron(>>)$, "Operation is signed",
	$C$, "Carry flag as an integer",
)

