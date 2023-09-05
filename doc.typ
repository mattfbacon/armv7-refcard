#import "@preview/tablex:0.0.5": tablex, colspanx, hlinex, vlinex
#import "template.typ": *

#set document(title: "ARMv7 Quick Reference Guide", author: ("Howard Miller", "Matthew Fellenz"))

#show: ref-card

// TODO:
// - Explain the meanings of mnemonic characters in the tables as comments in this document, for maintainability
// - Explain the meaning of the macron over operators
// - Figure out if it is possible to write Rd:Rn without the spacing around the colon
// - (Typst issue) math subscripts and superscripts are a bit cramped at the moment.

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
	".hword", "0x3456", "Allocate 2 bytes with hex 5635", // Intentional error
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
#let pmeq = sym.plus.minus + "="

#instruction-table("Bitwise and Move Instructions",
	"AND{S}", $Rd, Rn, op2$, $Rd = Rn land op2$, "",
	"ASR{S}", $Rd, Rn, \#shift_5$, $Rd = Rn asr shift$, "",
	"ASR{S}", $Rd, Rn, Rs$, $Rd = Rn asr Rs$, "",
	"BFC", $Rd, \#p, \#n$, $Rd_(p + n - 1:p) = 0_n$, "6t",
	"BFI", $Rd, Rn, \#p, \#n$, $Rd_(p + n - 1:p) = Rn_(n-1:0)$, "6t",
	"BIC{S}", $Rd, Rn, op2$, $Rd = Rn land space.med ~op2$, "",
	"CLZ", $Rd, Rn$, $Rd = upright("CountLeadingZeros")(Rn)$, "",
	"EOR{S}", $Rd, Rn, op2$, $Rd = Rn xor op2$, "",
	"LSL{S}", $Rd, Rn, \#shift_5$, $Rd = Rn << shift$, "",
	"LSL{S}", $Rd, Rn, Rs$, $Rd = Rn << Rs$, "",
	"LSR{S}", $Rd, Rn, \#shift_5$, $Rd = Rn >> shift$, "",
	"LSR{S}", $Rd, Rn, Rs$, $Rd = Rn >> Rs$, "",
	"MOV{S}", $Rd, op2$, $Rd = op2$, "",
	"MOVT", $Rd, \#i_16$, $Rd_(31:16) = i$, "6t",
	// TODO what does this superscript symbol mean?
	"MOVW", $Rd, \#i_16$, $Rd = i^diameter$, "6t",
	"MVN{S}", $Rd, op2$, $Rd = ~op2$, "",
	"ORR{S}", $Rd, Rn, op2$, $Rd = Rn | op2$, "",
	"RBIT", $Rd, Rn$, $Rd = upright("ReverseBits")(Rn)$, "6t",
	"REV", $Rd, Rn$, $Rd = Rn_"B0":Rn_"B1":Rn_"B2":Rn_"B3"$, "6",
	"REV16", $Rd, Rn$, $Rd = Rn_"B2":Rn_"B3":Rn_"B0":Rn_"B1"$, "6",
	"REVSH", $Rd, Rn$, $Rd = Rn^signed_"B0":Rn_"B1"$, "6",
	"ROR{S}", $Rd, Rn, \#shift_5$, $Rd = Rn >>> shift$, "",
	"ROR{S}", $Rd, Rn, Rs$, $Rd = Rn >>> Rs$, "",
	"RRX{S}", $Rd, Rn$, $Rd = C:Rn_(31:1); space carry = Rn_0$, "",
	"SBFX", $Rd, Rn, \#p, \#n$, $Rd = Rn^signed_(p+n-1:p)$, "6t",
	"TEQ", $Rd, op2$, $Rd xor op2$, "",
	"TST", $Rd, op2$, $Rd land op2$, "",
	"UBFX", $Rd, Rn, \#p, \#n$, $Rd = Rn^diameter_(p+n-1:p)$, "6t",
)

#info-table("Operand 2", 2,
	$\#i_32$, [$i_8 >>> i_4:0$ (aka ROR 4 bits \*2)],
	$Rm$, [$Rm$ (same as $Rm, "lsl" \#0$)],
	$Rm, "lsl" \#n$, $Rm << {1..31}$,
	$Rm, "lsr" \#n$, $Rm >> {1..32}$,
	$Rm, "asr" \#n$, $Rm asr {1..32}$,
	$Rm, "ror" \#n$, $Rm >>> {1..31}$,
	$Rm, "rrx"$, [$C:Rm_(31:1); space carry = Rm_0$],
	$Rm, "lsl" Rs$, $Rm << Rs$,
	$Rm, "lsr" Rs$, $Rm >> Rs$,
	$Rm, "asr" Rs$, $Rm asr Rs$,
	$Rm, "ror" Rs$, $Rm >>> Rs$,
)

#instruction-table("Branch and Jump Instructions",
	"B", $"rel"_26$, $PC = PC + "rel"^signed_"25:2":0_"1:0"$, "",
	"Bcc", $"rel"_21$, $"if"("cc") PC = PC + "rel"^signed_"20:1":0$, "I",
	"BKPT", $\#i_16$, $"BreakPoint"(i)$, "I",
	"BL", $"rel"_26$, $LR = PC_(31:1):0; PC "+=" "rel"^signed_(25:2):0_(1:0)$, "",
	// TODO What does "Set" mean?
	"BLX", $"rel"_26$, $LR = PC_(31:1):0; "Set" = 1; PC "+=" "rel"^signed_(25:1):0$, "",
	"BLX", $Rm$, $LR = PC_(31:1):0; "Set" = Rm_0; PC = Rm_(31:1):0$, "",
	"BX", $Rm$, $"Set" = Rm_0; PC = Rm_(31:1):0$, "",
)

#info-table("Keys", 2,
	"{S}", "Optional suffix; if present, update flags",
	"op2", "Immediate or shifted register",
)

#instruction-table("Load and Store Addressing Modes",
	"MOV{S}", $Rd, "#0b000111"$, $Rd = 000111_2$, "",
	"LDR", $Rt, "=some_label"$, [$Rt = upright("AddressOf")("some_label")$ \ #align(right, text(size: text-size - 1pt, "(Load 32-bit address from literal pool)"))], "P",
	"LDR", $Rt, [Rn]$, $Rt = Mem[Rn]$, "",
	"LDR", $Rt, [Rn, \#4]$, $Rt = Mem[Rn + 4]$, "",
	"LDR", $Rt, [Rn], \#+4$, $"Post:" Rt = Mem[Rn_"Orig"]; space.hair Rn = Rn + 4$, "",
	"LDR", $Rt, [Rn, Rm]$, $Rt = Mem[Rn + Rm]$, "",
	"LDR", $Rt, [Rn, Rm, "lsl" #3]$, $Rt = Mem[Rn + Rm << 3]$, "",
	"STR", $Rt, [Rn]$, $Mem[Rn] = Rt$, "",
	"STR", $Rt, [Rn, \#4]$, $Mem[Rn + 4] = Rt$, "",
	"STR", $Rt, [Rn, \#4]!$, $"Pre:" Rn = Rn + 4, Mem[Rn_"New"] = Rt$, "",
	"STR", $Rt, [Rn, Rm]$, $Mem[Rn + Rm] = Rt$, "",
	"STR", $Rt, [Rn, Rm, "lsr" \#2]$, $Mem[Rn + Rm >> 2] = Rt$, "",
)

#instruction-table("Arithmetic Instructions",
	"ADC{S}", $Rd, Rn, op2$, $Rd = Rn + op2 + carry$, "",
	"ADD{S}", $Rd, Rn, op2$, $Rd = Rn + op2$, "",
	"ADR", $Rd, signed "rel"_12$, $Rd = PC + "rel"^signed$, "",
	// TODO: consider marking this type of instruction as setting flags?
	// something like $flags <= Rd - op2$?
	"CMN", $Rd, op2$, $Rd + op2$, "",
	"CMP", $Rd, op2$, $Rd - op2$, "",
	"MUL{S}", $Rd, Rn, Rm$, $Rd = Rn times Rm$, "B",
	// TODO Address inconsistency of signed in Sat usage. If Sat^signed is signed, that implies that without the signed it's unsigned. Also, there is SATS and SATU in the source. What is the difference between S/U and the presence/absence of the signed superscript??
	"QADD", $Rd, Rm, Rn$, $Rd = "Sat"(Rm + Rn)$, "D",
	"QDADD", $Rd, Rm, Rn$, $Rd = "Sat"(Rm + "Sat"(2 times Rn))$, "D",
	"QDSUB", $Rd, Rm, Rn$, $Rd = "Sat"(Rm - "Sat"(2 times Rn))$, "D",
	"QSUB", $Rd, Rm, Rn$, $Rd = "Sat"(Rm - Rn)$, "D",
	"RSB{S}", $Rd, Rn, op2$, $Rd = op2 - Rn$, "",
	"RSC{S}", $Rd, Rn, op2$, $Rd = op2 - (Rn + carry)$, "",
	"SBC{S}", $Rd, Rn, op2$, $Rd = Rn - (op2 + carry)$, "",
	"SDIV", $Rd, Rn, Rm$, $Rd = Rn macron(div) Rm$, "7",
	// TODO Was this meant to be lsr? If not, what does slr mean?
	"SSAT", $Rd, \#"st"_5, Rn{"slr"}$, $Rd = "Sat"(Rn << macron(>>) "sh", "st")^signed$, "6",
	"SSAT16", $Rd, \#"sat"_4, Rn$, $Rd = "Sat"(Rn^signed_"H1", "sat")^signed:"Sat"(Rn^signed_"H0", "sat")^signed$, "6D",
	"SUB{S}", $Rd, Rn, op2$, $Rd = Rn - op2$, "",
	"UDIV", $Rd, Rn, Rm$, $Rd = Rn div Rm$, "7",
	"USAT", $Rd, \#"st"_5, Rn{"slr"}$, $Rd = "Sat"(Rn << macron(>>) "sh", "st")$, "6",
	// TODO What is $i$ here?
	"USAT16", $Rd, \#"sat"_4, Rn$, $Rd = "Sat"(Rn^signed_"H1", "sat"):"Sat"(Rn^signed_"H0", "i")$, "6D",
	colspanx(2, $"Sat"$), [If the result is out of bounds, it is clamped to the available range (by default, 32 bits). $"Sat"^signed$ = signed.], "",
)

#let fN = $upright(N)$
#let fC = $upright(C)$
#let fZ = $upright(Z)$
#let fV = $upright(V)$

#info-table("Condition Codes (cc)", 3,
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

#ascii-table

#powers-hex-table

// TODO add a small table here for frequencies and periods, like Ghz and picoseconds.

#gdb-table

#let rlist = $upright("rlist")$
#let cnt = $upright("cnt")$
#let addr = $upright("addr")$
#let SP = $upright("SP")$

#let AS = $upright("AS")$

#instruction-table("ARM LDR/STR Addressing Modes",
	// TODO what does the # mean? Is this meant to indicate a number literal?
	// TODO why the comma? Is this bash alternation syntax?
	"non-T", $[Rn{, \#signed i_8}]{!}$, $addr = Rn + i^signed; space "if"(!) Rn = addr$, "",
	"xxR{,B}", $[Rn{, \#signed i_12}]{!}$, $addr = Rn + i^signed; "if"(!) Rn = addr$, "",
	"any", $[Rn]{, \#signed i_8}$, $addr = Rn; space Rn "+=" i^signed$, "",
	// TODO now this just seems inconsistent!
	"xxR{,B}{T}", $[Rn], \#signed i_12$, $addr = Rn; space Rn "+=" i^signed$, "",
	"non-T", $[Rn, signed Rm]{!}$, $addr = Rn plus.minus Rm; space "if"(!) Rn = addr$, "",
	// TODO what is AS?
	"xxR{,B}", $[Rn, signed Rm{AS}]{!}$, $addr = Rn plus.minus AS(Rm); space "if"(!) Rn = addr$, "",
	"any", $[Rn], signed Rm$, $addr = Rn; space Rn pmeq Rm$, "",
	"xxR{,B}{T}", $[Rn], signed Rm{AS}$, $addr = Rn; Rn pmeq AS(Rm)$, "",
	"LD non-T", $signed "rel"_8$, $addr = PC + "rel"^signed$, "",
	"LDR{,B}", $signed "rel"_12$, $addr = PC + "rel"^signed$, "",
)

#instruction-table("Special Instructions",
	"DBG", $\#i_4$, $"DebugHint"(i)$, "7",
	"DMB", [_option_], $"DataMemoryBarrier"("option")$, "I,7",
	"DSB", [_option_], $"DataSynchronizationBarrier"("option")$, "I,7",
	"ISB", "SY", $"InstructionSynchronizationBarrier"("SY")$, "I,7",
	"NOP", "", "", "6k",
	"PLD{W}", $[addr]$, $"PreloadData"(addr)$, "",
	"PLI", $[addr]$, $"PreloadInstruction"(addr)$, "7",
	"SETEND", ${"BE/LE"}$, $"EndianState" = {"BE/LE"}$, "I,6",
	"SEV", "", $"SendEvent"()$, "6k",
	"SVC", $\#i_24$, $"CallSupervisor"(i)$, "",
	"UDF", $\#i_16$, $"UndefinedException"(i)$, "",
	"WFE", "", $"WaitForEvent"()$, "6k",
	"WFI", "", $"WaitForInterrupt"()$, "6k",
	"YIELD", "", $"HintYield"()$, "6k",
)

#let Ra = $upright("Ra")$
// TODO what does this mean? More descriptive name.
#let mtimes = $macron(times)$

// TODO why were some entries duplicated here, in the form "XYZ" + "XYZ{S}", when a single "XYZ{S}" entry would suffice?
// TODO explain meanings of a, xy, xyz, H0 and H1, plus.minus operator
#instruction-table("Multiplication Instructions",
	// Putting the addition first for consistency with MLS.
	"MLA{S}", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn times Rm$, "",
	"MLS", $Rd, Rn, Rm, Ra$, $Rd = Ra - Rn times Rm$, "6t",
	"MUL{S}", $Rd, Rn, Rm$, $Rd = Rn times Rm$, "",
	"SMLAxy", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn^signed_"Hx" mtimes Rm^signed_"Hy"$, "D",
	"SMLaD", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn^signed_"H0" mtimes Rm^signed_"H0" signed Rn^signed_"H1" mtimes Rm^signed_"H1"$, "6,D",
	"SMLaDX", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus_"xyz" Rn^signed_"H1" mtimes Rm^signed_"H1"$, "D",
	// TODO is Rd_1, Rd_2 an acceptable way to show this? Maybe Rd_lo, Rd_hi, or something like that?
	"SMLaLD", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$, "6,D",
	"SMLaLDX", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn^signed_"H0" mtimes Rm^signed_"H1" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H0"$, "D",
	"SMLAL{S}", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn mtimes Rm$, "",
	"SMLALxy", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn^signed_"Hx" mtimes Rm^signed_"Hy"$, "D",
	"SMLAWy", $Rd, Rn, Rm, Ra$, $Rd = Ra + Rn mtimes Rm^signed_"Hy"$, "D",
	"SMMLa", $Rd, Rn, Rm, Ra$, $Rd = Ra plus.minus (Rn mtimes Rm)_(63:32)$, "6,D",
	"SMMLaR", $Rd, Rn, Rm, Ra$, $Rd = Ra plus.minus (Rm mtimes Rm + "0x8000" #thousand 0000)_(63:32)$, "D",
	"SMMUL", $Rd, Rn, Rm$, $Rd = (Rn mtimes Rm)_(63:32)$, "6,D",
	"SMMULR", $Rd, Rn, Rm$, $Rd = (Rn mtimes Rm + "0x8000" #thousand 0000)_(63:32)$, "D",
	"SMUaD", $Rd, Rn, Rm$, $Rd = Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$, "6,D",
	"SMUaDX", $Rd, Rn, Rm$, $Rd = Rn^signed_"H0" mtimes Rm^signed_"H0" plus.minus Rn^signed_"H1" mtimes Rm^signed_"H1"$, "D",
	"SMULxy", $Rd, Rn, Rm$, $Rd = Rn^signed_"Hx" mtimes Rm^signed_"Hy"$, "D",
	"SMULL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rn mtimes Rm$, "",
	"SMULL{S}", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rn mtimes Rm$, "",
	"SMULWy", $Rd, Rn, Rm$, $Rd = (Rn mtimes Rm^signed_"Hy")_(47:16)$, "D",
	"UMAAL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rd_1 + Rd_2 + Rn times Rm$, "D",
	"UMLAL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 "+=" Rn times Rm$, "",
	"UMULL", $Rd_1, Rd_2, Rn, Rm$, $Rd_2:Rd_1 = Rn times Rm$, "",
)

#instruction-table("Load and Store Instructions",
	"LDMDA", $Rn{!}, rlist$, $rlist = [Rn - 4 times cnt + 4]; space "if"(!) Rn "-=" 4 times cnt$, "",
	"LDMDB", $Rn{!}, rlist$, $rlist = [Rn - 4 times cnt]; space "if"(!) Rn "-=" 4 times cnt$, "",
	// TODO why was the I italic in the original?
	"LDMIA", $Rn{!}, rlist$, $rlist = [Rn]; space "if"(!) Rn "+=" 4 times cnt$, "",
	"LDMIB", $Rn{!}, rlist$, $rlist = [Rn + 4]; space "if"(!) Rn "+=" 4 times cnt$, "",
	// TODO what is the T suffix?
	"LDR{T}", $Rt, [addr]$, $Rt = [addr]$, "",
	"LDRB{T}", $Rt, [addr]$, $Rt = [addr]^diameter_8$, "",
	"LDRD", $Rt_1, Rt_2, [addr]$, $Rt_2:Rt_1 = [addr]$, "",
	"LDRH{T}", $Rt, [addr]$, $Rt = [addr]^diameter_16$, "",
	"LDRSB{T}", $Rt, [addr]$, $Rt = [addr]^signed_8$, "",
	"LDRSH{T}", $Rt, [addr]$, $Rt = [addr]^signed_16$, "",
	// TODO are the italics in the original document meant to indicate pseudoinstructions?
	[_POP_], $rlist$, $rlist = [SP]; SP "+=" 4 times cnt$, "",
	[_PUSH_], $rlist$, $SP "-=" 4 times cnt; [SP] = rlist$, "",
	"STMDA", $Rn{!}, rlist$, $[Rn - 4 times cnt + 4] = rlist; space "if"(!) Rn "-=" 4 times cnt$, "",
	"STMDB", $Rn{!}, rlist$, $[Rn - 4 times cnt] = rlist; space "if"(!) Rn "-=" 4 times cnt$, "",
	// TODO why was the I italic in the original?
	"STMIA", $Rn{!}, rlist$, $[Rn] = rlist; space "if"(!) Rn "+=" 4 times cnt$, "",
	"STMIB", $Rn{!}, rlist$, $[Rn + 4] = rlist; space "if"(!) Rn "+=" 4 times cnt$, "",
	"STR{T}", $Rt, [addr]$, $[addr] = Rt$, "",
	"STRB{T}", $Rt, [addr]$, $[addr]_8 = Rt_(7:0)$, "",
	// TODO I used _64 here to be consistent with the _8 in the previous entry. However I noticed some other cases where this convention was not used, e.g., LDRD. We should probably make this consistent.
	"STRD", $Rt_1, Rt_2, [addr]$, $[addr]_64 = Rt_2:Rt_1$, "",
	"STRH{T}", $Rt, [addr]$, $[addr]_16 = Rt_(15:0)$, "",
)

#info-table("Notes for Instruction Set", 2,
	"6, 6k, 6, 7", "Introduced in ARMv6, v6k, v6T2, v7",
	// TODO What?
	"B", "Technically a Multiple Groups Instruction",
	// TODO What?
	"D", "Not available on ARM-M without DSP extension",
	"I", "Can't be conditional",
	// TODO I thought italics indicated pseudo-instructions? Should "PUSH" and "POP" be tagged with this?
	"P", "Pseudo-instruction",
)

#{
let setmon = $"SetExclusiveMonitor"()$
let pass = $"Pass"$
let rdpass = $Rd = pass class("binary", ?) 1 : 0$
instruction-table("Exclusive Load and Store Instructions",
	"CLREX", "", $"ClearExclusiveLocal"()$, "I,6k",
	// TODO inconsistent use of Rt as opposed to Rd. Is this based on whether it's an operation vs a simple load/store?
	"LDREX", $Rt, [Rn]$, $Rt = [Rn]; space setmon$, "6k",
	"LDREX", $Rt, [Rn, \#i_10]$, $Rt = [Rn + i^diameter_(9:2):0_(1:0)]; space setmon$, "6k",
	// Sorted by size. TODO do this for the rest of the document.
	"LDREXB", $Rt, [Rn]$, $Rt = [Rn]_(7:0); space setmon$, "6k",
	"LDREXH", $Rt, [Rn]$, $Rt = [Rn]_(15:0); space setmon$, "6k",
	"LDREXD", $Rt_1, Rt_2, [Rn]$, $Rt_2:Rt_1 = [Rn]; space setmon$, "6k",
	// XXX changed this so the "target" Rt is the store location.
	// TODO this notation may be a bit confusing because pass is a result of the store. maybe "try" would make more sense?
	// TODO the ternary operator seems a bit confusing.
	// TODO for some reason the spacing after the "if" looks wrong here. Putting off fixing this because we may not keep the "if".
	"STREX", $Rd, Rn, [Rt]$, $"if"(pass) [Rt] = Rn; rdpass$, "6k",
	"STREX", $Rd, Rn, [Rt, \#i_10]$, $"if"(pass) [Rt] = Rn; space setmon$, "6k",
	"STREXB", $Rd, Rn, [Rt]$, $"if"(pass) [Rt]_8 = Rn_(7:0); space rdpass$, "6k",
	"STREXH", $Rd, Rn, [Rt]$, $"if"(pass) [Rt]_16 = Rn_(15:0); space rdpass$, "6k",
	"STREXD", $Rd, Rn_1, Rn_2, [Rt]$, $"if"(pass) [Rt]_128 = Rn_2:Rn_1; space rdpass$, "6k",
)
}

// This break is currently disabled to better allocate space.
// #colbreak(weak: true)

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

// XXX I shuffled around d, n, m, etc for this whole section for consistency.
#instruction-table("Floating-Point Register Transfer Instructions",
	"VMOV.F32", $Sd, \#signed imm$, $Sd = signed imm$, "",
	// XXX fixed a typo here, Sd -> Dd. Was that intentional?
	"VMOV.F64", $Dd, \#signed imm$, $Dd = signed imm$, "",
	"VMOV.F32", $Sd, Sn$, $Sd = Sn$, "",
	"VMOV.F64", $Dd, Dn$, $Dd = Dn$, "",
	// XXX I think this function approach is clearer. Opinions?
	"VMOV", $Rd, Sn$, $Rd = Reinterpret(Sn)$, "",
	"VMOV", $Sd, Rn$, $Sd = Reinterpret(Rn)$, "",
	"VMOV", $Dd, Rn_1, Rn_2$, $Dd = Reinterpret(Rn_2:Rn_1)$, "",
	"VMOV", $Rd_1, Rd_2, Dn$, $Rd_2:Rd_1 = Reinterpret(Dn)$, "",
	// XXX this was phrased very strangely. Also, where is this specific instruction form documented? Also, the [x] format was inconsistent with previous usage.
	// XXX what is ".32"? single-precision?
	"VMOV{.32}", $"Ddx", Rn$, $Dd_"Wx" = Rn$, "",
	"VMOV{.32}", $Rd, "Dnx"$, $Rd = Dn_"Wx"$, "",
	"VMRS", $Rd, "system-reg"$, $Rd = "system-reg"$, "",
	// TODO what does the "nzcv" mean here?
	"VMRS", $"APSR_nzcv", FPSCR$, $"APSR"_nzcv = FPSCR_nzcv$, "",
	"VMSR", $"system-reg", Rn$, $"system-reg" = Rn$, "",
)

#let fx = $"fx"$
#let fy = $"fy"$
#let fz = $"fz"$
#let fd = $"fd"$
#let fn = $"fn"$
#let Compare = $"Compare"$

#instruction-table("Floating-Point Instructions",
	"VABS.f", $fx, fy$, $fx = |fy|$, "",
	"VADD.f", $fd, fx, fy$, $fd = fx + fy$, "",
	// TODO what is "E"?
	"VCMP{E}.f", $fx, \#0.0$, $FPSCR_nzcv = Compare(fx, 0.0)$, "",
	"VCMP{E}.f", $fx, fy$, $FPSCR_nzcv = Compare(fx, fy)$, "",
	"VCVT.F32.F64", $Sd, Dn$, $Sd = "Float2Float"(Dn)$, "",
	"VCVT.F64.F32", $Dd, Sn$, $Dd = "Float2Float"(Sn)$, "",
	// TODO fix spacing of angle brackets. Putting this off until we decide that this formatting is good.
	// TODO what did the $^s$ mean?
	// TODO the order here seems wrong. Usually with VCVT.x.y it converts from y to x. Is this an intentional error?
	// TODO this myriad of X2Y functions could be unified to just "Cast" if we found a unified notation for the "type" of values within floating point registers. For example "^int" could mean an integer and "^fixed.i" could mean fixed-point with "i" decimal places.
	"VCVT.sz.f", $fd, fx, \#i_5$, $fd = "Float2Fixed"(fx, i)$, "V3",
	"VCVT.f.sz", $fd, fx, \#i_5$, $fd = "Fixed2Float"(fx, i)$, "V3",
	// XXX What is "R"?
	"VCVT{R}.s32.f", $Sd, fn$, $Sd = "Float2Int"(fn)$, "",
	// XXX this was "sy" in the original. Typo?
	"VCVT.f.s32", $fd, Sn$, $fd = "Int2Float"(Sn)$, "",
	"VCVTx.F16.F32", $Sd, Sn$, $Sd_"Hx" = "Float2Float"(Sn)$, "V3",
	"VCVTx.F32.F16", $Sd, Sn$, $Sd = "Float2Float"(Sn_"Hx")$, "V3",
	// XXX this was "dz" in the original. Typo?
	"VDIV.f", $fx, fy, fz$, $fx = fy div fz$, "",
	"VFMa.f", $fx, fy, fz$, $fx pmeq fy times fz$, "V4",
	"VFMNa.f", $fx, fy, fz$, $fx = -fx plus.minus fy times fz$, "V4",
	"VMLa.f", $fx, fy, fz$, $fx pmeq floor(fy times fz)$, "",
	"VMUL.f", $fx, fy, fz$, $fx = fy times fz$, "",
	"VNEG.f", $fx, fy$, $fx = -fy$, "",
	"VNMLa.f", $fx, fy, fz$, $fx = -fx plus.minus -floor(fy times fz)$, "",
	"VNMUL.f", $fx, fy, fz$, $fx = -floor(fy times fz)$, "",
	"VSQRT.f", $fx, fy$, $fx = sqrt(fy)$, "",
	"VSUB.f", $fx, fy, fz$, $fx = fy - fz$, "",
)

#let mx = $"mx"$
#let my = $"my"$
#let rx = $"rx"$
#let ry = $"ry"$
#let dx = $"dx"$
#let dy = $"dy"$

#instruction-table("SIMD Register Transfer Instructions",
	"VDUP.z", $mx, ry$, $mx_"z*" = ry_"z"$, "",
	"VDUP.z", $mx, dy[i]$, $mx_"z*" = dy_"zi"$, "",
	"VMOV.z", $dx[i], ry$, $dx_"zi" = ry_"z"$, "",
	// What does "^s" mean?
	"VMOV.sz", $rx, dy[i]$, $rx = dy_"zi"^"s"$, "",
	"VMOV", $mx, my$, $mx = my$, "",
)

#info-table("Floating-Point System Registers", 2,
	"FPEXC", "Floating-Point EXception Control",
	"FPSCR", "Floating-Point Status and Control Register",
	"FPSID", "Floating-Point System ID",
	"MVFR{0..1}", "Media and VFP feature {0..1}",
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
	"RMode", bit(23, 22), "Rounding mode (RN, RP, RM, RZ)",
	// TODO what do 0 and 1 mean?
	"FZ", bit(24), "Flush-to-zero mode",
	// TODO what do 0 and 1 mean?
	"DN", bit(25), "Default NaN mode",
	// TODO what do 0 and 1 mean?
	"AHP", bit(26), "Alternative half-precision",
	// TODO what do 0 and 1 mean?
	"QC", bit(27), "Cumulative saturation",
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
	// TODO q never used
	"sx, dx, qx", "Single-/double-/quadword register",
	"fx, fy, fz", "Floating-point register (Sx or Dx depending on data size)",
	"mx, my, mz", "SIMD Register (Dx or Qx)",
	// TODO never used
	"cm", "SIMD comparison operator (GE, GT, LE, or LT)",
	$floor(x)$, "Value is rounded",
	// TODO never used for FP or SIMD stuff. Used a few times in the main ISA section. Should it be moved to a different key? Also, this doesn't address the meaning of the macron that is sometimes (always?) present over the right-shift part of the operator.
	$x << >> y$, $(y < 0) class("binary", ?) (x >> -y) : (x << y)$,
	// TODO never used
	$and or$, "Maximum/minimum value",
)

#info-table("Notes for Floating-Point and SIMD Instructions", 2,
	"V3, V4", "Introduced in VFP v3, v4",
	"SH, S2", "Introduced in SIMD with half-precision floats, SIMDv2",
	"F", "Instruction with data type F32 also exists",
)
