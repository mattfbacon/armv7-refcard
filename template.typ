#import "@preview/tablex:0.0.5": tablex, colspanx, hlinex, vlinex

#let serif = "Inter"
#let serif-italic = "Inter Italic"
#let monospace = "Fira Code"
#let text-size = 6pt
#let gray(v) = rgb(v, v, v)
#let left-pad(length, pad, input) = pad * calc.max(0, length - input.len()) + input
// A thin space to use as a thousands separator.
#let thousand = h(1pt)
#let chunks(length, input, from: "start") = {
	if from == "start" {
		for start in range(0, input.len(), step: length) {
			(input.slice(start, calc.min(start + length, input.len())),)
		}
	} else if from == "end" {
		(for end in range(input.len(), 0, step: -length) {
			(input.slice(calc.max(0, end - length), end),)
		}).rev()
	}
}
#let zip(..arrs) = {
	let arrs = arrs.pos()
	let length = calc.min(..arrs.map(arr => arr.len()))
	for i in range(0, length) {
		(arrs.map(arr => arr.at(i)),)
	}
}
#let quo-if-whole(dividend, divisor) = if calc.rem(dividend, divisor) == 0 { calc.quo(dividend, divisor) } else { none }

#let info-table(name, columns-or-num-columns, main-column: auto, ..args) = {
	let line-stroke = 0.4pt

	let columns = if type(columns-or-num-columns) == "array" {
		columns-or-num-columns
	} else {
		let num-columns = columns-or-num-columns
		let main-column = if main-column == none {
			none
		} else if main-column == auto {
			num-columns - 1
		} else {
			main-column
		};
		let columns = (auto,) * num-columns
		if main-column != none {
			columns.at(main-column) = 1fr
		}
		columns
	}
	let num-columns = columns.len()
	block(breakable: false, {
		block(fill: gray(230), outset: 0pt, inset: 2pt, below: 0pt, width: 100%, stroke: line-stroke, [*#name;*])
		tablex(columns: columns, align: left + horizon, header-rows: 1, map-hlines: (v) => (..v, stroke: line-stroke), map-vlines: (v) => (..v, stroke: line-stroke), auto-lines: false, inset: 3pt, fill: (_col, row) => if calc.even(row) { white } else { gray(244) }, hlinex(), vlinex(), ..args, hlinex(), vlinex())
	})
}
#let instruction-table(name, ..args) = info-table(name, 4, main-column: 2, (), vlinex(), vlinex(), (), ..args)

#let heading-group(content) = {
	show heading: set block(spacing: 5pt)
	content
}

#let ascii-table = {
let characters = (
	// 0x00
	"NUL",
	"SOH",
	"STX",
	"ETX",
	"EOT",
	"ENQ",
	"ACK",
	"BEL",
	"BS",
	"TAB",
	"LF",
	"VT",
	"FF",
	"CR",
	"SO",
	"SI",
	"DLE",
	// 0x10
	"DC1",
	"DC2",
	"DC3",
	"DC4",
	"NAK",
	"SYN",
	"ETB",
	"CAN",
	"EM",
	"SUB",
	"ESC",
	"FS",
	"GS",
	"RS",
	"US",
	// 0x20
	[\'#sym.space.en\'],
	..range(0x21, 0x7e + 1).map(str.from-unicode),
	text(size: text-size - 2pt, "DEL"),
)
let characters = characters.enumerate().map(((i, chr)) => {
	align(right, [#chr #h(1pt) #left-pad(2, "0", upper(str(i, base: 16)))])
})
let characters = zip(..chunks(16, characters)).flatten()

info-table("ASCII Character Set", (auto,) * 2 + (1fr,) * 6,
	..for column in range(8) {
		(vlinex(x: column),)
	},
	..characters
)
}

#let powers-hex-table = {
let make-row(num) = {
	let si-units = (none, "K", "M", "G", "T")

	let m16 = quo-if-whole(num, 4)
	let si-unit = {
		let i = quo-if-whole(num, 10)
		if i != none {
			let unit = si-units.at(i, default: none)
			if unit != none {
				unit + "iB"
			}
		}
	}
	let pow2 = text(font: monospace, chunks(3, str(calc.pow(2, num)), from: "end").join(sym.space.quarter))
	let binary = text(font: monospace, chunks(4, left-pad(4, "0", str(num, base: 2)), from: "end").join(sym.space.quarter))
	(str(num), upper(str(num, base: 16)), binary, m16, si-unit, pow2, "")
}
info-table("Powers/Hexadecimal", 7,
	$N=$, vlinex(), "Hex", vlinex(), "Binary", vlinex(), $M_16=$, vlinex(), "iB", vlinex(), [$2^N$ and $16^M$], vlinex(), "Notes",
	..(range(0, 16) + range(16, 40 + 1, step: 4) + (30,)).sorted().map(make-row).flatten(),
	align: (_x, y) => if y == 0 { center + horizon } else { right + horizon },
)
}

// TODO add a small table here for frequencies and periods, like Ghz and picoseconds.

#let gdb-table = {
let u = underline
info-table("GNU Debugger (GDB) Commands", 3,
	[#u("l")ist], [_line number_], "Show program source code listing, optional line number",
	[#u("b")reak], [_line number_], "Set a break point at a specific line number",
	[#u("del")ete], [_break number_], "Delete the specified break point (all if no specified)",
	[#u("r")un], [_arguments_], "Run the program with optional cmdline arguments",
	[#u("c")ontinue], [_ignore_], [Continue (_ignore_ times) after breaking from breakpoint],
	[#u("fin")ish], "", "Continue, finish function/loop, then break again",
	[#u("s")tep], [_lines_], [Runs a single (or _lines_) instruction or source line],
	[#u("i")nfo], [b, f, r, s], [Display #u("b")reakpoint, #u("r")egister, #u("f")p registers, or #u("s")tack],
	[#u("p")rint/f], [_(type) myVar_], [Display w/type of (int), (float) /f => x-hex, f-float, t-bin],
	[e#u("x")amine], [_/nfu address_], [Display value at address /n => Number of elements to display, f => #u("s")tring, #u("i")nstruction, he#u("x"), /u => size in #u("b")yte, #u("h")alfword, #u("w")ord],
	[#u("q")uit], "", "Quit GDB",
	[#u("h")elp], "", "Get help on a GDB command",
)
}

#let bit(..xs) = text(font: monospace, "0x" + chunks(4, left-pad(8, "0", str(xs.pos().map(x => calc.pow(2, x)).sum(), base: 16))).join(thousand))

#let ref-card = (body) => {
	set page(flipped: true, paper: "a4", margin: 15pt)
	set text(font: serif, stylistic-set: 2, size: text-size, fallback: false)

	show heading: it => align(center, it)
	show: it => columns(4, gutter: 5pt, it)
	set block(spacing: 5pt)

	body
}

