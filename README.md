<a id="x-28clones-2edocs-3a-40clones-20mgl-pax-3asection-29"></a>

# Clones - An NES Emulator

## Table of Contents

- [1 Links][da3b]
- [2 Overview][8818]
- [3 ROM Parsing][d5a6]
- [4 Mapper Interface][fc3d]
- [5 Memory Interface][76da]
- [6 Opcode Data][ad2e]
- [7 Disassembler][e916]
- [8 `CPU` Core][e1fe]
- [9 Picture Processing Unit][91d8]
- [10 The Rendering Logic][ba87]
- [11 Input Handling][fcfc]
- [12 Debugging Utilities][f08b]

<a id="x-28clones-2edocs-3a-40links-20mgl-pax-3asection-29"></a>

## 1 Links


[repo]: https://git.sr.ht/~kingcons/clones

[site]: https://clones.kingcons.io

[builds]: https://builds.sr.ht/~kingcons/clones
- [CI Builds][builds]

Here are links to the current [website][site] and [git repo][repo].

<a id="x-28clones-2edocs-3a-40overview-20mgl-pax-3asection-29"></a>

## 2 Overview


[goals]: https://blog.kingcons.io/posts/Research-Goals.html

[tgj]: https://twitter.com/leastfixedpoint/status/1026567416229314561
Clones is an early-stage NES emulator written in Common Lisp.

It is inspired by long standing beliefs about the power of computers
for experiential learning. See: [Research Goals][goals].

### Why another emulator?

> Reading things teaches people how to write. Analogous, if we are
> to place programming at the same fundamental level, using a
> program should teach how it works. But we don't see this.

- Tony Garnock-Jones, [@leastfixedpoint][tgj]

Many NES emulators already exist on the web, the desktop, and elsewhere.
Clones is intended to be a readable, tested, and compact code base sufficient
for mostly accurate emulation of many but not all popular Nintendo titles.

However, my goal is not to be able to relive childhood nostalgia since
that need is thoroughly solved. Clones exists to support curious programmers
learning about how the system and its titles worked rather than being a
vehicle for reliving the past. Running the games is just a prerequisite.

The codebase strives to be accessible for learning about emulation,
sacrificing total accuracy and performance for clarity and ease of modification.
Once some key games (Super Mario Bros, Mega Man 2) are playable, focus will
shift towards building out debugging and reverse engineering tools.

In addition to the usual tools to disassemble memory or view VRAM, we hope to
support building a directed graph of blocks and jumps as games are played.
Afterwards, we'll provide tools for users to visualize and annotate the graph
with notes about the code. This will move us towards our overall goal of making
the high-level structure of programs "discoverable" through using them,
calling back to the Tony Garnock-Jones quote above.

### Current Status

Work has just begun so nothing is playable yet. Hang in there.

- ROM Support: ✅

- NROM Mapper: ✅

- CPU Opcodes: ✅

- PPU Registers: ✅

- Rendering - Timing: ✅

- Rendering - Backgrounds: ✅

- Rendering - Sprites: ⌛

- Rendering - Scrolling: ⌛

- Input Handling: ✅

- MMC1 Mapper: ✅

- MMC3 Mapper: ❌

- Audio Support: ❌


<a id="x-28clones-2erom-3a-40rom-20mgl-pax-3asection-29"></a>

## 3 ROM Parsing

<a id="x-28clones-2erom-3aparse-rom-20function-29"></a>

- [function] **PARSE-ROM** *PATHNAME*

    Attempt to parse the file at `PATHNAME` as an Nintendo ROM, returning
    a property list suitable for passing to [`MAKE-INSTANCE`][dddd] for an appropriate
    [`MAPPER`][2e99]. An [`INVALID-ROM`][6ee9] condition will be signalled if
    the header does not conform to the [iNES format](http://rocknes.web.fc2.com/ines.txt).

<a id="x-28clones-2erom-3ainvalid-rom-20condition-29"></a>

- [condition] **INVALID-ROM** *[ERROR][d162]*

    Signalled when the file passed to [`PARSE-ROM`][f5d8] does not correspond
    to the [iNES format](http://rocknes.web.fc2.com/ines.txt).

<a id="x-28clones-2emappers-3a-40mappers-20mgl-pax-3asection-29"></a>

## 4 Mapper Interface

<a id="x-28clones-2emappers-3amapper-20class-29"></a>

- [class] **MAPPER**

    A Mapper is a virtual representation of a game cartridge,
    referenced by the PPU for purposes of accessing graphical data (`CHR`) and by the
    CPU for purposes of accessing program code (`PRG`).

<a id="x-28clones-2emappers-3aload-rom-20function-29"></a>

- [function] **LOAD-ROM** *&OPTIONAL (PATHNAME (ASDF/SYSTEM:SYSTEM-RELATIVE-PATHNAME :CLONES "roms/nestest.nes"))*

    Given a `PATHNAME` to a valid Nintendo ROM, process the file using [`PARSE-ROM`][f5d8]
    and return an appropriate instance of [`MAPPER`][2e99] for the cartridge type of the game.
    An [`UNIMPLEMENTED-MAPPER`][7945] condition will be signalled if the cartridge type is not
    yet supported by clones. If no `PATHNAME` is supplied, the NEStest ROM will be used.

<a id="x-28clones-2emappers-3aunimplemented-mapper-20condition-29"></a>

- [condition] **UNIMPLEMENTED-MAPPER**

    Signalled when no subclass of [`MAPPER`][2e99] implements the
    cartridge for the file supplied to [`LOAD-ROM`][b6b3].

<a id="x-28clones-2emappers-3amapper-pathname-20generic-function-29"></a>

- [generic-function] **MAPPER-PATHNAME** *OBJECT*

<a id="x-28clones-2emappers-3aget-prg-20generic-function-29"></a>

- [generic-function] **GET-PRG** *MAPPER ADDRESS*

    Retrieve the value at `ADDRESS` from the `PRG` bank of `MAPPER`.

<a id="x-28clones-2emappers-3aset-prg-20generic-function-29"></a>

- [generic-function] **SET-PRG** *MAPPER ADDRESS VALUE*

    Set `ADDRESS` in the `PRG` bank of `MAPPER` to `VALUE`.

<a id="x-28clones-2emappers-3aget-chr-20generic-function-29"></a>

- [generic-function] **GET-CHR** *MAPPER ADDRESS*

    Retrive the value at `ADDRESS` from the `CHR` bank of `MAPPER`.

<a id="x-28clones-2emappers-3aset-chr-20generic-function-29"></a>

- [generic-function] **SET-CHR** *MAPPER ADDRESS VALUE*

    Set `ADDRESS` in the `CHR` bank of `MAPPER` to `VALUE`.

<a id="x-28clones-2emappers-3amirroring-20-28mgl-pax-3aaccessor-20clones-2emappers-3amapper-29-29"></a>

- [accessor] **MIRRORING** *MAPPER (:MIRRORING)*

<a id="x-28clones-2ememory-3a-40memory-20mgl-pax-3asection-29"></a>

## 5 Memory Interface

<a id="x-28clones-2ememory-3amemory-20class-29"></a>

- [class] **MEMORY**

<a id="x-28clones-2ememory-3amake-memory-20function-29"></a>

- [function] **MAKE-MEMORY** *&KEY (RAM (MAKE-OCTET-VECTOR 2048)) (PPU (MAKE-PPU)) (CONTROLLER (MAKE-CONTROLLER)) (CART (LOAD-ROM))*

<a id="x-28clones-2ememory-3afetch-20function-29"></a>

- [function] **FETCH** *MEMORY ADDRESS*

<a id="x-28clones-2ememory-3astore-20function-29"></a>

- [function] **STORE** *MEMORY ADDRESS VALUE*

<a id="x-28clones-2ememory-3afetch-word-20function-29"></a>

- [function] **FETCH-WORD** *MEMORY ADDRESS*

<a id="x-28clones-2ememory-3afetch-indirect-20function-29"></a>

- [function] **FETCH-INDIRECT** *MEMORY START*

<a id="x-28clones-2ememory-3amemory-ppu-20-28mgl-pax-3areader-20clones-2ememory-3amemory-29-29"></a>

- [reader] **MEMORY-PPU** *MEMORY (:PPU)*

<a id="x-28clones-2ememory-3amemory-cart-20-28mgl-pax-3areader-20clones-2ememory-3amemory-29-29"></a>

- [reader] **MEMORY-CART** *MEMORY (:CART)*

<a id="x-28clones-2ememory-3amemory-controller-20-28mgl-pax-3areader-20clones-2ememory-3amemory-29-29"></a>

- [reader] **MEMORY-CONTROLLER** *MEMORY (:CONTROLLER)*

<a id="x-28clones-2ememory-3amemory-dma-3f-20-28mgl-pax-3aaccessor-20clones-2ememory-3amemory-29-29"></a>

- [accessor] **MEMORY-DMA?** *MEMORY (= NIL)*

<a id="x-28clones-2ememory-3aswap-cart-20function-29"></a>

- [function] **SWAP-CART** *MEMORY RELATIVE-PATH*

<a id="x-28clones-2eopcodes-3a-40opcodes-20mgl-pax-3asection-29"></a>

## 6 Opcode Data

<a id="x-28clones-2eopcodes-3afind-opcode-20function-29"></a>

- [function] **FIND-OPCODE** *BYTE*

    Find the `OPCODE` encoded as `BYTE`.

<a id="x-28clones-2eopcodes-3aopcode-name-20-28mgl-pax-3astructure-accessor-20clones-2eopcodes-3a-3aopcode-29-29"></a>

- [structure-accessor] **OPCODE-NAME** *OPCODE*

<a id="x-28clones-2eopcodes-3aopcode-code-20-28mgl-pax-3astructure-accessor-20clones-2eopcodes-3a-3aopcode-29-29"></a>

- [structure-accessor] **OPCODE-CODE** *OPCODE*

<a id="x-28clones-2eopcodes-3aopcode-size-20-28mgl-pax-3astructure-accessor-20clones-2eopcodes-3a-3aopcode-29-29"></a>

- [structure-accessor] **OPCODE-SIZE** *OPCODE*

<a id="x-28clones-2eopcodes-3aopcode-time-20-28mgl-pax-3astructure-accessor-20clones-2eopcodes-3a-3aopcode-29-29"></a>

- [structure-accessor] **OPCODE-TIME** *OPCODE*

<a id="x-28clones-2eopcodes-3aopcode-addressing-mode-20-28mgl-pax-3astructure-accessor-20clones-2eopcodes-3a-3aopcode-29-29"></a>

- [structure-accessor] **OPCODE-ADDRESSING-MODE** *OPCODE*

<a id="x-28clones-2eopcodes-3aopcode-access-pattern-20-28mgl-pax-3astructure-accessor-20clones-2eopcodes-3a-3aopcode-29-29"></a>

- [structure-accessor] **OPCODE-ACCESS-PATTERN** *OPCODE*

<a id="x-28clones-2edisassembler-3a-40disassembler-20mgl-pax-3asection-29"></a>

## 7 Disassembler

<a id="x-28clones-2edisassembler-3adisasm-20function-29"></a>

- [function] **DISASM** *MEMORY START END*

    Loop through `MEMORY` from `START` to `END` printing disassembly
    for each instruction found in the specified range. An error
    will be thrown if illegal instructions are present or if the
    start index is not the beginning of a 6502 instruction.

<a id="x-28clones-2edisassembler-3adisassemble-instruction-20function-29"></a>

- [function] **DISASSEMBLE-INSTRUCTION** *MEMORY INDEX &KEY (STREAM T)*

    Disassemble a single instruction from `MEMORY` beginning at `INDEX`.
    `STREAM` is the [`FORMAT`][ad78] destination of the disassembly output.

<a id="x-28clones-2ecpu-3a-40cpu-20mgl-pax-3asection-29"></a>

## 8 `CPU` Core

<a id="x-28clones-2ecpu-3acpu-20class-29"></a>

- [class] **CPU**

<a id="x-28clones-2ecpu-3amake-cpu-20function-29"></a>

- [function] **MAKE-CPU** *&KEY (MEMORY (MAKE-MEMORY))*

<a id="x-28clones-2ecpu-3acpu-memory-20-28mgl-pax-3aaccessor-20clones-2ecpu-3acpu-29-29"></a>

- [accessor] **CPU-MEMORY** *CPU (:MEMORY)*

<a id="x-28clones-2ecpu-3acpu-cycles-20-28mgl-pax-3aaccessor-20clones-2ecpu-3acpu-29-29"></a>

- [accessor] **CPU-CYCLES** *CPU (= 0)*

<a id="x-28clones-2ecpu-3asingle-step-20function-29"></a>

- [function] **SINGLE-STEP** *CPU*

    Step the `CPU` over the current instruction.

<a id="x-28clones-2ecpu-3areset-20function-29"></a>

- [function] **RESET** *CPU*

<a id="x-28clones-2ecpu-3anow-20function-29"></a>

- [function] **NOW** *CPU &KEY (STREAM T)*

    Disassemble the current instruction pointed to by the `CPU`'s program counter.
    `STREAM` is the [`FORMAT`][ad78] destination for the disassembly.

<a id="x-28clones-2ecpu-3anmi-20function-29"></a>

- [function] **NMI** *CPU*

<a id="x-28clones-2ecpu-3achange-game-20function-29"></a>

- [function] **CHANGE-GAME** *CPU RELATIVE-PATH*

<a id="x-28clones-2eppu-3a-40ppu-20mgl-pax-3asection-29"></a>

## 9 Picture Processing Unit

<a id="x-28clones-2eppu-3appu-20class-29"></a>

- [class] **PPU**

<a id="x-28clones-2eppu-3amake-ppu-20function-29"></a>

- [function] **MAKE-PPU** *&KEY (NAME-TABLE (MAKE-OCTET-VECTOR 4096)) (CART (LOAD-ROM))*

<a id="x-28clones-2eppu-3aread-ppu-20function-29"></a>

- [function] **READ-PPU** *PPU ADDRESS*

<a id="x-28clones-2eppu-3awrite-ppu-20function-29"></a>

- [function] **WRITE-PPU** *PPU ADDRESS VALUE*

<a id="x-28clones-2eppu-3aset-vblank-21-20function-29"></a>

- [function] **SET-VBLANK!** *PPU VALUE*

<a id="x-28clones-2eppu-3avblank-nmi-3f-20function-29"></a>

- [function] **VBLANK-NMI?** *PPU*

<a id="x-28clones-2eppu-3aread-palette-20function-29"></a>

- [function] **READ-PALETTE** *PPU ADDRESS*

<a id="x-28clones-2eppu-3aget-mirroring-20function-29"></a>

- [function] **GET-MIRRORING** *PPU*

<a id="x-28clones-2eppu-3aquad-position-20function-29"></a>

- [function] **QUAD-POSITION** *PPU*

<a id="x-28clones-2eppu-3arender-sprites-3f-20function-29"></a>

- [function] **RENDER-SPRITES?** *PPU*

<a id="x-28clones-2eppu-3arender-background-3f-20function-29"></a>

- [function] **RENDER-BACKGROUND?** *PPU*

<a id="x-28clones-2eppu-3arendering-enabled-3f-20function-29"></a>

- [function] **RENDERING-ENABLED?** *PPU*

<a id="x-28clones-2eppu-3asprite-20class-29"></a>

- [class] **SPRITE**

<a id="x-28clones-2eppu-3amake-sprite-20function-29"></a>

- [function] **MAKE-SPRITE** *PPU NUMBER*

<a id="x-28clones-2eppu-3aevaluate-sprites-20function-29"></a>

- [function] **EVALUATE-SPRITES** *PPU SCANLINE*

<a id="x-28clones-2eppu-3asprite-zero-hit-3f-20function-29"></a>

- [function] **SPRITE-ZERO-HIT?** *PPU*

<a id="x-28clones-2eppu-3aset-sprite-overflow-21-20function-29"></a>

- [function] **SET-SPRITE-OVERFLOW!** *PPU VALUE*

<a id="x-28clones-2eppu-3aset-sprite-zero-hit-21-20function-29"></a>

- [function] **SET-SPRITE-ZERO-HIT!** *PPU VALUE*

<a id="x-28clones-2eppu-3afetch-nt-byte-20function-29"></a>

- [function] **FETCH-NT-BYTE** *PPU*

    See: https://www.nesdev.org/wiki/PPU\_scrolling#Tile\_and\_attribute\_fetching

<a id="x-28clones-2eppu-3afetch-scanline-bytes-20function-29"></a>

- [function] **FETCH-SCANLINE-BYTES** *PPU TILE-DESCRIPTOR*

    Fetch the low and high bytes of the pattern matching `TILE-DESCRIPTOR`
    that are appropriate for display on the current scanline of `PPU`.

<a id="x-28clones-2eppu-3afetch-tile-bytes-20function-29"></a>

- [function] **FETCH-TILE-BYTES** *PPU TILE-DESCRIPTOR*

    Fetch all 16 bytes of the the pattern corresponding to `TILE-DESCRIPTOR`.

<a id="x-28clones-2eppu-3aflip-x-3f-20generic-function-29"></a>

- [generic-function] **FLIP-X?** *TILE-DESCRIPTOR*

    Check whether the X-axis of `TILE-DESCRIPTOR` should be flipped.

<a id="x-28clones-2eppu-3aflip-y-3f-20generic-function-29"></a>

- [generic-function] **FLIP-Y?** *TILE-DESCRIPTOR*

    Check whether the Y-axis of `TILE-DESCRIPTOR` should be flipped.

<a id="x-28clones-2eppu-3acompute-x-offset-20generic-function-29"></a>

- [generic-function] **COMPUTE-X-OFFSET** *PPU TILE-DESCRIPTOR*

    Compute the X offset for the tile currently being rendered by `PPU`.

<a id="x-28clones-2eppu-3apalette-low-bits-20function-29"></a>

- [function] **PALETTE-LOW-BITS** *LOW-BYTE HIGH-BYTE INDEX*

<a id="x-28clones-2eppu-3apalette-high-bits-20generic-function-29"></a>

- [generic-function] **PALETTE-HIGH-BITS** *PPU TILE-DESCRIPTOR*

    Determine the 2 high bits of the palette index for `TILE-DESCRIPTOR`.

<a id="x-28clones-2eppu-3afine-scroll-vertical-21-20function-29"></a>

- [function] **FINE-SCROLL-VERTICAL!** *PPU*

    A scroll operation that conceptually occurs at the end of each scanline.

<a id="x-28clones-2eppu-3acoarse-scroll-horizontal-21-20function-29"></a>

- [function] **COARSE-SCROLL-HORIZONTAL!** *PPU*

    A scroll operation that conceptually occurs at the end of each 8-pixel tile.

<a id="x-28clones-2eppu-3async-vertical-scroll-21-20function-29"></a>

- [function] **SYNC-VERTICAL-SCROLL!** *PPU*

<a id="x-28clones-2eppu-3async-horizontal-scroll-21-20function-29"></a>

- [function] **SYNC-HORIZONTAL-SCROLL!** *PPU*

<a id="x-28clones-2erenderer-3a-40renderer-20mgl-pax-3asection-29"></a>

## 10 The Rendering Logic

<a id="x-28clones-2erenderer-3arenderer-20class-29"></a>

- [class] **RENDERER**

<a id="x-28clones-2erenderer-3amake-renderer-20function-29"></a>

- [function] **MAKE-RENDERER** *&KEY (PPU (MAKE-PPU)) (ON-NMI (CONSTANTLY NIL)) ON-FRAME*

<a id="x-28clones-2erenderer-3async-20generic-function-29"></a>

- [generic-function] **SYNC** *RENDERER CPU FRAMEBUFFER*

    Synchronize the renderer to the `CPU` and return the next scanline.

<a id="x-28clones-2erenderer-3arender-pixel-20function-29"></a>

- [function] **RENDER-PIXEL** *FRAMEBUFFER X Y COLOR-INDEX*

<a id="x-28clones-2einput-3a-40input-20mgl-pax-3asection-29"></a>

## 11 Input Handling

<a id="x-28clones-2einput-3acontroller-20class-29"></a>

- [class] **CONTROLLER**

<a id="x-28clones-2einput-3amake-controller-20function-29"></a>

- [function] **MAKE-CONTROLLER**

<a id="x-28clones-2einput-3aread-controller-20function-29"></a>

- [function] **READ-CONTROLLER** *CONTROLLER*

<a id="x-28clones-2einput-3areset-controller-20function-29"></a>

- [function] **RESET-CONTROLLER** *CONTROLLER*

<a id="x-28clones-2einput-3aupdate-button-20function-29"></a>

- [function] **UPDATE-BUTTON** *CONTROLLER BUTTON VALUE*

<a id="x-28clones-2edebug-3a-40debug-20mgl-pax-3asection-29"></a>

## 12 Debugging Utilities

<a id="x-28clones-2edebug-3afor-sprites-20function-29"></a>

- [function] **FOR-SPRITES** *PPU CALLBACK*

<a id="x-28clones-2edebug-3afor-background-20function-29"></a>

- [function] **FOR-BACKGROUND** *PPU CALLBACK &KEY (NAME-TABLE 0)*

<a id="x-28clones-2edebug-3adump-graphics-20function-29"></a>

- [function] **DUMP-GRAPHICS** *BUFFER PPU &KEY ITERATOR MARGIN*

  [2e99]: #x-28clones-2emappers-3amapper-20class-29 "CLONES.MAPPERS:MAPPER CLASS"
  [6ee9]: #x-28clones-2erom-3ainvalid-rom-20condition-29 "CLONES.ROM:INVALID-ROM CONDITION"
  [76da]: #x-28clones-2ememory-3a-40memory-20mgl-pax-3asection-29 "Memory Interface"
  [7945]: #x-28clones-2emappers-3aunimplemented-mapper-20condition-29 "CLONES.MAPPERS:UNIMPLEMENTED-MAPPER CONDITION"
  [8818]: #x-28clones-2edocs-3a-40overview-20mgl-pax-3asection-29 "Overview"
  [91d8]: #x-28clones-2eppu-3a-40ppu-20mgl-pax-3asection-29 "Picture Processing Unit"
  [ad2e]: #x-28clones-2eopcodes-3a-40opcodes-20mgl-pax-3asection-29 "Opcode Data"
  [ad78]: http://www.lispworks.com/documentation/HyperSpec/Body/f_format.htm "FORMAT (MGL-PAX:CLHS FUNCTION)"
  [b6b3]: #x-28clones-2emappers-3aload-rom-20function-29 "CLONES.MAPPERS:LOAD-ROM FUNCTION"
  [ba87]: #x-28clones-2erenderer-3a-40renderer-20mgl-pax-3asection-29 "The Rendering Logic"
  [d162]: http://www.lispworks.com/documentation/HyperSpec/Body/e_error.htm "ERROR (MGL-PAX:CLHS CONDITION)"
  [d5a6]: #x-28clones-2erom-3a-40rom-20mgl-pax-3asection-29 "ROM Parsing"
  [da3b]: #x-28clones-2edocs-3a-40links-20mgl-pax-3asection-29 "Links"
  [dddd]: http://www.lispworks.com/documentation/HyperSpec/Body/f_mk_ins.htm "MAKE-INSTANCE (MGL-PAX:CLHS GENERIC-FUNCTION)"
  [e1fe]: #x-28clones-2ecpu-3a-40cpu-20mgl-pax-3asection-29 "CPU Core"
  [e916]: #x-28clones-2edisassembler-3a-40disassembler-20mgl-pax-3asection-29 "Disassembler"
  [f08b]: #x-28clones-2edebug-3a-40debug-20mgl-pax-3asection-29 "Debugging Utilities"
  [f5d8]: #x-28clones-2erom-3aparse-rom-20function-29 "CLONES.ROM:PARSE-ROM FUNCTION"
  [fc3d]: #x-28clones-2emappers-3a-40mappers-20mgl-pax-3asection-29 "Mapper Interface"
  [fcfc]: #x-28clones-2einput-3a-40input-20mgl-pax-3asection-29 "Input Handling"

* * *
###### \[generated by [MGL-PAX](https://github.com/melisgl/mgl-pax)\]
