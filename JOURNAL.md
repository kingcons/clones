## TODO

### Notes from PPU / Mezzanine work (05/27)

Currently we're directly copying #x2000 of CHR from the ROM into the PPU pattern table.
This is a dirty hack and will make bank switching a pain in the ass probably.
The PPU needs to read CHR as that's pattern table data / VROM
and MEMORY needs to read PRG as that's, well, game code for the CPU.

What would be better is to have a global variable for the `*MAPPER*` that both MEMORY and PPU
have access to. Or to store the mapper in the PPU and MEMORY but we wind up with 2 instances then.

One thing that has bothered me about working on the PPU is that unlike the CPU,
there's not really a good way to work on discrete chunks. I don't know of PPU test roms
that verify behavior at all like the CPU test roms do. That doesn't mean there isn't one of course.

With the CPU, I was able to get the instruction data, the CPU struct, a macro or two, and a
single-step function, and immediately start implementing things opcode by opcode.

There really is no equivalent for the PPU. You simultaneously need to spin up several pieces:

* A PPU struct / the state machine itself.
* The memory map and register behavior for the PPU.
* The actual scanline loop / rendering.
* Sprite priority handling.
* A way to output the graphics data from the PPU to the real hardware with SDL/OpenGL.
  * This can at least be done and tested separately but don't forget palette/color stuff.
* A way to synchronize the PPU and CPU / correct nmi-vblank behavior.

I _think_ those are all the pieces but for all I know I'm missing something.
Nowhere do I think I've seen written down a set of testable steps for bringing
up any of those pieces individually. I'm not sure if that's because:
  1. It can't be brought up and tested individually due to interactions between components.
  2. Everyone that writes a NES has no trouble just hacking up the whole thing in one go.

Anyway, I'm not a huge fan of the PPU not because any part of it seems intrinsically complicated
but because it seems we should be able to test chunks of the implementation without finishing the
whole damn thing. Ah, well. :-/

### Notes from Main Loop Hackery (05/28)

I hate all the code I wrote or saw in the last hour. Everything looks like a
mess of nested `sdl2:with-foo` calls. `sdl2kit` might show a way out though.

I'm not sure where to put functionality like `swap-game` as it touches: CPU, PPU, MemoryMap.
Trying to have a nice main loop is pretty tricky as everything winds up nested in the SDL
`with-event-loop` instead of me manually asking for missed events or something. It looks a mess.

I need a `SCANLINE-STEP` function in the PPU. That needs to either take a framebuffer as an
argument ... or the framebuffer just needs to be a global in the PPU file to begin with.
Then that `*framebuffer*` can be exported from PPU but imported only in `graphics` (maybe we
should rename it "display"?) and used in a `render-frame` method to output the frame once finished.

I think it's still fine for toplevel coordination to happen in `clones.lisp`, I just want all
the helpers and gunk for that pushed down into the individual files. And I don't _want_ to figure
out how input handling factors in but that's just because of `with-event-loop` so don't use it.

### Notes on Optimization Ideas (05/28)

[Brad Taylor's guide](http://nesdev.com/NES%20emulator%20development%20guide.txt) has some
very interesting ideas about optimization, particularly Hardware Port Queueing. I would also
be interested in how much CPU work could be saved by "busy loop" / "waiting for NMI" detection.
I also still like the idea of a `TREE-CASE` macro or other tricks for opcode dispatch.
Obviously, all that is less important than:

A) Getting graphics working.
B) Getting input working.
C) Getting sound working.
D) Getting more mappers working.
E) At least partially documenting / cleaning up / writing up what's been done.

But after that those optimizations could prove interesting along with more robust
debugging / reversing tools. :)

### Notes on Sketching (05/28)

FUUUUUUCK. I just wrote an unbelievable amount of code without testing it and cribbing
entirely too much from pcwalton's Sprocketnes which I still think is about as good a
readable NES emulator as presently exists. What's super annoying about this isn't so
much that I'm borrowing or transliterating someone else's code but I don't know what
reference would best guide me to write something like what I'm presently writing.
The closest thing is probably: https://wiki.nesdev.com/w/index.php/PPU_rendering

Really, I could write the PPU specific code from that reference but remembering how
to wire it up to SDL and synchronize with the CPU is what usually gets me.
It's probably silly to whine about this as there are guides for the clock synchronization
too but ... it gets back to wondering how you're supposed to actually _test_ subcomponents
as you go rather than writing like ... 500 lines of code before trying to run the whole
damn thing.

I also just hacked up a bunch of globals when initializing SDL2, etc. It may be better
to use a higher-level wrapper like SDL2kit but I worry about that calling render whenever
idle even if there isn't a new-frame available or otherwise mucking things up. That said,
the PPU knows when a frame is available so we could just sync that with a display object...

We can refactor for SDL2kit later. For now, God do I just want to get _something_ rendering.

### Notes on Background Rendering (05/29)

Honestly, things went pretty well this weekend. I got quite frustrated yesterday just because
I'd kind of put expectations on myself to get the whole PPU finished in a weekend. I'm sure
that wouldn't be hard for some folks but I didn't manage it. I also spent time relaxing with
Norma and friends throughout the weekend which was good for me, so there.

Anyway, I learned that I _do not_ want to do pixel-at-a-time rendering with the PPU. Certainly
it's accurate and the performance isn't even what I'm worried about. It's just the readability.
The pixel-at-a-time code I've read gives _zero_ intuitive feel for how the PPU behaves. Maybe
it's good for systems programmers or electrical engineers or something. Instead, I'd rather
do scanline-based rendering and work in tiles instead of pixels. See how far that gets me.

I still wish I had a better way of arriving at this stuff via first principles. Or proceeding
through PPU roms in some well defined order. Oh well. We're getting there.

### Questions about DMA (05/30)

For most games to work we need DMA support. How will we do that in Clones?
The PPU doesn't have access to the CPU, memory is what actually sees the store
to 4014. Not that we couldn't hard code something in, presumably, STA but that
would be quite a gross hack. No reason they couldn't use STX or STY among others.

Memory can manage the DMA but the problem is it doesn't have access to the CPU
and we need to bump the CPU cycle count after! It's also concerning that a DMA
takes 512 cycles which is almost _five_ scanline renders. It should always take
place during Vblank at least. So maybe we can just say fuck it, run the CPU that
long, then get the PPU to catch up instead of interleaving but still unclear how
to tell the CPU how many cycles to add. -_-

The best I can figure is that we can have the PPU handle the 0x4014 write and
go ahead and run the DMA, returning :oam-dma t in the ppu-result. Then the main
loop can get that and call a function on the CPU asking it to just screw around
for 512 cycles. The problem with _that_ is: A) the CPU will certainly execute
other instructions before we step the PPU and get back a result saying to idle
while DMA happens and B) trying to get the cycle timing to sync back up with PPU
and APU reasonably after that makes my head spin just thinking about it.

_MAYBE_ it's no big deal if A) the instructions immediately following a DMA
don't directly access or depend on the cycle count/timing being correct and
B) we can modify `play` to run _multiple_ scanlines to catch up to CPU cycles
and reset the CPU cycle count at beginning of `scanline-step`? T_T

What I really need is a non-shitty pattern for bidirectional data flow in OOP sans global vars.
At least one way to fix this would be adding a slot to Memory which then gets a reference to
the CPU after memory is created. This gets us the CPU<-->Memory visibility for implementing DMA.
Or the PPU could have access to the CPU. CPU<-->PPU is just as good for DMA purposes.
We still need PPU<-->Mapper visibility though to access pattern table data.

### Questions about Rendering (05/30)

It seems we're running things scanline by scanline. How do we render a scanline?
So we start rendering a scanline looking for the first tile.
The nametable points us to the pattern data and it is 16 bytes for an 8x8 tile
but since we're only rendering a one-pixel line we only need two bytes.

First byte describes the low-bit of each of the 8 pixels color palette index.
Eight bytes ahead is a second byte describing high-bit of the color palette index.
For reasons surpassing understanding, those bits are read right-to-left instead of
left-to-right. That is, the right-most bit refers to the left-most pixel.

One reason this memory representation seems incredibly silly is that you would
_think_ you'd want to put the two bytes for the same tile _next to each other_.

Representing this in lisp efficiently, we could use bit-vectors but it's probably
easier to come up with the palette-indexes per-pixel by just looping backwards
over both bytes simultaneously and combining the colors as we go.

### Thoughts on Approach (06/02)

Trying to render a scanline at a time is okay for now but trying to clock things
a scanline at a time is a kludge or premature optimization depending on perspective.
Let's get rid of scanline step and just do the standard `cpu.step(); ppu.step() * 3;`.

As for PPU needing access to CPU for DMA and the Mapper for pattern table data,
let's actually add `:oam-dma` to the PPU result for now and see how far that gets us.
I'll probably have to double back on this at some point, whether because I want more accuracy
or because I want to optimize / emulate in bigger increments than `cpu.step()` but until then...

BTW, if we're doing `:oam-dma` and a DMA normally takes 513 cycles or what have you,
we should be careful how we account for the fact that the `STA` instruction or what have
you that triggered the DMA actually finishes and fully increments the cycle count in my build.
On the real hardware, it suspends CPU operation entirely until after DMA. Absolute store
instructions are normally 4 cycles long so maybe we should only stall for 512 cycles or something?
Anyway:

1. Implement DMA. *DONE*
2. Redo syncing / stepping. *DONE*
3. Finish tile rendering (just for backgrounds). *DONE?*
4. Implement controller input / SDL polling.

If we get all that done, it will be a very good Saturday.

Dumb micro-optimization: We could avoid computing `old-cycles` every step by
rotating `new-cycles` / keeping that outside the loop, etc.

### Notes after rendering work (06/02)

Well, it hurts my soul that this doesn't seem to _quite_ work yet.
But I'm proud of the work I got done on scanline rendering.

I'm confident we're computing the various nametable, palette, etc addresses correctly.
But when I render I only get a gray screen. Performance seems acceptable, with most frames
drawing in under 5 seconds. But I'll be more pleased with that when something meaningful
is actually getting on the screen. That said, my brain is pretty fried and I've spent almost
no time with Norma today despite her being home for 7 hours now. It's time to sign off.

I wrote a simple TEST-FRAME function and got the expected color palette / results.
It seems likely that the problem is translating the `*FRAMEBUFFER*` to SDL / `RENDER-PIXEL`.
