## Dev Log

### 04/24 - Fighting Graphics Again

[4 weeks ago][4-weeks], I started over on Clones. It has been a productive
first month. The CPU is finished and tested and we have a disassembler and
PPU registers behave mostly correctly. Things have slowed down considerably
since the CPU was finished though and I'd like to talk about why and what
can be done about it.

The primary thing I struggle with once the CPU is finished is decomposing
graphics as a feature into a set of small, testable steps. Creating a record
to represent the PPU and then implementing it's memory interface and register
behavior is a decent enough starting point ... but then what?

To render anything, you have to synchronize the CPU and PPU and think about
how they will communicate with each other, often in the form of [NMI][nmi]
and [DMA][dma]. There are [test roms][test-roms] for this behavior, but they
expect rendering to be at least partially working already in that they output
a numeric value to the "screen". Unfortunately for me, I would prefer to know
the rendering behavior is correct before worrying about pulling SDL2 in as a
dependency and trying to draw to the display.

It would also be helpful to have a mature Lisp [GUI toolkit][gui-toolkits].
I suppose there is CAPI if you have a Lispworks license but I'd prefer something
open source. If I had a GUI toolkit right now, I could at least get distracted
building out a user interface and wiring up a debugger view and peek/poke level
support for the features that are already implemented. QT and GTK bindings exist
but are often as far as a major version behind and the API surfaces are huge.
Something similar to [dear-imgui][imgui] would be ideal for my purposes but
all the imgui, nuklear, and raylib bindings I've seen for Lisp are incomplete.

Maybe when I finish the emulator I'll set aside some time and write Raygui FFI
bindings myself. In the meantime, here's what I think about building a PPU:

1. PPU registers. This has to come first. You have to simulate the state
   before you can worry about the rest.
2. PPU timing. You need some way to synchronize the CPU and PPU, allowing
   the PPU to trigger an NMI when vblank starts. At least scanline-level
   timing needs to be correct. You can't render a frame at this point without
   just implementing the whole PPU in one go, so supply a way to run a callback
   when a frame is completed and write simple timing tests. It could be a good
   time to test behavior of PPUSCROLL syncing to PPUADDR during vblank.
3. Nametable rendering. Backgrounds can work independently of sprites and
   scrolling so they're a logical next step. It can also be tested without
   leaning on SDL2 because you can render to a framebuffer and then dump that
   using zpng or similar. Once a reference image is obtained, write a test
   to compare it with the output of the background renderer. There's no need
   to worry about accurate scrolling yet. Just support looping over the
   nametable, looking up the pattern bytes, attribute byte, and quad to
   construct a tile object. With each tile, compute the palette indexes for the
   current scanline and write the pixels to the right spot in the framebuffer.
4. VRAM quirks. This could be a good time to resolve issues like the PPU
   address not wrapping correctly after VRAM reads, or palette mirroring not
   working correctly either in PPUDATA reads or [universal background][un-bg].
5. Sprite behavior. Now we get into the nitty gritty. Sprite evaluation to
   detect sprites on a scanline is needed and after that you've got pixel
   priority to worry about. At least the work to do is [documented][sprites].
6. Wire it to SDL2. At this point, you should have working rendering that you
   can hook up to an OpenGL context or equivalent. With a bit of on-frame
   callback finagling, you may just be able to play a nice game of Donkey Kong.
7. PPU Scrolling. At long last, it's time to think about [sprite zero hit][szh]
   and [scrolling][scrolling]. You should start with [nametable mirroring][ntm]
   and then you can worry about trying to update scroll data appropriately.
   If all goes well, you should be able to run Super Mario Bros by the end of
   this and can start worrying about fancy things like more advanced mappers
   and audio support. 🙏

It's not an easy road but it's worth it.

[4-weeks]: https://git.sr.ht/~kingcons/clones/commit/b2fff81131b20bb979dde4282ad0fb53ed321d9d
[nmi]: https://www.nesdev.org/wiki/NMI
[dma]: https://www.nesdev.org/wiki/PPU_registers#OAMDMA
[test-roms]: https://www.nesdev.org/wiki/Emulator_tests#PPU_Tests
[gui-toolkits]: https://lispcookbook.github.io/cl-cookbook/gui.html
[imgui]: https://github.com/ocornut/imgui
[un-bg]: https://www.nesdev.org/wiki/PPU_palettes#Memory_Map
[sprites]: https://www.nesdev.org/wiki/PPU_sprite_evaluation
[scrolling]: https://www.nesdev.org/wiki/PPU_scrolling
[szh]: https://www.nesdev.org/wiki/PPU_OAM#Sprite_zero_hits
[ntm]: https://www.nesdev.org/wiki/Mirroring#Nametable_Mirroring

## 05/15 - Working on Background Rendering

Three weeks have flown by. I've probably spent the same number of hours in the
past 3 weeks as I spent in a single week previously. I have a nice 4 day weekend
though so I've been making some progress. Rather than talking about the current
status though, I'd like to talk about things that were more painful than they
could have been in the last 2 weeks.

A few things come to mind. When summarizing the work of the past few weeks,
there were 2 tricky bugs that took up a meaningful amount of hacking time. The
first was that relative branches only worked going forwards, not backwards. It
not only took a while to see what was happening, but also to figure out the
right way to work with an 8-bit value in two's complement form in Common Lisp.
It perhaps adds insult to injury that NEStest didn't cover this.

The second bug was that NMI was throwing values on the stack in the wrong order.
I was pushing the status register then the program counter instead of the other
way around. I'm not bothered by the fact that NEStest doesn't specifically
cover NMI as you won't be triggering NMI until the CPU is finished and the PPU
is started. The Nesdev wiki page could have more clearly called out the behavior
when NMI is triggered.

In both cases, part of the problem is that I assumed these were things that
would be tested already or break _immediately_ if they weren't working. Instead,
they half-worked until disaster ensued. In the future, it would be valuable to
either get Klaus' 6502 tests running and see if that covers relative branching
and NMI better, or manually test those pieces.

Tests would also be nice to have in the renderer though I haven't encountered
many difficult bugs _yet_. Once I finish background rendering, I may put some
tests in before getting too occupied with sprite rendering or other changes.

My development strategy for background rendering as a whole has been to rely on
Xach's zpng and dump the framebuffer to a PNG file for inspection. This is fine
as an integration level test, but we could definitely do better where unit tests
are concerned. An initial improvement would be unit tests that after nametables
are initialized appropriately, the correct attribute and pattern bytes are
fetched. Writing tests for the coarse-x (tile) and fine-y (scanline) scrolling
helpers would also have caught a small bug with updating a local variable rather
than the PPU address.

All of that is dependent on first having a working rendering loop / sync method.
There are tests around NMI timing but they could definitely be strengthened and
there are details I ommitted. PPUSCROLL -> PPUADDR at end of frame, for example.
Or that the on-frame handler is called at the proper time. Overall, things are
going pretty well. I would love to think through how to test the individual
pieces of RENDER-TILE with greater confidence though. Iterating in smaller
chunks with higher confidence is always good and while I'm pretty confident in
the fetching code, the remainder of the palette computation feels more dubious.

## 06/23 - Ready for Sprites

Well, it's been five weeks. There was a lull in Clones activity due to real life
but I've been back with a vengeance the last week or two. Before the lull, I got
background rendering working and despite [some][bug1] [tough][bug2] [bugs][bug3]
it was a mostly smooth process. The toughest bugs came from writing code to
fetch graphics data based on correct descriptions in NESdev but before I had
the supporting mirroring or scrolling code written. If I do this again, I either
need to get mirroring and scrolling working first or remember to fetch naively.
The sorts of visual artifacts you can get from incorrect graphics code are wild
and, at least for me, it took a fair amount of digging before I realized where I
had gone wrong.

[bug1]: https://git.sr.ht/~kingcons/clones/commit/687603b8e3a15a7a21cf4ff99f624c46ced6eec9
[bug2]: https://git.sr.ht/~kingcons/clones/commit/89b9e90a64476146f6c6a823e2279cd6c488c061
[bug3]: https://git.sr.ht/~kingcons/clones/commit/5611b0b5787773a6d9d7d752756c38a9e2f553f8

Once the kinks were ironed out, testing backgrounds with `zpng` and `diff`
worked well. There is some awkwardness because to have a reasonable test case
you need some useful nametable and pattern table data to work from. I dumped
JSON of the Donkey Kong title screen for that. Then life happened for a while.

After not thinking about graphics, sprite rendering seemed unpleasant. Since I
wasn't feeling that I started on an SDL app instead. At first, I just worried
about an input loop and some commands for disassembly and stepping the CPU. The
satisfaction from getting that working propelled me forward to wire up a proper
on-frame callback to hand the framebuffer off to SDL. Before you knew it, we had
graphics. Since then, I've added NES input handling and a nice refactoring so
the CPU instructions use EQL specialized methods.

I'm feeling good about things and hopeful that this burst of energy will carry
me through muscling through the thorny bits of sprite rendering this weekend. If
I can get that done and make some tweaks for fine scrolling, I'll be well on my
way to playing Mega Man 2 using my own code. 😎
