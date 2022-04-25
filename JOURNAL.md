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
   using cl-png or similar. Once a reference image is obtained, write a test
   to compare it with the output of the background renderer. There's no need
   to worry about accurate scrolling yet. Just support looping over the
   nametable, looking up the pattern bytes, attribute byte, and quad to
   construct a tile object. With each tile, compute the palette index for the
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
