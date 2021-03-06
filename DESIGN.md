## Clones

Clones is intended to be a Common Lisp Observable Nintendo Entertainment System.

### Motivation

For a long time, I've been captivated by simpler computers. [1] [2]

In particular, 80s microcomputers did very little out of the box. They would often
just boot you to a simple prompt. Even considering the available free and commercial
software, their capabilities were staggeringly limited compared to anything modern.

But the _distance_ between _access_ and _discovery_ seems much shorter to me.
Modern computers are marvels but they sit upon a bedrock of knowledge and
existing software so large, it impedes the ability to master it. Even deep
comprehension is attained by very few.

I want to write a new environment to simulate an old computer. An old computer
that had some cool applications that will be fun to use. A new environment with
built-in tools for _observation_ and _exploration_. A toolkit for interacting,
for rooting around in search of buried treasure.

I'm way out of my depth here, technically speaking. But maybe that's the point.
In the worst case, I'll learn something for myself. In the best case, maybe I'll
be able to help others learn something too.

[1]: http://redlinernotes.com/docs/talks/trp.html#1
[2]: http://blog.kingcons.io/posts/Towards-Comprehensible-Computing.html

### High Level Goals

I was asked about the project goals again today. I suppose it boils down to three things:

1. Seeing how close I can get the emulator in speed to C without it looking _too much_ like C.
2. Serving as good documentation and introduction to the _high level_ behavior of the NES.
3. Supporting tools to debug or reverse engineer games _interactively_.

The third point is definitely the murkiest and least well defined for me.
To expand, here are some notes which are part project planning, part philosophical:

1. Working emulation of the Nintendo Entertainment System.
  * "Working" means able to play many if not all games with "good enough" speed and accuracy.
  * Anti-goals: Support for all mappers/games, glitch accuracy, 60 FPS/no latency blips, etc.
  * Bronze: Test roms / CPU interpreter, Silver: Donkey Kong / PPU, Gold: Mega Man 2 / CPU JIT?
2. "Readable" lisp codebase, modern software practices.
  * Don't write Lisp like it is C. Use CI, test roms, and unit tests to ensure components work.
  * Readability is nebulous and for the moment adheres to my personal taste.
    * However, ideally the code and docs could serve as an introduction to low-level programming
      issues for someone with light lisp exposure and serious experience with a previous language.
    * Open question: How necessary is prior exposure to graphics or audio concepts?
  * Anti-goals: Complete code coverage, all test roms pass, etc.
  * Bronze: CPU + basic PPU tests pass, Silver: MGL-PAX or cl-6502-style literate program, Gold: ??
3. Tools for exploration, reverse engineering, static recompilation, etc.
  * Disclaimer: I am not versed in this space and don't know where the boundaries and pitfalls are.
    We could easily stumble into intractable problems or miss existing solutions.
    * Assuming steps 1 & 2 are taken care of, trying to get feedback from a wider community is key.
  * Ideas: Visualizing hardware state, Visualizing software control flow and data flow
  * Anti-goals: Tools that work without any human guidance.
  * Bronze: Disassembler / Debugger, Silver: PPU visualizer, Gold: Navigable CFG / recompiler
4. Tools for sharing
  * A companion webservice might be ideal, though clearly a separate project.
  * Formats for storing and disseminating partially deconstructed / annotated binaries.
  * Ways to export visualizations or recompiled code?
