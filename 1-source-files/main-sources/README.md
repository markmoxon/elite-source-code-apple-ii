# Annotated source code for the Apple II version of Elite

This folder contains the annotated source code for the Apple II version of Elite.

* Main source files:

  * [elite-source.asm](elite-source.asm) contains the main source for the game

  * [elite-data.asm](elite-data.asm) contains source for the game data, including the game text

  * [elite-bcfs.asm](elite-bcfs.asm) contains the Big Code File source, which concatenates individually assembled binaries into the final game binary

* Other source files:

  * [elite-mover.asm](elite-mover.asm) contains the source for the mover program that moves code in memory once loaded

  * [elite-checksum.asm](elite-checksum.asm) contains 6502 source code for the checksum routines that are implemented in the elite-checksum.py script (and which were implemented by the S.CODES and S.DATAS BBC BASIC programs in the original source discs); this file is purely for reference and is not used in the build process

  * [elite-transfer.asm](elite-transfer.asm) generates files that are suitable for transmitting to an Apple II connected to the BBC Micro doing the build; this file is purely for reference and is not used in the build process

  * [elite-readme.asm](elite-readme.asm) generates a README file for inclusion on the DSK disk image

* Files that are generated during the build process:

  * [elite-build-options.asm](elite-build-options.asm) stores the make options in BeebAsm format so they can be included in the assembly process

---

Right on, Commanders!

_Mark Moxon_