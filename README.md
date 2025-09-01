# Fully documented source code for Elite on the Apple II

[BBC Micro cassette Elite](https://github.com/markmoxon/elite-source-code-bbc-micro-cassette) | [BBC Micro disc Elite](https://github.com/markmoxon/elite-source-code-bbc-micro-disc) | [Acorn Electron Elite](https://github.com/markmoxon/elite-source-code-acorn-electron) | [6502 Second Processor Elite](https://github.com/markmoxon/elite-source-code-6502-second-processor) | [Commodore 64 Elite](https://github.com/markmoxon/elite-source-code-commodore-64) | **Apple II Elite** | [BBC Master Elite](https://github.com/markmoxon/elite-source-code-bbc-master) | [NES Elite](https://github.com/markmoxon/elite-source-code-nes) | [Elite-A](https://github.com/markmoxon/elite-a-source-code-bbc-micro) | [Teletext Elite](https://github.com/markmoxon/teletext-elite) | [Elite Universe Editor](https://github.com/markmoxon/elite-universe-editor) | [Elite Compendium (BBC Master)](https://github.com/markmoxon/elite-compendium-bbc-master) | [Elite Compendium (BBC Micro)](https://github.com/markmoxon/elite-compendium-bbc-micro) | [Elite Compendium (BBC Micro B+)](https://github.com/markmoxon/elite-compendium-bbc-micro-b-plus) | [Elite Compendium (Acorn Electron)](https://github.com/markmoxon/elite-compendium-acorn-electron) | [Elite over Econet](https://github.com/markmoxon/elite-over-econet) | [!EliteNet](https://github.com/markmoxon/elite-over-econet-acorn-archimedes) | [Flicker-free Commodore 64 Elite](https://github.com/markmoxon/c64-elite-flicker-free) | [BBC Micro Aviator](https://github.com/markmoxon/aviator-source-code-bbc-micro) | [BBC Micro Revs](https://github.com/markmoxon/revs-source-code-bbc-micro) | [Archimedes Lander](https://github.com/markmoxon/lander-source-code-acorn-archimedes)

![Screenshot of Elite on the Apple II](https://elite.bbcelite.com/images/github/Elite-AppleII.png)

This repository contains the original source code for Elite on the Apple II, with every single line documented and (for the most part) explained. It is literally the original source code, just heavily commented.

It is a companion to the [elite.bbcelite.com website](https://elite.bbcelite.com).

See the [introduction](#introduction) for more information, or jump straight into the [documented source code](1-source-files/main-sources).

## Contents

* [Introduction](#introduction)

* [Acknowledgements](#acknowledgements)

  * [A note on licences, copyright etc.](#user-content-a-note-on-licences-copyright-etc)

* [Browsing the source in an IDE](#browsing-the-source-in-an-ide)

* [Folder structure](#folder-structure)

* [Flicker-free Elite](#flicker-free-elite)

* [Building Apple II Elite from the source](#building-apple-ii-elite-from-the-source)

  * [Requirements](#requirements)
  * [Windows](#windows)
  * [Mac and Linux](#mac-and-linux)
  * [Build options](#build-options)
  * [Updating the checksum scripts if you change the code](#updating-the-checksum-scripts-if-you-change-the-code)
  * [Verifying the output](#verifying-the-output)
  * [Log files](#log-files)

* [Building different variants of Apple II Elite](#building-different-variants-of-apple-ii-elite)

  * [Building the 4am crack variant](#building-the-4am-crack-variant)
  * [Building the Ian Bell game disk variant](#building-the-ian-bell-game-disk-variant)
  * [Building the source disk build variant](#building-the-source-disk-build-variant)
  * [Building the source disk CODE files variant](#building-the-source-disk-code-files-variant)
  * [Building the source disk ELT files variant](#building-the-source-disk-elt-files-variant)
  * [Differences between the variants](#differences-between-the-variants)

* [Notes on the original source files](#notes-on-the-original-source-files)

  * [Producing byte-accurate binaries](#producing-byte-accurate-binaries)

## Introduction

This repository contains the original source code for Elite on the Apple II, with every single line documented and (for the most part) explained.

You can build the fully functioning game from this source. [Five variants](#building-different-variants-of-apple-ii-elite) are currently supported: the 4am crack disk, the game disk from Ian Bell's personal website, the variant built by the source disk build process, the variant built from the CODE* binary files already on the source disk, and the variant built from the ELT* binary files already on the source disk.

This repository is a companion to the [elite.bbcelite.com website](https://elite.bbcelite.com), which contains all the code from this repository, but laid out in a much more human-friendly fashion. The links at the top of this page will take you to repositories for the other versions of Elite that are covered by this project.

* If you want to browse the source and read about how Elite works under the hood, you will probably find [the website](https://elite.bbcelite.com) a better place to start than this repository.

* If you would rather explore the source code in your favourite IDE, then the [annotated source](1-source-files/main-sources) is what you're looking for. It contains the exact same content as the website, so you won't be missing out (the website is generated from the source files, so they are guaranteed to be identical). You might also like to read the section on [browsing the source in an IDE](#browsing-the-source-in-an-ide) for some tips.

* If you want to build Apple II Elite from the source on a modern computer, to produce a working game disk that can be loaded into an Apple II or an emulator, then you want the section on [building Apple II Elite from the source](#building-apple-ii-elite-from-the-source).

My hope is that this repository and the [accompanying website](https://elite.bbcelite.com) will be useful for those who want to learn more about Elite and what makes it tick. It is provided on an educational and non-profit basis, with the aim of helping people appreciate one of the most iconic games of the 8-bit era.

## Acknowledgements

Apple II Elite was written by Ian Bell and David Braben and is copyright &copy; D. Braben and I. Bell 1986.

The code on this site is identical to the source disks released on [Ian Bell's personal website](http://www.elitehomepage.org/) (it's just been reformatted to be more readable).

The commentary is copyright &copy; Mark Moxon. Any misunderstandings or mistakes in the documentation are entirely my fault.

Huge thanks are due to the original authors for not only creating such an important piece of my childhood, but also for releasing the source code for us to play with. Also, a big thumbs up to Andy McFadden for his excellent [disassembly of Elite for the Apple II](https://6502disassembly.com/a2-elite/), which is a really useful reference for anyone exploring the Apple II binaries.

The following archives from Ian Bell's personal website form the basis for this project:

* [Apple II Elite sources as a BBC Micro disc image](http://www.elitehomepage.org/archive/a/a6010080.zip)
* [Apple II Elite game disk](http://www.elitehomepage.org/archive/a/a6010010.zip)

### A note on licences, copyright etc.

This repository is _not_ provided with a licence, and there is intentionally no `LICENSE` file provided.

According to [GitHub's licensing documentation](https://docs.github.com/en/free-pro-team@latest/github/creating-cloning-and-archiving-repositories/licensing-a-repository), this means that "the default copyright laws apply, meaning that you retain all rights to your source code and no one may reproduce, distribute, or create derivative works from your work".

The reason for this is that my commentary is intertwined with the original Elite source code, and the original source code is copyright. The whole site is therefore covered by default copyright law, to ensure that this copyright is respected.

Under GitHub's rules, you have the right to read and fork this repository... but that's it. No other use is permitted, I'm afraid.

My hope is that the educational and non-profit intentions of this repository will enable it to stay hosted and available, but the original copyright holders do have the right to ask for it to be taken down, in which case I will comply without hesitation. I do hope, though, that along with the various other disassemblies and commentaries of this source, it will remain viable.

## Browsing the source in an IDE

If you want to browse the source in an IDE, you might find the following useful.

* The most interesting files are in the [main-sources](1-source-files/main-sources) folder:

  * The main game's source code is in the [elite-source.asm](1-source-files/main-sources/elite-source.asm) and [elite-data.asm](1-source-files/main-sources/elite-data.asm) files (containing the game code and game data respectively) - this is the motherlode and probably contains all the stuff you're interested in.

* It's probably worth skimming through the [notes on terminology and notations](https://elite.bbcelite.com/terminology/) on the accompanying website, as this explains a number of terms used in the commentary, without which it might be a bit tricky to follow at times (in particular, you should understand the terminology I use for multi-byte numbers).

* The accompanying website contains [a number of "deep dive" articles](https://elite.bbcelite.com/deep_dives/), each of which goes into an aspect of the game in detail. Routines that are explained further in these articles are tagged with the label `Deep dive:` and the relevant article name.

* There are loads of routines and variables in Elite - literally hundreds. You can find them in the source files by searching for the following: `Type: Subroutine`, `Type: Variable`, `Type: Workspace` and `Type: Macro`.

* If you know the name of a routine, you can find it by searching for `Name: <name>`, as in `Name: SCAN` (for the 3D scanner routine) or `Name: LL9` (for the ship-drawing routine).

* The entry point for the [main game code](1-source-files/main-sources/elite-source.asm) is routine `BEGIN`, which you can find by searching for `Name: BEGIN`. If you want to follow the program flow all the way from the title screen around the main game loop, then you can find a number of [deep dives on program flow](https://elite.bbcelite.com/deep_dives/) on the accompanying website.

* The source code is designed to be read at an 80-column width and with a monospaced font, just like in the good old days.

I hope you enjoy exploring the inner workings of Commmodore 64 Elite as much as I have.

## Folder structure

There are five main folders in this repository, which reflect the order of the build process.

* [1-source-files](1-source-files) contains all the different source files, such as the main assembler source files, image binaries, fonts, boot files and so on.

* [2-build-files](2-build-files) contains build-related scripts, such as the checksum, encryption and crc32 verification scripts.

* [3-assembled-output](3-assembled-output) contains the output from the assembly process, when the source files are assembled and the results processed by the build files.

* [4-reference-binaries](4-reference-binaries) contains the correct binaries for each variant, so we can verify that our assembled output matches the reference.

* [5-compiled-game-disks](5-compiled-game-disks) contains the final output of the build process: a DSK disk image that contains the compiled game and which can be run on real hardware or in an emulator.

## Flicker-free Elite

This repository also includes a flicker-free version, which incorporates a fix for planets so they no longer flicker. The flicker-free code is in a separate branch called `flicker-free`, and apart from the code differences for reducing flicker, this branch is identical to the main branch and the same build process applies.

The annotated source files in the `flicker-free` branch contain both the original Firebird code and all of the modifications for flicker-free Elite, so you can look through the source to see exactly what's changed. Any code that I've removed from the original version is commented out in the source files, so when they are assembled they produce the flicker-free binaries, while still containing details of all the modifications. You can find all the diffs by searching the sources for `Mod:`.

For more information on flicker-free Elite, see the [hacks section of the accompanying website](https://elite.bbcelite.com/hacks/flicker-free_elite.html).

## Building Apple II Elite from the source

Builds are supported for both Windows and Mac/Linux systems. In all cases the build process is defined in the `Makefile` provided.

### Requirements

You will need the following to build Apple II Elite from the source:

* BeebAsm, which can be downloaded from the [BeebAsm repository](https://github.com/stardot/beebasm). Mac and Linux users will have to build their own executable with `make code`, while Windows users can just download the `beebasm.exe` file.

* Python. The build process has only been tested on 3.x, but 2.7 might work.

* diskm8, which can be downloaded from the [diskm8 repository](https://github.com/paleotronic/diskm8).

* Mac and Linux users may need to install `make` if it isn't already present (for Windows users, `make.exe` is included in this repository).

You may be wondering why we're using BeebAsm - a BBC Micro assembler - to build the Apple II version of Elite. This is because Apple II Elite is a conversion of 6502 Second Processor Elite, which itself is a direct descendant of the original 1984 release for the BBC Micro and Acorn Electron (and the same is true of the Commodore 64 and NES versions of Elite - they are all cut from the same cloth). The NES version aside, all of the 6502 versions of Elite were built and assembled on a BBC Micro, including the Commodore and Apple versions, so BeebAsm is a good modern assembler to use for the Apple II version as well.

For details of how the build process works, see the [build documentation on bbcelite.com](https://elite.bbcelite.com/about_site/building_elite.html).

Let's look at how to build Apple II Elite from the source.

### Windows

For Windows users, there is a batch file called `make.bat` which you can use to build the game. Before this will work, you should edit the batch file and change the values of the `BEEBASM`, `PYTHON` and `DISKM8` variables to point to the locations of your `beebasm.exe`, `python.exe` and `diskm8.exe` executables. You also need to change directory to the repository folder (i.e. the same folder as `make.bat`).

All being well, entering the following into a command window:

```
make.bat
```

will produce a file called `elite-apple-4am-crack.dsk` in the `5-compiled-game-disks` folder that contains the 4am crack variant, which you can then load into an emulator, or into a real Apple II.

### Mac and Linux

The build process uses a standard GNU `Makefile`, so you just need to install `make` if your system doesn't already have it. If BeebAsm, Python or diskm8 are not on your path, then you can either fix this, or you can edit the `Makefile` and change the `BEEBASM`, `PYTHON` and `DISKM8` variables in the first three lines to point to their locations. You also need to change directory to the repository folder (i.e. the same folder as `Makefile`).

All being well, entering the following into a terminal window:

```
make
```

will produce a file called `elite-apple-4am-crack.dsk` in the `5-compiled-game-disks` folder that contains the 4am crack variant, which you can then load into an emulator, or into a real Apple II.

### Build options

By default the build process will create a typical Elite game disk with a standard commander and verified binaries. There are various arguments you can pass to the build to change how it works. They are:

* `variant=<name>` - Build the specified variant:

  * `variant=4am-crack` (default)
  * `variant=ib-disk`
  * `variant=source-disk-build`
  * `variant=source-disk-code-files`
  * `variant=source-disk-elt-files`

* `commander=max` - Start with a maxed-out commander (specifically, this is the test commander file from the original source, which is almost but not quite maxed-out)

* `encrypt=no` - Disable encryption and checksum routines

* `match=no` - Do not attempt to match the original game binaries (i.e. omit workspace noise)

* `verify=no` - Disable crc32 verification of the game binaries

So, for example:

`make variant=source-disk-build commander=max encrypt=no match=no verify=no`

will build an unencrypted source disk variant with a maxed-out commander, no workspace noise and no crc32 verification.

The unencrypted version should be more useful for anyone who wants to make modifications to the game code. As this argument produces unencrypted files, the binaries produced will be quite different to the binaries on the original source disk, which are encrypted.

See below for more on the verification process.

### Updating the checksum scripts if you change the code

If you change the source code in any way, you may break the game; if so, it will typically hang at the loading screen, though in some versions it may hang when launching from the space station.

To fix this, you may need to update some of the hard-coded addresses in the checksum script so that they match the new addresses in your changed version of the code. See the comments in the [elite-checksum.py](2-build-files/elite-checksum.py) script for details.

### Verifying the output

The default build process prints out checksums of all the generated files, along with the checksums of the files from the original sources. You can disable verification by passing `verify=no` to the build.

The Python script `crc32.py` in the `2-build-files` folder does the actual verification, and shows the checksums and file sizes of both sets of files, alongside each other, and with a Match column that flags any discrepancies. If you are building an unencrypted set of files then there will be lots of differences, while the encrypted files should mostly match (see the Differences section below for more on this).

The binaries in the `4-reference-binaries` folder are those extracted from the released version of the game, while those in the `3-assembled-output` folder are produced by the build process. For example, if you don't make any changes to the code and build the project with `make`, then this is the output of the verification process:

```
Results for variant: 4am-crack
[--originals--]  [---output----]
Checksum   Size  Checksum   Size  Match  Filename
-----------------------------------------------------------
796a263b  32768  796a263b  32768   Yes   CODE.unprot.bin
62271826  20480  62271826  20480   Yes   CODE1.bin
a6f4ed53  20480  a6f4ed53  20480   Yes   CODE1.unprot.bin
f7b5430f  12288  f7b5430f  12288   Yes   CODE2.bin
35bc6507  12288  35bc6507  12288   Yes   CODE2.unprot.bin
a831e7e2   5280  a831e7e2   5280   Yes   DATA.bin
e66e6208   5280  e66e6208   5280   Yes   DATA.unprot.bin
4520e38d  26400  4520e38d  26400   Yes   ELA.bin
62271826  20480  62271826  20480   Yes   ELB.bin
6adb264a   3710  6adb264a   3710   Yes   ELTA.bin
711df275   2363  711df275   2363   Yes   ELTB.bin
c4da2495   3082  c4da2495   3082   Yes   ELTC.bin
dc6bf353   3308  dc6bf353   3308   Yes   ELTD.bin
88396ce4   2243  88396ce4   2243   Yes   ELTE.bin
5ebd3212   3477  5ebd3212   3477   Yes   ELTF.bin
fef23147   2391  fef23147   2391   Yes   ELTG.bin
cb094bf3   1203  cb094bf3   1203   Yes   ELTH.bin
0196f101    306  0196f101    306   Yes   ELTI.bin
978e5515   1437  978e5515   1437   Yes   ELTJ.bin
3d92a08d   1824  3d92a08d   1824   Yes   ELTK.bin
d61caafe   3513  d61caafe   3513   Yes   IANTOK.bin
de4a0b89   1274  de4a0b89   1274   Yes   SEC3.bin
6a113fb1   7314  6a113fb1   7314   Yes   SHIPS.bin
f77b2a55    992  f77b2a55    992   Yes   WORDS.bin
-             -  c22cea95     41    -    MOVER.bin
```

All the compiled binaries match the originals, so we know we are producing the same final game as the 4am crack variant.

### Log files

During compilation, details of every step are output in a file called `compile.txt` in the `3-assembled-output` folder. If you have problems, it might come in handy, and it's a great reference if you need to know the addresses of labels and variables for debugging (or just snooping around).

## Building different variants of Apple II Elite

This repository contains the source code for four different variants of Apple II Elite:

* The 4am crack, which is the most authentic version of the original Firebird game (only the disk protection has been removed, and I have also removed the CTRL-E characters from the filenames on the original disk to make the the modern build process simpler)

* The game disk from Ian Bell's personal website, which contains a cracked version of the original Firebird game by The Student

* The variant produced by running the build process on the source disk from Ian Bell's personal website

* The variant produced using the CODE* binaries from the source disk on Ian Bell's personal website, which differ slightly from the binaries that are produced by running the build process

* The variant produced using the ELT* binaries from the source disk on Ian Bell's personal website, which differ slightly from the binaries that are produced by running the build process

By default the build process builds the FirebIan Bell game disk variant, but you can build a specified variant using the `variant=` build parameter.

### Building the 4am crack variant

You can add `variant=4am-crack` to produce the `elite-apple-4am-crack.dsk` file that contains the 4am crack variant, though that's the default value so it isn't necessary. In other words, you can build it like this:

```
make.bat variant=4am-crack
```

or this on a Mac or Linux:

```
make variant=4am-crack
```

This will produce a file called `elite-apple-4am-crack.dsk` in the `5-compiled-game-disks` folder that contains the 4am crack variant.

The verification checksums for this version are as follows:

```
Results for variant: 4am-crack
[--originals--]  [---output----]
Checksum   Size  Checksum   Size  Match  Filename
-----------------------------------------------------------
796a263b  32768  796a263b  32768   Yes   CODE.unprot.bin
62271826  20480  62271826  20480   Yes   CODE1.bin
a6f4ed53  20480  a6f4ed53  20480   Yes   CODE1.unprot.bin
f7b5430f  12288  f7b5430f  12288   Yes   CODE2.bin
35bc6507  12288  35bc6507  12288   Yes   CODE2.unprot.bin
a831e7e2   5280  a831e7e2   5280   Yes   DATA.bin
e66e6208   5280  e66e6208   5280   Yes   DATA.unprot.bin
4520e38d  26400  4520e38d  26400   Yes   ELA.bin
62271826  20480  62271826  20480   Yes   ELB.bin
6adb264a   3710  6adb264a   3710   Yes   ELTA.bin
711df275   2363  711df275   2363   Yes   ELTB.bin
c4da2495   3082  c4da2495   3082   Yes   ELTC.bin
dc6bf353   3308  dc6bf353   3308   Yes   ELTD.bin
88396ce4   2243  88396ce4   2243   Yes   ELTE.bin
5ebd3212   3477  5ebd3212   3477   Yes   ELTF.bin
fef23147   2391  fef23147   2391   Yes   ELTG.bin
cb094bf3   1203  cb094bf3   1203   Yes   ELTH.bin
0196f101    306  0196f101    306   Yes   ELTI.bin
978e5515   1437  978e5515   1437   Yes   ELTJ.bin
3d92a08d   1824  3d92a08d   1824   Yes   ELTK.bin
d61caafe   3513  d61caafe   3513   Yes   IANTOK.bin
c22cea95     41  c22cea95     41   Yes   MOVER.bin
de4a0b89   1274  de4a0b89   1274   Yes   SEC3.bin
6a113fb1   7314  6a113fb1   7314   Yes   SHIPS.bin
f77b2a55    992  f77b2a55    992   Yes   WORDS.bin
```

### Building the Ian Bell game disk variant

You can build the Ian Bell game disk variant by appending `variant=ib-disk` to the `make` command, like this on Windows:

```
make.bat variant=ib-disk
```

or this on a Mac or Linux:

```
make variant=ib-disk
```

This will produce a file called `elite-apple-ib-disk.dsk` in the `5-compiled-game-disks` folder that contains the Ian Bell game disk variant.

The verification checksums for this version are as follows:

```
Results for variant: ib-disk
[--originals--]  [---output----]
Checksum   Size  Checksum   Size  Match  Filename
-----------------------------------------------------------
dcd37bb9  32768  dcd37bb9  32768   Yes   CODE.unprot.bin
310a5ea5  20480  310a5ea5  20480   Yes   CODE1.bin
310a5ea5  20480  310a5ea5  20480   Yes   CODE1.unprot.bin
35bc6507  12288  35bc6507  12288   Yes   CODE2.bin
35bc6507  12288  35bc6507  12288   Yes   CODE2.unprot.bin
18be5848   5376  18be5848   5376   Yes   DATA.bin
18be5848   5376  18be5848   5376   Yes   DATA.unprot.bin
c41f1c49  26112  c41f1c49  26112   Yes   ELA.bin
310a5ea5  20480  310a5ea5  20480   Yes   ELB.bin
1bbcc440   3710  1bbcc440   3710   Yes   ELTA.bin
5125fc56   2363  5125fc56   2363   Yes   ELTB.bin
c4da2495   3082  c4da2495   3082   Yes   ELTC.bin
dc6bf353   3308  dc6bf353   3308   Yes   ELTD.bin
88396ce4   2243  88396ce4   2243   Yes   ELTE.bin
4c6071c1   3477  4c6071c1   3477   Yes   ELTF.bin
fef23147   2391  fef23147   2391   Yes   ELTG.bin
cb094bf3   1203  cb094bf3   1203   Yes   ELTH.bin
0196f101    306  0196f101    306   Yes   ELTI.bin
978e5515   1437  978e5515   1437   Yes   ELTJ.bin
3d92a08d   1824  3d92a08d   1824   Yes   ELTK.bin
d61caafe   3513  d61caafe   3513   Yes   IANTOK.bin
c22cea95     41  c22cea95     41   Yes   MOVER.bin
6a113fb1   7314  6a113fb1   7314   Yes   SHIPS.bin
f77b2a55    992  f77b2a55    992   Yes   WORDS.bin
-             -  de4a0b89   1274    -    SEC3.bin
```

### Building the source disk build variant

You can build the source disk build variant by appending `variant=source-disk-build` to the `make` command, like this on Windows:

```
make.bat variant=source-disk-build
```

or this on a Mac or Linux:

```
make variant=source-disk-build
```

This does not currently produce a game disk, but instead produces the same binaries as the original build process. In the original development system, these files would then be transmitted from the BBC Micro to the Apple II via a serial link, but that setup isn't emulated here.

The verification checksums for this version are as follows:

```
Results for variant: source-disk-build
[--originals--]  [---output----]
Checksum   Size  Checksum   Size  Match  Filename
-----------------------------------------------------------
b997aacf  32768  b997aacf  32768   Yes   CODE.unprot.bin
3b881cde  20480  3b881cde  20480   Yes   CODE1.bin
fa257f9a  20480  fa257f9a  20480   Yes   CODE1.unprot.bin
5cc4867f  12288  5cc4867f  12288   Yes   CODE2.bin
05dbc169  12288  05dbc169  12288   Yes   CODE2.unprot.bin
2aa536de   5280  2aa536de   5280   Yes   DATA.bin
fca02798   5280  fca02798   5280   Yes   DATA.unprot.bin
b4a81200  26112  b4a81200  26112   Yes   ELA.bin
3b881cde  20480  3b881cde  20480   Yes   ELB.bin
9d14b02b   3705  9d14b02b   3705   Yes   ELTA.bin
2a02c544   2364  2a02c544   2364   Yes   ELTB.bin
99d1fd8c   3083  99d1fd8c   3083   Yes   ELTC.bin
81b3f0d7   3318  81b3f0d7   3318   Yes   ELTD.bin
0354ad31   2222  0354ad31   2222   Yes   ELTE.bin
a23ed383   3474  a23ed383   3474   Yes   ELTF.bin
d024488b   2391  d024488b   2391   Yes   ELTG.bin
d251e768   1203  d251e768   1203   Yes   ELTH.bin
e9b436ce    306  e9b436ce    306   Yes   ELTI.bin
1375d27d   1437  1375d27d   1437   Yes   ELTJ.bin
e878870b   1805  e878870b   1805   Yes   ELTK.bin
d61caafe   3513  d61caafe   3513   Yes   IANTOK.bin
6a113fb1   7314  6a113fb1   7314   Yes   SHIPS.bin
998bb35a    992  998bb35a    992   Yes   WORDS.bin
-             -  da873557     41    -    MOVER.bin
-             -  de4a0b89   1274    -    SEC3.bin
```

### Building the source disk CODE files variant

You can build the source disk code files variant by appending `variant=source-disk-code-files` to the `make` command, like this on Windows:

```
make.bat variant=source-disk-code-files
```

or this on a Mac or Linux:

```
make variant=source-disk-code-files
```

This does not currently produce a game disk, but instead produces the CODE* binary files that can be found on the source disk. In the original development system, these files would then be transmitted from the BBC Micro to the Apple II via a serial link, but that setup isn't emulated here.

The verification checksums for this version are as follows:

```
Results for variant: source-disk-code-files
[--originals--]  [---output----]
Checksum   Size  Checksum   Size  Match  Filename
-----------------------------------------------------------
a127520f  32768  a127520f  32768   Yes   CODE.unprot.bin
1f041708  20480  1f041708  20480   Yes   CODE1.bin
6ad3ef26  20480  6ad3ef26  20480   Yes   CODE1.unprot.bin
f64fb5c3  12288  f64fb5c3  12288   Yes   CODE2.bin
582607aa  12288  582607aa  12288   Yes   CODE2.unprot.bin
a831e7e2   5280  a831e7e2   5280   Yes   DATA.bin
e66e6208   5280  e66e6208   5280   Yes   DATA.unprot.bin
6f2ca4e7  26112  6f2ca4e7  26112   Yes   ELA.bin
1f041708  20480  1f041708  20480   Yes   ELB.bin
3f274135   3709  3f274135   3709   Yes   ELTA.bin
31d0c63b   2364  31d0c63b   2364   Yes   ELTB.bin
93630390   3083  93630390   3083   Yes   ELTC.bin
7d92f313   3318  7d92f313   3318   Yes   ELTD.bin
daa83532   2252  daa83532   2252   Yes   ELTE.bin
e867f67f   3471  e867f67f   3471   Yes   ELTF.bin
a0d360a1   2391  a0d360a1   2391   Yes   ELTG.bin
edf8f5cf   1203  edf8f5cf   1203   Yes   ELTH.bin
b48b8c0a    306  b48b8c0a    306   Yes   ELTI.bin
ba7ba5ea   1437  ba7ba5ea   1437   Yes   ELTJ.bin
f7446a16   1809  f7446a16   1809   Yes   ELTK.bin
d61caafe   3513  d61caafe   3513   Yes   IANTOK.bin
6a113fb1   7314  6a113fb1   7314   Yes   SHIPS.bin
f77b2a55    992  f77b2a55    992   Yes   WORDS.bin
-             -  da873557     41    -    MOVER.bin
-             -  de4a0b89   1274    -    SEC3.bin
```

### Building the source disk ELT files variant

You can build the source disk code files variant by appending `variant=source-disk-elt-files` to the `make` command, like this on Windows:

```
make.bat variant=source-disk-elt-files
```

or this on a Mac or Linux:

```
make variant=source-disk-elt-files
```

This does not currently produce a game disk, but instead produces the ELT* binary files that can be found on the source disk. In the original development system, these files would then be transmitted from the BBC Micro to the Apple II via a serial link, but that setup isn't emulated here.

The verification checksums for this version are as follows:

```
Results for variant: source-disk-elt-files
[--originals--]  [---output----]
Checksum   Size  Checksum   Size  Match  Filename
-----------------------------------------------------------
b8b7a1cc  32768  b8b7a1cc  32768   Yes   CODE.unprot.bin
3b881cde  20480  3b881cde  20480   Yes   CODE1.bin
fa257f9a  20480  fa257f9a  20480   Yes   CODE1.unprot.bin
2524d466  12288  2524d466  12288   Yes   CODE2.bin
04fbca6a  12288  04fbca6a  12288   Yes   CODE2.unprot.bin
a831e7e2   5280  a831e7e2   5280   Yes   DATA.bin
e66e6208   5280  e66e6208   5280   Yes   DATA.unprot.bin
bc47c542  26112  bc47c542  26112   Yes   ELA.bin
3b881cde  20480  3b881cde  20480   Yes   ELB.bin
9d14b02b   3705  9d14b02b   3705   Yes   ELTA.bin
2a02c544   2364  2a02c544   2364   Yes   ELTB.bin
99d1fd8c   3083  99d1fd8c   3083   Yes   ELTC.bin
81b3f0d7   3318  81b3f0d7   3318   Yes   ELTD.bin
0354ad31   2222  0354ad31   2222   Yes   ELTE.bin
a23ed383   3474  a23ed383   3474   Yes   ELTF.bin
d024488b   2391  d024488b   2391   Yes   ELTG.bin
d251e768   1203  d251e768   1203   Yes   ELTH.bin
e9b436ce    306  e9b436ce    306   Yes   ELTI.bin
1375d27d   1437  1375d27d   1437   Yes   ELTJ.bin
e878870b   1805  e878870b   1805   Yes   ELTK.bin
d61caafe   3513  d61caafe   3513   Yes   IANTOK.bin
6a113fb1   7314  6a113fb1   7314   Yes   SHIPS.bin
f77b2a55    992  f77b2a55    992   Yes   WORDS.bin
-             -  da873557     41    -    MOVER.bin
-             -  de4a0b89   1274    -    SEC3.bin
```

### Differences between the variants

You can see the differences between the variants by searching the source code for `_IB_DISK` (for features in the Ian Bell game disk variant), `_SOURCE_DISK_BUILD` (for features in the source disk build variant), `_SOURCE_DISK_CODE_FILES` (for features in the source disk CODE files variant) or `_SOURCE_DISK_ELT_FILES` (for features in the source disk ELT files variant). You can also search for `_SOURCE_DISK` for features in all the source disk variants.

The main differences in the Ian Bell game disk variant compared to the 4am crack variant are:

* The loading screen is different, showing "Cracked by The Student" instead of the Firebird logo.

* The values of some of the variables have been changed from the placeholder values in the source code, as the game was cracked by extracting it from memory while the game was running.

The main differences in the three source disk variants compared to the 4am crack variant are:

* The values of the STORE and CODE2 configuration variables are different, so the game copies CODE2 out of bank-switched RAM when it starts.

* There is an extra CLD instruction on starting the game.

* The game uses JSR DOXC to move the text cursor to a specified column, while the released game uses STA XC.

* The game uses JSR DOYC to move the text cursor to a specified row, while the released game uses STA YC.

* The game uses JSR INCYC to move the text cursor to the next row, while the released game uses INC YC.

* The "J", "K", "M" and "I" key presses for moving the chart crosshairs are not supported.

* The game waits for the vertical sync to implement pauses, while the released game just performs loop iterations.

* The default setting is to display both upper and lower case letters, while the default for the released game is upper case only.

* The joystick modification is missing from the end of S%, so the game will still scan the joystick fire button, even if joysticks are not configured.

* "M" and "J" can still be pressed when the charts are showing.

* The joystick fire button can be used to configure joysticks from the title screen.

* If the energy bomb is already going off and we press "B", the effect restarts.

Further differences in the source disk build variant compared to the 4am crack variant are:

* The dashboard has a blue rim and different coloured labels to the released version, and the loading screen has the date "23 SEPT 1985" printed in the space view.

Further differences in the source disk CODE files variant compared to the 4am crack variant are:

* The dashboard has a blue rim and different coloured labels to the released version, and the loading screen has the date "7 SEPT 1985" printed in the space view.

* It contains the code to cope with phantom key presses from the Commodore 64 version.

* The Thargoid spawning logic is different.

* There's some unused code that would speed up chart crosshair movement when fire is pressed.

* The joystick is not disabled while the docking computer is activated.

Further differences in the source disk ELT files variant compared to the 4am crack variant are:

* The dashboard has a blue rim and different coloured labels to the released version, and the loading screen has the date "7 SEPT 1985" printed in the space view.

See the [accompanying website](https://elite.bbcelite.com/apple/releases.html) for a comprehensive list of differences between the variants.

## Notes on the original source files

### Producing byte-accurate binaries

Instead of initialising workspaces with null values like BeebAsm, the original BBC Micro source code creates its workspaces by simply incrementing the `P%` and `O%` program counters, which means that the workspaces end up containing whatever contents the allocated memory had at the time. As the source files are broken into multiple BBC BASIC programs that run each other sequentially, this means the workspaces in the source code tend to contain either fragments of these BBC BASIC source programs, or assembled code from an earlier stage. This doesn't make any difference to the game code, which either initialises the workspaces at runtime or just ignores their initial contents, but if we want to be able to produce byte-accurate binaries from the modern BeebAsm assembly process, we need to include this "workspace noise" when building the project. Workspace noise is only loaded by the `encrypt` target; for the `build` target, workspaces are initialised with zeroes.

You can disable the production of byte-accurate binaries by passing `match=no` to the build. This will omit most workspace noise, leaving workspaces initialised with zeroes instead.

Here's an example of how workspace noise is included, from the big code file in elite-bcfs.asm:

```
IF _MATCH_ORIGINAL_BINARIES

 IF _IB_DISK OR _4AM_CRACK

  EQUB &00, &5C, &A0, &6C, &73, &73, &31, &00
  EQUB &90, &20, &B8, &00, &00, &A9, &9F, &52
  EQUB &35, &00, &90, &20, &C0, &00, &00, &BE
  EQUB &9F, &52, &37, &00, &90, &20, &CB, &00
  EQUB &00, &28, &A1, &48, &50, &52, &00, &90
  EQUB &20, &E6, &00, &00, &CC, &9F, &52, &61
  EQUB &66, &74, &65, &72, &00, &90, &20, &F4
  EQUB &00, &00, &D7, &9F, &52, &58, &32, &00
  EQUB &90, &21, &00, &00, &00, &E2, &9F, &52
  EQUB &58, &31, &00, &90, &21, &04, &00, &00
  EQUB &EC, &9F, &52, &31, &00, &90, &21, &0C
  EQUB &00, &00, &F6, &9F, &52, &61, &00, &90
  EQUB &21, &17, &00, &00, &00, &A0, &52, &36
  EQUB &00, &90, &21, &30, &00, &00

 ELIF _SOURCE_DISK_BUILD

  EQUB &52, &00, &90, &20, &CB, &00, &00, &A7
  EQUB &9F, &52, &61, &66, &74, &65, &72, &00
  EQUB &90, &20, &D9, &00, &00, &B2, &9F, &52
  EQUB &58, &32, &00, &90, &20, &E5, &00, &00
  EQUB &BD, &9F, &52, &58, &31, &00, &90, &20
  EQUB &E9, &00, &00, &C7, &9F, &52, &31, &00
  EQUB &90, &20, &F1, &00, &00, &D1, &9F, &52
  EQUB &61, &00, &90, &20, &FC, &00, &00, &DB
  EQUB &9F, &52, &36, &00, &90, &21, &15, &00
  EQUB &00, &F2, &9F, &52, &34, &00, &90, &21
  EQUB &17, &00, &00, &FC, &9F, &65, &74, &74
  EQUB &65, &72, &00, &90, &21, &21, &00, &00
  EQUB &0A, &A0, &52, &39, &00, &90, &21, &2D
  EQUB &00, &00, &89, &A1, &65, &74

 ELIF _SOURCE_DISK_CODE_FILES OR _SOURCE_DISK_ELT_FILES

  EQUB &52, &00, &90, &20, &EA, &00, &00, &A7
  EQUB &9F, &52, &61, &66, &74, &65, &72, &00
  EQUB &90, &20, &F8, &00, &00, &B2, &9F, &52
  EQUB &58, &32, &00, &90, &21, &04, &00, &00
  EQUB &BD, &9F, &52, &58, &31, &00, &90, &21
  EQUB &08, &00, &00, &C7, &9F, &52, &31, &00
  EQUB &90, &21, &10, &00, &00, &D1, &9F, &52
  EQUB &61, &00, &90, &21, &1B, &00, &00, &DB
  EQUB &9F, &52, &36, &00, &90, &21, &34, &00
  EQUB &00, &F2, &9F, &52, &34, &00, &90, &21
  EQUB &36, &00, &00, &FC, &9F, &65, &74, &74
  EQUB &65, &72, &00, &90, &21, &40, &00, &00
  EQUB &0A, &A0, &52, &39, &00, &90, &21, &4C
  EQUB &00, &00, &A1, &A1, &65, &74

 ENDIF

ELSE

 SKIPTO &C000

ENDIF
```

---

Right on, Commanders!

_Mark Moxon_