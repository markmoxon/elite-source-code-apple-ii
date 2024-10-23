BEEBASM?=beebasm
PYTHON?=python
DISKM8?=diskm8

# A make command with no arguments will build the source disk variant with
# encrypted binaries, checksums enabled, the standard commander and crc32
# verification of the game binaries
#
# Optional arguments for the make command are:
#
#   variant=<release>   Build the specified variant:
#
#                         ib-disk (default)
#                         source-disk-build (the binaries we get from running a build)
#                         source-disk-code-files (the CODE* binaries already on the source disk)
#                         source-disk-elt-files (the ELT* binaries already on the source disk)
#
#   commander=max       Start with a maxed-out commander
#
#   encrypt=no          Disable encryption and checksum routines
#
#   match=no            Do not attempt to match the original game binaries
#                       (i.e. omit workspace noise)
#
#   verify=no           Disable crc32 verification of the game binaries
#
# So, for example:
#
#   make variant=source-disk-build commander=max encrypt=no match=no verify=no
#
# will build an unencrypted source disk variant with a maxed-out commander,
# no workspace noise and no crc32 verification
#
# The following variables are written into elite-build-options.asm depending on
# the above arguments, so they can be passed to BeebAsm:
#
# _VERSION
#   9 = Apple II
#
# _VARIANT
#   1 = Ian Bell's game disk (default)
#   2 = source disk build (the binaries from running a build of the source disk)
#   3 = source disk CODE files (the CODE* binaries already on the source disk)
#   4 = source disk ELT files (the ELT* binaries already on the source disk)
#
# _MAX_COMMANDER
#   TRUE  = Maxed-out commander
#   FALSE = Standard commander
#
# _REMOVE_CHECKSUMS
#   TRUE  = Disable checksum routines
#   FALSE = Enable checksum routines
#
# _MATCH_ORIGINAL_BINARIES
#   TRUE  = Match binaries to released version (i.e. fill workspaces with noise)
#   FALSE = Zero-fill workspaces
#
# The encrypt and verify arguments are passed to the elite-checksum.py and
# crc32.py scripts, rather than BeebAsm

ifeq ($(commander), max)
  max-commander=TRUE
else
  max-commander=FALSE
endif

ifeq ($(encrypt), no)
  unencrypt=-u
  remove-checksums=TRUE
else
  unencrypt=
  remove-checksums=FALSE
endif

ifeq ($(match), no)
  match-original-binaries=FALSE
else
  match-original-binaries=TRUE
endif

ifeq ($(variant), source-disk-build)
  variant-number=2
  folder=/source-disk-build
  suffix=-source-disk-build
else ifeq ($(variant), source-disk-code-files)
  variant-number=3
  folder=/source-disk-code-files
  suffix=-source-disk-code-files
else ifeq ($(variant), source-disk-elt-files)
  variant-number=4
  folder=/source-disk-elt-files
  suffix=-source-disk-elt-files
else
  variant-number=1
  folder=/ib-disk
  suffix=-ib-disk
endif

ifeq ($(OS),Windows_NT)
    RM = cmd //C del //Q //F
else
    RM = rm -fr
endif

.PHONY:all
all: apple-build apple-disk

apple-build:
	echo _VERSION=9 > 1-source-files/main-sources/elite-build-options.asm
	echo _VARIANT=$(variant-number) >> 1-source-files/main-sources/elite-build-options.asm
	echo _REMOVE_CHECKSUMS=$(remove-checksums) >> 1-source-files/main-sources/elite-build-options.asm
	echo _MATCH_ORIGINAL_BINARIES=$(match-original-binaries) >> 1-source-files/main-sources/elite-build-options.asm
	echo _MAX_COMMANDER=$(max-commander) >> 1-source-files/main-sources/elite-build-options.asm
	$(BEEBASM) -i 1-source-files/main-sources/elite-data.asm -v > 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-source.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-bcfs.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-mover.asm -v >> 3-assembled-output/compile.txt
	$(PYTHON) 2-build-files/elite-checksum.py $(unencrypt) -rel$(variant-number)
	$(BEEBASM) -i 1-source-files/main-sources/elite-transfer.asm -v >> 3-assembled-output/compile.txt
	$(BEEBASM) -i 1-source-files/main-sources/elite-readme.asm -v >> 3-assembled-output/compile.txt
ifneq ($(verify), no)
	@$(PYTHON) 2-build-files/crc32.py 4-reference-binaries$(folder) 3-assembled-output
endif

apple-disk:
ifeq ($(variant-number), 1)
	$(RM) 5-compiled-game-disks/*.bin
	cp 1-source-files/other-files$(folder)/blank.dsk 5-compiled-game-disks/elite-apple$(suffix).dsk
	cp 1-source-files/images$(folder)/A.SCREEN.bin 5-compiled-game-disks/elitepic#0x2000.bin
	cp 3-assembled-output/DATA.bin 5-compiled-game-disks/bee#0x3b00.bin
	cp 3-assembled-output/CODE1.bin 5-compiled-game-disks/four#0x4000.bin
	cp 3-assembled-output/CODE2.bin 5-compiled-game-disks/nine#0x5000.bin
	cp 3-assembled-output/MOVER.bin 5-compiled-game-disks/mover#0x0300.bin
	$(DISKM8) -with-disk 5-compiled-game-disks/elite-apple$(suffix).dsk -file-put 5-compiled-game-disks/elitepic#0x2000.bin
	$(DISKM8) -with-disk 5-compiled-game-disks/elite-apple$(suffix).dsk -file-put 5-compiled-game-disks/nine#0x5000.bin
	$(DISKM8) -with-disk 5-compiled-game-disks/elite-apple$(suffix).dsk -file-put 5-compiled-game-disks/bee#0x3b00.bin
	$(DISKM8) -with-disk 5-compiled-game-disks/elite-apple$(suffix).dsk -file-put 5-compiled-game-disks/four#0x4000.bin
	$(DISKM8) -with-disk 5-compiled-game-disks/elite-apple$(suffix).dsk -file-put 5-compiled-game-disks/mover#0x0300.bin
	$(DISKM8) -with-disk 5-compiled-game-disks/elite-apple$(suffix).dsk -file-put 3-assembled-output/readme.txt
	$(RM) 5-compiled-game-disks/*.bin
endif
