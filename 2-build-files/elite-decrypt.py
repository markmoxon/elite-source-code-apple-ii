#!/usr/bin/env python
#
# ******************************************************************************
#
# APPLE II ELITE DECRYPTION SCRIPT
#
# Written by Mark Moxon
#
# This script removes encryption and checksums from the compiled binaries for
# the Apple II version of Elite
#
# Files are saved using the decrypt.bin suffix so they don't overwrite any
# existing unprot.bin files, so they can be compared if required
#
# Run this script by changing directory to the repository's root folder and
# running the script with "python 2-build-files/elite-decrypt.py"
#
# You can decrypt specific releases by adding the following arguments, as in
# "python 2-build-files/elite-decrypt.py -rel2" for example:
#
#   -rel1   Decrypt the game disk on Ian Bell's site
#   -rel2   Decrypt the version built by the source disk
#   -rel3   Decrypt the CODE* binaries already on the source disk
#   -rel3   Decrypt the ELY* binaries already on the source disk
#
# Note that the script will do nothing for rel1, as the files are not encrypted
# in that variant
#
# If unspecified, the default is rel2
#
# ******************************************************************************

from __future__ import print_function
import sys

print()
print("Apple II Elite decryption")

argv = sys.argv
release = 2
folder = "source-disk-build"

for arg in argv[1:]:
    if arg == "-rel1":
        release = 1
        folder = "ib-disk"
        print("The ib-disk variant is not encrypted: exiting now")
        exit()
    if arg == "-rel2":
        release = 2
        folder = "source-disk-build"
    if arg == "-rel3":
        release = 3
        folder = "source-disk-code-files"
    if arg == "-rel4":
        release = 3
        folder = "source-disk-elt-files"

# Configuration variables for scrambling code and calculating checksums
#
# Values must match those in 3-assembled-output/compile.txt
#
# If you alter the source code, then you should extract the correct values for
# the following variables and plug them into the following, otherwise the game
# will fail the checksum process and will hang on loading
#
# You can find the correct values for these variables by building your updated
# source, and then searching compile.txt for "elite-checksum.py", where the new
# values will be listed

b = 0x4000                  # B%
g = 0x45E9                  # G%
na2_per_cent = 0x4DEE       # NA2%

# Load assembled CODE1 and CODE2 files

data_block = bytearray()

elite_file = open("4-reference-binaries/" + folder + "/CODE1.bin", "rb")
data_block.extend(elite_file.read())
elite_file = open("4-reference-binaries/" + folder + "/CODE2.bin", "rb")
data_block.extend(elite_file.read())
elite_file.close()

print()
print("[ Read    ] 4-reference-binaries/" + folder + "/CODE1.bin")
print("[ Read    ] 4-reference-binaries/" + folder + "/CODE2.bin")

# Do decryption

seed = 0x15
unscramble_from = len(data_block) - 2
unscramble_to = g - b - 1

updated_seed = seed

for n in range(unscramble_from, unscramble_to, -1):
    new = (data_block[n] - updated_seed) % 256
    data_block[n] = new
    updated_seed = new

print("[ Decrypt ] 4-reference-binaries/" + folder + "/CODE1.bin")
print("[ Decrypt ] 4-reference-binaries/" + folder + "/CODE2.bin")

# Save decrypted files

output_file = open("4-reference-binaries/" + folder + "/CODE.decrypted.bin", "wb")
output_file.write(data_block)
output_file.close()

print("[ Save    ] 4-reference-binaries/" + folder + "/CODE.decrypted.bin")

output_file = open("4-reference-binaries/" + folder + "/CODE1.decrypted.bin", "wb")
output_file.write(data_block[:0x5000])
output_file.close()

print("[ Save    ] 4-reference-binaries/" + folder + "/CODE1.decrypted.bin")

output_file = open("4-reference-binaries/" + folder + "/CODE2.decrypted.bin", "wb")
output_file.write(data_block[0x5000:])
output_file.close()

print("[ Save    ] 4-reference-binaries/" + folder + "/CODE2.decrypted.bin")

# Load assembled DATA file

data_block = bytearray()

elite_file = open("4-reference-binaries/" + folder + "/DATA.bin", "rb")
data_block.extend(elite_file.read())
elite_file.close()

print()
print("[ Read    ] 4-reference-binaries/" + folder + "/DATA.bin")

# Do decryption

seed = 0x69
unscramble_from = len(data_block) - 1
unscramble_to = 0 - 1

updated_seed = seed

for n in range(unscramble_from, unscramble_to, -1):
    new = (data_block[n] - updated_seed) % 256
    data_block[n] = new
    updated_seed = new

print("[ Decrypt ] 4-reference-binaries/" + folder + "/DATA.bin")

# Save decrypted file

output_file = open("4-reference-binaries/" + folder + "/DATA.decrypted.bin", "wb")
output_file.write(data_block)
output_file.close()

print("[ Save    ] 4-reference-binaries/" + folder + "/DATA.decrypted.bin")
print()
