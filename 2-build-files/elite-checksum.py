#!/usr/bin/env python
#
# ******************************************************************************
#
# APPLE II ELITE CHECKSUM SCRIPT
#
# Written by Mark Moxon, and inspired by Kieran Connell's version for the
# cassette version of Elite
#
# This script applies encryption and checksums to the compiled binaries for the
# Apple II version of Elite
#
# ******************************************************************************

from __future__ import print_function
import sys

argv = sys.argv
encrypt = True
release = 1

for arg in argv[1:]:
    if arg == "-u":
        encrypt = False
    if arg == "-rel1":
        release = 1
        encrypt = False
    if arg == "-rel2":
        release = 2
    if arg == "-rel3":
        release = 3
    if arg == "-rel4":
        release = 4
    if arg == "-rel5":
        release = 5

print("Apple II Elite Checksum")
print("Encryption = ", encrypt)

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

if release == 1:

    # Ian Bell disk
    b = 0x4000                  # B%
    g = 0x459C                  # G%
    na2_per_cent = 0x4D8C       # NA2%

elif release == 2:

    # Source disk build
    b = 0x4000                  # B%
    g = 0x45E9                  # G%
    na2_per_cent = 0x4DEE       # NA2%

elif release == 3:

    # Source disk CODE files
    b = 0x4000                  # B%
    g = 0x45E9                  # G%
    na2_per_cent = 0x4DF2       # NA2%

elif release == 4:

    # Source disk ELT files
    b = 0x4000                  # B%
    g = 0x45E9                  # G%
    na2_per_cent = 0x4DEE       # NA2%

elif release == 5:

    # 4am crack
    b = 0x4000                  # B%
    g = 0x459C                  # G%
    na2_per_cent = 0x4D8C       # NA2%

# Load assembled code files that make up the CODE1 and CODE2 files

data_block = bytearray()

# Append all assembled code files

elite_file = open("3-assembled-output/CODE.unprot.bin", "rb")
data_block.extend(elite_file.read())
elite_file.close()

# Commander data checksum

commander_start = na2_per_cent - b
commander_offset = 0x52
CH = 0x4C - 3
CY = 0
for i in range(CH, 0, -1):
    CH = CH + CY + data_block[commander_start + i + 7]
    CY = (CH > 255) & 1
    CH = CH % 256
    CH = CH ^ data_block[commander_start + i + 8]

print("Commander checksum = ", hex(CH))

data_block[commander_start + commander_offset] = CH ^ 0xA9
data_block[commander_start + commander_offset + 2] = CH

CH3 = 0x4C - 3
CY = 0
for i in range(CH3, 0, -1):
    CH3 = CH3 ^ i
    CY3 = CH3 & 1
    CH3 = (CH3 >> 1) | (CY << 7)
    CY = CY3
    CH3 = CH3 + CY + data_block[commander_start + i + 7]
    CY = (CH3 > 255) & 1
    CH3 = CH3 % 256
    CH3 = CH3 ^ data_block[commander_start + i + 8]

print("Commander checksum 3 = ", hex(CH3))

data_block[commander_start + commander_offset + 1] = CH3

# Encrypt the CODE file

scramble_from = g - b
scramble_to = len(data_block) - 2
seed = 0x15

if encrypt:
    for n in range(scramble_from, scramble_to):
        data_block[n] = (data_block[n] + data_block[n + 1]) % 256

    data_block[scramble_to] = (data_block[scramble_to] + seed) % 256

# Write output file for CODE1 (first 0x5000 bytes)

output_file = open("3-assembled-output/CODE1.bin", "wb")
output_file.write(data_block[:0x5000])
output_file.close()

print("versions/apple/3-assembled-output/CODE1.bin file saved")

# Write output file for CODE2 (rest of the file)

output_file = open("3-assembled-output/CODE2.bin", "wb")
output_file.write(data_block[0x5000:])
output_file.close()

print("versions/apple/3-assembled-output/CODE2.bin file saved")

# Load assembled code file for DATA

data_block = bytearray()

elite_file = open("3-assembled-output/DATA.unprot.bin", "rb")
data_block.extend(elite_file.read())
elite_file.close()

# Encrypt the DATA file

scramble_from = 0
scramble_to = len(data_block) - 1
seed = 0x69

if encrypt:
    for n in range(scramble_from, scramble_to):
        data_block[n] = (data_block[n] + data_block[n + 1]) % 256

    data_block[scramble_to] = (data_block[scramble_to] + seed) % 256

# Write output file for DATA

output_file = open("3-assembled-output/DATA.bin", "wb")
output_file.write(data_block)
output_file.close()

print("versions/apple/3-assembled-output/DATA.bin file saved")
