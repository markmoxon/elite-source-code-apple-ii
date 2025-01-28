; ******************************************************************************
;
; APPLE II ELITE LOADER SOURCE
;
; Apple II Elite was written by Ian Bell and David Braben and is copyright
; D. Braben and I. Bell 1986
;
; The code in this file has been reconstructed from a disassembly of the version
; released on Ian Bell's personal website at http://www.elitehomepage.org/
;
; The commentary is copyright Mark Moxon, and any misunderstandings or mistakes
; in the documentation are entirely my fault
;
; The terminology and notations used in this commentary are explained at
; https://elite.bbcelite.com/terminology
;
; The deep dive articles referred to in this commentary can be found at
; https://elite.bbcelite.com/deep_dives
;
; ------------------------------------------------------------------------------
;
; This source file contains code to move binaries in memory during the loading
; process.
;
; ------------------------------------------------------------------------------
;
; This source file produces the following binary file:
;
;   * SEC3.bin
;
; ******************************************************************************

 INCLUDE "1-source-files/main-sources/elite-build-options.asm"

 _IB_DISK                   = (_VARIANT = 1)
 _SOURCE_DISK_BUILD         = (_VARIANT = 2)
 _SOURCE_DISK_CODE_FILES    = (_VARIANT = 3)
 _SOURCE_DISK_ELT_FILES     = (_VARIANT = 4)
 _4AM_CRACK                 = (_VARIANT = 5)
 _SOURCE_DISK               = (_VARIANT = 2) OR (_VARIANT = 3) OR (_VARIANT = 4)

; ******************************************************************************
;
; Configuration variables
;
; ******************************************************************************

 CODE% = $2000          ; The address where the code will be run

 LOAD% = $2000          ; The address where the code will be loaded

 startGame = $4000      ; The entry point for the main game binary

 fireButtonMask = $4562 ; The address of the variable that controls the
                        ; joystick selection on the title screen

 phsoff = $C080         ; Disk controller I/O soft switch for turning the
                        ; stepper motor phase 0 off (PHASEOFF)

 mtroff = $C088         ; Disk controller I/O soft switch for turning the motor
                        ; off (MOTOROFF)

 mtron = $C089          ; Disk controller I/O soft switch for turning the motor
                        ; on (MOTORON)

 drv1en = $C08A         ; Disk controller I/O soft switch for enabling drive 1
                        ; (DRV0EN)

 drv2en = $C08B         ; Disk controller I/O soft switch for enabling drive 2
                        ; (DRV1EN)

 Q6L = $C08C            ; Disk controller I/O soft switch for strobing the data
                        ; latch for I/O (Q6L)

 Q6H = $C08D            ; Disk controller I/O soft switch for loading the data
                        ; latch (Q6H)

 Q7L = $C08E            ; Disk controller I/O soft switch for preparing the
                        ; latch for input (Q7L)

 Q7H = $C08F            ; Disk controller I/O soft switch for preparing the
                        ; latch for output (Q7H)

; ******************************************************************************
;
;       Name: ZP
;       Type: Workspace
;    Address: $00F0 to $00F7
;   Category: Workspaces
;    Summary: Important variables used by the loader
;
; ******************************************************************************

 ORG $00F0

.ztemp0

 SKIP 1                 ; Temporary storage used by the disk routines

.ztemp1

 SKIP 1                 ; Temporary storage used by the disk routines

.ztemp2

 SKIP 1                 ; Temporary storage used by the disk routines

.ztemp3

 SKIP 1                 ; Temporary storage used by the disk routines

.fromAddr

 SKIP 2                 ; The address to copy from in the CopyMemory routine

.toAddr

 SKIP 2                 ; The address to copy to in the CopyMemory routine

; ******************************************************************************
;
;       Name: Disk operations workspace 1
;       Type: Workspace
;    Address: $0300 to $0310
;   Category: Workspaces
;    Summary: Variables used by the disk operations and DOS 3.3 RWTS routines
;
; ******************************************************************************

 ORG $0300

.track

 SKIP 1                 ; Storage for a track number in the RWTS code

.sector

 SKIP 1                 ; Storage for a sector number in the RWTS code

.curtrk

 SKIP 1                 ; The current track before performing a seek in the RWTS
                        ; code

.tsltrk

 SKIP 1                 ; The track for the commander file's track/sector list

.tslsct

 SKIP 1                 ; The sector for the commander file's track/sector list

.filtrk

 SKIP 1                 ; The track for the commander file's contents

.filsct

 SKIP 1                 ; The sector for the commander file's contents

.mtimel

 SKIP 1                 ; The motor on time (low byte)

.mtimeh

 SKIP 1                 ; The motor on time (high byte)

.seeks

 SKIP 1                 ; The maximum number of seeks

.recals

 SKIP 1                 ; The maximum number of arm recalibrations to 2

.slot16

 SKIP 1                 ; The slot number containing the disk controller card,
                        ; multiplied by 16 to move the slot number into the top
                        ; nibble (so the value is $x0 for slot x)
                        ;
                        ; This can then be used as an offset to add to the soft
                        ; switch addresses for the disk controller, to ensure we
                        ; access the addresses for the correct slot

.atemp0

 SKIP 1                 ; Temporary storage for the read/write status bit

.idfld

 SKIP 4                 ; Storage for four bytes used in the RDADR16 routine:
                        ;
                        ;   * Checksum
                        ;   * Sector
                        ;   * Track
                        ;   * Volume

 PRINT "Disk operations workspace 1 from ", ~track, "to ", ~P%-1, "inclusive"

; ******************************************************************************
;
;       Name: Disk operations workspace 2
;       Type: Workspace
;    Address: $25D6 to $287B
;   Category: Workspaces
;    Summary: Variables used by the disk operations and DOS 3.3 RWTS routines
;
; ******************************************************************************

 ORG $25D6

.buffer

 SKIP 48                ; A 256-byte sector buffer, where we can load sectors
                        ; from the disk, such as the track/sector list, or the
                        ; commander file contents
                        ;
                        ; For file data, this is where we store the data that
                        ; we want to save, before it is pre-nibblized into
                        ; 6-bit nibbles in buff2 by the prenib routine
                        ;
                        ; It is also where file data is stored after being
                        ; post-nibblized, in which case the 6-bit nibbles in
                        ; buffr2 are converted into 8-bit bytes and stored here

.fretrk

 SKIP 1                 ; The number of the last track that we checked for a
                        ; free sector in the getsct routine

.dirtrk

 SKIP 3                 ; The direction in which we are searching tracks for
                        ; free sectors in the getsct routine (+1 or -1)

.tracks

 SKIP 1                 ; The number of tracks per disk

 SKIP 3                 ; Padding to ensure the bitmap variable lines up with
                        ; byte #56 ($38) for the bitmap of free sectors

.bitmap

 SKIP 200               ; Bit map of free sectors in track 0, at byte #56 ($38)
                        ; in the buffer

 SKIP 72                ; These bytes appear to be unused

.buffr2

 SKIP 350               ; A 342-byte buffer for storing data in the 6-bit nibble
                        ; format
                        ;
                        ; This is where we load file data from the disk in the
                        ; 6-bit nibble format, so it can be post-nibblized into
                        ; 8-bit bytes and stored in buffer
                        ;
                        ; It is also where we store nibblized data that is ready
                        ; to be saved to the disk

 PRINT "Disk operations workspace 2 from ", ~buffer, "to ", ~P%-1, "inclusive"

; ******************************************************************************
;
; ELITE LOADER
;
; ******************************************************************************

 ORG CODE%

; ******************************************************************************
;
;       Name: Elite loader
;       Type: Subroutine
;   Category: Loader
;    Summary: Load the SCRN and ELB1 binaries and run the game
;
; ------------------------------------------------------------------------------
;
; This game loader is only used by the official Firebird release (i.e. the 4am
; crack variant).
;
; The loader's aim is to load the game code and data into memory as follows:
;
;   * The game's data is from $0B60 to $1C99.
;
;   * The loading screen (including the dashboard) is from $2000 to $3FFF.
;
;   * The first block of the main game binary (CODE1) is $4000 to $8FFF.
;
;   * The second block of the main game binary (CODE2) is from $9000 to $BFFF.
;
; These blocks of code and data are packaged up into two large files - ELA and
; ELB - by the elite-transfer.asm source. Note that the ELB file is called ELB1
; on the 4am crack disk, but the source files refer to it as ELB, so I'll stick
; with that name here.
;
; For the Firebird release, the loading process is as follows:
;
;   * The ELITE BASIC file is run. This does BRUN ELA, followed by BRUN SEC3.
;     Let's see what those commands do.
;
;   * ELA (which includes this routine) is run first with a load address of
;     $0A00, so that loads the game data at $0B60, the loading screen at $2000
;     and CODE2 at $4000 to $8FFF. This means that the game data and dashboard
;     are now in the correct places for running the game (though the latter will
;     soon get corrupted, see below).
;
;   * Because the ELA file is run with a BRUN command, it calls its own ENTRY
;     routine after loading, which switches to the high-resolution screen mode.
;     It also copies some data into bank-switched RAM, but this has no effect in
;     the released game; it's a remnant of the transfer process used by the
;     source disk variant, as described in the elite-transfer.asm source.
;
;   * The game loader in SEC3 is then run, which starts by copying CODE2 from
;     $4000-$8FFF to $9000-$BFFF. This means that CODE2 is now in the correct
;     place for running the game. SEC3 itself loads at $2000, which means it
;     corrupts the loading screen (as $2000 is the start of screen memory).
;
;   * SEC3 then loads ELB (called ELB1 on disk) with a load address of $4000,
;     which loads CODE1 from $4000 to $8FFF. This means that CODE1 is now in
;     the correct place for running the game. 
;
;   * Finally, we start the game by calling the main game's S% routine at $4000,
;     which starts by restoring the loading screen (in particular the dashboard,
;     which is needed for the game to work), and then the game itself starts.
;
; So this loads the complete game binary into memory, and it's ready to run.
;
; ******************************************************************************

.ENTRY

 JSR CopyCode2          ; The ELITE BASIC program has already run by this point,
                        ; so the following step has already been done:
                        ;
                        ;   * ELA has been loaded and run, so CODE2 is in memory
                        ;     from $4000 to $8FFF
                        ;
                        ; The first step is therefore to copy the CODE2 block
                        ; from $4000-$6FFF to $9000-$BFFF

 JSR SetLoadVariables1  ; Configure the file load variables as follows:
                        ;
                        ;   * skipBytes = 4
                        ;
                        ;   * fileSize(1 0) = $0880
                        ;
                        ;   * trackSector = 0
                        ;
                        ;   * loadAddr = STA $0200,X

 JSR LoadFile           ; Load the SCRN file of size $0880 at $0200 (though this
                        ; isn't actually used)

 JSR SetFilename        ; Set the filename in comnam to ELB1

 JSR SetLoadVariables2  ; Configure the file load variables as follows:
                        ;
                        ;   * skipBytes = 4
                        ;
                        ;   * fileSize(1 0) = $4FFF
                        ;
                        ;   * trackSector = 0
                        ;
                        ;   * loadAddr = STA $4000,X

 JSR LoadFile           ; Load the ELB1 file of size $4FFF at $4000, so that's
                        ; from $4000 to $8FFF
                        ;
                        ; ELB1 contains the CODE1 block of the main game binary,
                        ; so the end result of all this loading is:
                        ;
                        ;   * CODE1 from $4000 to $8FFF
                        ;
                        ;   * CODE2 from $9000 to $BFFF
                        ;
                        ; In other words the game binary is now loaded and in
                        ; the correct location for the game to run

 JMP startGame          ; Jump to startGame to start the game

; ******************************************************************************
;
;       Name: filename
;       Type: Variable
;   Category: Save and load
;    Summary: The filename of the second file to load
;
; ******************************************************************************

.filename

EQUS "ELB1"

; ******************************************************************************
;
;       Name: comnam
;       Type: Variable
;   Category: Save and load
;    Summary: The filename of the first file to load, padded out with spaces to
;             a fixed size of 30 characters for the rfile routine
;
; ******************************************************************************

.comnam

EQUS "SCRN                          "

; ******************************************************************************
;
;       Name: findf
;       Type: Subroutine
;   Category: Save and load
;    Summary: Search the disk catalog for an existing file
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   C flag              The result of the search:
;
;                         * Clear = file found
;
;                         * Set = file not found
;
; ******************************************************************************

.findf

 CLC                    ; Clear the C flag to pass to rentry to indicate that we
                        ; should search the disk catalog for an existing file

 BCC rentry             ; Jump to rentry to find the file (this BCC is
                        ; effectively a JMP as we just cleared the C flag

; ******************************************************************************
;
;       Name: rentry
;       Type: Subroutine
;   Category: Save and load
;    Summary: Search the disk catalog for an existing file or an empty file
;             entry
;
; ------------------------------------------------------------------------------
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; ------------------------------------------------------------------------------
;
; Arguments:
;
;   C flag              The type of search:
;
;                         * Clear = search the catalog for an existing file
;
;                         * Set = search the catalog for an empty file entry
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   C flag              The result of the search:
;
;                         * Clear = file/entry found
;
;                         * Set = file/entry not found
;
;   Y                   The offset to the file entry in the catalog sector
;
; ******************************************************************************

.rentry

 ROR atemp0             ; Store the C flag in bit 7 of atemp0, so we can check
                        ; it later

 JSR rvtoc              ; Read the VTOC sector into the buffer

                        ; We now work through the catalog sectors to look for
                        ; the existing file entry (if bit 7 of atemp0 is clear)
                        ; or an empty file empty (if bit 7 of atemp0 is set)

.rentr2

 LDA buffer+1           ; Set track to the track number of the next catalog
 STA track              ; sector from byte #1 of the VTOC

 LDA buffer+2           ; Set sector to the sector number of the next catalog
 STA sector             ; sector from byte #2 of the VTOC

 JSR rsect              ; Read the catalog sector into the buffer

 LDY #$B                ; Set Y to use as an index to the first file entry in
                        ; the catalog sector (as the file entries start at
                        ; offset $B in the catalog, with each entry taking up
                        ; 35 bytes)

.rentr3

 LDA buffer,Y           ; Set A to the first byte from the file entry, which
                        ; will either be the track number of the file, or 0 to
                        ; indicate an empty file entry, or $FF to indicate a
                        ; deleted file

 BIT atemp0             ; If bit 7 of atemp0 is clear then we are searching the
 BPL rentr4             ; catalog for an existing file entry, so jump to rentr4
                        ; to do this

                        ; If we get here then we are searching for an empty file
                        ; entry

 TAX                    ; If A = 0 then we have just found an empty file entry,
 BEQ rentr6             ; so jump to rentr6 to return from the subroutine with a
                        ; successful result

 CMP #$FF               ; If A = $FF then we have just found a deleted file
 BEQ rentr6             ; entry, so jump to rentr6 to return from the subroutine
                        ; with a successful result

 BNE rentr8             ; This file entry doesn't match our requirements, so
                        ; jump to rentr8 to try the next file entry in this
                        ; catalog sector

.rentr4

                        ; If we get here then we are searching for an existing
                        ; file entry

 TAX                    ; If A = 0 then we have just found an empty file entry,
 BEQ rentr9             ; which means we have not found a match for our file, so
                        ; jump to rentr9 to return from the subroutine with the
                        ; C flag set to indicate that we can't find the file

 CMP #$FF               ; If A = $FF then we have just found a deleted file
 BEQ rentr8             ; entry, which is not a match for our file, so jump to
                        ; rentr8 to try the next file entry in this catalog
                        ; sector

 TYA                    ; Store the file entry index in Y on the stack, so we
 PHA                    ; can retrieve it after the following loop

                        ; We now check the file entry to see if it matches the
                        ; filename in comnam

 LDX #0                 ; Set X = 0 to use as a character index for the filename
                        ; in the file entry

.rentr5

 LDA buffer+3,Y         ; Set A to the Y-th character from the filename in the
 AND #%01111111         ; file entry we are checking (the filename in a file
                        ; entry starts at byte #3)

 CMP comnam,X           ; If the character does not match the X-th character of
 BNE rentr7             ; comnam then the names don't match, to jump to rentr7
                        ; to try the next file entry in this catalog sector

 INY                    ; Increment the character index for the file entry

 INX                    ; Increment the character index for the filename we are
                        ; searching for

 CPX #30                ; Loop back until we have checked all 30 characters
 BNE rentr5

                        ; If we get here then all 30 characters of the filename
                        ; in the file entry match the filename in comnam, so we
                        ; have found the file entry we are looking for

 PLA                    ; Set Y to the file entry index that we stored on the
 TAY                    ; stack above, so it once again points to the entry we
                        ; are checking

.rentr6

 CLC                    ; Clear the C flag to indicate that we have found the
                        ; file entry we are looking for

 RTS                    ; Return from the subroutine

.rentr7

 PLA                    ; Set Y to the file entry index that we stored on the
 TAY                    ; stack above, so it once again points to the entry we
                        ; are checking

.rentr8

 TYA                    ; Set Y = Y + 35
 CLC                    ;
 ADC #35                ; Each file entry in the catalog consists of 35 bytes,
 TAY                    ; so this increments Y to point to the next entry

 BNE rentr3             ; Loop back until we have reached the last file entry

 LDA buffer+1           ; Set track to the track number of the next catalog
                        ; sector from byte #1 of the VTOC

 BNE rentr2             ; If the next catalog sector is non-zero then loop back
                        ; to load and search this sector

                        ; Otherwise we have searched every catalog sector and we
                        ; haven't found what we're looking for, so fall through
                        ; into rentr9 to return from the subroutine with the C
                        ; flag set to indicate that the catalog is full

.rentr9

 SEC                    ; Clear the C flag to indicate that we have not found
                        ; the file entry we are looking for

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: getsct
;       Type: Subroutine
;   Category: Save and load
;    Summary: Analyse the VTOC sector to allocate one free sector
;
; ------------------------------------------------------------------------------
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; ------------------------------------------------------------------------------
;
; Arguments:
;
;   buffer              The VTOC sector for this disk
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   C flag              The result of the check:
;
;                         * Clear = free sector found
;
;                         * Set = no free sectors found (i.e. the disk is full)
;
;   X                   The track number containing the free sector
;
;   Y                   The free sector number
;
; ******************************************************************************

.getsct

 LDA #0                 ; Set ztemp0 = 0 to denote that we are starting this
 STA ztemp0             ; search in the outer half of the disk from track 16
                        ; down to track 0

 BEQ getsc4             ; Jump into the loop below at getsc4 with A = 0, so we
                        ; start the search at the last track number that we
                        ; checked, which is in fretrk

.getsc3

 LDA dirtrk             ; Set A to the direction we are moving in our search for
                        ; a free sector (-1 or +1)

.getsc4

 CLC                    ; Add the direction in A to the last allocated track so
 ADC fretrk             ; we move in the direction in A
                        ;
                        ; Or, if we just started searching with A = 0, we check
                        ; the last allocated track, as it might not have been
                        ; used last time and might still be free
                        ;
                        ; In either case, A now contains the next track to check
                        ; for a free sector

 BEQ getsc5             ; If we have reached track 0, jump to getsc5

 CMP tracks             ; If A is less than the number of tracks on the disc
 BCC getsc7             ; then we haven't reached the highest numbered track
                        ; yet, so jump to getsc7 to check this track for a free
                        ; sector

 LDA #$FF               ; Otherwise we have reached the highest numbered track,
                        ; so set A = -1 so we start searching from track 16 down
                        ; to track 0

 BNE getsc6             ; Jump to getsc6 to set the direction to -1 and start
                        ; searching from track 16 down to track 0 (this BNE is
                        ; effectively a JMP as A is always non-zero)

.getsc5

 LDA ztemp0             ; If ztemp0 is non-zero then we have already searched
 BNE getscB             ; the disk from track 18 up to track 34, and we jumped
                        ; here when we finished searching track 16 down to track
                        ; 0, so we have searched the whole disk and haven't
                        ; found a free sector, so jump to getscB to return from
                        ; the subroutine with a disk full error

 LDA #1                 ; Otherwise we have not already searched from track 18
                        ; up to track 34, so set A = +1 so we start searching
                        ; from track 18 up to track 34

 STA ztemp0             ; Set ztemp0 = 1 to record that we are now searching the
                        ; half of the disk track 18 up to track 34

.getsc6

 STA dirtrk             ; Set the search direction to A, so it's now -1 or +1

 CLC                    ; Set A = A + 17, so A is now the track next to the VTOC
 ADC #17                ; track in the direction we want to search (the VTOC is
                        ; always in track 17)
                        ;
                        ; So this is the track to start searching from, heading
                        ; in the new direction in dirtrk

.getsc7

 STA fretrk             ; Store the number of the track we are checking for a
                        ; free sector in fretrk

                        ; We now search the bitmap of free sectors for the track
                        ; in A, which is part of the VTOC and is therefore in
                        ; buffer
                        ;
                        ; The bitmaps for each track are stored at byte $38 (for
                        ; track 0) onwards, with four bitmap bytes per track,
                        ; though only the first two bytes contain bitmap data
                        ;
                        ; The bitmap variable points to byte #56 ($38) of the
                        ; buffer where we loaded the VTOC, so it points to the
                        ; first bitmap for track 0

 ASL A                  ; Set Y = A * 4
 ASL A                  ;
 TAY                    ; So we can use Y as an index into the bitmap of free
                        ; sectors in the buffer, so the bitmap for track Y is
                        ; at bitmap + Y

 LDX #16                ; Set X = 16 to denote that we are searching the first
                        ; byte of the bitmap

 LDA bitmap,Y           ; Set A to the first byte of the bitmap for the track
                        ; we are checking

 BNE getsc8             ; If A is non-zero then there is a non-zero bit in the
                        ; bitmap, which indicates a free sector, so jump to
                        ; getsc8 to convert this into a sector number

 INY                    ; Increment Y to point to the next byte in the bitmap
                        ; of free sectors

 LDX #8                 ; Set X = 8 to denote that we are searching the second
                        ; byte of the bitmap

 LDA bitmap,Y           ; Set A to the second byte of the bitmap for the track
                        ; we are checking

 BEQ getsc3             ; If A is zero then every sector is occupied in the
                        ; bitmap, so loop back getsc3 to move on to the next
                        ; track, as there are no free sectors in this one

.getsc8

                        ; If we get here then we have found a free sector in
                        ; the bitmap for this track, so we need to convert this
                        ; into a sector number
                        ;
                        ; We do this by looping through the bitmap byte in A
                        ; until we find a set bit to indicate a free sector

 STX ztemp0             ; Store X in ztemp0, so it is 16 if we found a free
                        ; sector in the first bitmap byte, or 8 if we found a
                        ; free sector in the second bitmap byte
                        ;
                        ; So ztemp0 is the sector number that corresponds to
                        ; bit 7 in the relevant byte, as the first byte covers
                        ; sectors 8 to 15 (bit 0 to 7), and the second byte
                        ; covers sectors 0 to 7 (bit 0 to 7)

 LDX #0                 ; Set a counter in X to keep track of the position of
                        ; the bit we are currently checking

.getsc9

 INX                    ; Increment the bit position in X

 DEC ztemp0             ; Decrement the sector number in ztemp0

 ROL A                  ; Set the C flag to the next bit from the bitmap byte

 BCC getsc9             ; Loop back to getsc9 until we shift a 1 out of the
                        ; bitmap byte, which indicates a free sector

                        ; We now change this 1 to a 0 and shift all the other
                        ; bits in the bitmap back to their original positions

 CLC                    ; Clear the C flag so the first rotation in the
                        ; following loop will replace the 1 we just found with a
                        ; 0, to indicate that it is no longer free

.getscA

 ROR A                  ; Rotate the bits back into A again

 DEX                    ; Decrement the position counter in X

 BNE getscA             ; Loop back until we have rotated all the bits back into
                        ; the bitmap, with the 1 changed to a 0

 STA bitmap,Y           ; update VTOC

 LDX fretrk             ; Set X to the track number where we found the free
                        ; sector, which we stored in fretrk, so we can return it
                        ; from the subroutine

 LDY ztemp0             ; Set X to the number of the free sector in ztemp0, so
                        ; we can return it from the subroutine

 CLC                    ; Clear the C flag to indicate that we have successfully
                        ; found a free sector

 RTS                    ; Return from the subroutine

.getscB

 SEC                    ; Clear the C flag to indicate that we have not found
                        ; a free sector and the disk is full

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: gettsl
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read a file's track/sector list
;
; ------------------------------------------------------------------------------
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; ------------------------------------------------------------------------------
;
; Arguments:
;
;   buffer              The catalog sector for this file
;
;   Y                   The offset within the catalog sector for the relevant
;                       file entry
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   buffer              The track/sector list for the file
;
;   track               The track number of the file's data
;
;   sector              The sector number of the file's data
;
; ******************************************************************************

.gettsl

 LDA buffer,Y           ; Set track to the track containing the track/sector
 STA track              ; list

 LDA buffer+1,Y         ; Set sector to the sector containing the track/sector
 STA sector             ; list

 JSR rsect              ; Read the track/sector list into the buffer

 LDY #$C                ; Set Y to offset $C, so it points to the track and
                        ; sector of first data sector in the track/sector list
                        ; we just loaded

 LDA buffer,Y           ; Set track to the track containing the file data
 STA track

 LDA buffer+1,Y         ; Set sector to the sector containing the file data
 STA sector

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: rvtoc
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read the VTOC sector into the buffer
;
; ------------------------------------------------------------------------------
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   buffer              Contains the VTOC sector
;
; ******************************************************************************

.rvtoc

 LDA #17                ; Set the track number to 17, which is where the VTOC is
 STA track              ; stored on the disk

 LDA #0                 ; Set the sector number to 0, which is where the VTOC is
 STA sector             ; stored on the disk

                        ; Fall through into rsect to read sector 0 in track 17,
                        ; so we read the VTOC sector from the disk into the
                        ; buffer

; ******************************************************************************
;
;       Name: rsect
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read a specific sector from disk into the buffer
;
; ------------------------------------------------------------------------------
;
; Arguments:
;
;   track               The track number
;
;   sector              The sector number
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   buffer              Contains the sector
;
; ******************************************************************************

.rsect

 CLC                    ; Clear the C flag to denote that this is a read
                        ; operation (this value will be read throughout the
                        ; RWTS code that follows)

 BCC wsect2             ; Jump to wsect2 to read the specified sector

; ******************************************************************************
;
;       Name: wsect
;       Type: Subroutine
;   Category: Save and load
;    Summary: Write a specific sector from the buffer to disk
;
; ------------------------------------------------------------------------------
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; ------------------------------------------------------------------------------
;
; Arguments:
;
;   track               The track number
;
;   sector              The sector number
;
;   buffer              Contains the data to write
;
; ------------------------------------------------------------------------------
;
; Other entry points:
;
;   wsect2              Read or write a sector, depending on the value of the
;                       C flag (clear = read, set = write)
;
; ******************************************************************************

.wsect

 SEC                    ; Set the C flag to denote that this is a write
                        ; operation (this value will be read throughout the
                        ; RWTS code that follows)

.wsect2

 PHP                    ; Store the read/write status on the stack (specifically
                        ; the C flag)

 LDA #$60               ; Set the slot number containing the disk controller
 STA slot16             ; to 6 (storing it as the number multiplied by 16 so we
                        ; can use this as an offset to add to the soft switch
                        ; addresses for the disk controller, to ensure we access
                        ; the addresses for slot 6)

 LDA #2                 ; Set the maximum number of arm recalibrations to 2
 STA recals

 LDA #4                 ; Set the maximum number of seeks to 4
 STA seeks

 LDA #$D8               ; Set the high byte of the motor on time to $D8
 STA mtimeh

 LDX slot16             ; Set X to the disk controller card slot number * 16

                        ; Fall through into rwts to read or write the specified
                        ; sector

; ******************************************************************************
;
;       Name: rwts
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read or write a specific sector
;
; ------------------------------------------------------------------------------
;
; This routine is almost identical to the RWTS routine in Apple DOS 3.3.
; It omits the code from the start of the routine that checks the command block
; and slot number, as Elite doesn't use either of those features.
;
; The original DOS 3.3 source code for this routine in is shown in the comments.
; For detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner.
;
; For details of the VTOC layout, catalog sector layout and file entry layout,
; see chapter 4, "Diskette organisation".
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

 LDA Q7L,X              ; SAMESLOT LDA Q7L,X      ; MAKE SURE IN READ MODE
 LDA Q6L,X              ;          LDA Q6L,X
 LDY #8                 ;          LDY #8         ; WE MAY HAFTA CHECK SEVERAL
                        ;                           TIMES TO BE SURE

.rwts2                  ; CHKIFON  EQU *

 LDA Q6L,X              ;          LDA Q6L,X      ; GET THE DATA
 PHA                    ;          PHA            ; DELAY FOR DISK DATA TO
                        ;                           CHANGE
 PLA                    ;          PLA
 PHA                    ;          PHA
 PLA                    ;          PLA
                        ;          STX SLOT

 CMP $100               ; This instruction replaces the STX SLOT instruction in
                        ; the original code
                        ;
                        ; It has no effect as any changes to the flags will be
                        ; overridden by the next instruction, but the important
                        ; thing is that both STX SLOT and CMP $100 take four CPU
                        ; cycles, so this is effectively a way of commenting out
                        ; the original instruction without affecting the timings
                        ; that are so crucial to the workings of the RWTS code

 CMP Q6L,X              ;          CMP Q6L,X      ; CHECK RUNNING HERE
 BNE rwts3              ;          BNE ITISON     ; =>IT'S ON...
 DEY                    ;          DEY            ; MAYBE WE DIDN'T CATCH IT
 BNE rwts2              ;          BNE CHKIFON    ; SO WE'LL TRY AGAIN

                        ; A chunk of the original DOS is omitted here, from
                        ; ITISON to the start of OK, where we pick up the story
                        ; once again

.rwts3

 PHP                    ; Save the result of the above checks on the stack, so
                        ; we have the Z flag clear (BNE) if the disk is
                        ; spinning, or the Z flag set (BEQ) if the disk is not
                        ; spinning

 LDA mtron,X            ; Read the disk controller I/O soft switch at MOTORON
                        ; for slot X to turn the disk motor on

                        ; The following code omits the drive select code, as
                        ; Elite only supports drive 1

                        ; OK       ROR A          ; BY GOING INTO THE CARRY
                        ;          BCC SD1        ; SELECT DRIVE 2 !
 LDA drv1en,X           ;          LDA DRV1EN,X   ; ASSUME DRIVE 1 TO HIT
                        ;          BCS DRVSEL     ; IF WRONG, ENABLE DRIVE 2
                        ;                           INSTEAD
                        ;
                        ; SD1      LDA DRV2EN,X
                        ;
                        ; DRVSEL   EQU *
                        ;          ROR DRIVNO     ; SAVE SELECTED DRIVE
                        ; *
                        ; * DRIVE SELECTED. IF MOTORING-UP,
                        ; *  WAIT BEFORE SEEKING...
                        ; *
 PLP                    ;          PLP            ; WAS THE MOTOR
 PHP                    ;          PHP            ; PREVIOUSLY OFF?
 BNE rwts5              ;          BNE NOWAIT     ; =>NO, FORGET WAITING.
 LDY #7                 ;          LDY #7         ; YES, DELAY 150 MS

.rwts4

 JSR armwat             ; SEEKW    JSR MSWAIT
 DEY                    ;          DEY
 BNE rwts4              ;          BNE SEEKW
 LDX slot16             ;          LDX SLOT       ; RESTORE SLOT NUMBER

.rwts5                  ; NOWAIT   EQU *
                        ; *
                        ; * SEEK TO DESIRED TRACK...
                        ; *
                        ;          LDY #4         ; SET TO IOBTRK
                        ;          LDA (IOBPL),Y  ; GET DESIRED TRACK

 LDA track              ; We fetch the track number from the track variable
                        ; rather than the IOBPL block, as the Elite code just
                        ; stores values in variables instead

 JSR seek               ;          JSR MYSEEK     ; SEEK!
                        ; *
                        ; * SEE IFMOTOR WAS ALREADY SPINNING.
                        ; *
 PLP                    ;          PLP            ; WAS MOTOR ON?
 BNE trytrk             ;          BNE TRYTRK     ; IF SO, DON'T DELAY, GET IT
                        ;                           TODAY!
                        ; *
                        ; *  WAIT FOR MOTOR SPEED TO COME UP.
                        ; *
 LDY mtimeh             ;          LDY MONTIME+1  ; IF MOTORTIME IS POSITIVE,
 BPL trytrk             ;          BPL MOTORUP    ; THEN SEEK WASTED ENUFF TIME
                        ;                           FOR US

.rwts6

 LDY #18                ; MOTOF    LDY #$12       ; DELAY 100 USEC PER COUNT

.rwts7

 DEY                    ; CONWAIT  DEY
 BNE rwts7              ;          BNE CONWAIT
 INC mtimel             ;          INC MONTIME
 BNE rwts6              ;          BNE MOTOF
 INC mtimeh             ;          INC MONTIME+1
 BNE rwts6              ;          BNE MOTOF      ; COUNT UP TO $0000

; ******************************************************************************
;
;       Name: trytrk
;       Type: Subroutine
;   Category: Save and load
;    Summary: Try finding a specific track on the disk
;
; ------------------------------------------------------------------------------
;
; This routine is almost identical to the TRYTRK routine in Apple DOS 3.3.
; It omits the code from the start of the routine that checks for the format
; command, as this is not required.
;
; The original DOS 3.3 source code for this routine in is shown in the comments.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ------------------------------------------------------------------------------
;
; Other entry points:
;
;   trytr4              Entry point for track errors, so we can try reading it
;                       again (must have the status flags on the stack with the
;                       C flag set)
;
; ******************************************************************************

.trytrk

                        ; TRYTRK   EQU *
                        ;          LDY #$0C
                        ;          LDA (IOBPL),Y  ; GET COMMAND CODE #
                        ;          BEQ GALLDONE   ; IF NULL COMMAND, GO HOME TO
                        ;                           BED.
                        ;          CMP #$04       ; FORMAT THE DISK?
                        ;          BEQ FORMDSK    ; ALLRIGHT,ALLRIGHT, I WILL...
                        ;          ROR A          ; SET CARRY=1 FOR READ, 0 FOR
                        ;                           WRITE
                        ;          PHP            ; AND SAVE THAT
                        ;          BCS TRYTRK2    ; MUST PRENIBBLIZE FOR WRITE.

 PLP                    ; Instead of the above checks, which we don't need to do
 PHP                    ; as we don't want to format the disk, we can simply
 BCC trytr2             ; fetch the read/write status into the C flag from the
                        ; stack, and if the C flag is clear then we are reading
                        ; a sector, so skip the following instruction as we
                        ; only need to call prenib if we are writing

 JSR prenib             ;          JSR PRENIB16

.trytr2

 LDY #48                ; TRYTRK2  LDY #$30       ; ONLY 48 RETRIES OF ANY KIND.
 STY ztemp2             ;          STY RETRYCNT

.trytr3

 LDX slot16             ; TRYADR   LDX SLOT       ; GET SLOT NUM INTO X-REG
 JSR rdaddr             ;          JSR RDADR16    ; READ NEXT ADDRESS FIELD
 BCC rdrght             ;          BCC RDRIGHT    ; IF READ IT RIGHT, HURRAH!

.trytr4

 DEC ztemp2             ; TRYADR2  DEC RETRYCNT   ; ANOTHER MISTAEK!!
 BPL trytr3             ;          BPL TRYADR     ; WELL, LET IT GO THIS TIME.,
                        ; *
                        ; * RRRRRECALIBRATE !!!!
                        ; *

.trytr5                 ; RECAL    EQU *
                        ;          LDA CURTRK
                        ;          PHA            ; SAVE TRACK WE REALLY WANT
                        ;          LDA #$60       ; RECALIBRATE ALL OVER AGAIN!
                        ;          JSR SETTRK     ; PRETEND TO BE ON TRACK 96
 DEC recals             ;          DEC RECALCNT   ; ONCE TOO MANY??
 BEQ drverr             ;          BEQ DRVERR     ; TRIED TO RECALIBRATE TOO
                        ;                           MANY TIMES, ERROR!
 LDA #4                 ;          LDA #MAXSEEKS  ; RESET THE
 STA seeks              ;          STA SEEKCNT    ; SEEK COUNTER

 LDA #$60               ; The instructions LDA #$60 and JSR SETTRK above have
 STA curtrk             ; been replaced by these two, which do the same thing
                        ; but without the more generalised code of the original

 LDA #0                 ;          LDA #$00
 JSR seek               ;          JSR MYSEEK     ; MOVE TO TRACK 00
                        ;          PLA

                        ; The first two instructions at RECAL (LDA CURTRK and
                        ; PHA) and the PLA instruction above have been replaced
                        ; by the LDA track instruction below, which do the same
                        ; thing

.trytr6

 LDA track              ; Fetch the track number into A

 JSR seek               ; RESEEK   JSR MYSEEK     ; GO TO CORRECT TRACK THIS
                        ;                           TIME!
 JMP trytr2             ;          JMP TRYTRK2    ; LOOP BACK, TRY AGAIN ON THIS
                        ;                           TRACK

; ******************************************************************************
;
;       Name: rdrght
;       Type: Subroutine
;   Category: Save and load
;    Summary: Check that this is the correct track
;
; ------------------------------------------------------------------------------
;
; This routine is almost identical to the RDRIGHT routine in Apple DOS 3.3.
; It omits the code that saves the destination track, as this is not required.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.rdrght

 LDY idfld+2            ; RDRIGHT  LDY TRACK      ; ON THE RIGHT TRACK?
 CPY track              ;          CPY CURTRK
 BEQ rttrk              ;          BEQ RTTRK      ; IF SO, GOOD
                        ; * NO, DRIVE WAS ON A DIFFERENT TRACK. TRY
                        ; * RESEEKING/RECALIBRATING FROM THIS TRACK
                        ;          LDA CURTRK     ; PRESERVE DESTINATION TRACK
                        ;          PHA
                        ;          TYA
                        ;          JSR SETTRK
                        ;          PLA
 DEC seeks              ;          DEC SEEKCNT    ; SHOULD WE RESEEK?
 BNE trytr6             ;          BNE RESEEK     ; =>YES, RESEEK
 BEQ trytr5             ;          BEQ RECAL      ; =>NO, RECALIBRATE!

; ******************************************************************************
;
;       Name: drverr
;       Type: Subroutine
;   Category: Save and load
;    Summary: Return from the RWTS code with a "Disk I/O error"
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   A                   The error number for the Disk I/O error (4)
;
;   C flag              The C flag is set
;
; ******************************************************************************

.drverr

 PLP                    ; Pull the read/write status off the stack as we don't
                        ; need it there any more

 LDY mtroff,X           ; Read the disk controller I/O soft switch at MOTOROFF
                        ; for slot X to turn the disk motor off

 SEC                    ; Set the C flag to denote that an error has occurred

 LDA #4                 ; Set A = 4 to return as the error number for the "Disk
                        ; I/O error"

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: rttrk
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read or write a sector on the current track
;
; ******************************************************************************

.rttrk

 LDY sector             ; Use the scttab lookup table to set A to the physical
 LDA scttab,Y           ; sector number of logical sector Y

 CMP idfld+1            ; If the physical sector number doesn't match the sector
 BNE trytr4             ; ID, jump to trytr4 to try reading the track again

 PLP                    ; Fetch the read/write status into the C flag from the
                        ; stack

 BCS rttrk2             ; If the C flag is set then we are writing a sector, so
                        ; jump to rttrk2 to write the sector to the disk

 JSR read               ; Otherwise we are reading a sector, so call the read
                        ; routine to read the current sector into the buffer at
                        ; buffr2, which will load the entire commander file as
                        ; it fits into one sector
                        ;
                        ; Note that this loads the file straight from disk, so
                        ; it is in the 6-bit nibble format

 PHP                    ; Store the status flags on the stack, so if we take the
                        ; following branch, the stack will be in the correct
                        ; state, with the read/write status on top

 BCS trytr4             ; If there was an error then the read routine will have
                        ; set the C flag, so if this is the case, jump to trytr4
                        ; to try reading the track again

 PLP                    ; Otherwise there was no error, so pull the status flags
                        ; back off the stack as we don't need them there any
                        ; more

 JSR pstnib             ; Call pstnib to convert the sector data that we just
                        ; read into 8-bit bytes, processing the 6-bit nibbles in
                        ; buffr2 into 8-bit bytes in buffer

 JMP rttrk3             ; Jump to rttrk3 to return from the RWTS code with no
                        ; error reported

.rttrk2

 JSR write              ; This does nothing except clear the C flag, as we do
                        ; not need to write anything to disk in the game loader

 LDA #1                 ; Set A = 1 to return as the error number for the "Disk
                        ; write protected" error

 BCS rttrk4             ; This beanch is never taken as the call to write clears
                        ; the C flag

                        ; Fall through into rttrk3 to successfully return from
                        ; the RWTS code with no error reported

; ******************************************************************************
;
;       Name: rttrk3
;       Type: Subroutine
;   Category: Save and load
;    Summary: Successfully return from the RWTS code with no error reported
;
; ------------------------------------------------------------------------------
;
; Returns:
;
;   A                   The error number for no error (0)
;
;   C flag              The C flag is clear
;
; ------------------------------------------------------------------------------
;
; Other entry points:
;
;   rttrk4              Turn off the disk motor and return from the RWTS code
;                       with the error number in A and the error status in the
;                       C flag
;
; ******************************************************************************

.rttrk3

 LDA #0                 ; Set A = 0 to indicate there is no error

 CLC                    ; Clear the C flag to indicate there is no disk error

.rttrk4

 PHA                    ; Store A on the stack so we can retrieve it below,
                        ; though this has no effect as A is not changed in the
                        ; following

 LDX slot16             ; Set X to the disk controller card slot number * 16

 LDY mtroff,X           ; Read the disk controller I/O soft switch at MOTOROFF
                        ; for slot X to turn the disk motor off

 PLA                    ; Retrieve A from the stack

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: read
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read a sector's worth of data into the buffr2 buffer
;
; ------------------------------------------------------------------------------
;
; This routine is identical to the READ16 routine in Apple DOS 3.3.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.read

 LDY #32                ; READ16   LDY #$20       ; 'MUST FIND' COUNT.

.read2

 DEY                    ; RSYNC    DEY IF         ; CAN'T FIND MARKS
 BEQ readE              ;          BEQ RDERR      ; THEN EXIT WITH CARRY SET.

.read3

 LDA Q6L,X              ; READ1    LDA Q6L,X      ; READ NIBL.
 BPL read3              ;          BPL READ1      ; *** NO PAGE CROSS! ***

.read4

 EOR #$D5               ; RSYNC1   EOR #$D5       ; DATA MARK 1?
 BNE read2              ;          BNE RSYNC      ; LOOP IF NOT.
 NOP                    ;          NOP DELAY      ; BETWEEN NIBLS.

.read5

 LDA Q6L,X              ; READ2    LDA Q6L,X
 BPL read5              ;          BPL READ2      ; *** NO PAGE CROSS! ***
 CMP #$AA               ;          CMP #$AA       ; DATA MARK 2?
 BNE read4              ;          BNE RSYNC1     ; (IF NOT, IS IT DM1?)
 LDY #$56               ;          LDY #$56       ; INIT NBUF2 INDEX.
                        ; *       (ADDED NIBL DELAY)

.read6

 LDA Q6L,X              ; READ3    LDA Q6L,X
 BPL read6              ;          BPL READ3      ; *** NO PAGE CROSS! ***
 CMP #$AD               ;          CMP #$AD       ; DATA MARK 3?
 BNE read4              ;          BNE RSYNC1     ; (IF NOT, IS IT DM1?)
                        ; *       (CARRY SET IF DM3!)
 LDA #0                 ;          LDA #$00       ; INIT CHECKSUM.

.read7

 DEY                    ; RDATA1   DEY
 STY ztemp0             ;          STY IDX

.read8

 LDY Q6L,X              ; READ4    LDY Q6L,X
 BPL read8              ;          BPL READ4      ; *** NO PAGE CROSS! ***
 EOR rtable-$96,Y       ;          EOR DNIBL,Y    ; XOR 6-BIT NIBL.
 LDY ztemp0             ;          LDY IDX
 STA buffr2+256,Y       ;          STA NBUF2,Y    ; STORE IN NBUF2 PAGE.
 BNE read7              ;          BNE RDATA1     ; TAKEN IF Y-REG NONZERO.

.read9

 STY ztemp0             ; RDATA2   STY IDX

.readA

 LDY Q6L,X              ; READ5    LDY Q6L,X
 BPL readA              ;          BPL READ5      ; *** NO PAGE CROSS! ***
 EOR rtable-$96,Y       ;          EOR DNIBL,Y    ; XOR 6-BIT NIBL.
 LDY ztemp0             ;          LDY IDX
 STA buffr2,Y           ;          STA NBUF1,Y    ; STORE IN NBUF1 PAGE.
 INY                    ;          INY
 BNE read9              ;          BNE RDATA2

.readB

 LDY Q6L,X              ; READ6    LDY Q6L,X      ; READ 7-BIT CSUM NIBL.
 BPL readB              ;          BPL READ6      ; *** NO PAGE CROSS! ***
 CMP rtable-$96,Y       ;          CMP DNIBL,Y    ; IF LAST NBUF1 NIBL NOT
 BNE readE              ;          BNE RDERR      ; EQUAL CHKSUM NIBL THEN ERR.

.readC

 LDA Q6L,X              ; READ7    LDA Q6L,X
 BPL readC              ;          BPL READ7      ; *** NO PAGE CROSS! ***
 CMP #$DE               ;          CMP #$DE       ; FIRST BIT SLIP MARK?
 BNE readE              ;          BNE RDERR      ; (ERR IF NOT)
 NOP                    ;          NOP DELAY      ; BETWEEN NIBLS.

.readD

 LDA Q6L,X              ; READ8    LDA Q6L,X
 BPL readD              ;          BPL READ8      ; *** NO PAGE CROSS! ***
 CMP #$AA               ;          CMP #$AA       ; SECOND BIT SLIP MARK?
 BEQ readF              ;          BEQ RDEXIT     ; (DONE IF IT IS)

.readE

 SEC                    ; RDERR    SEC INDICATE   ; 'ERROR EXIT'.
 RTS                    ;          RTS RETURN     ; FROM READ16 OR RDADR16.

.readF

 CLC                    ; RDEXIT   CLC CLEAR      ; CARRY ON
 RTS                    ;          RTS NORMAL     ; READ EXITS.

; ******************************************************************************
;
;       Name: write
;       Type: Subroutine
;   Category: Save and load
;    Summary: Write a sector's worth of data from the buffr2 buffer to the
;             current track and sector
;
; ------------------------------------------------------------------------------
;
; This routine does nothing except clear the C flag to indicate success, as we
; do not need to write to disk in the game loader.
;
; ******************************************************************************

.write

 CLC                    ; Clear the C flag to indicate success

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: rdaddr
;       Type: Subroutine
;   Category: Save and load
;    Summary: Read a track address field
;
; ------------------------------------------------------------------------------
;
; This routine is identical to the RDADR16 routine in Apple DOS 3.3.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.rdaddr

 LDY #$FC               ; RDADR16  LDY #$FC
 STY ztemp0             ;          STY COUNT      ; 'MUST FIND' COUNT.

.rdadr2

 INY                    ; RDASYN   INY
 BNE rdadr3             ;          BNE RDA1       ; LOW ORDER OF COUNT.
 INC ztemp0             ;          INC COUNT      ; (2K NIBLS TO FIND
 BEQ rdadrD             ;          BEQ RDERR      ; ADR MARK, ELSE ERR)

.rdadr3

 LDA Q6L,X              ; RDA1     LDA Q6L,X      ; READ NIBL.
 BPL rdadr3             ;          BPL RDA1       ; *** NO PAGE CROSS! ***

.rdadr4

 CMP #$D5               ; RDASN1   CMP #$D5       ; ADR MARK 1?
 BNE rdadr2             ;          BNE RDASYN     ; (LOOP IF NOT)
 NOP                    ;          NOP ADDED      ; NIBL DELAY.

.rdadr5

 LDA Q6L,X              ; RDA2     LDA Q6L,X
 BPL rdadr5             ;          BPL RDA2       ; *** NO PAGE CROSS! ***
 CMP #$AA               ;          CMP #$AA       ; ADR MARK 2?
 BNE rdadr4             ;          BNE RDASN1     ; (IF NOT, IS IT AM1?)
 LDY #3                 ;          LDY #$3        ; INDEX FOR 4-BYTE READ.
                        ; *       (ADDED NIBL DELAY)

.rdadr6

 LDA Q6L,X              ; RDA3     LDA Q6L,X
 BPL rdadr6             ;          BPL RDA3       ; *** NO PAGE CROSS! ***
 CMP #$96               ;          CMP #$96       ; ADR MARK 3?
 BNE rdadr4             ;          BNE RDASN1     ; (IF NOT, IS IT AM1?)
                        ; *       (LEAVES CARRY SET!)
 LDA #0                 ;          LDA #$0        ; INIT CHECKSUM.

.rdadr7

 STA ztemp1             ; RDAFLD   STA CSUM

.rdadr8

 LDA Q6L,X              ; RDA4     LDA Q6L,X      ; READ 'ODD BIT' NIBL.
 BPL rdadr8             ;          BPL RDA4       ; *** NO PAGE CROSS! ***
 ROL A                  ;          ROL A          ; ALIGN ODD BITS, '1' INTO
                        ;                           LSB.
 STA ztemp0             ;          STA LAST       ; (SAVE THEM)

.rdadr9

 LDA Q6L,X              ; RDA5     LDA Q6L,X      ; READ 'EVEN BIT' NIBL.
 BPL rdadr9             ;          BPL RDA5       ; *** NO PAGE CROSS! ***
 AND ztemp0             ;          AND LAST       ; MERGE ODD AND EVEN BITS.
 STA idfld,Y            ;          STA CSSTV,Y    ; STORE DATA BYTE.
 EOR ztemp1             ;          EOR CSUM       ; XOR CHECKSUM.
 DEY                    ;          DEY
 BPL rdadr7             ;          BPL RDAFLD     ; LOOP ON 4 DATA BYTES.
 TAY                    ;          TAY IF         ; FINAL CHECKSUM
 BNE rdadrD             ;          BNE RDERR      ; NONZERO, THEN ERROR.

.rdadrA

 LDA Q6L,X              ; RDA6     LDA Q6L,X      ; FIRST BIT-SLIP NIBL.
 BPL rdadrA             ;          BPL RDA6       ; *** NO PAGE CROSS! ***
 CMP #$DE               ;          CMP #$DE
 BNE rdadrD             ;          BNE RDERR      ; ERROR IF NONMATCH.
 NOP                    ;          NOP DELAY      ; BETWEEN NIBLS.

.rdadrB

 LDA Q6L,X              ; RDA7     LDA Q6L,X      ; SECOND BIT-SLIP NIBL.
 BPL rdadrB             ;          BPL RDA7       ; *** NO PAGE CROSS! ***
 CMP #$AA               ;          CMP #$AA
 BNE rdadrD             ;          BNE RDERR      ; ERROR IF NONMATCH.

.rdadrC

 CLC                    ; RDEXIT   CLC CLEAR      ; CARRY ON
 RTS                    ;          RTS NORMAL     ; READ EXITS.

.rdadrD

 SEC                    ; RDERR    SEC INDICATE   ; 'ERROR EXIT'.
 RTS                    ;          RTS RETURN     ; FROM READ16 OR RDADR16.

; ******************************************************************************
;
;       Name: seek
;       Type: Subroutine
;   Category: Save and load
;    Summary: Fast seek routine
;
; ------------------------------------------------------------------------------
;
; This routine is almost identical to the SEEK routine in Apple DOS 3.3. There
; is one extra instruction and one moved instruction when compared to the
; original DOS.
;
; These extra instructions double the track number in A.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ------------------------------------------------------------------------------
;
; Arguments:
;
;   A                   The track number
;
; ******************************************************************************

.seek

 STX ztemp0             ; SEEK     STX SLOTTEMP   ; SAVE X-REG
                        ;          STA TRKN       ; SAVE TARGET TRACK

 ASL A                  ; This is an extra instruction that doubles the track
                        ; number in A

 CMP curtrk             ;          CMP CURTRK     ; ON DESIRED TRACK?
 BEQ step3              ;          BEQ SEEKRTS    ; YES, RETURN

 STA ztemp1             ; This is the second instruction from above, which has
                        ; been moved here (STA TRKN)
                        ;
                        ; This saves the now-doubled track number in ztemp1

 LDA #0                 ;          LDA #$0
 STA ztemp2             ;          STA TRKCNT     ; HALFTRACK COUNT.

.seek2

 LDA curtrk             ; SEEK2    LDA CURTRK     ; SAVE CURTRK FOR
 STA ztemp3             ;          STA PRIOR      ; DELAYED TURNOFF.
 SEC                    ;          SEC
 SBC ztemp1             ;          SBC TRKN       ; DELTA-TRACKS.
 BEQ seek7              ;          BEQ SEEKEND    ; BR IF CURTRK=DESTINATION
 BCS seek3              ;          BCS OUT        ; (MOVE OUT, NOT IN)
 EOR #$FF               ;          EOR #$FF       ; CALC TRKS TO GO.
 INC curtrk             ;          INC CURTRK     ; INCR CURRENT TRACK (IN).
 BCC seek4              ;          BCC MINTST     ; (ALWAYS TAKEN)

.seek3

 ADC #$FE               ; OUT      ADC #$FE       ; CALC TRKS TO GO.
 DEC curtrk             ;          DEC CURTRK     ; DECR CURRENT TRACK (OUT).

.seek4

 CMP ztemp2             ; MINTST   CMP TRKCNT
 BCC seek5              ;          BCC MAXTST     ; AND 'TRKS MOVED'.
 LDA ztemp2             ;          LDA TRKCNT

.seek5

 CMP #12                ; MAXTST   CMP #$C
 BCS seek6              ;          BCS STEP2      ; IF TRKCNT>$B LEAVE Y ALONE
                        ;                           (Y=$B).
 TAY                    ; STEP     TAY            ; ELSE SET ACCELERATION INDEX
                        ;                           IN Y

.seek6                  ; STEP2    EQU *

 SEC                    ;          SEC            ; CARRY SET=PHASE ON
 JSR step               ;          JSR SETPHASE   ; PHASE ON
 LDA armtab,Y           ;          LDA ONTABLE,Y  ; FOR 'ONTIME'.
 JSR armwat             ;          JSR MSWAIT     ; (100 USEC INTERVALS)
                        ; *
 LDA ztemp3             ;          LDA PRIOR
 CLC                    ;          CLC            ; CARRY CLEAR=PHASE OFF
 JSR step2              ;          JSR CLRPHASE   ; PHASE OFF
 LDA armtb2,Y           ;          LDA OFFTABLE,Y ; THEN WAIT 'OFFTIME'.
 JSR armwat             ;          JSR MSWAIT     ; (100 USEC INTERVALS)
 INC ztemp2             ;          INC TRKCNT     ; 'TRACKS MOVED' COUNT.
 BNE seek2              ;          BNE SEEK2      ; (ALWAYS TAKEN)
                        ; *
.seek7                  ; SEEKEND  EQU *          ; END OF SEEKING

 JSR armwat             ;          JSR MSWAIT     ; A=0: WAIT 25 MS SETTLE
 CLC                    ;          CLC            ; AND TURN OFF PHASE
                        ; *
                        ; * TURN HEAD STEPPER PHASE ON/OFF
                        ; *

.step                   ; SETPHASE EQU *

 LDA curtrk             ;          LDA CURTRK     ; GET CURRENT PHASE

.step2                  ; CLRPHASE EQU *

 AND #3                 ;          AND #3         ; MASK FOR 1 OF 4 PHASES
 ROL A                  ;          ROL A          ; DOUBLE FOR PHASE INDEX
 ORA ztemp0             ;          ORA SLOTTEMP
 TAX                    ;          TAX
 LDA phsoff,X           ;          LDA PHASEOFF,X ; FLIP THE PHASE
 LDX ztemp0             ;          LDX SLOTTEMP   ; RESTORE X-REG

.step3

 RTS                    ; SEEKRTS  RTS            ; AND RETURN

; ******************************************************************************
;
;       Name: armwat
;       Type: Subroutine
;   Category: Save and load
;    Summary: Implement the arm move delay
;
; ------------------------------------------------------------------------------
;
; This routine is identical to the MSWAIT routine in Apple DOS 3.3.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.armwat

 LDX #17                ; MSWAIT   LDX #$11

.armwt2

 DEX                    ; MSW1     DEX DELAY      ; 86 USEC.
 BNE armwt2             ;          BNE MSW1
 INC mtimel             ;          INC MONTIMEL
 BNE armwt3             ;          BNE MSW2       ; DOUBLE-BYTE
 INC mtimeh             ;          INC MONTIMEH   ; INCREMENT.

.armwt3

 SEC                    ; MSW2     SEC
 SBC #1                 ;          SBC #$1        ; DONE 'N' INTERVALS?
 BNE armwat             ;          BNE MSWAIT     ; (A-REG COUNTS)
 RTS                    ;          RTS

; ******************************************************************************
;
;       Name: armtab
;       Type: Variable
;   Category: Save and load
;    Summary: Phase-on time table in 100-usec intervals
;
; ------------------------------------------------------------------------------
;
; This table is identical to the ONTABLE table in Apple DOS 3.3.
;
; The original DOS 3.3 source code for this table in is shown in the comments.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.armtab

 EQUB 1                 ; ONTABLE  DFB 1,$30,$28
 EQUB $30               ;          DFB $24,$20,$1E
 EQUB $28               ;          DFB $1D,$1C,$1C
 EQUB $24               ;          DFB $1C,$1C,$1C
 EQUB $20
 EQUB $1E
 EQUB $1D
 EQUB $1C
 EQUB $1C
 EQUB $1C
 EQUB $1C
 EQUB $1C

; ******************************************************************************
;
;       Name: armtb2
;       Type: Variable
;   Category: Save and load
;    Summary: Phase-off time table in 100-usec intervals
;
; ------------------------------------------------------------------------------
;
; This table is identical to the OFFTABLE table in Apple DOS 3.3.
;
; The original DOS 3.3 source code for this table in is shown in the comments.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.armtb2

 EQUB $70               ; OFFTABLE DFB $70,$2C,$26
 EQUB $2C               ;          DFB $22,$1F,$1E
 EQUB $26               ;          DFB $1D,$1C,$1C
 EQUB $22               ;          DFB $1C,$1C,$1C
 EQUB $1F
 EQUB $1E
 EQUB $1D
 EQUB $1C
 EQUB $1C
 EQUB $1C
 EQUB $1C
 EQUB $1C

; ******************************************************************************
;
;       Name: prenib
;       Type: Subroutine
;   Category: Save and load
;    Summary: Convert 256 8-bit bytes in buffer into 342 6-bit nibbles in buffr2
;
; ------------------------------------------------------------------------------
;
; This routine does nothing, as we do not need to write to disk in the game
; loader.
;
; ******************************************************************************

.prenib

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: pstnib
;       Type: Subroutine
;   Category: Save and load
;    Summary: Convert 342 6-bit nibbles in buffr2 into 256 8-bit bytes in buffer
;
; ------------------------------------------------------------------------------
;
; This routine is almost identical to the POSTNB16 routine in Apple DOS 3.3. The
; CPY T0 instruction from the original source is omitted as we only need to
; check whether the byte counter in Y has reached zero.
;
; For a detailed look at how DOS works, see the book "Beneath Apple DOS" by Don
; Worth and Pieter Lechner. In particular, see chapter 4 for the layout of the
; VTOC, catalog sector, file entry and file/track list.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.pstnib

 LDY #0                 ; POSTNB16 LDY #0         ; USER DATA BUF IDX.

.pstnb2

 LDX #$56               ; POST1    LDX #$56       ; INIT NBUF2 INDEX.

.pstnb3

 DEX                    ; POST2    DEX NBUF       ; IDX $55 TO $0.
 BMI pstnb2             ;          BMI POST1      ; WRAPAROUND IF NEG.
 LDA buffr2,Y           ;          LDA NBUF1,Y
 LSR buffr2+256,X       ;          LSR NBUF2,X    ; SHIFT 2 BITS FROM
 ROL A                  ;          ROL A          ; CURRENT NBUF2 NIBL
 LSR buffr2+256,X       ;          LSR NBUF2,X    ; INTO CURRENT NBUF1
 ROL A                  ;          ROL A          ; NIBL.
 STA buffer,Y           ;          STA (BUF),Y    ; BYTE OF USER DATA.
 INY                    ;          INY NEXT       ; USER BYTE.
                        ;          CPY T0         ; DONE IF EQUAL T0.
 BNE pstnb3             ;          BNE POST2
 RTS                    ;          RTS RETURN.

; ******************************************************************************
;
;       Name: wbyte
;       Type: Subroutine
;   Category: Save and load
;    Summary: Write one byte to disk
;
; ------------------------------------------------------------------------------
;
; This routine does nothing, as we do not need to write to disk in the game
; loader.
;
; ******************************************************************************

.wbyte

 RTS

 RTS                    ; These instructions are not used
 RTS

; ******************************************************************************
;
;       Name: scttab
;       Type: Variable
;   Category: Save and load
;    Summary: Lookup table to translate logical (requested) sector number to
;             physical sector number
;
; ------------------------------------------------------------------------------
;
; This table is identical to the INTRLEAV table in Apple DOS 3.3.
;
; The original DOS 3.3 source code for this table in is shown in the comments.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.scttab                 ; INTRLEAV EQU *

 EQUD $090B0D00         ;          DFB $00,$0D,$0B,$09
 EQUD $01030507         ;          DFB $07,$05,$03,$01
 EQUD $080A0C0E         ;          DFB $0E,$0C,$0A,$08
 EQUD $0F020406         ;          DFB $06,$04,$02,$0F

 NOP                    ; This instruction is not used

; ******************************************************************************
;
;       Name: rtable
;       Type: Variable
;   Category: Save and load
;    Summary: 64 disk nibbles of "6-and-2" Read Translate Table
;
; ------------------------------------------------------------------------------
;
; This table is identical to the table at address $3A96 in Apple DOS 3.3. The
; table doesn't have a label in the original source.
;
; The original DOS 3.3 source code for this table in is shown in the comments.
;
; Elite uses different label names to the original DOS 3.3 source, but the code
; is the same.
;
; This code forms part of the RWTS ("read/write track sector") layer from Apple
; DOS, which was written by Randy Wigginton and Steve Wozniak. It implements the
; low-level functions to read and write Apple disks, and is included in Elite so
; the game can use the memory that's normally allocated to DOS for its own use.
;
; ******************************************************************************

.rtable

 EQUD $99980100         ;          DFB $00,$01,$98
 EQUD $049C0302         ;          DFB $99,$02,$03
 EQUD $A1A00605         ;          DFB $9C,$04,$05
 EQUD $A5A4A3A2         ;          DFB $06,$A0,$A1
 EQUD $A9A80807         ;          DFB $A2,$A3,$A4
 EQUD $0B0A09AA         ;          DFB $A5,$07,$08
 EQUD $B1B00D0C         ;          DFB $A8,$A9,$AA
 EQUD $11100F0E         ;          DFB $09,$0A,$0B
 EQUD $14B81312         ;          DFB $0C,$0D,$B0
 EQUD $18171615         ;          DFB $B1,$0E,$0F
 EQUD $C1C01A19         ;          DFB $10,$11,$12
 EQUD $C5C4C3C2         ;          DFB $13,$B8,$14
 EQUD $C9C8C7C6         ;          DFB $15,$16,$17
 EQUD $1CCC1BCA         ;          DFB $18,$19,$1A
 EQUD $D1D01E1D         ;          DFB $C0,$C1,$C2
 EQUD $D5D41FD2         ;          DFB $C3,$C4,$C5
 EQUD $22D82120         ;          DFB $C6,$C7,$C8
 EQUD $26252423         ;          DFB $C9,$CA,$1B
 EQUD $E1E02827         ;          DFB $CC,$1C,$1D
 EQUD $29E4E3E2         ;          DFB $1E,$D0,$D1
 EQUD $2CE82B2A         ;          DFB $D2,$1F,$D4
 EQUD $302F2E2D         ;          DFB $D5,$20,$21
 EQUD $F1F03231         ;          DFB $D8,$22,$23
 EQUD $36353433         ;          DFB $24,$25,$26
 EQUD $39F83837         ;          DFB $27,$28,$E0
 EQUD $3D3C3B3A         ;          DFB $E1,$E2,$E3
 EQUW $3F3E             ;          DFB $E4,$29,$2A
                        ;          DFB $2B,$E8,$2C
                        ;          DFB $2D,$2E,$2F
                        ;          DFB $30,$31,$32
                        ;          DFB $F0,$F1,$33
                        ;          DFB $34,$35,$36
                        ;          DFB $37,$38,$F8
                        ;          DFB $39,$3A,$3B
                        ;          DFB $3C,$3D,$3E
                        ;          DFB $3F

; ******************************************************************************
;
;       Name: LoadFile
;       Type: Subroutine
;   Category: Loader
;    Summary: Load a multi-sector file
;
; ******************************************************************************

.LoadFile

 LDA #12                ; Set tslIndex = 12, so we can use this as an index into
 STA tslIndex           ; the track/sector list for the file we want to load,
                        ; starting with the index of the first track/sector pair
                        ; in byte #12

 JSR findf              ; Search the disk catalog for a file with the filename
                        ; in comnam

 BCS load4              ; If no file is found with this name then findf will
                        ; set the C flag, so jump to load4 to return from the
                        ; subroutine as the file cannot be found

 JSR gettsl             ; Get the track/sector list of the file and populate the
                        ; track and sector variables with the track and sector
                        ; of the file's contents, to pass to the call to rsect

 JSR CopyTrackSector    ; Copy the track/sector list from the buffer into
                        ; trackSector so we can work our way through it to load
                        ; the file one sector at a time

.load1

 JSR rsect              ; Read the first sector of the file's data into the
                        ; buffer (or the next sector if we loop back from below)

 LDY skipBytes          ; Set Y to the number of bytes to skip from the start of
                        ; the file, so we can use it as a starting index into
                        ; the first sector that we copy

 LDX #0                 ; Set X = 0 to act as a byte index into the destination
                        ; address for the file

.load2

 LDA buffer,Y           ; Set A to the Y-th byte in the buffer

.loadAddr

 STA $4000,X            ; And copy it to the X-th byte of the load address,
                        ; which by this point has been modified to the correct
                        ; load address by the SetLoadVariables1 or
                        ; SetLoadVariables2 routine

 JSR DecrementFileSize  ; Decrement the file size in fileSize(1 0) by 1 as we
                        ; have just loaded one more byte of the file

 BMI load4              ; If the result is negative then we have copied all
                        ; fileSize(1 0) bytes, so jump to load4 to return from
                        ; the subroutine

 INX                    ; Increment the destination index in X

 INY                    ; Increment the source index in Y

 BNE load2              ; Loop back until we have reached the end of the page
                        ; page in the source index

 INC loadAddr+2         ; Update the load address as follows:
 LDA loadAddr+1         ;
 SEC                    ;   loadAddr = loadAddr + 256 - skipBytes
 SBC skipBytes          ;
 STA loadAddr+1         ; This makes sure that loadAddr points to the correct
 LDA loadAddr+2         ; load address for the next sector, as we just loaded a
 SBC #0                 ; sector's worth of bytes (256) and skipped the first
 STA loadAddr+2         ; skipBytes bytes

 LDA skipBytes          ; Set skipBytes = 0 so we don't skip any bytes from the
 BEQ load3              ; remaining sectors (as we only want to skip bytes from
 LDA #0                 ; the first sector)
 STA skipBytes

.load3

 INC tslIndex           ; Set tslIndex = tslIndex + 2 to point to the next track
 INC tslIndex           ; and sector pair in the track/sector list

 LDY tslIndex           ; Set track to the track number from the next pair in
 LDA trackSector,Y      ; the track/sector list
 STA track

 LDA trackSector+1,Y    ; Set sector to the sector number from the next pair in
 STA sector             ; the track/sector list

 BNE load1              ; If either the track or sector number is non-zero, loop
 LDA track              ; back to load1 to load the next sector
 BNE load1

.load4

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: CopyTrackSector
;       Type: Subroutine
;   Category: Loader
;    Summary: Copy the track/sector list from the disk buffer to trackSector
;
; ******************************************************************************

.CopyTrackSector

 LDY #0                 ; Set Y = 0 to use as a byte counter

.cpts1

 LDA buffer,Y           ; Copy the Y-th byte of buffer to the Y-th byte of
 STA trackSector,Y      ; trackSector

 INY                    ; Increment the byte counter

 BNE cpts1              ; Loop back until we have copied a whole page of data

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: DecrementFileSize
;       Type: Subroutine
;   Category: Loader
;    Summary: Decrement the file size in fileSize(1 0) by 1
;
; ******************************************************************************

.DecrementFileSize

 LDA fileSize           ; Set fileSize(1 0) = fileSize(1 0) - 1
 SEC
 SBC #1
 STA fileSize
 LDA fileSize+1
 SBC #0
 STA fileSize+1

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: SetLoadVariables2
;       Type: Subroutine
;   Category: Loader
;    Summary: Configure the file load variables for loading the ELB1 file
;
; ******************************************************************************

.SetLoadVariables2

 LDA #4                 ; Set skipBytes = 4 so we skip the first four bytes of
 STA skipBytes          ; the file (as these contain the program start address
                        ; and file length)

 LDA #$FF               ; Set fileSize(1 0) = $4FFF
 STA fileSize
 LDA #$4F
 STA fileSize+1

 LDA #0                 ; Set trackSector = 0 (though this appears to have no
 STA trackSector        ; effect, as it isn't checked anywhere and the first
                        ; byte of the track/sector list is unused)

 LDA #$00               ; Modify the instruction at loadAddr to STA $4000,X
 STA loadAddr+1
 LDA #$40
 STA loadAddr+2

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: SetLoadVariables1
;       Type: Subroutine
;   Category: Loader
;    Summary: Configure the file load variables for loading the SCRN file
;
; ******************************************************************************

.SetLoadVariables1

 LDA #4                 ; Set skipBytes = 4 so we skip the first four bytes of
 STA skipBytes          ; the file (as these contain the program start address
                        ; and file length)

 LDA #$80               ; Set fileSize(1 0) = $0880
 STA fileSize
 LDA #$08
 STA fileSize+1

 LDA #0                 ; Set trackSector = 0 (though this appears to have no
 STA trackSector        ; effect, as it isn't checked anywhere and the first
                        ; byte of the track/sector list is unused)

 LDA #$00               ; Modify the instruction at loadAddr to STA $0200,X
 STA loadAddr+1
 LDA #$02
 STA loadAddr+2

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: SetFilename
;       Type: Subroutine
;   Category: Loader
;    Summary: Set the filename in comnam to ELB1
;
; ******************************************************************************

.SetFilename

 LDY #0                 ; Set Y = 0 to use as a character counter

.name1

 LDA filename,Y         ; Copy the Y-th character of filename to the Y-th
 STA comnam,Y           ; character of comnam

 INY                    ; Increment the character counter

 CPY #4                 ; Loop back until we have copied all four characters of
 BNE name1              ; filename to comnam

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
;       Name: AllowJoystick
;       Type: Subroutine
;   Category: Loader
;    Summary: An unused routine to check the joysticks and configure the title
;             screen to check for the joystick fire button
;
; ******************************************************************************

.AllowJoystick

 LDA #0                 ; Set fireButtonMask to 0 to prevent the joystick fire
 STA fireButtonMask     ; button from being able to select joysticks on the
                        ; title screen

 LDA $C064              ; Set A to the value of the soft switch containing the
                        ; status of the joystick 0 x-axis (GC0)

 CMP #$FF               ; If it is not $FF then jump to joys1 to start the game
 BNE joys1              ; with the fire button disabled

 LDA $C065              ; Set A to the value of the soft switch containing the
                        ; status of the joystick 0 y-axis (GC1)

 CMP #$FF               ; If it is not $FF then jump to joys1 to start the game
 BNE joys1              ; with the fire button disabled

 LDA $C066              ; Set A to the value of the soft switch containing the
                        ; status of the joystick 1 x-axis (GC2)

 CMP #$FF               ; If it is not $FF then jump to joys1 to start the game
 BNE joys1              ; with the fire button disabled

 LDA $C067              ; Set A to the value of the soft switch containing the
                        ; status of the joystick 1 y-axis (GC3)

 CMP #$FF               ; If it is not $FF then jump to joys1 to start the game
 BNE joys1              ; with the fire button disabled

                        ; If we get here then all four joystick soft switches
                        ; are returning $FF

 LDA #$FF               ; Set fireButtonMask to $FF to allow the joystick fire
 STA fireButtonMask     ; button to select joysticks on the title screen

.joys1

 JMP startGame          ; Jump to startGame to start the game

; ******************************************************************************
;
;       Name: fileSize
;       Type: Variable
;   Category: Loader
;    Summary: The file size of the file we are loading
;
; ******************************************************************************

.fileSize

 EQUW $4FFF

; ******************************************************************************
;
;       Name: skipBytes
;       Type: Variable
;   Category: Loader
;    Summary: The number of bytes to skip at the start of the file that we are
;             loading
;
; ******************************************************************************

.skipBytes

 EQUB 4

; ******************************************************************************
;
;       Name: tslIndex
;       Type: Variable
;   Category: Loader
;    Summary: The index of the current entry in the track/sector list, as we
;             work our way through the list
;
; ******************************************************************************

.tslIndex

 EQUB 0

; ******************************************************************************
;
;       Name: trackSector
;       Type: Variable
;   Category: Loader
;    Summary: The track/sector list for the file we are loading
;
; ******************************************************************************

.trackSector

 EQUB 0

; ******************************************************************************
;
;       Name: CopyCode2
;       Type: Subroutine
;   Category: Loader
;    Summary: Copy CODE2 from $4000-$6FFF to $9000-$BFFF
;
; ******************************************************************************

.CopyCode2

 LDA #$00               ; Set fromAddr(1 0) = $4000
 STA fromAddr
 LDA #$40
 STA fromAddr+1

 LDA #$00               ; Set toAddr(1 0) = $9000
 STA toAddr
 LDA #$90
 STA toAddr+1

 LDX #$30               ; Set X = $30 to use as a page counter, so we copy
                        ; $4000-$6FFF to $9000-$BFFF

 LDY #0                 ; Set Y = 0 to use as a byte counter

.copy1

 LDA (fromAddr),Y       ; Copy the Y-th byte of fromAddr(1 0) to the Y-th byte
 STA (toAddr),Y         ; of toAddr(1 0)

 INY                    ; Increment the byte counter

 BNE copy1              ; Loop back until we have copied a whole page of bytes

 INC fromAddr+1         ; Increment the high bytes of fromAddr(1 0) and
 INC toAddr+1           ; toAddr(1 0) so they point to the next page in memory

 DEX                    ; Decrement the page counter

 BNE copy1              ; Loop back until we have copied X pages

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
; Save SEC3.bin
;
; ******************************************************************************

 PRINT "P% = ", ~P%
 SAVE "3-assembled-output/SEC3.bin", CODE%, P%, LOAD%
