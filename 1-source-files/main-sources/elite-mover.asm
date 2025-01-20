; ******************************************************************************
;
; APPLE II ELITE MOVER SOURCE
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
;   * MOVER.bin
;
; ******************************************************************************

 INCLUDE "1-source-files/main-sources/elite-build-options.asm"

 _IB_DISK                   = (_VARIANT = 1)
 _SOURCE_DISK_BUILD         = (_VARIANT = 2)
 _SOURCE_DISK_CODE_FILES    = (_VARIANT = 3)
 _SOURCE_DISK_ELT_FILES     = (_VARIANT = 4)
 _SOURCE_DISK               = (_VARIANT = 2) OR (_VARIANT = 3) OR (_VARIANT = 4)

; ******************************************************************************
;
; Configuration variables
;
; ******************************************************************************

 CODE% = $0300          ; The address where the code will be run

 LOAD% = $0300          ; The address where the code will be loaded

IF _IB_DISK

 BEGIN = $7EB9          ; The address of the BEGIN routine in the main game code

 TT170 = $7ED2          ; The address of the TT170 routine in the main game code

ELIF _SOURCE_DISK

 BEGIN = $7EAB          ; The address of the BEGIN routine in the main game code

 TT170 = $7EC4          ; The address of the TT170 routine in the main game code

ENDIF

 C% = $9000             ; C% is set to the location that the main game code gets
                        ; moved to after it is loaded

 L% = $D000             ; L% is the load address of the main game code file

; ******************************************************************************
;
;       Name: ZP
;       Type: Workspace
;    Address: $0000 to $0004
;   Category: Workspaces
;    Summary: Important variables used by the loader
;
; ******************************************************************************

 ORG $0000

.ZP

 SKIP 2                 ; Stores addresses used for moving content around

.P

 SKIP 2                 ; Stores addresses used for moving content around

; ******************************************************************************
;
; ELITE MOVER
;
; ******************************************************************************

 ORG CODE%

; ******************************************************************************
;
;       Name: Mover
;       Type: Subroutine
;   Category: Loader
;    Summary: Move the game code from its load address to the address where it
;             will be run
;
; ******************************************************************************

.Mover

 STA $C080              ; Set ROM bank 2 to read from RAM and not write to RAM
                        ; by accessing the READBSR2 soft switch, with bit 3
                        ; clear (bank 2), bit 1 clear (read RAM) and bit 0 clear
                        ; (do not write to RAM)

 LDY #0                 ; Set the source and destination addresses for the copy:
 STY ZP                 ;
 STY P                  ;   ZP(1 0) = L% = $D000
 LDA #HI(L%)            ;   P(1 0) = C% = $9000
 STA ZP+1               ;
 LDA #HI(C%)            ; and set Y = 0 to act as a byte counter in the
 STA P+1                ; following loop

.MVDL

 LDA (ZP),Y             ; Copy the Y-th byte from the source to the Y-th byte of
 STA (P),Y              ; the destination

 INY                    ; Increment the byte counter

 BNE MVDL               ; Loop back until we have copied a whole page of bytes

 INC ZP+1               ; Increment the high bytes of ZP(1 0) and P(1 0) so we
 INC P+1                ; copy bytes from the next page in memory

 LDA P+1                ; Loop back until the P(1 0) = $C000
 CMP #$C0
 BCC MVDL

 STA $C082              ; Set ROM bank 2 to read from ROM and not write to RAM
                        ; by accessing the OFFBSR2 soft switch, with bit 3 clear
                        ; (bank 2), bit 1 set (read ROM) and bit 0 clear (do
                        ; not write to RAM)

IF _MAX_COMMANDER

 JMP BEGIN              ; Jump to BEGIN to run the game from just after the
                        ; call to JAMESON, so the maxed-out commander file
                        ; doesn't get overwritten
                        ;
                        ; The standard mover program on the game disk from Ian
                        ; Bell's site jumps to TT170 to start the game, but we
                        ; want to ensure that the maxed-out commander in NA2% is
                        ; copied into the current commander slot by the JAMESON
                        ; routine, which is called by the the BEGIN routine
                        ; before it falls through into TT170 to run the game

ELSE

 JMP TT170              ; Jump to TT170 to run the game from the main entry
                        ; point

ENDIF

 NOP                    ; This instruction has no effect

; ******************************************************************************
;
; Save MOVER.bin
;
; ******************************************************************************

 PRINT "P% = ", ~P%
 SAVE "3-assembled-output/MOVER.bin", CODE%, P%, LOAD%
