; ******************************************************************************
;
; APPLE II ELITE TRANSFER PROGRAM SOURCE
;
; Apple II Elite was written by Ian Bell and David Braben and is copyright
; D. Braben and I. Bell 1986
;
; The code in this file is identical to the source disks released on Ian Bell's
; personal website at http://www.elitehomepage.org/ (it's just been reformatted
; to be more readable)
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
; This source file produces game binaries that are suitable for transfer to an
; Apple II that's attached to a BBC Micro. The same files are also included on
; the Firebird game disk, with the transfer code still embedded.
;
; For file transers, the BASIC program S.APMAKES on the source disk compiles
; this code into the ELA and ELB binaries, which it then sends it to the Apple
; using a *APPLE command (this latter utility is not included on the disk).
; This source replicates the functionality in S.APMAKES that produces the ELA
; and ELB files, but it stops short of the actual transfer process.
;
; It forms part of the PDS (Programmers' Development System) that was used when
; developing Apple II Elite on a BBC Micro.
;
; ------------------------------------------------------------------------------
;
; This source file produces the following binary files:
;
;   * ELA.bin
;   * ELB.bin
;
; after reading in the following files:
;
;   * DATA.bin
;   * SCREEN.bin
;   * CODE2.bin
;   * CODE1.bin
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

 CODE% = $0A00          ; The address where the transfer code is run

 LOAD% = $0A00          ; The address where the transfer code is loaded

 CODE2 = $4000          ; The address where the main game code file is loaded

 STORE = $D000          ; The address where the main game code file is run

; ******************************************************************************
;
;       Name: ZP
;       Type: Workspace
;    Address: $0018 to $001B
;   Category: Workspaces
;    Summary: Important variables used by the loader
;
; ******************************************************************************

 ORG $0000

.ZP1

 SKIP 2                 ; Stores addresses used for moving content around

.ZP2

 SKIP 2                 ; Stores addresses used for moving content around

; ******************************************************************************
;
; ELITE TRANSFER
;
; ******************************************************************************

 ORG CODE%

; ******************************************************************************
;
;       Name: ENTRY
;       Type: Subroutine
;   Category: Utility routines
;    Summary: Copy the second block of the game code, between CODE2 and STORE,
;             into bank-switched RAM at $D000
;
; ------------------------------------------------------------------------------
;
; This transfer program can be found on the source disk on Ian Bell's site. It
; packs the entire game binary into memory, ready to be transmitted to an Apple
; II computer connected to the BBC Micro's User Port.
;
; This code is run in the released variants of the game, but it doesn't have any
; effect (apart from slowing down the load process). In the source disk variant,
; it forms part of the development process.
;
; As part of the build process, the assembled game is split into two parts,
; CODE1 and CODE2 (in the original build process, the BASIC program S.SCODES
; creates these files, while in the modern build this split is done in the
; elite-checksum.py Python script). The split is as follows:
;
;   * CODE1 contains the first $5000 bytes, from $4000 to $8FFF.
;
;   * CODE2 contains the rest of the binary, from $9000 to $BFFF.
;
; The source you are reading now takes these two parts and produces two more
; binaries, ELA and ELB, which contain the two parts of the game binary, plus
; the following routine. ELA contains CODE2, while ELB contains CODE1 and the
; rest of the data.
;
; For the official Firebird variant of the game, the loading process is managed
; by a dedicated game loader. See the elite-loader.asm source for details.
;
; For the variant of the game on Ian Bell's website, the ELA and ELB files are
; not used, and instead CODE1 and CODE2 are loaded directly from disk and are
; moved to their correct addressses by a simple mover program. See the
; elite-mover.asm source for details.
;
; For the source disk variant, the process is different, as the game isn't run
; on an Apple II, but instead it's transmitted across a cable from the BBC Micro
; development machine to the test Apple II machine. The transfer process goes
; like this:
;
;   * ELA is transmitted first, which loads the game data at $0B60, the loading
;     screen at $2000 and CODE2 at $9000 to $BFFF.
;
;   * The ENTRY routine in ELA (i.e. this routine) is called on the Apple II,
;     which copies CODE2 into bank-switched RAM at $D000.
;
;   * ELB is transmitted next, which loads CODE1 from $4000 to $8FFF.
;
;   * The S% routine in ELB is called on the Apple II, which copies CODE2 out of
;     bank-switched RAM back to $9000 to $BFFF.
;
; We don't have copies of the transfer software - there are some tantalising
; clues in the S.APMAKES BASIC program, which includes a *APPLE command that is
; presumably part of the transfer process - but I'm assumimg this memory-moving
; process ensures that CODE2 doesn't get corrupted by the second transfer
; process.
;
; Interestingly, this routine is run by all variants of the game when ELA is
; loaded, so the released game also copies CODE2 into bank-switched RAM when it
; loads. The only difference is that the copied code is ignored in the released
; game.
;
; ******************************************************************************

.ENTRY

 LDA $C054              ; Select page 1 display (i.e. main screen memory) by
                        ; reading the PAGE20FF soft switch

 LDA $C052              ; Configure graphics on the whole screen by reading the
                        ; MIXEDOFF soft switch

 LDA $C057              ; Select high-resolution graphics by reading the HIRESON
                        ; soft switch

 LDA $C050              ; Select the graphics mode by reading the TEXTOFF soft
                        ; switch

 LDA ZP1                ; Store the current contents of ZP1(1 0) and ZP2(1 0) on
 PHA                    ; the stack, so we can restore them later
 LDA ZP1+1
 PHA
 LDA ZP2
 PHA
 LDA ZP2+1
 PHA

 LDA $C08B              ; Set RAM bank 1 to read RAM and write RAM by reading
                        ; the RDWRBSR1 soft switch, with bit 3 set (bank 1),
                        ; bit 1 set (read RAM) and bit 0 set (write RAM)
                        ;
                        ; So this enables bank-switched RAM at $D000

                        ; We now want to copy all the data between $9000 and
                        ; $C000 into the bank-switched RAM at $D000

 LDA #LO(CODE2)         ; Set ZP1(1 0) = CODE2
 STA ZP1
 LDA #HI(CODE2)
 STA ZP1+1

 LDA #LO(STORE)         ; Set ZP2(1 0) = STORE
 STA ZP2
 LDA #HI(STORE)
 STA ZP2+1

 LDY #0                 ; Set Y = 0 to use as a byte counter

 LDX #HI($C000-$9000)   ; We want to copy all the data between $9000 and $C000,
                        ; so set X to the number of pages to copy

.MVLP1

 LDA (ZP1),Y            ; Copy the Y-th byte of ZP1(1 0) to the Y-th byte of
 STA (ZP2),Y            ; ZP2(1 0)

 INY                    ; Increment the byte counter

 BNE MVLP1              ; Loop back until we have copied a whole page of bytes

 INC ZP1+1              ; Increment the high bytes of ZP1(1 0) and ZP2(1 0) so
 INC ZP2+1              ; they point to the next page in memory

 DEX                    ; Decrement the page counter

 BNE MVLP1              ; Loop back until we have copied X pages

 LDA $C081              ; Set ROM bank 2 to read ROM and write RAM by reading
                        ; the WRITEBSR2 soft switch, with bit 3 clear (bank 2),
                        ; bit 1 clear (read ROM) and bit 0 set (write RAM)

 PLA                    ; Restore the contents of ZP1(1 0) and ZP2(1 0) from
 STA ZP2+1              ; the stack, so they are unchanged by the subroutine
 PLA
 STA ZP2
 PLA
 STA ZP1+1
 PLA
 STA ZP1

 RTS                    ; Return from the subroutine

; ******************************************************************************
;
; Load the compiled binaries to create the files to transfer to an Apple II
;
; ******************************************************************************

IF _SOURCE_DISK_BUILD

 EQUB $32, $30, $32, $30, $3A, $45, $51, $55    ; These bytes appear to be
 EQUB $57, $26, $32, $31, $32, $31, $3A, $45    ; unused and just contain random
 EQUB $51, $55, $57, $26, $32, $32, $32, $32    ; workspace noise left over from
 EQUB $3A, $45, $51, $55, $57, $26, $32, $33    ; the BBC Micro assembly process
 EQUB $32, $33, $0D, $07, $E9, $32, $20, $20    ;
 EQUB $20, $20, $20, $20, $20, $45, $51, $55    ; They contain part of the BASIC
 EQUB $57, $26, $32, $30, $32, $30, $3A, $45    ; source code from the SCTBH
 EQUB $51, $55, $57, $26, $32, $31, $32, $31    ; variable
 EQUB $3A, $45, $51, $55, $57, $26, $32, $32
 EQUB $32, $32, $3A, $45, $51, $55, $57, $26
 EQUB $32, $33, $32, $33, $0D, $07, $EB, $3C
 EQUB $20, $20, $20, $20, $20, $20, $20, $45
 EQUB $51, $55, $57, $26, $32, $30, $32, $30
 EQUB $3A, $45, $51, $55, $57, $26, $32, $30
 EQUB $32, $30, $3A, $45, $51, $55, $57, $26
 EQUB $32, $30, $32, $30, $3A, $45, $51, $55
 EQUB $57, $26, $32, $30, $32, $30, $20, $20
 EQUB $20, $5C, $73, $61, $66, $65, $74, $79
 EQUB $0D, $07, $EE, $33, $2E, $53, $43, $54
 EQUB $42, $48, $32, $20, $45, $51, $55, $57
 EQUB $26, $33, $43, $33, $43, $3A, $45, $51
 EQUB $55, $57, $26, $33, $44, $33, $44, $3A
 EQUB $45, $51, $55, $57, $26, $33, $45, $33
 EQUB $45, $3A, $45, $51, $55, $57, $26, $33
 EQUB $46, $33, $46, $0D, $07, $F3, $A3, $0D
 EQUB $00, $83, $40, $00, $00, $00, $AC, $0D
 EQUB $50, $00, $87, $60, $00, $00, $00, $43
 EQUB $00, $50, $32, $00, $87, $64, $00, $00
 EQUB $00, $44, $00, $4E, $54, $52, $59, $00
 EQUB $8C, $39, $A0, $00, $00, $CD, $0D, $55
 EQUB $4D, $50, $00, $8C, $3B, $D0, $00, $00
 EQUB $D9, $0D, $55, $4D, $50, $33, $00, $8C
 EQUB $3C, $80, $00, $00, $55, $00, $55, $4D
 EQUB $50, $32, $00, $8C, $3D, $40, $00, $00
 EQUB $26, $33, $44

ELIF _4AM_CRACK

 EQUB $20, $20, $45, $51, $55, $57, $26, $44    ; These bytes appear to be
 EQUB $30, $35, $30, $3A, $45, $51, $55, $57    ; unused and just contain random
 EQUB $26, $44, $30, $35, $30, $3A, $45, $51    ; workspace noise left over from
 EQUB $55, $57, $26, $44, $30, $35, $30, $3A    ; the BBC Micro assembly process
 EQUB $45, $51, $55, $57, $26, $44, $30, $35    ;
 EQUB $30, $0D, $07, $DF, $32, $2E, $53, $43    ; They contain part of the BASIC
 EQUB $54, $42, $48, $20, $45, $51, $55, $57    ; source code from the SCTBH
 EQUB $26, $32, $30, $32, $30, $3A, $45, $51    ; variable
 EQUB $55, $57, $26, $32, $31, $32, $31, $3A
 EQUB $45, $51, $55, $57, $26, $32, $32, $32
 EQUB $32, $3A, $45, $51, $55, $57, $26, $32
 EQUB $33, $32, $33, $0D, $07, $E4, $32, $20
 EQUB $20, $20, $20, $20, $20, $20, $45, $51
 EQUB $55, $57, $26, $32, $30, $32, $30, $3A
 EQUB $45, $51, $55, $57, $26, $32, $31, $32
 EQUB $31, $3A, $45, $51, $55, $57, $26, $32
 EQUB $32, $32, $32, $3A, $45, $51, $55, $57
 EQUB $26, $32, $33, $32, $33, $0D, $07, $E9
 EQUB $32, $20, $20, $20, $20, $20, $20, $20
 EQUB $45, $51, $55, $57, $26, $32, $30, $32
 EQUB $30, $3A, $45, $51, $55, $57, $26, $32
 EQUB $31, $32, $31, $3A, $45, $51, $55, $57
 EQUB $26, $32, $32, $32, $32, $3A, $45, $51
 EQUB $55, $57, $26, $32, $33, $32, $33, $0D
 EQUB $07, $EB, $3C, $20, $20, $20, $20, $20
 EQUB $20, $20, $45, $51, $55, $57, $26, $32
 EQUB $30, $32, $30, $3A, $45, $51, $55, $57
 EQUB $26, $32, $30, $32, $30, $3A, $45, $51
 EQUB $55, $57, $26, $32, $30, $32, $30, $3A
 EQUB $45, $51, $55, $57, $26, $32, $30, $32
 EQUB $30, $20, $20, $20, $5C, $73, $61, $66
 EQUB $65, $74, $79, $0D, $07, $EE, $33, $2E
 EQUB $53, $43, $54, $42, $48, $32, $20, $45
 EQUB $51, $55, $57, $26, $33, $43, $33, $43
 EQUB $3A, $45, $51

ELIF _SOURCE_DISK_CODE_FILES OR _SOURCE_DISK_ELT_FILES

 SKIPTO CODE%+$160      ; Skip to $0B60

ENDIF

 INCBIN "3-assembled-output/DATA.bin"

 SKIPTO CODE%+$1600     ; Skip to $2000

IF _IB_DISK

 INCBIN "1-source-files/images/ib-disk/A.SCREEN.bin"

ELIF _4AM_CRACK

 INCBIN "1-source-files/images/4am-crack/A.SCREEN1.bin"
 INCBIN "1-source-files/images/4am-crack/A.SCREEN2.bin"

ELIF _SOURCE_DISK_BUILD

 INCBIN "1-source-files/images/source-disk-build/A.SCREEN.bin"

ELIF _SOURCE_DISK_CODE_FILES

 INCBIN "1-source-files/images/source-disk-code-files/A.SCREEN.bin"

ELIF _SOURCE_DISK_ELT_FILES

 INCBIN "1-source-files/images/source-disk-elt-files/A.SCREEN.bin"

ENDIF

 SKIPTO CODE%+$3600     ; Skip to $4000

 INCBIN "3-assembled-output/CODE2.bin"

IF _4AM_CRACK

 EQUB $C5, $20, $1A, $6C, $4C, $7B, $6F, $38    ; These bytes appear to be
 EQUB $E9, $01, $0A, $A8, $BE, $92, $70, $B9    ; unused and just contain random
 EQUB $93, $70, $A8, $60, $AD, $F2, $02, $C9    ; workspace noise left over from
 EQUB $08, $90, $05, $A9, $20, $20, $B5, $94    ; the BBC Micro assembly process
 EQUB $A9, $10, $A8, $85, $36, $A9, $0C, $85
 EQUB $34, $98, $18, $69, $20, $20, $72, $64    ; They contain part of the CODE1
 EQUB $A5, $36, $18, $69, $50, $20, $43, $71    ; binary, from file offset $306E
 EQUB $E6, $36, $A4, $36, $C0, $14, $90, $E5    ; to $318E (which is loaded into
 EQUB $20, $1A, $A2, $A9, $AF, $20, $1A, $6C    ; memory at $7000 to $7120),
 EQUB $20, $E1, $84, $38, $E9, $30, $C9, $04    ; from when it was assembled in
 EQUB $90, $06, $20, $1A, $A2, $4C, $43, $70    ; memory
 EQUB $AA, $60, $20, $5F, $69, $20, $9D, $6A
 EQUB $70, $70, $69, $4C, $1A, $A2, $85, $06
 EQUB $BD, $9F, $02, $F0, $1F, $A0, $04, $C9
 EQUB $0F, $F0, $0E, $A0, $05, $C9, $8F, $F0
 EQUB $08, $A0, $0C, $C9, $97, $F0, $02, $A0
 EQUB $0D, $86, $B4, $98, $20, $0A, $70, $20
 EQUB $56, $6E, $A6, $B4, $A5, $06, $9D, $9F
 EQUB $02, $60, $01, $00, $2C, $01, $A0, $0F
 EQUB $70, $17, $A0, $0F, $10, $27, $82, $14
 EQUB $10, $27, $28, $23, $98, $3A, $10, $27
 EQUB $50, $C3, $60, $EA, $40, $1F, $A2, $05
 EQUB $B5, $92, $95, $A1, $CA, $10, $F9, $A0
 EQUB $03, $24, $92, $70, $01, $88, $84, $D0
 EQUB $A5, $97, $29, $1F, $F0, $05, $09, $80
 EQUB $20, $43, $71, $20, $1D, $64, $C6, $D0
 EQUB $10, $EE, $A2, $05, $B5, $A1, $95, $92
 EQUB $CA, $10, $F9, $60, $A0, $00, $B9, $87
 EQUB $02, $C9, $0D, $F0, $06, $20, $32, $55
 EQUB $C8, $D0, $F3, $60, $2C, $6E, $02, $30
 EQUB $15, $20, $F7, $70, $20, $AE, $70, $A2
 EQUB $05, $B5, $92, $BC, $F5, $02, $9D, $F5
 EQUB $02, $94, $92, $CA, $10, $F3, $60, $18
 EQUB $AE, $9E, $02, $E8, $4C, $63, $54, $A9
 EQUB $69, $20, $3E, $71, $AE, $9C, $02, $38
 EQUB $20, $63, $54, $A9, $C3, $20, $38, $71

ENDIF

.endA

 INCBIN "3-assembled-output/CODE1.bin"

.endB

; ******************************************************************************
;
; Save ELA.bin
;
; ******************************************************************************

 PRINT "P% = ", ~P%
 PRINT "S.A.ELA ", ~LOAD%, " ", ~endA, " ", ~LOAD%, " ", ~LOAD%
 SAVE "3-assembled-output/ELA.bin", CODE%, endA, LOAD%

; ******************************************************************************
;
; Save ELB.bin
;
; ******************************************************************************

 PRINT "S.A.ELB ", ~endA, " ", ~endB, " ", ~LOAD%, " ", ~LOAD%
 SAVE "3-assembled-output/ELB.bin", endA, endB, LOAD%
