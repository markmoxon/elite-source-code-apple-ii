; ******************************************************************************
;
; APPLE II ELITE PDS TRANSFER SOURCE
;
; Apple II Elite was written by Ian Bell and David Braben and is copyright
; D. Braben and I. Bell 1986
;
; The code on this site is identical to the source disks released on Ian Bell's
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
; Apple II that's attached to a BBC Micro. The original S.APMAKES file on the
; source disk compiles this code and sends it to the Apple using a *APPLE
; command (this latter utility is not included on the disk).
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
;    Summary: ???
;
; ******************************************************************************

.ENTRY

 LDA $C054
 LDA $C052
 LDA $C057
 LDA $C050 ;HGR
 LDA ZP1
 PHA
 LDA ZP1+1
 PHA
 LDA ZP2
 PHA
 LDA ZP2+1
 PHA
 LDA $C08B ; page in RAM card
 LDA #LO(CODE2)
 STA ZP1
 LDA #HI(CODE2)
 STA ZP1+1
 LDA #LO(STORE)
 STA ZP2
 LDA #HI(STORE)
 STA ZP2+1
 LDY #0
 LDX #HI($C000-$9000) ;move X pages

.MVLP1

 LDA (ZP1),Y
 STA (ZP2),Y
 INY
 BNE MVLP1
 INC ZP1+1
 INC ZP2+1
 DEX
 BNE MVLP1
 LDA $C081 ; page in ROMs
 PLA
 STA ZP2+1
 PLA
 STA ZP2
 PLA
 STA ZP1+1
 PLA
 STA ZP1
 RTS

; ******************************************************************************
;
;       Name: Game data and binaries
;       Type: Subroutine
;   Category: Utility routines
;    Summary: The game data and binaries to transfer to an Apple II
;
; ******************************************************************************

IF _SOURCE_DISK_BUILD

 EQUB $C6, $C7, $C7, $C8, $C9, $C9, $CA, $CA    ; These bytes appear to be
 EQUB $CB, $CC, $CC, $CD, $CD, $CE, $CE, $CF    ; unused and just contain random
 EQUB $CF, $D0, $D0, $D1, $D1, $D2, $D2, $D3    ; workspace noise left over from
 EQUB $D3, $D4, $D4, $D5, $D5, $D5, $D6, $D6    ; the BBC Micro assembly process
 EQUB $D7, $D7, $D8, $D8, $D9, $D9, $D9, $DA
 EQUB $DA, $DB, $DB, $DB, $DC, $DC, $DD, $DD
 EQUB $DD, $DE, $DE, $DE, $DF, $DF, $E0, $E0
 EQUB $E0, $E1, $E1, $E1, $E2, $E2, $E2, $E3
 EQUB $E3, $E3, $E4, $E4, $E4, $E5, $E5, $E5
 EQUB $E6, $E6, $E6, $E7, $E7, $E7, $E7, $E8
 EQUB $E8, $E8, $E9, $E9, $E9, $EA, $EA, $EA
 EQUB $EA, $EB, $EB, $EB, $EC, $EC, $EC, $EC
 EQUB $ED, $ED, $ED, $ED, $EE, $EE, $EE, $EE
 EQUB $EF, $EF, $EF, $EF, $F0, $F0, $F0, $F1
 EQUB $F1, $F1, $F1, $F1, $F2, $F2, $F2, $F2
 EQUB $F3, $F3, $F3, $F3, $F4, $F4, $F4, $F4
 EQUB $F5, $F5, $F5, $F5, $F5, $F6, $F6, $F6
 EQUB $F6, $F7, $F7, $F7, $F7, $F7, $F8, $F8
 EQUB $F8, $F8, $F9, $F9, $F9, $F9, $F9, $FA
 EQUB $FA, $FA, $FA, $FA, $FB, $FB, $FB, $FB
 EQUB $FB, $FC, $FC, $FC, $FC, $FC, $FD, $FD
 EQUB $FD, $FD, $FD, $FD, $FE, $FE, $FE, $FE
 EQUB $FE, $FF, $FF, $FF, $FF, $FF, $03, $00
 EQUB $00, $B8, $00, $4D, $B8, $D6, $00, $70
 EQUB $4D, $B4, $B8, $6A, $D6, $05, $00, $CC
 EQUB $70, $EF, $4D, $8E, $B4, $C1, $B8, $9A
 EQUB $6A, $28, $D6, $75, $05, $89, $00, $6C
 EQUB $CC, $23, $70, $B4, $EF, $22, $4D, $71
 EQUB $8E, $A4, $B4, $BD, $C1, $BF, $B8, $AC
 EQUB $9A, $85, $6A, $4B, $28, $01, $D6, $A7
 EQUB $75, $3F, $05, $C9, $89, $46, $00, $B7
 EQUB $6C, $1D, $CC, $79, $23, $CB, $70, $13
 EQUB $B4, $52, $EF, $8A, $22, $B9, $4D, $E0
 EQUB $71, $00, $8E, $1A, $A4, $2D, $B4, $39
 EQUB $BD, $40, $C1

ELIF _SOURCE_DISK_CODE_FILES OR _SOURCE_DISK_ELT_FILES

 SKIPTO CODE%+$160

ENDIF

 INCBIN "3-assembled-output/DATA.bin"

 SKIPTO CODE%+$1600

IF _IB_DISK

 INCBIN "1-source-files/images/source-disk-build/A.SCREEN.bin"

ELIF _SOURCE_DISK_BUILD

 INCBIN "1-source-files/images/source-disk-build/A.SCREEN.bin"

ELIF _SOURCE_DISK_CODE_FILES

 INCBIN "1-source-files/images/source-disk-code-files/A.SCREEN.bin"

ELIF _SOURCE_DISK_ELT_FILES

 INCBIN "1-source-files/images/source-disk-elt-files/A.SCREEN.bin"

ENDIF

 SKIPTO CODE%+$3600

 INCBIN "3-assembled-output/CODE2.bin"

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
