; ******************************************************************************
;
; APPLE II ELITE BIG CODE FILE SOURCE
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
; This source file contains code to produce the Big Code File for Apple II
; Elite. The Big Code File comprises the game code and the ship blueprints.
;
; ------------------------------------------------------------------------------
;
; This source file produces the following binary files:
;
;   * CODE.unprot.bin
;   * CODE1.unprot.bin
;   * CODE2.unprot.bin
;
; after reading in the following files:
;
;   * ELTA.bin
;   * ELTB.bin
;   * ELTC.bin
;   * ELTD.bin
;   * ELTE.bin
;   * ELTF.bin
;   * ELTG.bin
;   * ELTH.bin
;   * ELTI.bin
;   * ELTJ.bin
;   * ELTK.bin
;   * SHIPS.bin
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

 CODE% = $4000          ; The address where the main game code file is run

 LOAD% = $4000          ; The load address of the main game code file, which is
                        ; the same as the load address as it doesn't get moved
                        ; after loading

; ******************************************************************************
;
; Load the compiled binaries to create the Big Code File
;
; ******************************************************************************

 ORG CODE%

.elitea

 PRINT "elitea = ", ~P%
 INCBIN "3-assembled-output/ELTA.bin"

.eliteb

 PRINT "eliteb = ", ~P%
 INCBIN "3-assembled-output/ELTB.bin"

.elitec

 PRINT "elitec = ", ~P%
 INCBIN "3-assembled-output/ELTC.bin"

.elited

 PRINT "elited = ", ~P%
 INCBIN "3-assembled-output/ELTD.bin"

.elitee

 PRINT "elitee = ", ~P%
 INCBIN "3-assembled-output/ELTE.bin"

.elitef

 PRINT "elitef = ", ~P%
 INCBIN "3-assembled-output/ELTF.bin"

.eliteg

 PRINT "eliteg = ", ~P%
 INCBIN "3-assembled-output/ELTG.bin"

.eliteh

 PRINT "eliteh = ", ~P%
 INCBIN "3-assembled-output/ELTH.bin"

.elitei

 PRINT "elitei = ", ~P%
 INCBIN "3-assembled-output/ELTI.bin"

.elitej

 PRINT "elitej = ", ~P%
 INCBIN "3-assembled-output/ELTJ.bin"

.elitek

 PRINT "elitek = ", ~P%
 INCBIN "3-assembled-output/ELTK.bin"

 F% = P%

 PRINT "F% = ", ~F%
 PRINT "P% = ", ~P%

IF _SOURCE_DISK_BUILD

 EQUB $79, $68, $00, $00, $EA, $82, $74, $31    ; These bytes appear to be
 EQUB $00, $8F, $79, $6E, $00, $00, $05, $8A    ; unused and just contain random
 EQUB $74, $33, $00, $8F, $79, $7C, $00, $00    ; workspace noise left over from
 EQUB $FE, $83, $4C, $4F, $4F, $50, $00, $8F    ; the BBC Micro assembly process
 EQUB $79, $A6, $00, $00

ELIF _IB_DISK OR _SOURCE_DISK_CODE_FILES OR _SOURCE_DISK_ELT_FILES

 SKIPTO $A300

ENDIF

.ships

 PRINT "ships = ", ~P%
 INCBIN "3-assembled-output/SHIPS.bin"

IF _MATCH_ORIGINAL_BINARIES

 IF _IB_DISK

  EQUB $00, $5C, $A0, $6C, $73, $73, $31, $00   ; These bytes appear to be
  EQUB $90, $20, $B8, $00, $00, $A9, $9F, $52   ; unused and just contain random
  EQUB $35, $00, $90, $20, $C0, $00, $00, $BE   ; workspace noise left over from
  EQUB $9F, $52, $37, $00, $90, $20, $CB, $00   ; the BBC Micro assembly process
  EQUB $00, $28, $A1, $48, $50, $52, $00, $90
  EQUB $20, $E6, $00, $00, $CC, $9F, $52, $61
  EQUB $66, $74, $65, $72, $00, $90, $20, $F4
  EQUB $00, $00, $D7, $9F, $52, $58, $32, $00
  EQUB $90, $21, $00, $00, $00, $E2, $9F, $52
  EQUB $58, $31, $00, $90, $21, $04, $00, $00
  EQUB $EC, $9F, $52, $31, $00, $90, $21, $0C
  EQUB $00, $00, $F6, $9F, $52, $61, $00, $90
  EQUB $21, $17, $00, $00, $00, $A0, $52, $36
  EQUB $00, $90, $21, $30, $00, $00

 ELIF _SOURCE_DISK_BUILD

  EQUB $52, $00, $90, $20, $CB, $00, $00, $A7   ; These bytes appear to be
  EQUB $9F, $52, $61, $66, $74, $65, $72, $00   ; unused and just contain random
  EQUB $90, $20, $D9, $00, $00, $B2, $9F, $52   ; workspace noise left over from
  EQUB $58, $32, $00, $90, $20, $E5, $00, $00   ; the BBC Micro assembly process
  EQUB $BD, $9F, $52, $58, $31, $00, $90, $20
  EQUB $E9, $00, $00, $C7, $9F, $52, $31, $00
  EQUB $90, $20, $F1, $00, $00, $D1, $9F, $52
  EQUB $61, $00, $90, $20, $FC, $00, $00, $DB
  EQUB $9F, $52, $36, $00, $90, $21, $15, $00
  EQUB $00, $F2, $9F, $52, $34, $00, $90, $21
  EQUB $17, $00, $00, $FC, $9F, $65, $74, $74
  EQUB $65, $72, $00, $90, $21, $21, $00, $00
  EQUB $0A, $A0, $52, $39, $00, $90, $21, $2D
  EQUB $00, $00, $89, $A1, $65, $74

 ELIF _SOURCE_DISK_CODE_FILES OR _SOURCE_DISK_ELT_FILES

  EQUB $52, $00, $90, $20, $EA, $00, $00, $A7   ; These bytes appear to be
  EQUB $9F, $52, $61, $66, $74, $65, $72, $00   ; unused and just contain random
  EQUB $90, $20, $F8, $00, $00, $B2, $9F, $52   ; workspace noise left over from
  EQUB $58, $32, $00, $90, $21, $04, $00, $00   ; the BBC Micro assembly process
  EQUB $BD, $9F, $52, $58, $31, $00, $90, $21
  EQUB $08, $00, $00, $C7, $9F, $52, $31, $00
  EQUB $90, $21, $10, $00, $00, $D1, $9F, $52
  EQUB $61, $00, $90, $21, $1B, $00, $00, $DB
  EQUB $9F, $52, $36, $00, $90, $21, $34, $00
  EQUB $00, $F2, $9F, $52, $34, $00, $90, $21
  EQUB $36, $00, $00, $FC, $9F, $65, $74, $74
  EQUB $65, $72, $00, $90, $21, $40, $00, $00
  EQUB $0A, $A0, $52, $39, $00, $90, $21, $4C
  EQUB $00, $00, $A1, $A1, $65, $74

 ENDIF

ELSE

 SKIPTO $C000

ENDIF

; ******************************************************************************
;
; Save CODE.unprot.bin
;
; ******************************************************************************

 PRINT "P% = ", ~P%
 PRINT "S.A.CODE ", ~LOAD%, " ", ~P%, " ", ~LOAD%, " ", ~LOAD%
 SAVE "3-assembled-output/CODE.unprot.bin", CODE%, P%, LOAD%

; ******************************************************************************
;
; Save CODE1.unprot.bin
;
; ******************************************************************************

 SAVE "3-assembled-output/CODE1.unprot.bin", CODE%, CODE% + $5000, LOAD%

; ******************************************************************************
;
; Save CODE2.unprot.bin
;
; ******************************************************************************

 SAVE "3-assembled-output/CODE2.unprot.bin", CODE% + $5000, P%, LOAD%
