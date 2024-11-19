; ******************************************************************************
;
; APPLE II ELITE MOVER SOURCE
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

ELIF _SOURCE_DISK_BUILD OR _SOURCE_DISK_ELT_FILES OR _SOURCE_DISK_CODE_FILES

 BEGIN = $7EAB          ; The address of the BEGIN routine in the main game code

 TT170 = $7EC4          ; The address of the TT170 routine in the main game code

ENDIF

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
;    Summary: ???
;
; ******************************************************************************

.Mover

 STA $C080

 LDY #0
 STY $00
 STY $02

 LDA #$D0
 STA $01

 LDA #$90
 STA $03

.move1

 LDA ($00),Y
 STA ($02),Y

 INY

 BNE move1

 INC $01
 INC $03

 LDA $03
 CMP #$C0
 BCC move1

 STA $C082

IF _MAX_COMMANDER

 JMP BEGIN

ELSE

 JMP TT170

ENDIF

 NOP

; ******************************************************************************
;
; Save MOVER.bin
;
; ******************************************************************************

 PRINT "P% = ", ~P%
 SAVE "3-assembled-output/MOVER.bin", CODE%, P%, LOAD%
