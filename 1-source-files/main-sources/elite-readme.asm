; ******************************************************************************
;
; APPLE II ELITE README SOURCE
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
; This source file produces a README file for Apple II Elite.
;
; ------------------------------------------------------------------------------
;
; This source file produces the following binary file:
;
;   * README.txt
;
; ******************************************************************************

 INCLUDE "1-source-files/main-sources/elite-build-options.asm"

 _IB_DISK                   = (_VARIANT = 1)
 _SOURCE_DISK_BUILD         = (_VARIANT = 2)
 _SOURCE_DISK_CODE_FILES    = (_VARIANT = 3)
 _SOURCE_DISK_ELT_FILES     = (_VARIANT = 4)
 _4AM_CRACK                 = (_VARIANT = 5)
 _SOURCE_DISK               = (_VARIANT = 2) OR (_VARIANT = 3) OR (_VARIANT = 4)

.readme

 EQUB 13
 EQUS "---------------------------------------"
 EQUB 13
 EQUS "FIREBIRD ELITE (FLICKER-FREE VERSION)"
 EQUB 13
 EQUB 13
 EQUS "VERSION: APPLE II"
 EQUB 13

IF _IB_DISK

 EQUS "VARIANT: IAN BELL'S GAME DISK"
 EQUB 13

ELIF _4AM_CRACK

 EQUS "VARIANT: 4AM CRACK (FIREBIRD RELEASE)"
 EQUB 13

ELIF _SOURCE_DISK_BUILD

 EQUS "VARIANT: SOURCE DISK BUILD OUTPUT"
 EQUB 13

ELIF _SOURCE_DISK_CODE_FILES

 EQUS "VARIANT: SOURCE DISK CODE BINARY FILES"
 EQUB 13

ELIF _SOURCE_DISK_ELT_FILES

 EQUS "VARIANT: SOURCE DISK ELT BINARY FILES"
 EQUB 13

ENDIF

 EQUB 13
 EQUS "CONTAINS FLICKER-FREE PLANET DRAWING"
 EQUB 13
 EQUS "ROUTINES BY MARK MOXON"
 EQUB 13
 EQUB 13
 EQUS "SEE WWW.BBCELITE.COM FOR DETAILS"
 EQUB 13
 EQUS "---------------------------------------"
 EQUB 13

 SAVE "3-assembled-output/README.txt", readme, P%

