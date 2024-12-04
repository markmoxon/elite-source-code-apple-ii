; ******************************************************************************
;
; APPLE II ELITE ENCRYPTION SOURCE
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
; The following routines from the S.CODES and S.DATAS BBC BASIC source files
; are implemented in the elite-checksum.py script. This file is purely for
; reference and is not used in the build process.
;
; ******************************************************************************

; ******************************************************************************
;
;       Name: S.CODES encryption
;       Type: Subroutine
;   Category: Copy protection
;    Summary: Encrypts CODE1 and CODE2
;
; ******************************************************************************

 ZP = $70               ; ZP(1 0) stores the source address for the encryption

 ZP2 = $72              ; ZP2(1 0) stores the end address

 KEY1 = $15             ; The seed for encrypting CODE1 and CODE2 from G% to R%,
                        ; where CODE1 is the portion of ELTA-ELTK up to memory
                        ; location $9000, and CODE2 is the portion from $9000
                        ; onwards

                        ; These two variables come from the build process:
                        ;
                        ; G% is the address in the main game code of the ELTA
                        ; file, at the start of CODE1
                        ;
                        ; R% is the address in the main game code of the last
                        ; byte of ELTK, at the end of CODE2
                        ;
                        ; These two variables are calculated by S.CODES when
                        ; loading the binary files into memory in order to
                        ; encrypt them
                        ;
                        ; L% is the address in local memory in S.CODES where we
                        ; load the ELTA-K files
                        ;
                        ; U% is the address in local memory in S.CODES of the
                        ; last byte of ELTK
                        ;
                        ; We can therefore convert an in-game address to a local
                        ; S.CODES address by adding L%-C%
                        ;
                        ; This code encrypts as follows:
                        ;
                        ;   G% to R% with KEY1 (i.e. L%+G%-C% to U% in local
                        ;                            memory)

 LDA U%                 ; Store the contents of the byte at U% on the stack,
 PHA                    ; so we can restore it below

 LDA #KEY1              ; Set the byte at U% to KEY1, so we can work up from
 STA U%                 ; L% to U% to encrypt LOCODE, ending with the correct
                        ; seed (which we can then use to "unzip" the encrypted
                        ; data in the opposite direction, from U% to L%)

 LDA #0                 ; Set ZP = 0, so (ZP+1 Y) will equal ZP(1 0) + Y
 STA ZP

 LDY #LO(L%+G%-C%)      ; Set (ZP+1 Y) to the address in local memory of the
 LDA #HI(L%+G%-C%)      ; start of the code that we want to encrypt (i.e. G% in
 STA ZP+1               ; the main game code, or the first byte of ELTA)

 LDA #LO(U%)            ; Set ZP2(1 0) to U%, the address in local memory of the
 STA ZP2                ; end of the code that we want to encrypt (i.e. R% in
 LDA #HI(U%)            ; the main game code, or the last byte of ELTK)
 STA ZP2+1

 JSR WUMP               ; Encrypt from (ZP+1 Y) up to ZP2(1 0), i.e. from G% up
                        ; to R% in the main game code

 PLA                    ; Restore the byte at U%, which we corrupted above
 STA U%

 RTS                    ; Return from the subroutine

.WUMP

                        ; This routine encrypts from (ZP+1 Y) up to ZP2(1 0)
                        ;
                        ; This is the same as ZP2(1 0) down to ZP(1 0) + Y

 LDA (ZP),Y             ; If Y = 255, jump to WUMP2 to add the bytes across the
 CLC                    ; page boundary and jump back to WUMP3
 INY                    ;
 BEQ WUMP2              ; Otherwise add byte Y+1 of ZP(1 0) to byte Y of ZP(1 0)
 ADC (ZP),Y
 DEY
 STA (ZP),Y

.WUMP3

 INY                    ; Increment Y to point to the next byte to encrypt

 CPY ZP2                ; If we haven't reached ZP2(1 0), loop back to WUMP to
 BNE WUMP               ; encrypt the next byte
 LDA ZP+1
 CMP ZP2+1
 BNE WUMP

 RTS                    ; Otherwise we have reached ZP2(1 0), so return from the
                        ; subroutine

.WUMP2

                        ; If we get here then Y has just been incremented from
                        ; 255 to 0

 INC ZP+1               ; Increment ZP(1 0) to point to the next page

 ADC (ZP),Y             ; Add byte 0 of the new page to byte 255 of the old page
 DEC ZP+1               ; (C is still clear from above)
 DEY
 STA (ZP),Y
 INC ZP+1

 BNE WUMP3              ; Jump to WUMP3 to continue encrypting the new page

; ******************************************************************************
;
;       Name: S.DATAS encryption
;       Type: Subroutine
;   Category: Copy protection
;    Summary: Encrypts DATA
;
; ******************************************************************************

 ZP = $70               ; ZP(1 0) stores the source address for the encryption

 ZP2 = $72              ; ZP2(1 0) stores the end address

 KEY2 = $69             ; The seed for encrypting DATA from WORDS to $2000,
                        ; which is the whole data file

                        ; These three variables are calculated by S.COMLODS when
                        ; loading the binary files into memory in order to
                        ; encrypt them
                        ;
                        ; CODE is the address of the DATA file when loaded into
                        ; local memory by S.DATAS
                        ;
                        ; DL% is the address of the DATA file when loaded by
                        ; the game (this is hardcoded to $0B60)
                        ;
                        ; U% is the address of the end of the DATA file when
                        ; loaded into local memory by S.DATAS (this is hardcoded
                        ; to $2000+CODE-DL%)
                        ;
                        ; We can therefore convert an in-game address to a local
                        ; S.DATA address by adding CODE-DL%
                        ;
                        ; This code encrypts as follows:
                        ;
                        ;   WORDS to U% with KEY2 (i.e. WORDS+CODE-DL% to U%
                        ;                               in local memory)

 LDA U%                 ; Store the contents of the byte at U% on the stack,
 PHA                    ; so we can restore it below

 LDA #KEY2              ; Set the byte at U% to KEY2, so we can work up from
 STA U%                 ; WORDS to U% to encrypt the data file, ending with the
                        ; correct seed (which we can then use to "unzip" the
                        ; encrypted data in the opposite direction, from U% to
                        ; WORDS)

 LDA #0                 ; Set ZP = 0, so (ZP+1 Y) will equal ZP(1 0) + Y
 STA ZP

 LDY #LO(WORDS+CODE-DL%)    ; Set (ZP+1 Y) to the address in local memory of the
 LDA #HI(WORDS+CODE-DL%)    ; start of the code that we want to encrypt (i.e.
 STA ZP+1                   ; location WORDS in the data file)

 LDA #LO(U%)            ; Set ZP2(1 0) to the address in local memory of the
 STA ZP2                ; end of the code that we want to encrypt (i.e. U% at
 LDA #HI(U%)            ; the end of the data file)
 STA ZP2+1

 JSR WUMP               ; Encrypt from (ZP+1 Y) up to ZP2(1 0), i.e. from WORDS
                        ; up to U% in the data file

 PLA                    ; Restore the byte at U%, which we corrupted above
 STA U%

 RTS                    ; Return from the subroutine

.WUMP

                        ; This routine encrypts from (ZP+1 Y) up to ZP2(1 0)
                        ;
                        ; This is the same as ZP2(1 0) down to ZP(1 0) + Y

 LDA (ZP),Y             ; If Y = 255, jump to WUMP2 to add the bytes across the
 CLC                    ; page boundary and jump back to WUMP3
 INY                    ;
 BEQ WUMP2              ; Otherwise add byte Y+1 of ZP(1 0) to byte Y of ZP(1 0)
 ADC (ZP),Y
 DEY
 STA (ZP),Y

.WUMP3

 INY                    ; Increment Y to point to the next byte to encrypt

 CPY ZP2                ; If we haven't reached ZP2(1 0), loop back to WUMP to
 BNE WUMP               ; encrypt the next byte
 LDA ZP+1
 CMP ZP2+1
 BNE WUMP

 RTS                    ; Otherwise we have reached ZP2(1 0), so return from the
                        ; subroutine

.WUMP2

                        ; If we get here then Y has just been incremented from
                        ; 255 to 0

 INC ZP+1               ; Increment ZP(1 0) to point to the next page

 ADC (ZP),Y             ; Add byte 0 of the new page to byte 255 of the old page
 DEC ZP+1               ; (C is still clear from above)
 DEY
 STA (ZP),Y
 INC ZP+1

 BNE WUMP3              ; Jump to WUMP3 to continue encrypting the new page
