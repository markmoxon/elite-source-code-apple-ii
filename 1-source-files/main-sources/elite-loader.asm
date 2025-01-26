\ ******************************************************************************
\
\ APPLE II ELITE LOADER SOURCE
\
\ Apple II Elite was written by Ian Bell and David Braben and is copyright
\ D. Braben and I. Bell 1986
\
\ The code in this file has been reconstructed from a disassembly of the version
\ released on Ian Bell's personal website at http://www.elitehomepage.org/
\
\ The commentary is copyright Mark Moxon, and any misunderstandings or mistakes
\ in the documentation are entirely my fault
\
\ The terminology and notations used in this commentary are explained at
\ https://elite.bbcelite.com/terminology
\
\ The deep dive articles referred to in this commentary can be found at
\ https://elite.bbcelite.com/deep_dives
\
\ ------------------------------------------------------------------------------
\
\ This source file contains code to move binaries in memory during the loading
\ process.
\
\ ------------------------------------------------------------------------------
\
\ This source file produces the following binary file:
\
\   * SEC2.bin
\
\ ******************************************************************************

 INCLUDE "versions/apple/1-source-files/main-sources/elite-build-options.asm"

 _CASSETTE_VERSION      = (_VERSION = 1)
 _DISC_VERSION          = (_VERSION = 2)
 _6502SP_VERSION        = (_VERSION = 3)
 _MASTER_VERSION        = (_VERSION = 4)
 _ELECTRON_VERSION      = (_VERSION = 5)
 _ELITE_A_VERSION       = (_VERSION = 6)
 _NES_VERSION           = (_VERSION = 7)
 _C64_VERSION           = (_VERSION = 8)
 _APPLE_VERSION         = (_VERSION = 9)
 _IB_DISK                   = (_VARIANT = 1)
 _SOURCE_DISK_BUILD         = (_VARIANT = 2)
 _SOURCE_DISK_CODE_FILES    = (_VARIANT = 3)
 _SOURCE_DISK_ELT_FILES     = (_VARIANT = 4)
 _4AM_CRACK                 = (_VARIANT = 5)
 _SOURCE_DISK               = (_VARIANT = 2) OR (_VARIANT = 3) OR (_VARIANT = 4)

\ ******************************************************************************
\
\ Configuration variables
\
\ ******************************************************************************

 CODE% = &2000          \ The address where the code will be run

 LOAD% = &2000          \ The address where the code will be loaded

\ ******************************************************************************
\
\       Name: ZP
\       Type: Workspace
\    Address: &0000 to &0004
\   Category: Workspaces
\    Summary: Important variables used by the loader
\
\ ******************************************************************************

 ORG &0000

.ZP

 SKIP 2                 \ Stores addresses used for moving content around

.P

 SKIP 2                 \ Stores addresses used for moving content around

\ ******************************************************************************
\
\ ELITE LOADER
\
\ ******************************************************************************

 ORG CODE%


 JSR L24D7
 JSR L247C
 JSR L23DC
 JSR L249B
 JSR L245D
 JSR L23DC
 JMP &4000

EQUS "ELB1"
EQUS "SCRN"
EQUS "        "
EQUS "        "
EQUS "        "
EQUS "  "

.L2037

 CLC
 BCC L203A

.L203A

 ROR &030C
 JSR L2105

.L2040

 LDA &25D7
 STA &0300
 LDA &25D8
 STA &0301
 JSR L210F
 LDY #&0B

.L2051

 LDA &25D6,Y
 BIT &030C
 BPL L2062
 TAX
 BEQ L207F
 CMP #&FF
 BEQ L207F
 BNE L2083

.L2062

 TAX
 BEQ L208F
 CMP #&FF
 BEQ L2083
 TYA
 PHA
 LDX #&00

.L206D

 LDA &25D9,Y
 AND #&7F
 CMP &2019,X
 BNE L2081
 INY
 INX
 CPX #&1E
 BNE L206D
 PLA
 TAY

.L207F

 CLC
 RTS

.L2081

 PLA
 TAY

.L2083

 TYA
 CLC
 ADC #&23
 TAY
 BNE L2051
 LDA &25D7
 BNE L2040

.L208F

 SEC
 RTS
 LDA #&00
 STA &F0
 BEQ L209A

.L2097

 LDA &2607

.L209A

 CLC
 ADC &2606
 BEQ L20A9
 CMP &260A
 BCC L20B7
 LDA #&FF
 BNE L20B1

.L20A9

 LDA &F0
 BNE L20E5
 LDA #&01
 STA &F0

.L20B1

 STA &2607
 CLC
 ADC #&11

.L20B7

 STA &2606
 ASL A
 ASL A
 TAY
 LDX #&10
 LDA &260E,Y
 BNE L20CC
 INY
 LDX #&08
 LDA &260E,Y
 BEQ L2097

.L20CC

 STX &F0
 LDX #&00

.L20D0

 INX
 DEC &F0
 ROL A
 BCC L20D0
 CLC

.L20D7

 ROR A
 DEX
 BNE L20D7
 STA &260E,Y
 LDX &2606
 LDY &F0
 CLC
 RTS

.L20E5

 SEC
 RTS

.L20E7

 LDA &25D6,Y
 STA &0300
 LDA &25D7,Y
 STA &0301
 JSR L210F
 LDY #&0C
 LDA &25D6,Y
 STA &0300
 LDA &25D7,Y
 STA &0301
 RTS

.L2105

 LDA #&11
 STA &0300
 LDA #&00
 STA &0301

.L210F

 CLC
 BCC L2113
 SEC

.L2113

 PHP
 LDA #&60
 STA &030B
 LDA #&02
 STA &030A
 LDA #&04
 STA &0309
 LDA #&D8
 STA &0308
 LDX &030B
 LDA &C08E,X
 LDA &C08C,X
 LDY #&08

.L2133

 LDA &C08C,X
 PHA
 PLA
 PHA
 PLA
 CMP &0100
 CMP &C08C,X
 BNE L2145
 DEY
 BNE L2133

.L2145

 PHP
 LDA &C089,X
 LDA &C08A,X
 PLP
 PHP
 BNE L215B
 LDY #&07

.L2152

 JSR L2319
 DEY
 BNE L2152
 LDX &030B

.L215B

 LDA &0300
 JSR L22BB
 PLP
 BNE L2178
 LDY &0308
 BPL L2178

.L2169

 LDY #&12

.L216B

 DEY
 BNE L216B
 INC &0307
 BNE L2169
 INC &0308
 BNE L2169

.L2178

 PLP
 PHP
 BCC L217F
 JSR L2344

.L217F

 LDY #&30
 STY &F2

.L2183

 LDX &030B
 JSR L225D
 BCC L21AC

.L218B

 DEC &F2
 BPL L2183

.L218F

 DEC &030A
 BEQ L21BB
 LDA #&04
 STA &0309
 LDA #&60
 STA &0302
 LDA #&00
 JSR L22BB

.L21A3

 LDA &0300
 JSR L22BB
 JMP L217F

.L21AC

 LDY &030F
 CPY &0300
 BEQ L21C3
 DEC &0309
 BNE L21A3
 BEQ L218F

.L21BB

 PLP
 LDY &C088,X
 SEC
 LDA #&04
 RTS

.L21C3

 LDY &0301
 LDA &2361,Y
 CMP &030E
 BNE L218B
 PLP
 BCS L21DE
 JSR L21F1
 PHP
 BCS L218B
 PLP
 JSR L2345
 JMP L21E5

.L21DE

 JSR L225B
 LDA #&01
 BCS L21E8

.L21E5

 LDA #&00
 CLC

.L21E8

 PHA
 LDX &030B
 LDY &C088,X
 PLA
 RTS

.L21F1

 LDY #&20

.L21F3

 DEY
 BEQ L2257

.L21F6

 LDA &C08C,X
 BPL L21F6

.L21FB

 EOR #&D5
 BNE L21F3
 NOP

.L2200

 LDA &C08C,X
 BPL L2200
 CMP #&AA
 BNE L21FB
 LDY #&56

.L220B

 LDA &C08C,X
 BPL L220B
 CMP #&AD
 BNE L21FB
 LDA #&00

.L2216

 DEY
 STY &F0

.L2219

 LDY &C08C,X
 BPL L2219
 EOR L22DC,Y
 LDY &F0
 STA &281E,Y
 BNE L2216

.L2228

 STY &F0

.L222A

 LDY &C08C,X
 BPL L222A
 EOR L22DC,Y
 LDY &F0
 STA &271E,Y
 INY
 BNE L2228

.L223A

 LDY &C08C,X
 BPL L223A
 CMP L22DC,Y
 BNE L2257

.L2244

 LDA &C08C,X
 BPL L2244
 CMP #&DE
 BNE L2257
 NOP

.L224E

 LDA &C08C,X
 BPL L224E
 CMP #&AA
 BEQ L2259

.L2257

 SEC
 RTS

.L2259

 CLC
 RTS

.L225B

 CLC
 RTS

.L225D

 LDY #&FC
 STY &F0

.L2261

 INY
 BNE L2268
 INC &F0
 BEQ L22B9

.L2268

 LDA &C08C,X
 BPL L2268

.L226D

 CMP #&D5
 BNE L2261
 NOP

.L2272

 LDA &C08C,X
 BPL L2272
 CMP #&AA
 BNE L226D
 LDY #&03

.L227D

 LDA &C08C,X
 BPL L227D
 CMP #&96
 BNE L226D
 LDA #&00

.L2288

 STA &F1

.L228A

 LDA &C08C,X
 BPL L228A
 ROL A
 STA &F0

.L2292

 LDA &C08C,X
 BPL L2292
 AND &F0
 STA &030D,Y
 EOR &F1
 DEY
 BPL L2288
 TAY
 BNE L22B9

.L22A4

 LDA &C08C,X
 BPL L22A4
 CMP #&DE
 BNE L22B9
 NOP

.L22AE

 LDA &C08C,X
 BPL L22AE
 CMP #&AA
 BNE L22B9
 CLC
 RTS

.L22B9

 SEC
 RTS

.L22BB

 STX &F0
 ASL A
 CMP &0302
 BEQ L2318
 STA &F1
 LDA #&00
 STA &F2

.L22C9

 LDA &0302
 STA &F3
 SEC
 SBC &F1
 BEQ L2306
 BCS L22DC
 EOR #&FF
 INC &0302
 BCC L22E1

.L22DC

 ADC #&FE
 DEC &0302

.L22E1

 CMP &F2
 BCC L22E7
 LDA &F2

.L22E7

 CMP #&0C
 BCS L22EC
 TAY

.L22EC

 SEC
 JSR L230A
 LDA &232C,Y
 JSR L2319
 LDA &F3
 CLC
 JSR L230D
 LDA &2338,Y
 JSR L2319
 INC &F2
 BNE L22C9

.L2306

 JSR L2319
 CLC

.L230A

 LDA &0302

.L230D

 AND #&03
 ROL A
 ORA &F0
 TAX
 LDA &C080,X
 LDX &F0

.L2318

 RTS

.L2319

 LDX #&11

.L231B

 DEX
 BNE L231B
 INC &0307
 BNE L2326
 INC &0308

.L2326

 SEC
 SBC #&01
 BNE L2319
 RTS

 EQUB &01, &30, &28, &24, &20, &1E, &1D, &1C
 EQUB &1C, &1C, &1C, &1C, &70, &2C, &26, &22
 EQUB &1F, &1E, &1D, &1C, &1C, &1C, &1C, &1C

.L2344

 RTS

.L2345

 LDY #&00

.L2347

 LDX #&56

.L2349

 DEX
 BMI L2347
 LDA &271E,Y
 LSR &281E,X
 ROL A
 LSR &281E,X
 ROL A
 STA &25D6,Y
 INY
 BNE L2349
 RTS
 RTS
 RTS
 RTS

 EQUB &00, &0D, &0B, &09, &07, &05, &03, &01
 EQUB &0E, &0C, &0A, &08, &06, &04, &02, &0F
 EQUB &EA, &00, &01, &98, &99, &02, &03, &9C
 EQUB &04, &05, &06, &A0, &A1, &A2, &A3, &A4
 EQUB &A5, &07, &08, &A8, &A9, &AA, &09, &0A
 EQUB &0B, &0C, &0D, &B0, &B1, &0E, &0F, &10
 EQUB &11, &12, &13, &B8, &14, &15, &16, &17
 EQUB &18, &19, &1A, &C0, &C1, &C2, &C3, &C4
 EQUB &C5, &C6, &C7, &C8, &C9, &CA, &1B, &CC
 EQUB &1C, &1D, &1E, &D0, &D1, &D2, &1F, &D4
 EQUB &D5, &20, &21, &D8, &22, &23, &24, &25
 EQUB &26, &27, &28, &E0, &E1, &E2, &E3, &E4
 EQUB &29, &2A, &2B, &E8, &2C, &2D, &2E, &2F
 EQUB &30, &31, &32, &F0, &F1, &33, &34, &35
 EQUB &36, &37, &38, &F8, &39, &3A, &3B, &3C
 EQUB &3D, &3E, &3F

.L23DC

 LDA #&0C
 STA &24D5
 JSR L2037
 BCS L243E
 JSR L20E7
 JSR L243F

.L23EC

 JSR L210F
 LDY &24D4
 LDX #&00

.L23F4

 LDA &25D6,Y
 STA &4000,X
 JSR L244B
 BMI L243E
 INX
 INY
 BNE L23F4
 INC &23F9
 LDA &23F8
 SEC
 SBC &24D4
 STA &23F8
 LDA &23F9
 SBC #&00
 STA &23F9
 LDA &24D4
 BEQ L2422
 LDA #&00
 STA &24D4

.L2422

 INC &24D5
 INC &24D5
 LDY &24D5
 LDA &24D6,Y
 STA &0300
 LDA L24D7,Y
 STA &0301
 BNE L23EC
 LDA &0300
 BNE L23EC

.L243E

 RTS

.L243F

 LDY #&00

.L2441

 LDA &25D6,Y
 STA &24D6,Y
 INY
 BNE L2441
 RTS

.L244B

 LDA &24D2
 SEC
 SBC #&01
 STA &24D2
 LDA &24D3
 SBC #&00
 STA &24D3
 RTS

.L245D

 LDA #&04
 STA &24D4
 LDA #&FF
 STA &24D2
 LDA #&4F
 STA &24D3
 LDA #&00
 STA &24D6
 LDA #&00
 STA &23F8
 LDA #&40
 STA &23F9
 RTS

.L247C

 LDA #&04
 STA &24D4
 LDA #&80
 STA &24D2
 LDA #&08
 STA &24D3
 LDA #&00
 STA &24D6
 LDA #&00
 STA &23F8
 LDA #&02
 STA &23F9
 RTS

.L249B

 LDY #&00

.L249D

 LDA &2015,Y
 STA &2019,Y
 INY
 CPY #&04
 BNE L249D
 RTS
 LDA #&00
 STA &4562
 LDA &C064
 CMP #&FF
 BNE L24CF
 LDA &C065
 CMP #&FF
 BNE L24CF
 LDA &C066
 CMP #&FF
 BNE L24CF
 LDA &C067
 CMP #&FF
 BNE L24CF
 LDA #&FF
 STA &4562

.L24CF

 JMP &4000

 EQUB &FF, &4F, &04, &00, &00

.L24D7

 LDA #&00
 STA &F4
 LDA #&40
 STA &F5
 LDA #&00
 STA &F6
 LDA #&90
 STA &F7
 LDX #&30
 LDY #&00

.L24EB

 LDA (&F4),Y
 STA (&F6),Y
 INY
 BNE L24EB
 INC &F5
 INC &F7
 DEX
 BNE L24EB
 RTS

\ ******************************************************************************
\
\ Save MOVER.bin
\
\ ******************************************************************************

 PRINT "P% = ", ~P%
 SAVE "versions/apple/3-assembled-output/SEC3.bin", CODE%, P%, LOAD%
