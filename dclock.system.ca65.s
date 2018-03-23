; Fully disassembled and analyzed source to AE
; DCLOCK.SYSTEM by M.G. - 04/18/2017
; Assembles to a binary match for AE code unless
; FIX_BUGS is set.

; speaking of FIX_BUGS, there are critical bugs in the
; original AE code:
; * When driver loader is initially probing it corrupts the 
;   Apple //c Memory Expansion Card:
;   - it saves, but fails to restore, data at address $080000
;   - it fails to reset slinky pointer, and *will* trash $080000-$080007
; * When the clock is read, it corrupts data at address $08xx01
;   - John Brooks spotted this, I totally missed this.
; Setting FIX_BUGS to 1 will fix these issues.

; other notes:
; * uses direct block access to read volume directory,
;   so won't launch from an AppleShare volume.

; Build instructions:
; ca65 dclock.system.s -l dclock.system.lst
; ld65 -t none -o dclock.system dclock.system.o
; put dclock.system as a SYS file on a ProDOS disk.

FIX_BUGS        := 0                            ; set to 1 to fix critical bugs

.setcpu "65C02"

; zero page locations
SLASHOFFS       := $00                          ; offset of last '/' in our path
MYNAMELEN       := $01                          ; length of our file name in path
FENTPTR         := $02                          ; directory file entry pointer
FENTPTRL        := FENTPTR
FENTPTRH        := FENTPTR+1
CURENT          := $04                          ; current file entry in block
ENTLEN          := $05                          ; length of a file entry
ENTPERBLK       := $06                          ; number of entries per block
DIRBLK          := $07                          ; directory block to read
DIRBLKL         := DIRBLK
DIRBLKH         := DIRBLK+1
CHRPTR          := $09                          ; character pointer for print routine
CHRPTRL         := CHRPTR
CHRPTRH         := CHRPTR+1
SCRATCH         := $0B                          ; scratch value for BCD range checks  
SAVEBYTE        := $0C                          ; slinky overwritten byte save location
BCDTMP          := $3A                          ; location clock driver uses for BCD->Binary

; entry points
PRODOS          := $BF00
HOME            := $FC58
WAIT            := $FCA8
COUT1           := $FDF0

; buffers & other spaces
INBUF           := $0200                        ; input buffer
PATHBUF         := $0280                        ; path buffer
CLOCKBUF        := $0300                        ; clock buffer
RELOCT          := $1200                        ; reloc target 
BLOCKBUF        := $1000                        ; buffer for BLOCK_READ
READBUF         := $1C00                        ; I/O buffer for READ
SYSEXEC         := $2000                        ; location of SYS file executable
CLKCODE         := $D742                        ; Clock code location

; global Page entries
CLKENTRY        := $BF06                        ; clock routine entry point
DATELO          := $BF90
DATEHI          := $BF91
TIMELO          := $BF92
TIMEHI          := $BF93
MACHID          := $BF98                        ; machine ID

; I/O and hardware
ROMIn2          := $C082                        ; access to read ROM/no write RAM
LCBank1         := $C08B                        ; Access twice to write bank 1
C8OFF           := $CFFF                        ; C8xx ROM off
SLOT4ROM        := $C400                        ; Slot 4 ROM space
SLOT4IO         := $C0C0                        ; Slot 4 I/O space
DPTRL           := SLOT4IO+0                    ; Slinky data ptr low
DPTRM           := SLOT4IO+1                    ; Slinky data ptr middle
DPTRH           := SLOT4IO+2                    ; Slinky data ptr high
DATA            := SLOT4IO+3                    ; Slinky data byte

; Misc
CLKCODEMAX      := $7D

        .org    SYSEXEC
; ----------------------------------------------------------------------------
; relocate code from RELOCS to RELOCT
.proc   DClockSystem
        ldy     #(rseg1e-rseg1b) 
:       lda     RELOCS,y
        sta     RELOCT-1,y 
        dey
        bne     :-
        ; greetings
        jsr     HOME
        jsr     iprint
        .byte   $07,"DClock",$0d
        jsr     iprint
        .byte   $27,"Copyright (c)1988 Applied Engineering",$0D,$0D
        jsr     ClockRead
        jsr     ValidTime
        bcc     InstallDriver
        ; clock not found
        jsr     iprint
        .byte   $11,"Can't find clock",$0D
        jmp     NextSys
.endproc
; ----------------------------------------------------------------------------
; Install clock driver        
.proc   InstallDriver
        ; make language card writable
        lda     LCBank1
        lda     LCBank1
        ; copy clock driver into ProDOS
        ; should not be hard-coded, Apple says to put it at
        ; the address pointed to at ($BF07). [PDOS8TRM 6.1.1]
        ldx     #$00
:       lda     rclkdrv-1,x
        sta     CLKCODE,x
        inx
        cpx     #CLKCODEMAX
        bcc     :-
        ; make LC write-protected
        lda     ROMIn2
        ; make sure clock vector is preceded by a JMP
        ; (it is RTS if no clock installed)
        lda     #$4C
        sta     CLKENTRY
        ; indicate in MACHID that clock is present
        lda     MACHID
        ora     #$01
        sta     MACHID
.endproc
; ----------------------------------------------------------------------------
; find and launch the next .SYSTEM file
.proc   NextSys
        ; check our path for last / (counted string at $0280)
        ldy     PATHBUF
:       lda     PATHBUF,y
        cmp     #'/'
        beq     LSlash
        dey
        bne     :-
LSlash: sty     SLASHOFFS                       ; offset of '/' into $00
        lda     PATHBUF
        sec
        sbc     SLASHOFFS                       ; calculate length of our name
        sta     MYNAMELEN                       ; and put in $01
        jsr     ReadVolDir1                     ; read first block of volume directory
        bcs     BadSys                          ; didn't get anything valid
        ; Now search for our own file name
ChkSlf: lda     (FENTPTR)                       ; get length & storage type
        and     #$0F                            ; mask in length
        cmp     MYNAMELEN                       ; same as our name?
        bne     INext                           ; Nope, next entry
        tay                                     ; Now let's see if name matches
        ldx     PATHBUF
:       lda     (FENTPTR),y
        cmp     PATHBUF,x
        bne     INext                           ; no match, next entry
        dex
        dey
        bne     :-
        ; ok, found ourself, now get next entry and start looking for *.SYSTEM
        bra     SNext                           ; Get next entry and start checking for .SYSTEM file
INext:  jsr     NextFileEnt                     ; Get next entry and check again for ourself
        bcc     ChkSlf
        bra     BadSys                          ; no more valid entries
; check the current entry for .SYSTEM
ChkSys: lda     (FENTPTR)                       ; get storage & length
        and     #$F0                            ; mask in storage type
        beq     SNext                           ; not valid if 0
        cmp     #$40
        bcs     SNext                           ; not valid if >= $40 (not type 1-3)
        lda     (FENTPTR)                       ; load it again
        and     #$0F                            ; but mask in length this time
        cmp     #$07
        bcc     SNext                           ; not valid if not at least 7 chars long
        ; now check to see if it ends with .SYSTEM
        tay
        ldx     #$07
:       lda     (FENTPTR),y
        cmp     dSYSTEM,x
        bne     SNext                           ; no match, next!
        dey
        dex
        bne     :-
        ldy     #$10
        lda     (FENTPTR),y                     ; get file Type
        cmp     #$FF                            ; is SYS
        bne     SNext                           ; Nope
        ; okay we have a .SYSTEM file to load
        lda     (FENTPTR)                       ; get storage type/name length again
        and     #$0F                            ; mask in name length
        tay                                     ; and save for copying
        ; calculate total length of path and put in length byte at PATHBUF
        clc
        adc     PATHBUF
        sec
        sbc     MYNAMELEN
        sta     PATHBUF
        ; copy filename into path
        tax
:       lda     (FENTPTR),y
        sta     PATHBUF,x
        dex
        dey
        bne     :-
        jmp     LaunchNext                      ; proceed to launch code
        ; get next entry for looking for .SYSTEM
SNext:  jsr     NextFileEnt
        bcc     ChkSys
        ; if we get here, unable to identify next .SYSTEM
BadSys: cmp     #$00                            ; error code in accumulator?
        beq     NoSys                           ; no, must have not found it
        pha
        jsr     iprint
        .byte   $18,"Directory ProDOS error #"
        pla
        jsr     PrHex
        lda     #$8D
        jsr     COUT1
        jmp     DoQuit                          ; quit to ProDOS
NoSys:  jsr     iprint
        .byte   $1d,"Can't find next .SYSTEM file",$0d
        jmp     DoQuit                          ; quit to ProDOS
dSYSTEM   = * - 1
        .byte   ".SYSTEM"
.endproc
; ----------------------------------------------------------------------------
; this looks like some unused debugging code to print the filename
; from the current directory entry pointed to at ($02)
.proc   PrintFN
        lda     (FENTPTR)                       ; get storage type/name length
        pha                                     ; save it
        and     #$0F                            ; mask in name length
        sta     (FENTPTR)                       ; put in file entry
        beq     done                            ; if zero, we are done
        ldy     #$01                            ; start with byte 1 of entry
:       lda     (FENTPTR),y                     ; get file name char
        ora     #$80
        jsr     COUT1                           ; print it
        tya
        iny
        cmp     (FENTPTR)                       ; printed all the chars?
        bcc     :-                              ; nope
        lda     #$8D                            ; CR
        jsr     COUT1
done:   pla                                     ; get type/name length
        sta     (FENTPTR)                           ; and restore it in file entry
        rts
.endproc
; ----------------------------------------------------------------------------
; get volume directory first block ($0002)
; and initialize values for processing
; returns carry clear if no error, set otherwise
.proc ReadVolDir1
        lda     #$02
        sta     DIRBLKL                         ; initialize block number low byte
        stz     DIRBLKH                         ; and high byte
        jsr     ReadVolDir                      ; read volume directory block
        bcs     done                            ; done if error
        lda     BLOCKBUF+$23                    ; get entry length
        sta     ENTLEN                          ; and save it
        lda     BLOCKBUF+$24                    ; get entries per block
        sta     ENTPERBLK                       ; and save it
        ; intial values $02/03 = $102B (offset of first file entry in first VD block)
        lda     #<(BLOCKBUF+$2b)
        sta     FENTPTRL
        lda     #>(BLOCKBUF+$2b)
        sta     FENTPTRH
        lda     #$02                            ; on first block, start with entry #2
        sta     CURENT
        clc
done:   rts
.endproc
; ----------------------------------------------------------------------------
; Update pointer to the next directory file entry
; read the next block if necessary
; returns carry clear if no error, set otherwise
.proc   NextFileEnt
        ; add entry length to current entry pointer at ($02)
        clc        
        lda     FENTPTRL
        adc     ENTLEN
        sta     FENTPTRL
        lda     FENTPTRH
        adc     #$00
        sta     FENTPTRH
        inc     CURENT                          ; increment current entry number
        lda     CURENT                          ; and load it
        cmp     ENTPERBLK                       ; did the last one in block?
        bcc     Okay                            ; nope, exit
        ; check if last block of Volume Directory
        lda     DIRBLKL                         ; block low byte
        bne     NxtBlk                          ; another one to read
        lda     DIRBLKH                         ; block high byte
        beq     Fail                            ; also zero, exit with error
        ; read next block of Volume Directory
NxtBlk: jsr     ReadVolDir                      ; next volume directory block
        bcs     Fail                            ; read problem, exit with error
        lda     #<(BLOCKBUF+$04)                ; now set entry pointer to the offset of the first
        sta     FENTPTRL                        ; one in the block
        lda     #>(BLOCKBUF+$04)                ; ...
        sta     FENTPTRH                        ; ...
        lda     #$01                            ; and set current entry
        sta     CURENT                          ; to 1
Okay:   clc
        rts
Fail:   sec
        rts
.endproc
; ----------------------------------------------------------------------------
; read volume directory block at ($07)
; and update to point to next block
; returns carry clear if no error, set otherwise
.proc   ReadVolDir
        ; most recent accessed device into param list for READ_BLOCK
        lda     $BF30                           ; most recent access device
        sta     PL_READ_BLOCK+1                 ; into READ_BLOCK parameter list
        ; copy block number to param list
        lda     DIRBLKL                         ; copy block number
        sta     PL_READ_BLOCK+4                 ; to parameter list
        lda     DIRBLKH                         ; ...
        sta     PL_READ_BLOCK+5                 ; ...
        jsr     PRODOS                          ; now get the block
        .byte   $80                             ; READ_BLOCK
        .word   PL_READ_BLOCK
        bcs     Done                            ; error, bail out
        lda     BLOCKBUF+$02                    ; otherwise get next block number
        sta     DIRBLKL                         ; from offset $02/$03
        lda     BLOCKBUF+$03                    ; and save it
        sta     DIRBLKH                         ; ...
Done:   rts
        ; Parameter list for READ_BLOCK
PL_READ_BLOCK:
        .byte   $03                             ; param count
        .byte   $00                             ; unit number
        .word   BLOCKBUF                        ; data buffer
        .word   $0000                           ; block number
.endproc
; ----------------------------------------------------------------------------
; enable slinky registers, set adddress and save byte we intend to trash
.proc   SlinkyEnable
        lda     C8OFF                           ; not needed on //c, but release $C8xx firmware
        lda     SLOT4ROM                        ; enable slinky registers
        lda     #$08                            ; set addr $080000
        sta     DPTRH
        stz     DPTRM
        stz     DPTRL
        lda     DATA                            ; read data byte
        sta     SAVEBYTE                        ; save it to restore later
        rts
.endproc
; ----------------------------------------------------------------------------
; Routine to restore trashed byte in slinky RAM
; WARNING: never called by unfixed, so the value is never restored
.proc   SlinkyRestore
        lda     #$08                            ; set adddr $080000
        sta     DPTRH
        stz     DPTRM
        stz     DPTRL
        lda     SAVEBYTE                        ; get saved byte
        sta     DATA                            ; and put it back
        lda     C8OFF                           ; not needed on //c, but release $C8xx firmware
        rts
.endproc
; ----------------------------------------------------------------------------
; Write 8 bits to clock
; WARNING: the unfixed code has a bug that trashes 8 bytes in the RAM card at $080000
.proc   ClockWrite8b
        ldx     #$08                            ; set adddr $080000
.if ::FIX_BUGS
        stx     DPTRH
        stz     DPTRM
:       stz     DPTRL                           ; restore low byte to 0
        sta     DATA                            ; write byte
.else
        stz     DPTRL
        stz     DPTRM
        stx     DPTRH
:       sta     DATA                            ; write byte
.endif
        lsr     a                               ; next bit into 0 position
        dex
        bne     :-   
        rts          
.endproc
; ----------------------------------------------------------------------------
; unlock the clock by writing the magic bit sequence
.proc   ClockUnlock
        ldy     #$08
:       lda     unlock,y
        jsr     ClockWrite8b                    ; write 8 bits
        dey
        bne     :-
        rts
unlock  = * - 1
        .byte   $5c, $a3, $3a, $c5, $5c, $a3, $3a, $c5
.endproc
; ----------------------------------------------------------------------------
; Read 8 bits from the clock
.proc   ClockRead8b
        ldx     #$08                            ; set adddr $080000
        stz     DPTRL
        stz     DPTRM
        stx     DPTRH
:       pha                                     ; save accumulator
        lda     DATA                            ; get data byte
        lsr     a                               ; bit 0 into carry
        pla                                     ; restore accumulator
        ror     a                               ; put read bit into position
        dex
        bne     :-
        rts
.endproc
; ----------------------------------------------------------------------------
; read the clock data into memory at CLOCKBUF
; WARNING: unfixed code never restores byte we trashed
.proc   ClockRead
        jsr     SlinkyEnable
        jsr     ClockUnlock 
        ldy     #$00
:       jsr     ClockRead8b 
        sta     CLOCKBUF,y  
        iny                 
        cpy     #$08                            ; have we read 8 bytes?        
        bcc     :-                              ; nope
.if ::FIX_BUGS
        jsr     SlinkyRestore
.endif
        rts                 
.endproc
; ----------------------------------------------------------------------------
; validate the DClock data makes sense
; return carry clear if it does, carry set if it does not
.proc   ValidTime
        ; validate ms
        ldx     #$00
        ldy     #$99
        lda     CLOCKBUF    
        jsr     CheckBCD    
        bcs     :+          
        ; validate seconds
        ldx     #$00
        ldy     #$59
        lda     CLOCKBUF+$01
        jsr     CheckBCD    
        bcs     :+          
        ; validate minutes
        ldx     #$00
        ldy     #$59
        lda     CLOCKBUF+$02
        jsr     CheckBCD    
        bcs     :+          
        ; validate hours
        ldx     #$00
        ldy     #$23
        lda     CLOCKBUF+$03
        jsr     CheckBCD    
        bcs     :+          
        ; validate day of week
        ldx     #$01
        ldy     #$07
        lda     CLOCKBUF+$04
        jsr     CheckBCD    
        bcs     :+          
        ; validate day of month
        ldx     #$01
        ldy     #$31
        lda     CLOCKBUF+$05
        jsr     CheckBCD    
        bcs     :+          
        ; validate month
        ldx     #$01
        ldy     #$12
        lda     CLOCKBUF+$06
        jsr     CheckBCD    
        bcs     :+          
        ; validate year
        ldx     #$00
        ldy     #$99
        lda     CLOCKBUF+$07
        jsr     CheckBCD    
        bcs     :+
        clc                                     ; all good
        rts
:       sec                                     ; problem
        rts
.endproc
; ----------------------------------------------------------------------------
; Check BCD number in range of [x,y]
; return carry clear if it is, carry set if it is not
.proc   CheckBCD
        sed                                     ; decimal mode
        stx     SCRATCH                         ; lower bound into scratch
        cmp     SCRATCH                         ; compare it
        bcc     :++                             ; fail if out of range
        sty     SCRATCH                         ; upper bound into scratch
        cmp     SCRATCH                         ; compare it
        beq     :+                              ; OK if equal
        bcs     :++                             ; fail if out of range
:       cld                                     ; in range
        clc
        rts
:       cld                                     ; not in range
        sec
        rts
.endproc
; ----------------------------------------------------------------------------
; This code segment is relocated to RELOCT at the start of program
; ----------------------------------------------------------------------------
RELOCS  = * - 1
rseg1bm = *
        .org    RELOCT
rseg1b  = *
; ----------------------------------------------------------------------------
; This routine attempts to execute the next system file that we identified
; in the main code
.proc   LaunchNext
        jsr     PRODOS
        .byte   $C8                             ; OPEN
        .word   PL_OPEN
        bcs     LaunchFail                      ; if error
        lda     PL_OPEN+5                       ; copy reference number
        sta     PL_GET_EOF+1                    ; to parm list for GET_EOF
        sta     PL_READ+1                       ; and parm list for READ
        sta     PL_CLOSE+1                      ; and parm list for CLOSE
        jsr     PRODOS
        .byte   $D1                             ; GET_EOF  
        .word   PL_GET_EOF
        bcs     LaunchFail                      ; if error
        lda     PL_GET_EOF+4                    ; high byte of length
        bne     LaunchFail                      ; bigger than 64K... sheesh
        lda     PL_GET_EOF+2                    ; copy EOF middle and low
        sta     PL_READ+4                       ; to read reqest count
        lda     PL_GET_EOF+3                    ; ...
        sta     PL_READ+5                       ; ...
        jsr     PRODOS                          ; and do read
        .byte   $CA                             ; READ
        .word   PL_READ
        bcs     LaunchFail                      ; if error
        jsr     PRODOS     
        .byte   $CC                             ; CLOSE        
        .word   PL_CLOSE
        bcs     LaunchFail                      ; if error
        jmp     SYSEXEC                         ; execute system file
LaunchFail:
        pha                                     ; save accumulator
        jsr     iprint                          ; because this trashes it
        .byte   $22,"Can't start next SYS file: error #"
        pla                                     ; restore accumulator
        jsr     PrHex                           ; print error code
        lda     #$8D  
        jsr     COUT1 
        jmp     DoQuit                          ; quit to ProDOS
; parameter list for OPEN
PL_OPEN:
        .byte   $03                             ; param count
        .word   PATHBUF                           ; pathname address
        .word   READBUF                           ; I/O buffer
        .byte   $00                             ; reference number
; parameter list for GET_EOF
PL_GET_EOF:
        .byte   $02                             ; param count
        .byte   $00                             ; ref number
        .byte   $00,$00,$00                     ; EOF
; parameter list for READ
PL_READ:
        .byte   $04                             ; param count
        .byte   $00                             ; ref number
        .word   SYSEXEC                           ; data buffer
        .word   $0000                           ; request count
        .word   $0000                           ; transfer count
; parameter list for CLOSE
PL_CLOSE:
        .byte   $01                             ; param count
        .byte   $00                             ; ref number
.endproc
; ----------------------------------------------------------------------------
; Routine to print hex number via LUT
; the firmware has a perfectly good routine for this, not sure why
; AE engineers put this in here.  Nice wheel, though.
.proc   PrHex
        pha
        lsr     a    
        lsr     a    
        lsr     a    
        lsr     a    
        tax
        lda     :+,x 
        jsr     COUT1
        pla
        and     #$0F 
        tax
        lda     :+,x 
        jsr     COUT1
        rts          
:       .byte   "0123456789ABCDEF"
.endproc
; ----------------------------------------------------------------------------
; Execute QUIT call after long delay, exit via RTS if QUIT fails
.proc   DoQuit
        ldx     #$0C
:       lda     #$FF
        jsr     WAIT
        dex         
        bne     :-  
        jsr     PRODOS 
        .byte   $65    
        .word   PL_QUIT
        rts            
; parameter list for QUIT
PL_QUIT:
        .byte   $04                             ; param count
        .byte   $00                             ; quit type - $00 is only type
        .word   $0000                           ; reserved
        .byte   $00                             ; reserved
        .word   $0000                           ; reserved
; unnecessary fill?
        brk
        brk
.endproc
; ----------------------------------------------------------------------------
; 'inline print'
; print counted string following JSR
.proc   iprint
        ; get pointer into $09/$0a
        pla
        clc
        adc     #$01
        sta     CHRPTRL 
        pla
        adc     #$00
        sta     CHRPTRH
        lda     (CHRPTR)
        beq     done
        ; now print the string
        tay          
        ldy     #$01 
ploop:  lda     (CHRPTR),y
        ora     #$80 
        jsr     COUT1
        tya          
        iny          
        cmp     (CHRPTR)
        bcc     ploop
        ; done, fix up stack and exit
done:   clc         
        adc     CHRPTRL 
        sta     CHRPTRL
        lda     #$00
        adc     CHRPTRH
        pha         
        lda     CHRPTRL
        pha         
        rts         
.endproc
; ----------------------------------------------------------------------------
; end of relocated code
rseg1e  = *
        ; fix up org address so we get a clean ref to rclkdrv
        .org rseg1bm + (rseg1e - rseg1b + 1)
; ----------------------------------------------------------------------------
; clock driver code inserted into ProDOS at $D742 - this shouldn't be
; hard-coded but it is.
; critical bug: Corrupts byte in $08xx01 of the //c memory expansion card.
rclkdrv = *
        .org    CLKCODE
.proc   clockdrv
begin   = *
        lda     #$08                            ; useless instruction
        php
        sei
        lda     SLOT4ROM                        ; activate slinky registers
                                                ; ($08 from above overwritten)
        stz     DPTRL                           ; set slinky address to $08xx00
        ldy     #$08                            ; also counter for unlock bytes
        sty     DPTRH
        lda     DATA                            ; get destroyed byte
                                                ; (slinky now at $08xx01)
        pha                                     ; save value on stack
        ; unlock dclock registers
ubytlp: lda     regulk,y
        ldx     #$08                            ; bit counter
.if ::FIX_BUGS
ubitlp: stz     DPTRL                           ; reset pointer to $08xx00
        sta     DATA                            ; write to $08xx00
.else
ubitlp: sta     DATA                            ; write to $08xx00
                                                ; (!but not on first iteration!)
        stz     DPTRL                           ; reset pointer to $08xx00
.endif
        lsr     a                               ; next bit into 0 position
        dex
        bne     ubitlp
        dey
        bne     ubytlp
        ; now read 64 bits (8 bytes) from dclock
        ldx     #$08                            ; byte counter
rbytlp: ldy     #$08                            ; bit counter
rbitlp: pha
        lda     DATA                            ; data byte
        lsr     a                               ; bit 0 into carry
        pla
        ror     a                               ; carry into bit 7
        dey
        bne     rbitlp
        ; got 8 bits now, convert from BCD to binary
        pha
        and     #$0F
        sta     BCDTMP
        pla         
        and     #$F0
        lsr     a   
        pha         
        adc     BCDTMP
        sta     BCDTMP
        pla         
        lsr     a   
        lsr     a   
        adc     BCDTMP
        ; place in input buffer, which is OK because the ThunderClock driver does this
        sta     INBUF-1,x
        dex              
        bne     rbytlp   
        ; done copying, now put necessary values into ProDOS time locations
        ; copy hours to ProDOS hours
        lda     INBUF+4
        sta     TIMEHI 
        ; copy minutes to ProDOS minutes
        lda     INBUF+5
        sta     TIMELO 
        ; copy month ...
        lda     INBUF+1
        lsr     a
        ror     a
        ror     a
        ror     a
        ; ... and day of month to ProDOS month/day
        ora     INBUF+2 
        sta     DATELO  
        ; copy year and final bit of month to ProDOS year/month
        lda     INBUF 
        rol     a     
        sta     DATEHI
        stz     DPTRL                           ; set slinky back to $08xx00
        pla                                     ; get saved byte
        sta     DATA                            ; put it back
        plp
        rts
; DS1215 unlock sequence (in reverse)
regulk  = * - 1
        .byte   $5C, $A3, $3A, $C5, $5C, $A3, $3A, $C5
end     = *
.assert (begin - end + 1) < CLKCODEMAX, error, "DCLOCK driver too big"
.endproc
