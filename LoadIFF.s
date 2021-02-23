; First example to load and display an iff image. 
; Image saved in Personal Paint as an uncompressed IFF
;
; Stages required:
;
; Turn off interrupts, DMA and then set the 
; Save system copper using gfx library
; Allocate ram for the image
; Incbin image
; Set bitplane registers
; Load address of image to the bltpointers

; Image is (320/8bits)*256*4 = 40*256*4 =  40960 bytes (4*10240 per plane)
; Hardware offsets

; Base custom address

;-----------------[ Includes ]-----------------------
             INCDIR     "include"
             include    "i/custom.i"
             include    "exec/types.i"
             include    "exec/exec.i"
             include    "exec/exec_lib.i"
             include    "libraries/dos.i"
             include    "libraries/dos_lib.i"
             include    "graphics/gfxbase.i"
             include    "graphics/graphics_lib.i"
;----------------------------------------------------

;-----------------[ Constants ]-----------------------
SCREEN_WIDTH   equ 320
SCREEN_HEIGHT  equ 256
BITPLANE_SIZE  equ SCREEN_WIDTH / 8
SCREEN_DEPTH   equ 4

;-----------------[ Library Offsets ]-----------------------
exec           equ $4
openlibrary    equ -408
closelibrary   equ -414
forbid         equ -132
permit         equ -138
supervisor     equ -30
loadview       equ -222
waittoff       equ -270
ownblitter     equ -456
disownblitter  equ -462
waitblit       equ -228

; My offsets

old_View_off   equ 34
old_Clist1_off equ 38
old_Clist2_off equ 50


start:
             movem.l    d0-d7/a0-a6,-(sp)                           ; Preserve registers
             move.l     $4,a6                                       ; execbase
             lea        CUSTOM,a5                                   ; custom base ($DFF000)
	
	; Allocate ram for copperlist
             move.l     #1024,d0                                    ; 1k to reserve
             move.l     #MEMF_CHIP,d1                               ; We need chipram
             jsr        _LVOAllocMem(a6)                            ; Allocate the memory
             move.l     d0,copperlist                               ; save copperlist address
	
	; Load gfx library and get the system copperlist
             clr.l      d0                                          ; any version
             move.l     #gfxname,a1                                 ; gfx library
             jsr        _LVOOpenLibrary(a6)                         ; openlibrary
             tst.l      d0                                          ; base address, or 0 for failure
             beq        exit
             move.l     d0,gfxbase                                  ; save gfx lib base address
	
	; Save the system copperlist
             move.l     d0,a1                                       ; Gfxlib base address
             move.l     old_Clist1_off(a1),sys1cop                  ; save the copperlist
             move.l     old_Clist2_off(a1),sys2cop
             move.l     old_View_off(a1),sysview

             bsr        setnewcop
	;bsr		SetPalette

takesys:

	; Backup our system
	
             move.w     DMACONR(a5),d0
             or.w       #$8000,d0
             move.w     d0,dmasys
	
             move.w     ADKCONR(a5),d0
             or.w       #$8000,d0
             move.w     d0,adksys
	
             move.w     INTENAR(a5),d0
             or.w       #$8000,d0
             move.w     d0,intsys
	
             move.w     INTREQR(a5),d0
             or.w       #$8000,d0
             move.w     d0,intrqs
	
	; Turn off multitasking
	
             jsr        forbid(a6)

	; Take the system
	
             move.w     #$7fff,DMACON(a5)
             move.w     #$7fff,INTENA(a5)
             move.w     #$7fff,INTREQ(a5)
	
	; Now setup my system
             move.w     #$8380,DMACON(a5)
             move.w     #$C010,INTENA(a5)                           ; Vertical blamk, and master enable	

	
             move.l     copperlist,COP1LCH(a5)                      ; pop my copperlist in
             move.w     #0,COPJMP1(a5)                              ; Initiate copper
mwait:	
             btst       #6,$BFE001
             bne        mwait


restoresys:
	; Restore the system
             move.w     #$7fff,DMACON(a5)
             move.w     dmasys,DMACON(a5)

             move.w     #$7fff,INTENA(a5)
             move.w     intsys,INTENA(a5)
	
             move.w     #$7fff,INTREQ(a5)
             move.w     intrqs,INTREQ(a5)
	
             move.l     sys1cop,COP1LCH(a5)
;	move.l		sys2cop,COP2LCH(a5)


	; Restore the gfx view
             move.l     gfxbase,a0
             move.l     sysview,a1
	
             jsr        loadview(a6)                                ; Restore the original view

             move.l     a0,d0                                       ; GFXBase ready for closing
             MOVE.l     $4,A6                                       ; Execbase
             jsr        permit(a6)                                  ; Enable multitasking
	
exit:
             jsr        closelibrary(a6)                            ; Close gfx library
	
             movem.l    (sp)+,d0-d7/a0-a6                           ; Restore registers
             move.l     #0,d0                                       ; Ensure d0 is cleared
             rts                                                    ; exit


	; Now we work out the bitplane info.
	; Bitplane pointers need to go into the 4 registers. Each bitplane is 10240 bytes
	;
	; d7 - Number of bitplanes
	; a0 - Pointer to copperlist
	; d1 - Pointer to image - We add 10240 after each pointer has been copied into

setnewcop:

             movem.l    d0-d7/a0-a1,-(sp)
             move.l     copperlist,a0                               ; address of our copperlist
             move.l     #FMODE<<16+0,(a0)+                          ; Set FMODE into copperlist
             move.w     #BPLCON0,(a0)+
             move.w     #$4200,(a0)+                                ; Lowres 4bpp screen 010000100000000
             move.w     #BPLCON1,(a0)+
             move.w     #0,(a0)+
             move.w     #BPLCON2,(a0)+
             move.w     #0,(a0)+
             move.w     #BPL1MOD,(a0)+
             move.w     #(SCREEN_WIDTH/8)*(SCREEN_DEPTH-1),(a0)+
             move.w     #BPL2MOD,(a0)+
             move.w     #(SCREEN_WIDTH/8)*(SCREEN_DEPTH-1),(a0)+
             move.l     #DIWSTRT<<16+$2c81,(a0)+
             move.l     #DIWSTOP<<16+$f4c1,(a0)+
             move.l     #DIWSTOP<<16+$38c1,(a0)+                    ; PAL offset > 256
             move.l     #DDFSTRT<<16+$0038,(a0)+
             move.l     #DDFSTOP<<16+$00D0,(a0)+

             lea        myimage,a1                                  ; My image address
.loop        cmp.l      #"BODY",(a1)                                ; We need to search for the body of the image
             beq.s      .found                                      ; Have we found it? Branch if so
             addq       #2,a1                                       ; Not found, so lets increment and try again
             bra.s      .loop


.found       addq       #8,a1                                       ; Move past the BODY header

             moveq      #SCREEN_DEPTH-1,d7                          ; Number of bitplanes
	
             move.l     #(BPL0PTH<<16),d0                           ; Bitplane high pointer to $00E00000
.1           move.l     a1,d1                                       ; Address of image copied
             swap       d1                                          ; swap the address round so the high word is moveable
             move.w     d1,d0                                       ; and move it into d0 (d0 = $00E0xxxx)
             move.l     d0,(a0)+                                    ; Pop it into the copperlist
             swap       d1                                          ; Swap the address back
             add.l      #$20000,d0                                  ; move to the BPLxPTL
             move.w     d1,d0                                       ; Low part of the address in
             move.l     d0,(a0)+                                    ; ANd pop the address into the copper
             add.l      #$20000,d0                                  ; Next BPLxPTH (next bitplane)
             add.l      #(SCREEN_WIDTH/8),a1                        ; Next bitplane image
             dbf        d7,.1                                       ; loop
	;rts

SetPalette:

             lea        myimage,a1                                  ; Image into a1 again to search for the palette	
.loop        cmp.l      #"CMAP",(a1)                                ; Looking for the colormap
             beq        .found                                      ; Have we found the colormap
             addq       #2,a1                                       ; Not found, move on :)
             bra.s      .loop
	
.found       addq       #8,a1                                       ; Jump over the header

             move.l     #SCREEN_DEPTH<<2-1,d7                       ; Number of colours in the image
	
             clr.l      d4
             clr.l      d3
             clr.l      d2
             clr.l      d1
	
             move.l     #(COLOR00<<16),d5                           ; First colour entry
.2           move.b     (a1)+,d1                                    ; Red 
             move.b     (a1)+,d2                                    ; Green
             move.b     (a1)+,d3                                    ; Blue
	
             and.b      #%11110000,d1                               ; Drop lower nibble
             and.b      #%11110000,d2
             and.b      #%11110000,d3	
             move.b     d1,d5                                       ; Move red
             lsl.w      #4,D5                                       ; Shift to the left
             or.w       D2,D5                                       ; Move green in
             lsl.w      #4,D5                                       ; And shift
             or.w       D3,D5                                       ; Move in blue
             lsr.w      #4,d5                                       ; fix the offset
	
             move.l     d5,(a0)+                                    ; Put the colour register into the copperlist
	
             add.l      #$20000,d5                                  ; Next colour register
             dbf        d7,.2                                       ; Loop
	
             move.l     #$FFFFFFFE,(a0)                             ; End the copperlist
             movem.l    (sp)+,d0-d7/a0-a1

             rts
	
; Next we find the palette entries and populate the color registers with the correct entries. We will also need to determine if the palette
; is a 12 bit or 24 bit palette. We are going to only 	

; OLD COPPERLIST - WE DON'T GET CALLED	
	
setcopper:

             moveq      #4-1,d0                                     ; Number of planes
             move.l     #bplane,a0                                  ; Address of the copper
             move.w     #BPL1PTH,d2                                 ; Lower half of BPlane 1
             move.l     #myimage,d1                                 ; Pointer to start of image data
             swap       d1
	
.1           move.w     d2,d4                                       ;  d4 = $000000e0
             swap       d4                                          ;  d4 = $00e00000
             move.w     d1,d4                                       ;  d4 = Â£00e00001

             move.l     d4,(a0)+                                    ; Copy complete copper instruction ($00ex,$memloc)

             clr.l      d4                                          ; Clr d4
             add        #2,d2                                       ; Move to next hardware address
             move.w     d2,d4                                       ; Copy into low byte of d4
             swap       d4                                          ; Swap to hugh byte
             swap       d1                                          ; 
             move.w     d1,d4                                       ; 
             move.l     d4,(a0)+
	
             add.l      #10240,d1
             swap       d1
             add        #2,d2
             dbf        d0,.1
	
	; Copy the palette into the colour registers
	; d0 - red
	; d1 - Green
	; d2 - Bue
	; d4 - 12 bit palette entry
	; a1 - palette entry
	; a2 - colour register
	; d5 counter
	
             clr.l      d0
             clr.l      d1
             clr.l      d2
             clr.l      d4
		
             moveq      #16-1,d5                                    ; number of colour entries
	;move.l	#palette,a1
             move.l     #COLOR00,d3
	
	; %1100 1100
	; %1100 0000
	
.2           move.b     (a1)+,d0                                    ; Drop first byte
             move.b     (a1)+,d0                                    ; Red 
             move.b     (a1)+,d1                                    ; Green
             move.b     (a1)+,d2                                    ; Blue
	
             and.b      #%11110000,d0                               ; Drop lower nibble
             and.b      #%11110000,d1
             and.b      #%11110000,d2	
             move.b     d0,d4                                       ; Move red
             lsl.w      #4,D4                                       ; Shift to the left
             or.w       D1,D4                                       ; Move green in
             lsl.w      #4,D4                                       ; ANd shift
             or.w       D2,D4                                       ; Move in blue
             lsr.w      #4,d4                                       ; fix the offset
	
             move.w     d3,(a0)+                                    ; Put the colour register into the copperlist
             move.w     d4,(a0)+                                    ; And the colour
	
             add        #2,d3
             dbf        d5,.2                                       ; Loop
	
             move.l     #$FFFFFFFE,(a0)                             ; End the copperlist
	
             rts
	
;***********************************************************************************************
	
             SECTION    coplistexample,DATA_C

copperlist1:
             dc.w       $01fc,$0000                                 ; Slow fetch mode, AGA compatibility
             dc.w       $0100,$5200                                 ; Lowres 4BPP screen
             dc.w       $0102,$0000
             dc.w       $0104,$0000	
             dc.w       $0108,$0000                                 ; Bpl1Mod (odd planws)
             dc.w       $010a,$0000                                 ; Bpl2mod (even planes)
             dc.w       DIWSTRT,$2c81                               ; Upper left
             dc.w       DIWSTOP,$f4c1
             dc.w       DIWSTOP,$38C1                               ; Pal
             dc.w       DDFSTRT,$0038
             dc.w       DDFSTOP,$00D0		

bplane:      ds.b       1024                                        ; reserve some copperlist space
             even
	

copperlist:  dc.l       0	
             even
gfxname:
             dc.b       "graphics.library",0
             even
gfxbase:
             dc.l       0
             even
sys1cop:     dc.l       0                                           ; System copperlist
sys2cop:     dc.l       0                                           ; System copperlist2
dmasys:      dc.l       0                                           ; dma
adksys:      dc.l       0                                           ; ADKconR
intsys:      dc.l       0                                           ; Intenar
intrqs:      dc.l       0                                           ; Intreq
sysview:     dc.l       0                                           ; Systemview

myimage:
             incbin     "BippyM.pic"
             even