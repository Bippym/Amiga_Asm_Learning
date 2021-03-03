; First example to load and display an iff image. 
; Image saved in Personal Paint as an uncompressed
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

              jmp        Begin
; Base custom address
              incdir     "include"
              include    "funcdef.i"
              include    "i/custom.i"
              include    "exec/types.i"
              include    "exec/exec.i"
              include    "exec/exec_lib.i"
              include    "libraries/dos.i"
              include    "libraries/dos_lib.i"
              include    "graphics/gfxbase.i"
              include    "graphics/graphics_lib.i"
              include    "Startup.i"

Begin:
; Constants

SCREEN_WIDTH   equ 320
;SCREEN_HEIGHT  equ 256
BITPLANE_SIZE  equ SCREEN_WIDTH / 8
SCREEN_DEPTH   equ 4

;library offsets

exec           equ $4
openlibrary    equ -$198
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
              lea        $DFF000,a5                                  ; custom base
	
	; Allocate ram for copperlist
              move.l     #1024,d0
              move.l     #MEMF_CHIP,d1
              jsr        _LVOAllocMem(a6)
              move.l     d0,copperlist                               ; save copperlist address
	
	; Load gfx library first and get the system copperlist
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

    ; Set my copperlist up
              bsr        setnewcop
              jsr        takesys

	; Now setup my system
              move.w     #$87e0,DMACON(a5)
              move.w     #$C010,INTENA(a5)                           ; Vertical blamk, and master enable	

              move.l     copperlist,COP1LCH(a5)                      ; pop my copperlist in
              move.w     #0,COPJMP1(a5)                              ; Initiate copper

MoveSprite:
              clr.l      d1
              lea        Sprite+1,a1                                 ; Address of sprite into a1
              move.b     (a1),d1                                     ; Sprite control into d1
              move.b     #215,d2                                     ; End of movement rhs of screen
              move.b     #$40,d3                                     ; Start of movement lhs

.1          
              jsr        WaitRaster
              bsr        mwait
              add.b      #1,d1                                       ; Add 1 to sprite H
              move.b     d1,(a1)                                     ; Move into the control word
              cmp.b      d2,d1                                       ; Are we at the RHS of screen
              bls        .1                                          ; No, loop

              ; Update animation frame
              ; Copy control word from sprite 1 to sprite 2/3
              lea        Sprite_f3,a2                                ; Sprite address
              lea        Sprite_f3+1,a1
              move.b     d1,(a1)
              bsr        Setspriteframe                              ; Set it
.2           
              jsr        WaitRaster
              bsr        mwait
              sub.b      #1,d1                                       ; Subtract 1 from position
              move.b     d1,(a1)                                     ; Move into control work H
              cmp.b      d3,d1                                       ; Are we lower then LHS
              bhi        .2                                          ; No, loop

              ; Change sprite frame
              lea        Sprite,a2                                   ; Sprite address
              lea        Sprite+1,a1
              move.b     d1,(a1)
              bsr        Setspriteframe                              ; Set it

              bra        .1                                          ; Back to the start

Setspriteframe:
 ; set the next animation frame
 ; a2 -> pointer to next frame to display

              movem.l    d0-d1/a0-a2,-(sp)                           ; preserve our registers
              move.l     spr0copaddr,a0                              ; Address of the sprite control
              move.l     #(SPR0PTH<<16),d0                           ; Sprite high pointer $01020000
              move.l     a2,d1                                       ; Address of sprite copied
              swap       d1                                          ; swap the address round so the high word is moveable
              move.w     d1,d0                                       ; and move it into d0 (d0 = $0102xxxx)
              move.l     d0,(a0)+                                    ; Pop it into the copperlist
              swap       d1                                          ; Swap the address back
              add.l      #$20000,d0                                  ; move to the SPR0PTL
              move.w     d1,d0                                       ; Low part of the address in
              move.l     d0,(a0)+                                    ; And pop the address into the copper
              movem.l    (sp)+,d0-d1/a0-a2
              rts

mwait:	
             
              btst       #6,$BFE001
              beq        restoresys
              rts



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

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-

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
              move.w     #$24,(a0)+
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
.loop         cmp.l      #"BODY",(a1)                                ; We need to search for the body of the image
              beq.s      .found                                      ; Have we found it? Branch if so
              addq       #2,a1                                       ; Not found, so lets increment and try again
              bra.s      .loop


.found        addq       #8,a1                                       ; Move past the BODY header

              moveq      #SCREEN_DEPTH-1,d7                          ; Number of bitplanes
	
              move.l     #(BPL0PTH<<16),d0                           ; Bitplane high pointer to $00E00000
.1            move.l     a1,d1                                       ; Address of image copied
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


; Finds the palette within the IFF and sets the palette registers

SetPalette:

              lea        myimage,a1                                  ; Image into a1 again to search for the palette	
.loop         cmp.l      #"CMAP",(a1)                                ; Looking for the colormap
              beq        .found                                      ; Have we found the colormap
              addq       #2,a1                                       ; Not found, move on :)
              bra.s      .loop
	
.found        addq       #8,a1                                       ; Jump over the header

              move.l     #SCREEN_DEPTH<<2-1,d7                       ; Number of colours in the image
	
              clr.l      d4
              clr.l      d3
              clr.l      d2
              clr.l      d1
	
              move.l     #(COLOR00<<16),d5                           ; First colour entry
.2            move.b     (a1)+,d1                                    ; Red 
              move.b     (a1)+,d2                                    ; Green
              move.b     (a1)+,d3                                    ; Blue
	
              and.b      #%11110000,d1                               ; Drop lower nibble
              and.b      #%11110000,d2
              and.b      #%11110000,d3	
              move.b     d1,d5                                       ; Move red
              lsl.w      #4,D5                                       ; Shift to the left
              or.w       D2,D5                                       ; Move green in
              lsl.w      #4,D5                                       ; ANd shift
              or.w       D3,D5                                       ; Move in blue
              lsr.w      #4,d5                                       ; fix the offset
	
              move.l     d5,(a0)+                                    ; Put the colour register into the copperlist
	
              add.l      #$20000,d5                                  ; Next colour register
              dbf        d7,.2                                       ; Loop


	; We work out the sprite info.
	; Sprite pointers need to go into the 16 registers.
	;
	; d7 - Number of sprites (8-1)
	; a0 - Pointer to offset in the copperlist
	; d1 - Pointer to sprite data image
	
             ; Set sprite pointers into registers
SetSprite:
              move.l     a0,spr0copaddr                              ; Offset to the sprite copper address
              move.l     #8-1,d7                                     ; Number of sprites
              lea        Sprite,a1                                   ; Sprite address
              move.l     #(SPR0PTH<<16),d0                           ; Sprite high pointer $01020000
.3            move.l     a1,d1                                       ; Address of sprite copied
              swap       d1                                          ; swap the address round so the high word is moveable
              move.w     d1,d0                                       ; and move it into d0 (d0 = $0102xxxx)
              move.l     d0,(a0)+                                    ; Pop it into the copperlist
              swap       d1                                          ; Swap the address back
              add.l      #$20000,d0                                  ; move to the SPR0PTL
              move.w     d1,d0                                       ; Low part of the address in
              move.l     d0,(a0)+                                    ; And pop the address into the copper
              add.l      #$20000,d0                                  ; Next SPRxPTH (next sprite)
              lea        NullSpr,a1                                  ; pointer to the null sprite data
             ;add.l      #(SCREEN_WIDTH/8),a1                       ; Next bitplane image
              dbf        d7,.3                                       ; loop

              move.l     #$1a20000,(a0)+                             ; Sprite colour registers (17-19) (Black)
              move.l     #$1a40f00,(a0)+                             ; (red)
              move.l     #$1a60ff0,(a0)+                             ; (yellow)

          
              move.l     #$FFFFFFFE,(a0)                             ; End the copperlist
              movem.l    (sp)+,d0-d7/a0-a1

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

bplane:       ds.b       1024                                        ; reserve some copperlist space
              even
	

copperlist:   dc.l       0	
              even
gfxname:
              dc.b       "graphics.library",0
              even
gfxbase:
              dc.l       0
              even
sys1cop:      dc.l       0                                           ; System copperlist
sys2cop:      dc.l       0                                           ; System copperlist2
sysview:      dc.l       0                                           ; Systemview

; Below we have sprite information, this includes variables for saving the various sprite
; memory locations within the copperlist. This will allow us to update the copperlist dynamically
; Allowing some animation.

spr0copaddr:  dc.l       0                                           ; Address within copperlist to adjust sprite pointer

Sprite:
              dc.w       $2c40,$3c00
              dc.w       %0000011111000000,%0000000000000000
              dc.w       %0001111111110000,%0000000000000000
              dc.w       %0011111111111000,%0000000000000000
              dc.w       %0111111111111100,%0000000000000000
              dc.w       %0110011111001100,%0001100000110000
              dc.w       %1110011111001110,%0001100000110000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0010000000001000
              dc.w       %1111111111111110,%0001100000110000
              dc.w       %0111111111111100,%0000011111000000
              dc.w       %0111111111111100,%0000000000000000
              dc.w       %0011111111111000,%0000000000000000
              dc.w       %0001111111110000,%0000000000000000
              dc.w       %0000011111000000,%0000000000000000
              dc.w       %0000000000000000,%0000000000000000
              dc.w       0,0

; Frame 2 of our 3 frame sprite
Sprite_f2:
              dc.w       $2c40,$3c00
              dc.w       %0000011111000000,%0000000000000000
              dc.w       %0001111111110000,%0000000000000000
              dc.w       %0011111111111000,%0000000000000000
              dc.w       %0111111111111100,%0000000000000000
              dc.w       %0110011111001100,%0001100000110000
              dc.w       %1110011111001110,%0001100000110000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %0111111111111100,%0011111111111000
              dc.w       %0111111111111100,%0000000000000000
              dc.w       %0011111111111000,%0000000000000000
              dc.w       %0001111111110000,%0000000000000000
              dc.w       %0000011111000000,%0000000000000000
              dc.w       %0000000000000000,%0000000000000000
              dc.w       0,0

; Frame 3 of our 3 frame sprite
Sprite_f3:
              dc.w       $2c40,$3c00
              dc.w       %0000011111000000,%0000000000000000
              dc.w       %0001111111110000,%0000000000000000
              dc.w       %0011111111111000,%0000000000000000
              dc.w       %0111111111111100,%0000000000000000
              dc.w       %0110011111001100,%0001100000110000
              dc.w       %1110011111001110,%0001100000110000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %1111111111111110,%0000000000000000
              dc.w       %0111111111111100,%0000011111000000
              dc.w       %0111111111111100,%0001100000110000
              dc.w       %0011111111111000,%0010000000001000
              dc.w       %0001111111110000,%0000000000000000
              dc.w       %0000011111000000,%0000000000000000
              dc.w       %0000000000000000,%0000000000000000
              dc.w       0,0
NullSpr:
              dc.w       $2a20,$2b00
              dc.w       0,0
              dc.w       0,0

myimage:
              incbin     "BippyM.pic"
              even