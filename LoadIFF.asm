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

                jmp        ProgStart
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

; My includes
                include    "Startup.i"
                include    "Functions.asm"
                include    "joy.asm"

ProgStart:
; Constants

SCREEN_WIDTH   equ 320
;SCREEN_HEIGHT  equ 256
BITPLANE_SIZE  equ SCREEN_WIDTH / 8
SCREEN_DEPTH   equ 4

; Each sprite is composed of 2 sprites attached (4 sprites for each frame). Each half sprite is $160 bytes (352 bytes Decimal)
; Therefore each sprite is $160*4 

sprheight      equ 86
sprsize        equ (sprheight*4)+8                                            ; Bytes to skip to next sprite (sprheight*bytesperline+controlwords)
sprskip        equ sprsize*4
                                                                              ; We multiply this by 4 to get the next frame

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
                movem.l    d1-d7/a0-a6,-(sp)                                  ; Preserve registers

                move.l     $4,a6                                              ; execbase
                lea        $DFF000,a5                                         ; custom base
	
	; Allocate ram for copperlist
                move.l     #1024,d0
                move.l     #MEMF_CHIP,d1
                jsr        _LVOAllocMem(a6)
                move.l     d0,copperlist                                      ; save copperlist address
	
	; Load gfx library first and get the system copperlist
                clr.l      d0                                                 ; any version
                move.l     #gfxname,a1                                        ; gfx library
                jsr        _LVOOpenLibrary(a6)                                ; openlibrary
                tst.l      d0                                                 ; base address, or 0 for failure
                beq        exit
                move.l     d0,gfxbase                                         ; save gfx lib base address
	
	; Save the system copperlist
                move.l     d0,a1                                              ; Gfxlib base address
                move.l     old_Clist1_off(a1),sys1cop                         ; save the copperlist
                move.l     old_Clist2_off(a1),sys2cop
                move.l     old_View_off(a1),sysview

    ; Set my copperlist up
                bsr        setnewcop
                jsr        takesys

	; Now setup my system
                move.w     #$87e0,DMACON(a5)
                move.w     #$C010,INTENA(a5)                                  ; Vertical blank, and master enable	

                move.l     copperlist,COP1LCH(a5)                             ; pop my copperlist in
                move.w     #0,COPJMP1(a5)                                     ; Initiate copper

                jmp        AnimateSprite
                ;jmp        skipsprite
MoveSprite:
                clr.l      d0
                move.l     spr_cur_frame,d0

              
              
              ; Set sprite control words. Attached sprites share the same control except for the attach bit

                lea        Sprite1a+1,a1                                      ; Address of sprite into a1
                lea        Sprite1b+1,a3                                      ; Address of attached sprite
                move.b     (a1),d1                                            ; Sprite control into d1
              
      
                move.b     #215,d2                                            ; End of movement rhs of screen
                move.b     #$40,d3                                            ; Start of movement lhs

.1          
                jsr        WaitRaster
                bsr        mwait
                add.b      #1,d1                                              ; Add 1 to sprite H
                move.b     d1,(a1)                                            ; Move into the control word
                move.b     d1,(a3)                                            ; Move into attached sprite control word
                cmp.b      d2,d1                                              ; Are we at the RHS of screen
                bls        .1                                                 ; No, loop

              ; Update animation frame
              ; Copy control word from sprite 1 to sprite 2/3
                lea        Sprite2a,a2                                        ; Sprite address
                lea        Sprite2a+1,a1
                move.b     d1,(a1)
                bsr        Setspriteframe                                     ; Set it
.2           
                jsr        WaitRaster
                bsr        mwait
                sub.b      #1,d1                                              ; Subtract 1 from position
                move.b     d1,(a1)                                            ; Move into control work H
                cmp.b      d3,d1                                              ; Are we lower then LHS
                bhi        .2                                                 ; No, loop

              ; Change sprite frame
                lea        Sprite1a,a2                                        ; Sprite address
                lea        Sprite1a+1,a1
                move.b     d1,(a1)
                bsr        Setspriteframe                                     ; Set it

                bra        .1                                                 ; Back to the start

Setspriteframe:
            ; set the next animation frame
            ; a2 -> pointer to next frame to display

                movem.l    d0-d1/a0-a2,-(sp)                                  ; preserve our registers
                move.l     spr0copaddr,a0                                     ; Address of the sprite control
                move.l     #(SPR0PTH<<16),d0                                  ; Sprite high pointer $01020000
                move.l     a2,d1                                              ; Address of sprite copied
                swap       d1                                                 ; swap the address round so the high word is moveable
                move.w     d1,d0                                              ; and move it into d0 (d0 = $0102xxxx)
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                swap       d1                                                 ; Swap the address back
                add.l      #$20000,d0                                         ; move to the SPR0PTL
                move.w     d1,d0                                              ; Low part of the address in
                move.l     d0,(a0)+                                           ; And pop the address into the copper
                movem.l    (sp)+,d0-d1/a0-a2
                rts

; Animate sprite routine. Animates a sprite
; Registers used
; d0 - Current sprite frame (0-15) and then offset
; d1 - number of sprites to skip
; d2 - Sprite address for swapping about
; d3 - Loop counter
; a0 - Sprite address from the copper
; a1 - Location of current sprite

AnimateSprite:

                movem.l    d0-d3/a0-a2,-(sp)

              ; Get address of first sprite
                clr.l      d0
                clr.l      d3

                move.b     #4-1,d3                                            ; Num of sprites to make a frame
                move.l     spr0copaddr,a0                                     ; Location of the first sprite in the copperlist ($0120xxxx)
                move.b     spr_cur_frame,d0
                move.w     #sprskip,d1                                        ; Number of sprites to skip
                mulu       d0,d1                                              ; offset
                lea        Sprite1a,a1                                        ; address of first sprite

.1              add.l      #$2,a0
                add.l      d1,a1                                              ; Point to the first sprite of the next frame 
                move.l     a1,d2                                              ; Address of sprite
                swap       d2                                                 ; Get the high word
                move.w     d2,(a0)+                                           ; Pop address into the copper
                add.l      #$2,a0                                             ; Get the low word
                swap       d2
                move.w     d2,(a0)+                                           ; Pop into the copper and advance to next sprite pointer
                move.l     #sprsize,d1                                        ; Size of 1 sprite
                dbf        d3,.1                                              ; Loop to next sprite
                addq       #1,d0
                cmp.b      #17,d0                                             ; Last frame check
                beq        .2
                bra        .3

.2              move.l     #0,d0                                              ; Back to frame 0
                
.3              move.b     d0,spr_cur_frame                                   ; Increase the frame counter                        
                movem.l    (sp)+,d0-d3/a0-a2

                jmp        skipsprite
mwait:	
             
                btst       #6,$BFE001
                beq        restoresys
                rts

skipsprite:
wframe:
                btst       #0,$dff005
                bne.b      wframe
                cmp.b      #$2a,$dff006
                bne.b      wframe
wframe2:
                cmp.b      #$2a,$dff006
                beq.b      wframe2
                btst       #6,$BFE001
                bne        skipsprite
              
              

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
	
                jsr        loadview(a6)                                       ; Restore the original view
	
                move.l     a0,d0                                              ; GFXBase ready for closing
                MOVE.l     $4,A6                                              ; Execbase
                jsr        permit(a6)                                         ; Enable multitasking
	
exit:
                jsr        closelibrary(a6)                                   ; Close gfx library
	
                movem.l    (sp)+,d1-d7/a0-a6                                  ; Restore registers
                move.l     #0,d0                                              ; Ensure d0 is cleared
                rts                                                           ; exit

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=-=-=-=-

	; Now we work out the bitplane info.
	; Bitplane pointers need to go into the 4 registers. Each bitplane is 10240 bytes
	;
	; d7 - Number of bitplanes
	; a0 - Pointer to copperlist
	; d1 - Pointer to image - We add 10240 after each pointer has been copied into

setnewcop:

                movem.l    d0-d7/a0-a1,-(sp)
                move.l     copperlist,a0                                      ; address of our copperlist
                move.l     #FMODE<<16+0,(a0)+                                 ; Set FMODE into copperlist
                move.w     #BPLCON0,(a0)+
                move.w     #$4200,(a0)+                                       ; Lowres 4bpp screen 010000100000000
                move.w     #BPLCON1,(a0)+
                move.w     #0,(a0)+
                move.w     #BPLCON2,(a0)+
                move.w     #$24,(a0)+
                move.w     #BPL1MOD,(a0)+
                move.w     #(SCREEN_WIDTH/8)*(SCREEN_DEPTH-1),(a0)+
                move.w     #BPL2MOD,(a0)+
                move.w     #(SCREEN_WIDTH/8)*(SCREEN_DEPTH-1),(a0)+
                move.l     #DIWSTRT<<16+$2c81,(a0)+
                move.l     #DIWSTOP<<16+$2cc1,(a0)+                           ; f4c1/2c
                ;move.l     #DIWSTOP<<16+$38c1,(a0)+                           ; PAL offset > 256
                move.l     #DDFSTRT<<16+$0038,(a0)+
                move.l     #DDFSTOP<<16+$00D0,(a0)+

                lea        myimage,a1                                         ; My image address
.loop           cmp.l      #"BODY",(a1)                                       ; We need to search for the body of the image
                beq.s      .found                                             ; Have we found it? Branch if so
                addq       #2,a1                                              ; Not found, so lets increment and try again
                bra.s      .loop


.found          addq       #8,a1                                              ; Move past the BODY header

                moveq      #SCREEN_DEPTH-1,d7                                 ; Number of bitplanes
	
                move.l     #(BPL0PTH<<16),d0                                  ; Bitplane high pointer to $00E00000
.1              move.l     a1,d1                                              ; Address of image copied
                swap       d1                                                 ; swap the address round so the high word is moveable
                move.w     d1,d0                                              ; and move it into d0 (d0 = $00E0xxxx)
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                swap       d1                                                 ; Swap the address back
                add.l      #$20000,d0                                         ; move to the BPLxPTL
                move.w     d1,d0                                              ; Low part of the address in
                move.l     d0,(a0)+                                           ; ANd pop the address into the copper
                add.l      #$20000,d0                                         ; Next BPLxPTH (next bitplane)
                add.l      #(SCREEN_WIDTH/8),a1                               ; Next bitplane image
                dbf        d7,.1                                              ; loop


; Finds the palette within the IFF and sets the palette registers

SetPalette:

                lea        myimage,a1                                         ; Image into a1 again to search for the palette	
.loop           cmp.l      #"CMAP",(a1)                                       ; Looking for the colormap
                beq        .found                                             ; Have we found the colormap
                addq       #2,a1                                              ; Not found, move on :)
                bra.s      .loop
	
.found          addq       #8,a1                                              ; Jump over the header

                move.l     #SCREEN_DEPTH<<2-1,d7                              ; Number of colours in the image
	
                clr.l      d4
                clr.l      d3
                clr.l      d2
                clr.l      d1
	
                move.l     #(COLOR00<<16),d5                                  ; First colour entry
.2              move.b     (a1)+,d1                                           ; Red 
                move.b     (a1)+,d2                                           ; Green
                move.b     (a1)+,d3                                           ; Blue
	
                and.b      #%11110000,d1                                      ; Drop lower nibble
                and.b      #%11110000,d2
                and.b      #%11110000,d3	
                move.b     d1,d5                                              ; Move red
                lsl.w      #4,D5                                              ; Shift to the left
                or.w       D2,D5                                              ; Move green in
                lsl.w      #4,D5                                              ; ANd shift
                or.w       D3,D5                                              ; Move in blue
                lsr.w      #4,d5                                              ; fix the offset
	
                move.l     d5,(a0)+                                           ; Put the colour register into the copperlist
	
                add.l      #$20000,d5                                         ; Next colour register
                dbf        d7,.2                                              ; Loop

              ; Set Palette entries 16-31 for the sprites
                move.l     #16-1,d7                                           ; No' Colours
                lea        SpritePal,a1                                       ; Address of sprite colours
                move.l     #(COLOR16<<16),d0                                  ; Palette 16
.4              move.w     (a1)+,d0                                           ; colour copied
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                add.l      #$20000,d0                                         ; move to the SPR0PTL
                dbf        d7,.4


	; We work out the sprite info.
	; Sprite pointers need to go into the 16 registers. 
  ; d7 - Number of sprites (8-1)
	; a0 - Pointer to offset in the copperlist
  ; a1 - Point to the sprite data
	; d1 - Pointer to sprite data image
	
             ; Set sprite pointers into registers
SetSprite:
              ; left half of the owl. Sprite 0
                move.l     a0,spr0copaddr                                     ; Offset to the sprite copper address
                move.l     #8-5,d7                                            ; Number of sprites (8 - the attached sprites 0/1 and 2/3)

                lea        Sprite1a,a1                                        ; Sprite address
                move.l     #(SPR0PTH<<16),d0                                  ; Sprite high pointer $01020000
                move.l     a1,d1                                              ; Address of sprite copied
                swap       d1                                                 ; swap the address round so the high word is moveable
                move.w     d1,d0                                              ; and move it into d0 (d0 = $0102xxxx)
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                swap       d1                                                 ; Swap the address back
                add.l      #$20000,d0                                         ; move to the SPR0PTL
                move.w     d1,d0                                              ; Low part of the address in
                move.l     d0,(a0)+                                           ; And pop the address into the copper

              ; Sprite 1, attached
                move.l     a0,spr1copaddr                                     ; Save attached sprite address
                lea        Sprite1b,a1                                        ; Attach sprite location
                move.l     #(SPR1PTH<<16),d0                                  ; Sprite 1 high pointer
                move.l     a1,d1                                              ; Address of sprite copied
                swap       d1                                                 ; swap the address round so the high word is moveable
                move.w     d1,d0                                              ; and move it into d0 (d0 = $0102xxxx)
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                swap       d1                                                 ; Swap the address back
                add.l      #$20000,d0                                         ; move to the SPRxPTL
                move.w     d1,d0                                              ; Low part of the address in
                move.l     d0,(a0)+                                           ; And pop the address into the copper

              ; Right half of the owl Sprite 2
                move.l     a0,spr2copaddr                                     ; Save attached sprite address
                lea        Sprite2a,a1                                        ; Attach sprite location
                move.l     #(SPR2PTH<<16),d0                                  ; Sprite 1 high pointer
                move.l     a1,d1                                              ; Address of sprite copied
                swap       d1                                                 ; swap the address round so the high word is moveable
                move.w     d1,d0                                              ; and move it into d0 (d0 = $0102xxxx)
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                swap       d1                                                 ; Swap the address back
                add.l      #$20000,d0                                         ; move to the SPRxPTL
                move.w     d1,d0                                              ; Low part of the address in
                move.l     d0,(a0)+                                           ; And pop the address into the copper

              ; Sprite 1, attached
                move.l     a0,spr3copaddr                                     ; Save attached sprite address
                lea        Sprite2b,a1                                        ; Attach sprite location
                move.l     #(SPR3PTH<<16),d0                                  ; Sprite 1 high pointer
.3              move.l     a1,d1                                              ; Address of sprite copied
                swap       d1                                                 ; swap the address round so the high word is moveable
                move.w     d1,d0                                              ; and move it into d0 (d0 = $0102xxxx)
                move.l     d0,(a0)+                                           ; Pop it into the copperlist
                swap       d1                                                 ; Swap the address back
                add.l      #$20000,d0                                         ; move to the SPRxPTL
                move.w     d1,d0                                              ; Low part of the address in
                move.l     d0,(a0)+                                           ; And pop the address into the copper

                move.b     #1,spr_cur_frame                                   ; Set the initial animation frame


              ; Set the null sprites for the remaining 6 sprites
                add.l      #$20000,d0                                         ; next SPRxPTH
                lea        NullSpr,a1                                         ; pointer to the null sprite data
                dbf        d7,.3                                              ; loop

                move.l     #$FFFFFFFE,(a0)                                    ; End the copperlist
                movem.l    (sp)+,d0-d7/a0-a1

                rts
	
	
;***********************************************************************************************
	
                SECTION    coplistexample,DATA_C

copperlist:     dc.l       0	
                even
gfxname:
                dc.b       "graphics.library",0
                even
gfxbase:
                dc.l       0
                even
sys1cop:        dc.l       0                                                  ; System copperlist
sys2cop:        dc.l       0                                                  ; System copperlist2
sysview:        dc.l       0                                                  ; Systemview

; Below we have sprite information, this includes variables for saving the various sprite
; memory locations within the copperlist. This will allow us to update the copperlist dynamically
; Allowing some animation.


spr0copaddr:    dc.l       0                                                  ; Address within copperlist to adjust sprite pointer
spr1copaddr:    dc.l       0
spr2copaddr:    dc.l       0                                                  ; Address within copperlist to adjust sprite pointer
spr3copaddr:    dc.l       0

spr_cur_frame:  dc.b       0                                                  ; Current sprite frame (1-16)

                even

SpritePal:    
                dc.w       $0F0F,$07DF,$0FFF,$008F,$0865,$0975,$0B97,$0DB9
                dc.w       $0EDC,$0CBE,$0A9C,$087A,$0658,$0436,$0755,$0F0F

; Include the agony sprite images. 32 attached sprites for the full animation
                include    "owl.src"
              ; include    "Agony.spr"  

NullSpr:
                dc.w       $2a20,$2b00
                dc.w       0,0
                dc.w       0,0

myimage:
                incbin     "BippyM.pic"
                even