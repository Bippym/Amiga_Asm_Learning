; All sprite functions

MoveSprite:
    clr.l      d0
              ; Set sprite control words. Attached sprites share the same control except for the attach bit

    lea        Sprite1a+1,a1        ; Address of sprite into a1
    lea        Sprite1b+1,a3        ; Address of attached sprite
    move.b     (a1),d1              ; Sprite control into d1
    move.b     #215,d2              ; End of movement rhs of screen
    move.b     #$40,d3              ; Start of movement lhs

.1          
    jsr        WaitRaster
    bsr        mwait
    add.b      #1,d1                ; Add 1 to sprite H
    move.b     d1,(a1)              ; Move into the control word
    move.b     d1,(a3)              ; Move into attached sprite control word
    cmp.b      d2,d1                ; Are we at the RHS of screen
    bls        .1                   ; No, loop

              ; Update animation frame
              ; Copy control word from sprite 1 to sprite 2/3
    lea        Sprite2a,a2          ; Sprite address
    lea        Sprite2a+1,a1
    move.b     d1,(a1)
    bsr        Setspriteframe       ; Set it
.2           
    jsr        WaitRaster
    bsr        mwait
    sub.b      #1,d1                ; Subtract 1 from position
    move.b     d1,(a1)              ; Move into control work H
    cmp.b      d3,d1                ; Are we lower then LHS
    bhi        .2                   ; No, loop

              ; Change sprite frame
    lea        Sprite1a,a2          ; Sprite address
    lea        Sprite1a+1,a1
    move.b     d1,(a1)
    bsr        Setspriteframe       ; Set it

    bra        .1                   ; Back to the start

Setspriteframe:
            ; set the next animation frame
            ; a2 -> pointer to next frame to display

;    movem.l    d0-d1/a0-a2,-(sp)    ; preserve our registers
;    move.l     spr0copaddr,a0       ; Address of the sprite control
;    move.l     #(SPR0PTH<<16),d0    ; Sprite high pointer $01020000
;    move.l     a2,d1                ; Address of sprite copied
;    swap       d1                   ; swap the address round so the high word is moveable
;    move.w     d1,d0                ; and move it into d0 (d0 = $0102xxxx)
;    move.l     d0,(a0)+             ; Pop it into the copperlist
;    swap       d1                   ; Swap the address back
;    add.l      #$20000,d0           ; move to the SPR0PTL
;    move.w     d1,d0                ; Low part of the address in
;    move.l     d0,(a0)+             ; And pop the address into the copper
;    movem.l    (sp)+,d0-d1/a0-a2
;    rts

AnimateSprite:
; Animate sprite routine. Animates a sprite
; Registers used
; d0 - Current sprite frame (0-15) and then offset
; d1 - number of sprites to skip
; d2 - Sprite address for swapping about
; d3 - Loop counter
; d4 - control word to copy in
; a0 - Sprite address from the copper
; a1 - Location of current sprite
; a2 - Control word to move into the next frame

    movem.l    d0-d4/a0-a2,-(sp)

    moveq      #0,d0
    moveq      #0,d1
    moveq      #0,d2
    moveq      #0,d3

    lea        spr_data,a2          ; Control word into d4
    move.l     (a2),d4


              ; Get address of first sprite
    move.b     #2-1,d3              ; Num of sprites to make a frame
    move.l     spr0copaddr,a0       ; Location of the first sprite in the copperlist ($0120xxxx)
    move.b     spr_cur_frame,d0     ; What frame are we on?
    move.w     #sprskip,d1          ; Number of sprites to skip
    mulu       d0,d1                ; offset
    lea        Sprite1a,a1          ; address of first sprite

.1  add.l      #$2,a0
    add.l      d1,a1                ; Point to the first sprite of the next frame
    move.l     d4,(a1)              ; Pop in new position to control word 
    move.l     a1,d2                ; Address of sprite
    swap       d2                   ; Get the high word
    move.w     d2,(a0)+             ; Pop address into the copper
    add.l      #$2,a0               ; Get the low word
    swap       d2
    move.w     d2,(a0)+             ; Pop into the copper and advance to next sprite pointer
    move.l     #sprsize,d1          ; Size of 1 sprite


    ; Attached sprite 
    bchg       #7,d4                ; toggle the attached bit

    add.l      #$2,a0               ; Next sprite address
    add.l      d1,a1                ; Point to the first sprite of the next frame
    move.l     d4,(a1)              ; Pop in new position to control word 
    move.l     a1,d2                ; Address of sprite
    swap       d2                   ; Get the high word
    move.w     d2,(a0)+             ; Pop address into the copper
    add.l      #$2,a0               ; Get the low word
    swap       d2
    move.w     d2,(a0)+             ; Pop into the copper and advance to next sprite pointer
    move.l     #sprsize,d1          ; Size of 1 sprite
    bchg       #7,d4                ; toggle the atached bit

    add.l      #$00080000,d4        ; Sprite offset
    dbf        d3,.1                ; Loop to next sprite








    addq       #1,d0

                ; Check if we are on the last frame, if so we go back to the first frame
    cmp.b      #16,d0                                             
    beq        .2                   ; Last frame, so we reset to 0
    bra        .3                   ; Less than the last frame

.2  move.l     #0,d0                ; Back to frame 0
                
.3  move.b     d0,spr_cur_frame     ; Save the frame counter                     
    movem.l    (sp)+,d0-d4/a0-a2
    rts










  