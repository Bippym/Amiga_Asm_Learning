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

  movem.l    d0-d1/a0-a2,-(sp)    ; preserve our registers
  move.l     spr0copaddr,a0       ; Address of the sprite control
  move.l     #(SPR0PTH<<16),d0    ; Sprite high pointer $01020000
  move.l     a2,d1                ; Address of sprite copied
  swap       d1                   ; swap the address round so the high word is moveable
  move.w     d1,d0                ; and move it into d0 (d0 = $0102xxxx)
  move.l     d0,(a0)+             ; Pop it into the copperlist
  swap       d1                   ; Swap the address back
  add.l      #$20000,d0           ; move to the SPR0PTL
  move.w     d1,d0                ; Low part of the address in
  move.l     d0,(a0)+             ; And pop the address into the copper
  movem.l    (sp)+,d0-d1/a0-a2
  rts










  