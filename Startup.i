; My basic startup routines.
; Registers
;
; a5 = Custom ($Dff000)
; a6 = Execbase ($4)


; Take system routines

         jmp        Begin

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
	
         move.w     #$138,d0             ;wait for EOFrame
         bsr.w      WaitRaster

	; Turn off multitasking
	
         jsr        forbid(a6)

	; Take the system
	
         move.w     #$7fff,DMACON(a5)
         move.w     #$7fff,INTENA(a5)
         move.w     #$7fff,INTREQ(a5)
	
         rts

WaitRaster:
         movem.l    d1-d2/a0,-(sp)       ; Preserve registers
         move.l     #$1ff00,D2
         lsl.l      #8,d0
         and.l      d2,d0
         lea        $dff004,a0
.wr      move.l     (a0),d1
         and.l      d2,d1
         cmp.l      d1,d0
         bne.s      .wr
         movem.l    (sp)+,d1-d2/a0       ; Restore registers
         rts


dmasys:  dc.l       0                    ; dma
adksys:  dc.l       0                    ; ADKconR
intsys:  dc.l       0                    ; Intenar
intrqs:  dc.l       0                    ; Intreq
