; My basic startup routines.
; Registers
;
; a5 = Custom ($Dff000)
; a6 = Execbase ($4)


; Take system routines

         jmp       Begin

takesys:

	; Backup our system
         move.w    DMACONR(a5),d0
         or.w      #$8000,d0
         move.w    d0,dmasys
	
         move.w    ADKCONR(a5),d0
         or.w      #$8000,d0
         move.w    d0,adksys
	
         move.w    INTENAR(a5),d0
         or.w      #$8000,d0
         move.w    d0,intsys
	
         move.w    INTREQR(a5),d0
         or.w      #$8000,d0
         move.w    d0,intrqs
	
	; Turn off multitasking
	
         jsr       forbid(a6)

	; Take the system
	
         move.w    #$7fff,DMACON(a5)
         move.w    #$7fff,INTENA(a5)
         move.w    #$7fff,INTREQ(a5)
	
         rts




dmasys:  dc.l      0                    ; dma
adksys:  dc.l      0                    ; ADKconR
intsys:  dc.l      0                    ; Intenar
intrqs:  dc.l      0                    ; Intreq
