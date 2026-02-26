;***********************************************************
;*	This is the skeleton file for Lab 5 of ECE 375
;*
;*	 Authors: Noam Yaffe, Scott Bremmer
;*	   Date: 2/20/2026
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	waitcnt = r17			; Wait Loop Counter
.def	ilcnt = r18				; Inner Loop Counter
.def	olcnt = r19				; Outer Loop Counter

; Counter Registers
.def    countR = r23	            ; Right hit counter
.def    countL = r24	            ; Left hit counter

.equ	WTime = 100				; 1 second delay

; Motor Direction Constants (for Port B)
.equ	EngDirR = 4							; Right Engine Direction Bit
.equ	EngDirL = 7							; Left Engine Direction Bit
.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; $90 - Move Forward
.equ	MovBck = $00				        ; $00 - Move Backward
.equ	TurnR = (1<<EngDirL)			    ; $80 - Turn Right
.equ	TurnL = (1<<EngDirR)			    ; $10 - Turn Left

; LCD memory constants
.equ	LCD_L1 = $0100						; Start of Line 1 in SRAM (Data Memory)
.equ	LCD_L2 = $0110						; Start of Line 2 in SRAM (Data Memory)

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used

.org	$0002					; External Interrupt INT0 --> Right Whisker
		rcall HIT_RIGHT
		reti

.org	$0004					; External Interrupt INT1 --> Left Whisker
		rcall HIT_LEFT
		reti

.org	$0008					; External Interrupt INT3 --> Clear Counters
		rcall CLEAR_COUNTERS
		reti

.org	$0056					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi mpr, low(RAMEND)
		out SPL, mpr			; Load SPL with high byte of RAMEND
		ldi mpr, high(RAMEND)
		out SPH, mpr			; Load SPL with low byte of RAMEND

		; Initialize Port B for output
		ldi mpr, $FF	   ; Set all Port B bits to output
		out DDRB, mpr
		ldi mpr, $00	   ; Initialize Port B to move forward
		out PORTB, mpr

		; Initialize Port D for input
		ldi mpr, $00       ; Set all Port D bits to input
		out DDRD, mpr
		ldi mpr, $FF       ; Enable internal pull-up resistors
		out PORTD, mpr

		; Clear Counters --> set both of them to zero
		clr countR
		clr countL

		; Initialize LCD
		rcall LCDInit			; LCDDriver function that configures display pins & internal registers
		rcall LCDBacklightOn	; LCDDriver function that turns display backlight ON & make characters visible
		rcall UpdateLCD			; Initial 0 display

		; External Interrupt configuration
		; Falling edge for INT0, INT1, INT3 --> EICRA should be 0b10100010
		ldi mpr, (1<<ISC01)|(0<<ISC00)|(1<<ISC11)|(0<<ISC10)|(1<<ISC31)|(0<<ISC30)
		sts EICRA, mpr

		; Enable interrupts INT0, INT1, and INT3
		ldi mpr, (1<<INT0)|(1<<INT1)|(1<<INT3)
		out EIMSK, mpr

		; Clear any pending flags
		ldi mpr, (1<<INTF1)|(1<<INTF0)
		out EIFR, mpr

		sei ; Turn on Global Interrupts

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		; infinite loop of the bump bot moving forward
		ldi mpr, MovFwd
		out PORTB, mpr
		rjmp MAIN				; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: HIT_RIGHT
; Desc: This function programs the behavior of the bump bot
;		when its right whisker is hit. Steps are as follows:
;		Move backwards for one second, turn left (enable right
;		signal) for a second, increment the right counter, and
;		update the LCD display (MAIN makes the bot move forward).
;-----------------------------------------------------------
HIT_RIGHT:
    push mpr				; Save mpr register
	push waitcnt			; Save wait register
    in mpr, SREG			; Save program state
    push mpr

    ; Move Backwards for one second
    ldi mpr, MovBck			; Load Move Backward Command
    out PORTB, mpr			; Send command to port
    ldi waitcnt, WTime		; Wait for 1 second
    rcall Wait				; Call wait function

    ; Turn left for one second
    ldi mpr, TurnL			; Load Turn Left Command
    out PORTB, mpr			; Send command to port
    ldi waitcnt, WTime		; Wait for one second
    rcall Wait				; Call wait function

    ; Update the LCD Right Hit counter
    inc countR				; Increment the right whisker hit counter
    rcall UpdateLCD			; Update the LCD Driver

	; Clear pending whisker interrupts so they don't queue
    ldi mpr, (1<<INTF1)|(1<<INTF0)
    out EIFR, mpr

    pop mpr					; Restore program state
    out SREG, mpr
	pop waitcnt				; Restore wait register
    pop mpr					; Restore mpr
    ret						; Return from subroutine

;-----------------------------------------------------------
; Func: HIT_LEFT
; Desc: This function programs the behavior of the bump bot
;		when its left whisker is hit. Steps are as follows:
;		Move backwards, wait one second, turn right(enable left
;		signal) for a second, increment the left counter, and
;		update the LCD display (MAIN makes the bot move forward).
;-----------------------------------------------------------
HIT_LEFT:
    push mpr				; Save mpr register
	push waitcnt			; Save wait register
    in mpr, SREG			; Save program state
    push mpr

    ; Move Backwards for one second
    ldi mpr, MovBck			; Load Move Backward Command
    out PORTB, mpr			; Send command to port
    ldi waitcnt, WTime		; Wait for 1 second
    rcall Wait				; Call wait function

    ; Turn right for one second
    ldi mpr, TurnR			; Load Turn Right Command
    out PORTB, mpr			; Send command to port
    ldi waitcnt, WTime		; Wait for one second
    rcall Wait				; Call wait function

	; Update the LCD Left Hit counter
    inc countL				; Increment the right whisker hit counter
    rcall UpdateLCD			; Update the LCD Driver

	; Clear pending whisker interrupts so they don't queue
    ldi mpr, (1<<INTF1)|(1<<INTF0)
    out EIFR, mpr

    pop mpr					; Restore program state
    out SREG, mpr
	pop waitcnt				; Restore wait register
    pop mpr					; Restore mpr
    ret						; Return from subroutine

;-----------------------------------------------------------
; Func: UpdateLCD
; Desc: This function updates the LCD by copying the L1_TEXT
;		and L2_TEXT variables into the LCD and updating using
;		the updated countR and countL in each.
;-----------------------------------------------------------
UpdateLCD:
		; Save current variable contexts
		push mpr
		push ZH
		push ZL
		push XH
		push XL

		; Clear the SRAM buffers with texts to avoid "junk"
		; Setup to copy Line 1 Text
		ldi ZL, low(L1_Text << 1)	; Load low byte of L1_TEXT into ZL
		ldi ZH, high(L1_Text << 1)	; Load high byte of L1_TEXT into ZH
		ldi XL, low(LCD_L1)			; Load low byte of LCD's Line 1 into XL
		ldi XH, high(LCD_L1)		; Load high byte of LCD's Line 1 into XH
		ldi mpr, 16					; Set a counter for 16 (representing the 16 bits of each line in the LCD)
COPY_L1:
		; load L1_TEXT from program memory into X register (currently pointing at LCD line 1)
		lpm r0, Z+
		st X+, r0
		dec mpr
		brne COPY_L1 ; continue the loop until mpr counter reaches 0

		; Overlay Right Count
		ldi XL, low(LCD_L1 + 12)	; Set new starting oosition right after "Right Hits: "
		ldi XH, high(LCD_L1)
		mov mpr, countR				; Store countR into mpr
		rcall Bin2ASCII				; Convert countR into ASCII to properly store in the LCD

		; Setup to copy Line 2 Text
		ldi ZL, low(L2_Text << 1)	; Load low byte of L2_TEXT into ZL
		ldi ZH, high(L2_Text << 1)	; Load high byte of L2_TEXT into ZH
		ldi XL, low(LCD_L2)			; Load low byte of LCD's Line 2 into XL
		ldi XH, high(LCD_L2)		; Load high byte of LCD's Line 2 into XH
		ldi mpr, 16					; Set a counter for 16 (representing the 16 bits of each line in the LCD)
COPY_L2:
		; load L2_TEXT from program memory into X register (currently pointing at LCD line 2)
		lpm r0, Z+
		st X+, r0
		dec mpr
		brne COPY_L2 ; continue the loop until mpr counter reaches 0

		; Overlay Left Count
		ldi XL, low(LCD_L2 + 12)	; Set new starting oosition right after "Left Hits: "
		ldi XH, high(LCD_L2)
		mov mpr, countL				; Store countL into mpr
		rcall Bin2ASCII				; Convert countR into ASCII to properly store in the LCD

		rcall LCDWrite			; Update display

		; Pop previous variable contexts
		pop XL
		pop XH
		pop ZL
		pop ZH
		pop mpr
		ret

;-----------------------------------------------------------
; Func: ClearCounters
; Desc: This function clears the counters for the number of
;		times that the right & left whiskers were hit, and
;		calls the UdateLCD function to update the display.
;-----------------------------------------------------------
CLEAR_COUNTERS:
    clr countR
    clr countL
    rcall UpdateLCD
    ret

; Wait funtion (+ loop, OLoop, and ILoop functions) all copied from previous labs
Wait:
    push	waitcnt
    push	ilcnt
    push	olcnt
Loop:	ldi		olcnt, 224
OLoop:	ldi		ilcnt, 237
ILoop:	dec		ilcnt
    brne	ILoop
    dec		olcnt
    brne	OLoop
    dec		waitcnt
    brne	Loop
    pop		olcnt
    pop		ilcnt
    pop		waitcnt
    ret

;***********************************************************
;*	Stored Program Data
;***********************************************************

L1_Text:
	.DB "Right Hits:     " 

L2_Text:
	.DB "Left Hits:      "

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
.include "LCDDriver.asm"		; Include LCDDriver file
;
