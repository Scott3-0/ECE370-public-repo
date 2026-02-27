;***********************************************************
;*
;*	This is the skeleton file for Lab 6 of ECE 375
;*
;*	 Author: Scott Bremmer, Noam Yaffe
;*	   Date: Feb 26, 2026
;*
;*	Desc: Assembly program that controls TekBot speed via
;*	  PWM using 16-bit Timer/Counter 1 in 8-bit Fast PWM
;*	  mode. User modifies speed through Port D buttons:
;*	    PD7 - Increase speed by one level
;*	    PD4 - Decrease speed by one level
;*	    PD5 - Set speed to max (full speed)
;*	  Speed levels 0-15 displayed on Port B LEDs (pins 3:0).
;*	  Motor direction (move forward) on Port B upper nibble.
;*
;***********************************************************

.include "m32U4def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	speedLvl = r17			; Current speed level (0-15)

.equ	EngEnR = 5				; right Engine Enable Bit (OC1A on PB5)
.equ	EngEnL = 6				; left Engine Enable Bit (OC1B on PB6)
.equ	EngDirR = 4				; right Engine Direction Bit (PB4)
.equ	EngDirL = 7				; left Engine Direction Bit (PB7)

; Motor forward command: both direction bits set
.equ	MovFwd = (1<<EngDirR)|(1<<EngDirL)	; = 0b10010000 = 0x90

; Speed step: 255 / 15 = 17 per level
.equ	SpeedStep = 17

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; No additional interrupt vectors needed for polling

.org	$0056					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr

		; Configure I/O ports
		; Port B: all output (upper nibble = motor direction,
		;         PB6/PB5 = PWM outputs, lower nibble = LEDs)
		ldi		mpr, $FF
		out		DDRB, mpr

		; Port D: all input with pull-ups for buttons
		ldi		mpr, $00
		out		DDRD, mpr
		ldi		mpr, $FF
		out		PORTD, mpr

		; Configure 16-bit Timer/Counter 1A and 1B
		; TCCR1A: COM1A1:0 = 10 (non-inverting PWM on OC1A/PB5)
		;         COM1B1:0 = 10 (non-inverting PWM on OC1B/PB6)
		;         WGM11:0  = 01 (part of Fast PWM 8-bit mode)
		ldi		mpr, (1<<COM1A1)|(1<<COM1B1)|(1<<WGM10)
		sts		TCCR1A, mpr

		; TCCR1B: WGM13:2 = 01 (completes WGM = 0101 = Fast PWM 8-bit)
		;         CS12:0   = 001 (no prescaling, clk_I/O)
		ldi		mpr, (1<<WGM12)|(1<<CS10)
		sts		TCCR1B, mpr

		; Set initial speed to 0 (stopped)
		ldi		speedLvl, 0

		; Set OCR1A = 0 (right motor duty cycle = 0%)
		ldi		mpr, 0
		sts		OCR1AH, mpr
		sts		OCR1AL, mpr

		; Set OCR1B = 0 (left motor duty cycle = 0%)
		sts		OCR1BH, mpr
		sts		OCR1BL, mpr

		; Set TekBot to Move Forward on Port B upper nibble
		; Lower nibble = 0 (speed level 0, all LEDs off)
		ldi		mpr, MovFwd
		out		PORTB, mpr

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; Poll Port D pushbuttons
		in		mpr, PIND

		; Check PD7 - Speed Up (active low: 0 = pressed)
		sbrs	mpr, 7				; Skip next if PD7 is HIGH (not pressed)
		rjmp	SPEED_UP

		; Check PD4 - Speed Down (active low: 0 = pressed)
		sbrs	mpr, 4				; Skip next if PD4 is HIGH (not pressed)
		rjmp	SPEED_DOWN

		; Check PD5 - Max Speed (active low: 0 = pressed)
		sbrs	mpr, 5				; Skip next if PD5 is HIGH (not pressed)
		rjmp	SPEED_MAX

		rjmp	MAIN				; No button pressed, loop

SPEED_UP:
		; Increase speed by one level (if not already at max)
		cpi		speedLvl, 15		; Already at max speed?
		breq	WAIT_RELEASE		; If yes, don't overflow, just wait
		inc		speedLvl			; Increment speed level
		rcall	UPDATE_SPEED		; Update PWM and LEDs
		rjmp	WAIT_RELEASE

SPEED_DOWN:
		; Decrease speed by one level (if not already at min)
		cpi		speedLvl, 0			; Already stopped?
		breq	WAIT_RELEASE		; If yes, don't underflow, just wait
		dec		speedLvl			; Decrement speed level
		rcall	UPDATE_SPEED		; Update PWM and LEDs
		rjmp	WAIT_RELEASE

SPEED_MAX:
		; Immediately set speed to maximum (level 15)
		ldi		speedLvl, 15		; Set to full speed
		rcall	UPDATE_SPEED		; Update PWM and LEDs
		rjmp	WAIT_RELEASE

WAIT_RELEASE:
		; Wait for all relevant buttons to be released before
		; returning to MAIN. This ensures a single press results
		; in a single speed change (debounce by wait-for-release).
		in		mpr, PIND
		andi	mpr, (1<<PD7)|(1<<PD5)|(1<<PD4)
		cpi		mpr, (1<<PD7)|(1<<PD5)|(1<<PD4)
		brne	WAIT_RELEASE		; Loop until all three buttons read HIGH

		rjmp	MAIN				; Return to main polling loop

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func:	UPDATE_SPEED
; Desc:	Updates the PWM duty cycle for both motors and
;		the speed-level LED display based on the current
;		value of the 'speedLvl' register (0-15).
;		OCR value = speed * 17 (gives 0 at level 0,
;		255 at level 15).
;		Port B lower nibble is set to the speed level.
;-----------------------------------------------------------
UPDATE_SPEED:
		push	r0					; Save MUL result registers
		push	r1
		push	r18

		; Calculate OCR value: speed * 17
		ldi		r18, SpeedStep		; r18 = 17
		mul		speedLvl, r18		; r1:r0 = speed * 17

		; Update OCR1A (right motor enable, OC1A/PB5)
		ldi		mpr, 0
		sts		OCR1AH, mpr			; High byte = 0 (8-bit mode)
		mov		mpr, r0
		sts		OCR1AL, mpr			; Low byte = speed * 17

		; Update OCR1B (left motor enable, OC1B/PB6)
		ldi		mpr, 0
		sts		OCR1BH, mpr			; High byte = 0
		mov		mpr, r0
		sts		OCR1BL, mpr			; Low byte = speed * 17

		; Update Port B: upper nibble = MovFwd, lower nibble = speed level
		; PB5/PB6 are overridden by timer (COM bits), so writing them is harmless
		mov		mpr, speedLvl		; Load speed level into mpr (0-15)
		ori		mpr, MovFwd			; OR with motor forward bits (0x90)
		out		PORTB, mpr			; Output to Port B

		pop		r18
		pop		r1
		pop		r0
		ret							; Return from subroutine

;***********************************************************
;*	Stored Program Data
;***********************************************************
		; No stored program data needed for this lab

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program
