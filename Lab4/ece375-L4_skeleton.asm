;***********************************************************
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;*	 Author: Scott Bremmer
;*	   Date: Feb 13, 2026
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

.def	oloop = r17				; Outer Loop Counter
.def	iloop = r18				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0056					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:
		; Initialize Stack Pointer
		ldi		mpr, low(RAMEND)
		out		SPL, mpr
		ldi		mpr, high(RAMEND)
		out		SPH, mpr

		clr		zero			; Set the zero register to zero, maintain
										; these semantics, meaning, don't
										; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program

		; Call function to load ADD16 operands
		; Load OperandA from program memory into ADD16_OP1 in data memory
		ldi		ZL, low(OperandA<<1)	; Z points to program memory source
		ldi		ZH, high(OperandA<<1)
		ldi		YL, low(ADD16_OP1)		; Y points to data memory destination
		ldi		YH, high(ADD16_OP1)
		lpm		mpr, Z+					; Load low byte of OperandA
		st		Y+, mpr					; Store to ADD16_OP1 low byte
		lpm		mpr, Z					; Load high byte of OperandA
		st		Y, mpr					; Store to ADD16_OP1 high byte

		; Load OperandB from program memory into ADD16_OP2 in data memory
		ldi		ZL, low(OperandB<<1)
		ldi		ZH, high(OperandB<<1)
		ldi		YL, low(ADD16_OP2)
		ldi		YH, high(ADD16_OP2)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		nop ; Check load ADD16 operands (Set Break point here #1)

		; Call ADD16 function to display its results (calculate FCBA + FFFF)
		rcall	ADD16

		nop ; Check ADD16 result (Set Break point here #2)


		; Call function to load SUB16 operands
		; Load OperandC from program memory into SUB16_OP1 in data memory
		ldi		ZL, low(OperandC<<1)
		ldi		ZH, high(OperandC<<1)
		ldi		YL, low(SUB16_OP1)
		ldi		YH, high(SUB16_OP1)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		; Load OperandD from program memory into SUB16_OP2 in data memory
		ldi		ZL, low(OperandD<<1)
		ldi		ZH, high(OperandD<<1)
		ldi		YL, low(SUB16_OP2)
		ldi		YH, high(SUB16_OP2)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		nop ; Check load SUB16 operands (Set Break point here #3)

		; Call SUB16 function to display its results (calculate FCB9 - E420)
		rcall	SUB16

		nop ; Check SUB16 result (Set Break point here #4)


		; Call function to load MUL24 operands
		; Load OperandE (3 bytes: E1 low word, E2 high byte) into MUL24_OP1
		ldi		ZL, low(OperandE1<<1)
		ldi		ZH, high(OperandE1<<1)
		ldi		YL, low(MUL24_OP1)
		ldi		YH, high(MUL24_OP1)
		lpm		mpr, Z+					; byte 0 of operand E
		st		Y+, mpr
		lpm		mpr, Z+					; byte 1 of operand E
		st		Y+, mpr
		lpm		mpr, Z					; byte 2 of operand E (low byte of OperandE2)
		st		Y, mpr

		; Load OperandF (3 bytes: F1 low word, F2 high byte) into MUL24_OP2
		ldi		ZL, low(OperandF1<<1)
		ldi		ZH, high(OperandF1<<1)
		ldi		YL, low(MUL24_OP2)
		ldi		YH, high(MUL24_OP2)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		nop ; Check load MUL24 operands (Set Break point here #5)

		; Call MUL24 function to display its results (calculate FFFFFF * FFFFFF)
		rcall	MUL24

		nop ; Check MUL24 result (Set Break point here #6)

		; Setup the COMPOUND function direct test
		; Load OperandG into SUB16_OP1
		ldi		ZL, low(OperandG<<1)
		ldi		ZH, high(OperandG<<1)
		ldi		YL, low(SUB16_OP1)
		ldi		YH, high(SUB16_OP1)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		; Load OperandH into SUB16_OP2
		ldi		ZL, low(OperandH<<1)
		ldi		ZH, high(OperandH<<1)
		ldi		YL, low(SUB16_OP2)
		ldi		YH, high(SUB16_OP2)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		; Load OperandI into ADD16_OP2 (will be used after SUB16 result is moved)
		ldi		ZL, low(OperandI<<1)
		ldi		ZH, high(OperandI<<1)
		ldi		YL, low(ADD16_OP2)
		ldi		YH, high(ADD16_OP2)
		lpm		mpr, Z+
		st		Y+, mpr
		lpm		mpr, Z
		st		Y, mpr

		nop ; Check load COMPOUND operands (Set Break point here #7)

		; Call the COMPOUND function
		rcall	COMPOUND

		nop ; Check COMPOUND result (Set Break point here #8)

DONE:	rjmp	DONE			; Create an infinite while loop to signify the
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;       where the high byte of the result contains the carry
;       out bit.
;-----------------------------------------------------------
ADD16:
		; Load beginning address of first operand into X
		ldi		XL, low(ADD16_OP1)	; Load low byte of address
		ldi		XH, high(ADD16_OP1)	; Load high byte of address

		; Load beginning address of second operand into Y
		ldi		YL, low(ADD16_OP2)
		ldi		YH, high(ADD16_OP2)

		; Load beginning address of result into Z
		ldi		ZL, low(ADD16_Result)
		ldi		ZH, high(ADD16_Result)

		; Execute the function
		; Add low bytes
		ld		A, X+			; Load low byte of operand 1
		ld		B, Y+			; Load low byte of operand 2
		add		A, B			; Add low bytes
		st		Z+, A			; Store low byte of result

		; Add high bytes with carry
		ld		A, X			; Load high byte of operand 1
		ld		B, Y			; Load high byte of operand 2
		adc		A, B			; Add high bytes with carry
		st		Z+, A			; Store high byte of result

		; Store carry out as third byte
		clr		A
		adc		A, zero			; A = 0 + carry
		st		Z, A			; Store carry byte

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;       result. Always subtracts from the bigger values.
;-----------------------------------------------------------
SUB16:
		; Load beginning address of first operand into X
		ldi		XL, low(SUB16_OP1)
		ldi		XH, high(SUB16_OP1)

		; Load beginning address of second operand into Y
		ldi		YL, low(SUB16_OP2)
		ldi		YH, high(SUB16_OP2)

		; Load beginning address of result into Z
		ldi		ZL, low(SUB16_Result)
		ldi		ZH, high(SUB16_Result)

		; Subtract low bytes
		ld		A, X+
		ld		B, Y+
		sub		A, B
		st		Z+, A

		; Subtract high bytes with borrow
		ld		A, X
		ld		B, Y
		sbc		A, B
		st		Z, A

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit
;       result.
;-----------------------------------------------------------
MUL24:
;* - Simply adopting MUL16 ideas to MUL24 will not give you steady results. You should come up with different ideas.
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Clear the result area (6 bytes for 48-bit result)
		ldi		ZL, low(MUL24_Result)
		ldi		ZH, high(MUL24_Result)
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z+, zero
		st		Z, zero

		; Outer loop: iterate over each byte of OP2
		ldi		YL, low(MUL24_OP2)
		ldi		YH, high(MUL24_OP2)

		ldi		ZL, low(MUL24_Result)
		ldi		ZH, high(MUL24_Result)

		ldi		oloop, 3
MUL24_OLOOP:
		; Save Z start position for this outer iteration
		push	ZL
		push	ZH

		ldi		XL, low(MUL24_OP1)
		ldi		XH, high(MUL24_OP1)

		ldi		iloop, 3
MUL24_ILOOP:
		ld		A, X+			; Get byte of OP1
		ld		B, Y			; Get byte of OP2
		mul		A, B			; rhi:rlo = A * B

		; Add rlo to result[Z]
		ld		A, Z			; Load current result byte
		add		A, rlo			; Add low product byte
		st		Z+, A			; Store and advance

		; Add rhi + carry to result[Z+1]
		ld		A, Z			; Load next result byte
		adc		A, rhi			; Add high product byte + carry
		st		Z, A			; Store (don't advance yet)

		; Propagate carry through remaining bytes if needed
		brcc	MUL24_NCARRY
		push	ZL
		push	ZH
MUL24_CARRY_LOOP:
		inc		ZL				; Advance Z by 1 (inc doesn't affect C flag)
		brne	MUL24_CARRY_NZOV
		inc		ZH				; Handle ZL overflow
MUL24_CARRY_NZOV:
		ld		A, Z
		inc		A				; A = A + 1
		st		Z, A
		tst		A				; Did it wrap to zero? (i.e. was 0xFF)
		breq	MUL24_CARRY_LOOP	; If zero, carry needs to propagate
		pop		ZH
		pop		ZL
MUL24_NCARRY:
		; Z currently points to result[offset+1]
		; Next inner iteration should start at result[offset+1]
		; Z is already there, so just continue

		dec		iloop
		brne	MUL24_ILOOP

		; Restore Z to saved position, then advance by 1 for next outer byte
		pop		ZH
		pop		ZL
		adiw	ZH:ZL, 1		; Shift result position by 1 for next OP2 byte
		adiw	YH:YL, 1		; Next byte of OP2

		dec		oloop
		brne	MUL24_OLOOP

		pop		iloop			; Restore all registers in reverse order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((G - H) + I)^2
;       by making use of SUB16, ADD16, and MUL24.
;
;       D, E, and F are declared in program memory, and must
;       be moved into data memory for use as input operands.
;
;       All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:
		; Setup SUB16 with operands G and H
		; (Operands G and H were already loaded into SUB16_OP1 and SUB16_OP2 in MAIN)

		; Perform subtraction to calculate G - H
		rcall	SUB16

		; Setup the ADD16 function with SUB16 result and operand I
		; Copy SUB16_Result into ADD16_OP1
		ldi		XL, low(SUB16_Result)
		ldi		XH, high(SUB16_Result)
		ldi		YL, low(ADD16_OP1)
		ldi		YH, high(ADD16_OP1)
		ld		mpr, X+
		st		Y+, mpr
		ld		mpr, X
		st		Y, mpr

		; (OperandI was already loaded into ADD16_OP2 in MAIN)

		; Clear ADD16 result before use
		ldi		ZL, low(ADD16_Result)
		ldi		ZH, high(ADD16_Result)
		st		Z+, zero
		st		Z+, zero
		st		Z, zero

		; Perform addition next to calculate (G - H) + I
		rcall	ADD16

		; Setup the MUL24 function with ADD16 result as both operands
		; Copy ADD16_Result (3 bytes, but we use low 2 bytes as 24-bit with high=0) into MUL24_OP1
		ldi		XL, low(ADD16_Result)
		ldi		XH, high(ADD16_Result)
		ldi		YL, low(MUL24_OP1)
		ldi		YH, high(MUL24_OP1)
		ld		mpr, X+
		st		Y+, mpr
		ld		mpr, X+
		st		Y+, mpr
		ld		mpr, X				; Third byte (carry byte)
		st		Y, mpr

		; Copy the same value into MUL24_OP2
		ldi		XL, low(ADD16_Result)
		ldi		XH, high(ADD16_Result)
		ldi		YL, low(MUL24_OP2)
		ldi		YH, high(MUL24_OP2)
		ld		mpr, X+
		st		Y+, mpr
		ld		mpr, X+
		st		Y+, mpr
		ld		mpr, X
		st		Y, mpr

		; Perform multiplication to calculate ((G - H) + I)^2
		rcall	MUL24

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;       A - Operand A is gathered from address $0101:$0100
;       B - Operand B is gathered from address $0103:$0102
;       Res - Result is stored in address
;             $0107:$0106:$0105:$0104
;       You will need to make sure that Res is cleared before
;       calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop

		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;       beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here

		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;*	Do not  section.
;***********************************************************
; ADD16 operands
OperandA:
	.DW 0xFCBA
OperandB:
	.DW 0xFFFF

; SUB16 operands
OperandC:
	.DW 0XFCB9
OperandD:
	.DW 0XE420

; MUL24 operands
OperandE1:
	.DW	0XFFFF
OperandE2:
	.DW	0X00FF
OperandF1:
	.DW	0XFFFF
OperandF2:
	.DW	0X00FF

; Compoud operands
OperandG:
	.DW	0xFCBA				; test value for operand G
OperandH:
	.DW	0x2022				; test value for operand H
OperandI:
	.DW	0x21BB				; test value for operand I

;***********************************************************
;*	Data Memory Allocation
;***********************************************************
.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; ADD16 data memory
.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result

; SUB16 data memory
.org	$0130
SUB16_OP1:
		.byte 2				; allocate two bytes for first operand of SUB16
SUB16_OP2:
		.byte 2				; allocate two bytes for second operand of SUB16

.org	$0140
SUB16_Result:
		.byte 2				; allocate two bytes for SUB16 result

; MUL24 data memory
.org	$0150
MUL24_OP1:
		.byte 3				; allocate three bytes for first operand of MUL24
MUL24_OP2:
		.byte 3				; allocate three bytes for second operand of MUL24

.org	$0160
MUL24_Result:
		.byte 6				; allocate six bytes for MUL24 result (48-bit)

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program