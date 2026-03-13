
;***********************************************************
;*
;*	This is the TRANSMIT skeleton file for Lab 7 of ECE 375
;*
;*  	Rock Paper Scissors
;* 	Requirement:
;* 	1. USART1 communication
;* 	2. Timer/counter1 Normal mode to create a 1.5-sec delay
;***********************************************************
;*
;*	 Authors: Noam Yaffe, Scott Bremmer
;*	   Date: 3/12/2026
;*
;***********************************************************

.include "m32U4def.inc"         ; Include definition file

;***********************************************************
;*  Internal Register Definitions and Constants
;***********************************************************
.def    mpr = r16               ; Multi-Purpose Register

; Use this signal code between two boards for their game ready
.equ    SendReady = 0b11111111

;***********************************************************
;*  Start of Code Segment
;***********************************************************
.cseg                           ; Beginning of code segment

;***********************************************************
;*  Interrupt Vectors
;***********************************************************
.org    $0000                   ; Beginning of IVs
	    rjmp    INIT            	; Reset interrupt


.org    $0056                   ; End of Interrupt Vectors

;***********************************************************
;*  Program Initialization
;***********************************************************
INIT:
	;Stack Pointer (VERY IMPORTANT!!!!)
	ldi     mpr, low(RAMEND)
    out     SPL, mpr
    ldi     mpr, high(RAMEND)
    out     SPH, mpr

	; Configuring PORT B LEDs as countdown indicators (PB7-PB4)
    ldi     mpr, 0b11110000     ; Set PB7:4 as outputs
    out     DDRB, mpr
    ldi     mpr, 0b00000000     ; Initialize LEDs to off
    out     PORTB, mpr

	; Configuring Port D (Buttons and USART)
    ldi     mpr, 0b00001000     ; Set PD3 (TXD1) as output, PD7, PD4, and PD2 (RXD1) as inputs
    out     DDRD, mpr
    ldi     mpr, 0b10010100     ; Enable pull-up resistors on inputs (PD7, PD4, PD2)
    out     PORTD, mpr

	;USART1 Setup
	
	; Set baud rate at 2400bps
	;UBRRn = (8,000,000 / (16 * 2400)) - 1 = 207 (decimal) = 0x00CF (hex)
	ldi     mpr, high(207)
    sts     UBRR1H, mpr
    ldi     mpr, low(207)
    sts     UBRR1L, mpr

	;Enable receiver and transmitter
	ldi     mpr, (1<<RXEN1)|(1<<TXEN1)
    sts     UCSR1B, mpr

	;Set frame format: 8 data bits, 2 stop bits
	; USBS1 = 1 (2 stop bits), UCSZ11 = 1 & UCSZ10 = 1 (8 data bits)
    ldi     mpr, (1<<USBS1)|(1<<UCSZ11)|(1<<UCSZ10)
    sts     UCSR1C, mpr

	;TIMER/COUNTER1
	; Set Normal mode (TCCR1A and TCCR1B = 0)
    ldi     mpr, 0x00
    sts     TCCR1A, mpr
    sts     TCCR1B, mpr

	;Other
	rcall LCDInit				; LCDDriver function that configures display pins & internal registers
	rcall LCDBacklightOn		; LCDDriver function that turns display backlight ON

;***********************************************************
;*  Main Program
;***********************************************************
MAIN:
	rcall LCD_WELCOME		; display welcome message on the LCD screen
	clr r18					; r18 will track the user's gesture (0=Rock, 1=Paper, 2=Scissor)
	clr r23					; r20 will be our PD4 button state flag (prevents multi-triggers)

WAIT_FOR_START:
	; Poll PD7 (the Start button)
    in      mpr, PIND
    andi    mpr, (1<<PD7)
    brne    WAIT_FOR_START	; If PD7 is not pressed, keep waiting

	; If PD7 is pressed, show ready message and send the provided SendReady signal to the other board
	rcall   LCD_READY
	ldi     r17, SendReady
    rcall   USART_TRANSMIT

	; Wait to receive the "Ready" signal from the opponent
	rcall   USART_RECEIVE

	; Verify that the opponent sent the SendReady signal
    cpi     r17, SendReady
	brne	WAIT_FOR_START	; start over if not

	; At this point in the program, both players should be ready. Display "Game start" and set initial gesture
	rcall   LCD_GAME_START
    rcall   LCD_UPDATE_GESTURE ; Displays "Rock" on Line 2 initially

	; Turn on all 4 LEDs (PB7-PB4) without overwriting PB3-PB0
	in      mpr, PORTB
    ori     mpr, 0b11110000
    out     PORTB, mpr

	; Execute four 1.5-second delays (6 seconds total)
    ldi     r24, 4          ; Countdown loop counter

COUNTDOWN_LOOP:
    rcall   DELAY_TIMER     ; Call 1.5 sec delay (which also polls PD4 internally)
    
    ; Turn off LEDs one by one every 1.5-seconds
    cpi     r24, 4			; Compare loop counter to 4
    brne    CHK_LED3		; If not 4, branch to check for 3
    cbi     PORTB, 7		; Clear bit 7 of Port B (LED 4 --> OFF)
    rjmp    LED_DONE		; Jump to the end LED countdown check
CHK_LED3:
    cpi     r24, 3			; Compare loop counter to 3
    brne    CHK_LED2		; If not 3, branch to check for 2
    cbi     PORTB, 6		; Clear bit 6 of Port B (LED 3 --> OFF)
    rjmp    LED_DONE		; Jump to the end LED countdown check
CHK_LED2:
    cpi     r24, 2			; Compare loop counter to 2
    brne    CHK_LED1		; If not 3, branch to check for 1
    cbi     PORTB, 5		; Clear bit 5 of Port B (LED 2 --> OFF)
    rjmp    LED_DONE		; Jump to the end LED countdown check
CHK_LED1:
    cbi     PORTB, 4		; If we get here, we just need to clear bit 4 of Port B (LED 1 --> OFF)
LED_DONE:
    dec     r24				; Decrement r24
    brne    COUNTDOWN_LOOP

	; 6-second timer is over. Transmit final choice and receive opponent's choice
    mov     r17, r18        ; Move our gesture choice (0, 1, or 2) to transmit register
    rcall   USART_TRANSMIT
    rcall   USART_RECEIVE   ; Received opponent's choice into r17
    mov     r19, r17        ; Store opponent's choice in r19

	; Display Opponent choice on Line 1 and User choice on Line 2
    rcall   LCD_SHOW_CHOICES

	; Wait 6 seconds while the choices are displayed (LEDs count down)
	rcall   DELAY_6_SEC

	; Compare Gestures & Determine Winner
    cp      r18, r19
    breq    GAME_DRAW       ; If choices are equal, it's a draw
    
	; Checking what the user picked, and displaying the proper message based on the opponent's pick in relation
    cpi     r18, 0          ; If user picked Rock, must check if opponent picked pPper
    breq    USER_ROCK
    cpi     r18, 1          ; If user picked Paper, must check if opponent picked Rock
    breq    USER_PAPER

USER_SCISSOR:               ; User = 2
    cpi     r19, 1          ; Opponent = 1 (Paper)
    breq    USER_WINS
    rjmp    USER_LOSES

USER_ROCK:                  ; User = 0
    cpi     r19, 2          
    breq    USER_WINS		; Opponent = 2 (Scissors) --> User wins!
    rjmp    USER_LOSES		; Opponent = 1 (Paper) --> User loses

USER_PAPER:                 ; User = 1
    cpi     r19, 0
    breq    USER_WINS		; Opponent = 0 (Rock) --> User wins!
    rjmp    USER_LOSES		; Opponent = 2 (Scissors) --> User loses

; Display "Draw" message on LCD, and end the game
GAME_DRAW:
    rcall   LCD_DRAW
    rjmp    END_ROUND

; Display "Draw" message on LCD, and end the game
USER_WINS:
    rcall   LCD_WIN
    rjmp    END_ROUND

; Display "Draw" message on LCD, and end the game
USER_LOSES:
    rcall   LCD_LOSE

; Game has ended --> start 6-second countdown to display results, and start the game over
END_ROUND:
    rcall   DELAY_6_SEC
    rjmp    MAIN			; Return to prompt

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: USART_TRANSMIT
; Desc: Waits for transmit buffer to be empty before loading
;		r17 into UDR1. PREREQ: Data to send should be loaded
;		into r17 by the time this function is called.
;-----------------------------------------------------------
USART_TRANSMIT:
    lds     mpr, UCSR1A
    sbrs    mpr, UDRE1          ; Wait for empty transmit buffer
    rjmp    USART_TRANSMIT
    sts     UDR1, r17           ; Put data (r17) into buffer to send
    ret

;-----------------------------------------------------------
; Func: USART_RECEIVE
; Desc: returns the received data through UDR1, placed back
;		into r17.
;-----------------------------------------------------------
USART_RECEIVE:
    lds     mpr, UCSR1A
    sbrs    mpr, RXC1           ; Wait for data to be received
    rjmp    USART_RECEIVE
    lds     r17, UDR1           ; Get and return received data from buffer
    ret

;-----------------------------------------------------------
; Func: DELAY_TIMER & WAIT_DELAY
; Desc: The two functions below work together to create a 6
;		second countdown timer (four 1.5 increments) using
;		the provided Timer/Counter control registers. At an
;		8MHz clock using a prescaler of 1024, the timer ticks
;		7812.5 times per second. For 1.5 seconds, we would need
;		7812.5 * 1.5 = 11719 ticks. In Normal mode, the timer
;		counts up to 65535 (0xFFFF). We therefore need to preload
;		the timer with 65536 - 11719 = 53817 (0xD239 in hex)
;		whenever these delay functions are called.
;-----------------------------------------------------------
DELAY_TIMER:
    ; Preload timer with 53817 (0xD239)
    ldi     mpr, 0xD2
    sts     TCNT1H, mpr
    ldi     mpr, 0x39
    sts     TCNT1L, mpr

    ; Start Timer 1 with Prescaler 1024 (CS12=1, CS10=1)
    ldi     mpr, (1<<CS12)|(1<<CS10)
    sts     TCCR1B, mpr
WAIT_DELAY:
    ; Check if PD4 is pressed (Logic Low)
    sbic    PIND, 4
    rjmp    BUTTON_NOT_PRESSED
    
    ; Button is pressed. Check if it's a new press using flag (r23)
    tst     r23
    brne    CHECK_TIMER     ; If flag != 0, we already registered this press
    
    ; Register new press
    ldi     r23, 1          ; Set the "pressed" flag
    inc     r18             ; Increment gesture choice
    cpi     r18, 3          ; If gesture reaches 3, wrap around to 0
    brne    UPDATE_LCD
    clr     r18             ; Wrap back to Rock (0)

UPDATE_LCD:
    rcall   LCD_UPDATE_GESTURE
    rjmp    CHECK_TIMER

BUTTON_NOT_PRESSED:
    clr     r23             ; Clear the flag when button is released

CHECK_TIMER:
    ; Check overflow flag TOV1 in TIFR1
    in      mpr, TIFR1
    sbrs    mpr, TOV1
    rjmp    WAIT_DELAY

    ; Clear TOV1 flag by writing 1 to it
    ldi     mpr, (1<<TOV1)
    out     TIFR1, mpr

    ; Stop timer
    ldi     mpr, 0x00
    sts     TCCR1B, mpr
    ret

;-----------------------------------------------------------
; Func: WAIT_1_5_SEC, WAIT_1_5_SEC_LOOP
; Desc: Helper functions that create a 1.5 second timer, used
;		in the DELAY_6_SEC series of functions below.
;-----------------------------------------------------------
WAIT_1_5_SEC:
    ; Preload timer with 53817 (0xD239)
    ldi     mpr, 0xD2
    sts     TCNT1H, mpr
    ldi     mpr, 0x39
    sts     TCNT1L, mpr

    ; Start Timer 1 with Prescaler 1024 (CS12=1, CS10=1)
    ldi     mpr, (1<<CS12)|(1<<CS10)
    sts     TCCR1B, mpr

WAIT_1_5_SEC_LOOP:
    ; Check overflow flag TOV1 in TIFR1
    in      mpr, TIFR1
    sbrs    mpr, TOV1
    rjmp    WAIT_1_5_SEC_LOOP

    ; Clear TOV1 flag by writing 1 to it
    ldi     mpr, (1<<TOV1)
    out     TIFR1, mpr

    ; Stop timer
    ldi     mpr, 0x00
    sts     TCCR1B, mpr
    ret

;-----------------------------------------------------------
; Func: DELAY_6_SEC, LOOP, and D6_CHK_LED*
; Desc: This is a series of functions that is very similar
;		to the countdown functionality in the MAIN program
;		flow, except it doesn't poll user input whatsoever.
;-----------------------------------------------------------
DELAY_6_SEC:
    ; TURN ON ALL 4 LEDs (PB7-PB4)
    ; (If this is missing on Board B, the LEDs will stay off!)
    in      mpr, PORTB
    ori     mpr, 0b11110000
    out     PORTB, mpr

    ; COUNTDOWN LOOP
    ldi     r24, 4          ; Use r24 for loop counter
DELAY_6_SEC_LOOP:
    rcall   WAIT_1_5_SEC    ; Use the clean delay (no polling)
    
    ; 3. TURN OFF LEDs ONE BY ONE
    cpi     r24, 4
    brne    D6_CHK_LED3
    cbi     PORTB, 7
    rjmp    D6_LED_DONE
D6_CHK_LED3:
    cpi     r24, 3
    brne    D6_CHK_LED2
    cbi     PORTB, 6
    rjmp    D6_LED_DONE
D6_CHK_LED2:
    cpi     r24, 2
    brne    D6_CHK_LED1
    cbi     PORTB, 5
    rjmp    D6_LED_DONE
D6_CHK_LED1:
    cbi     PORTB, 4
D6_LED_DONE:
    dec     r24             ; Decrement r24
    brne    DELAY_6_SEC_LOOP
    ret

;-----------------------------------------------------------
; Func: LCD_[ALL]
; Desc: The LCD_* functions below all have serve a single
;		purpose, which is to display their corresponding
;		message on the LCD screen. For example, LCD_WELCOME
;		displays the text in Welcome_MsgL1 & L2, LCD_READY
;		displays the text in Ready_MsgL1 & L2, etc.
;-----------------------------------------------------------
LCD_WELCOME:
    ; Set Z pointer to the first line of the welcome message
    ldi     ZL, low(Welcome_MsgL1 << 1)
    ldi     ZH, high(Welcome_MsgL1 << 1)
    rcall   WRITE_LINE_1				; write to LCD line 1

    ; Set Z pointer to the second line of the welcome message
    ldi     ZL, low(Welcome_MsgL2 << 1)
    ldi     ZH, high(Welcome_MsgL2 << 1)
    rcall   WRITE_LINE_2				; write to LCD line 2
    
    ret

LCD_READY:
    ; Set Z pointer to the first line of the ready message
    ldi     ZL, low(Ready_MsgL1 << 1)
    ldi     ZH, high(Ready_MsgL1 << 1)
    rcall   WRITE_LINE_1				; write to LCD line 1

    ; Set Z pointer to the second line of the ready message
    ldi     ZL, low(Ready_MsgL2 << 1)
    ldi     ZH, high(Ready_MsgL2 << 1)
    rcall   WRITE_LINE_2				; write to LCD line 2
    
    ret

LCD_GAME_START:
	; Set Z pointer to the game start message variable
    ldi ZL, low(GameStart_Msg << 1)
    ldi ZH, high(GameStart_Msg << 1)
    rjmp WRITE_LINE_1			; write to LCD line 1

LCD_WIN:
	; Set Z pointer to the "You Win!" message variable
    ldi ZL, low(Win_Msg << 1)
    ldi ZH, high(Win_Msg << 1)
    rjmp WRITE_LINE_1			; write to LCD line 1

LCD_LOSE:
	; Set Z pointer to the "You lose" message variable
    ldi ZL, low(Lose_Msg << 1)
    ldi ZH, high(Lose_Msg << 1)
    rjmp WRITE_LINE_1			; write to LCD line 1

LCD_DRAW:
	; Set Z pointer to the "Draw" message variable
    ldi ZL, low(Draw_Msg << 1)
    ldi ZH, high(Draw_Msg << 1)
    rjmp WRITE_LINE_1			; write to LCD line 1

;-----------------------------------------------------------
; Func: LCD_UPDATE_GESTURE, DISP_ROCK2, DISP_PAPER2
; Desc: This series of functions checks the user's inputted
;		move (rock, paper or scissors) from the value of r18
;		(0, 1, or 2, respectively). Based on the current value
;		of r18, these functions write the user's move to the
;		second line of the LCD screen.
;-----------------------------------------------------------
LCD_UPDATE_GESTURE:
    cpi r18, 0						; if r18 = 0, the user's choice is rock
    breq DISP_ROCK2					; display "Rock" on the LCD line 2
    cpi r18, 1						; if r18 = 1, the user's choice is paper
    breq DISP_PAPER2				; display "Paper" on the LCD line 2
	; Else, r18 = 2, meaning the user's choice is scissors
    ldi ZL, low(Scissor_Msg << 1)	; set Z pointer to "Scissors" message variable
    ldi ZH, high(Scissor_Msg << 1)
    rjmp WRITE_LINE_2				; write to LCD line 2 (user's line)
DISP_ROCK2:
	; Set Z pointer to "Rock" message variable
    ldi ZL, low(Rock_Msg << 1)
    ldi ZH, high(Rock_Msg << 1)
    rjmp WRITE_LINE_2				; write to LCD line 2 (user's line)
DISP_PAPER2:
	; Set Z pointer to "Paper" message variable
    ldi ZL, low(Paper_Msg << 1)
    ldi ZH, high(Paper_Msg << 1)
    rjmp WRITE_LINE_2				; write to LCD line 2 (user's line)

;-----------------------------------------------------------
; Func: LCD_SHOW_CHOICES, OP_ROCK, OP_PAPER
; Desc: This series of functions essentially carry the exact
;		same logic as the LCD_UPDATE_GESTURE series above.
;		Whatever move the opponent picked (which is pulled
;		from r19) is displayed on LCD line 1, and this series
;		then calls LCD_UPDATE_GESTURE to display the user's
;		move on LCD line 2.
;-----------------------------------------------------------
LCD_SHOW_CHOICES:
    ; Handle Opponent (r19) to Line 1
    cpi r19, 0						; if r19 = 0, the opponent's choice is rock
    breq OP_ROCK					; display "Rock" on the LCD line 1
    cpi r19, 1						; if r19 = 1, the opponent's choice is paper
    breq OP_PAPER					; display "Paper" on the LCD line 2
    ; Else, r19 = 2, meaning the opponent's choice is scissors
	ldi ZL, low(Scissor_Msg << 1)	; set Z pointer to "Scissors" message variable
    ldi ZH, high(Scissor_Msg << 1)
    rjmp WRITE_OP_L1
OP_ROCK:
	; Set Z pointer to "Rock" message variable
    ldi ZL, low(Rock_Msg << 1)
    ldi ZH, high(Rock_Msg << 1)
    rjmp WRITE_OP_L1
OP_PAPER:
	; Set Z pointer to "Paper" message variable
    ldi ZL, low(Paper_Msg << 1)
    ldi ZH, high(Paper_Msg << 1)

WRITE_OP_L1:
    rcall WRITE_LINE_1				; write to LCD line 2 (opponent's line)
    rjmp LCD_UPDATE_GESTURE			; Let the previous function handle writing User to Line 2

;-----------------------------------------------------------
; Func: WRITE_LINE_1, WRITE_LINE_2, DO_COPY, and COPY_LOOP
; Desc: These functions contains the LCD writing logic, where
;		the Z pointer (which contains the message to be
;		displayed) is stored into the X pointer (which points
;		to 0x0100 [start of LCD line 1] for WRITE_LINE_1and
;		0x0110 [start of LCD line 2] for WRITE_LINE_2. Logic
;		taken from previous labs.
;-----------------------------------------------------------
WRITE_LINE_1:
    push mpr
    ldi XL, 0x00
    ldi XH, 0x01
    rjmp DO_COPY
WRITE_LINE_2:
    push mpr
    ldi XL, 0x10
    ldi XH, 0x01
DO_COPY:
    ldi mpr, 16
COPY_LOOP:
    lpm r0, Z+
    st X+, r0
    dec mpr
    brne COPY_LOOP
    rcall LCDWrite
    pop mpr
    ret

;***********************************************************
;*	Stored Program Data
;***********************************************************
Welcome_MsgL1:
    .DB		"Welcome!        "
Welcome_MsgL2:
    .DB		"Please press PD7"
Ready_MsgL1:
    .DB		"READY. Waiting  "
Ready_MsgL2:
    .DB		"for the opponent"
GameStart_Msg:
	.DB "GAME START      "
Rock_Msg:
	.DB "ROCK            "
Paper_Msg:
	.DB "PAPER           "
Scissor_Msg:
	.DB "SCISSORS        "
Win_Msg:
	.DB "You won!        "
Lose_Msg:
	.DB "You lost        "
Draw_Msg:
	.DB "Draw            "
;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver

