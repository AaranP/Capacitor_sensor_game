
;FEATURES:
;Capacitor Sensor game in which two players listen to a sound produced by a speaker - if the tone is high, the first person to slap
;their sensor wins a point. If the tone is low, the first person to slap their sensor loses a point (stay at 0 if they were already at 0)
;A LED lights up to indicate a successful/registered slap of either player's sensor
;There is a beginning message prompting players to hit the start button/pin to begin the game
;The first person to reach 5 points is the winner - music plays after the winner is announced on the LCD screen


$NOLIST
$MODLP51
$LIST

org 0000H
   ljmp MyProgram

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR
	
org 0x001B
	reti


CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE0  EQU ((2048*2)+100)
TIMER0_RATE1  EQU ((2048*2)-100)
RATE3		  EQU ((2048*2)-200)
RATE4		  EQU ((2048*2)-300)
RATE5		  EQU ((2048*2)-400)
TIMER0_RELOAD0 EQU ((65536-(CLK/TIMER0_RATE0)))
TIMER0_RELOAD1 EQU ((65536-(CLK/TIMER0_RATE1)))
TIMER1_RATE EQU 1000
TIMER1_RELOAD EQU ((65536-(CLK/TIMER1_RATE)))
SOUND1	EQU ((65536-(CLK/(RATE3)))
SOUND2  EQU ((65536-(CLK/(RATE4)))
SOUND3  EQU ((65536-(CLK/(RATE5)))

SOUND_OUT EQU P0.2

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P3.2
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P3.3
LCD_D4 equ P3.4
LCD_D5 equ P3.5
LCD_D6 equ P3.6
LCD_D7 equ P3.7

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$include(math32.inc)
$LIST

DSEG at 0x30
Period_A: ds 2
Period_B: ds 2
p1score: ds 1
p2score: ds 1
Seed: ds 4
Count1ms: ds 2
x: ds 4
y: ds 4
bcd: ds 5

BSEG
player: dbit 1
tone: dbit 1
one_second_flag: dbit 1
mf: dbit 1


CSEG
;                      1234567890123456    <- This helps determine the location of the counter
Initial_Message1:  db 'P1:       ', 0
Initial_Message2:  db 'P2:       ', 0
Player1_Win: db 'Player 1 Wins!', 0
Player2_Win: db 'Player 2 Wins!', 0
PressStart: db 'Press Start!', 0
Rules:		db 'First to 5 Wins!', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
InitTimer0:
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD1)
	mov TL0, #low(TIMER0_RELOAD1)
	; Set autoreload value
	mov RH0, #high(TIMER0_RELOAD1)
	mov RL0, #low(TIMER0_RELOAD1)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    ;setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P1.1 ;
;---------------------------------;
Timer0_ISR:
	cpl SOUND_OUT
	reti

InitTimer1:
    clr a
    mov a, TMOD
    anl a, #00001111B
    orl a, #00010000B
    mov TMOD, a
    mov TH1, #high(TIMER1_RELOAD)
    mov TL1, #low(TIMER1_RELOAD)
    ;Set autoreload value
    mov RH1, #high(TIMER1_RELOAD)
    mov RL1, #low(TIMER1_RELOAD)
    ;Enable the timer and interrupts
    setb ET1 ; Enable Timer 1 interrupts
   ; setb TR0 ; Start Timer 1
    ret

Timer1_ISR:
    clr TF1
    push acc
    push psw

    ;Increment 16-bit one milisecond counter
    inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if a second has passed
	mov a, Count1ms+0
	cjne a, #low(500), Timer1_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(500), Timer1_ISR_done
	clr c
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb one_second_flag ; Let the main program know a second has passed
    	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a

Timer1_ISR_done:
    pop psw
    pop acc
    reti


; When using a 22.1184MHz crystal in fast mode
; one cycle takes 1.0/22.1184MHz = 45.21123 ns
; (tuned manually to get as close to 1s as possible)
Wait1s:
    mov R2, #176
X3: mov R1, #250
X2: mov R0, #166
X1: djnz R0, X1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, X2 ; 22.51519us*250=5.629ms
    djnz R2, X3 ; 5.629ms*176=1.0s (approximately)
    ret

;Initializes timer/counter 2 as a 16-bit timer
InitTimer2:
	mov T2CON, #0b_0000_0000 ; Stop timer/counter.  Set as timer (clock input is pin 22.1184MHz).
	; Set the reload value on overflow to zero (just in case is not zero)
	mov RCAP2H, #0
	mov RCAP2L, #0
    ret

;Converts the hex number in TH2-TL2 to BCD in R2-R1-R0
hex2bcd22:
	clr a
    mov R0, #0  ;Set BCD result to 00000000 
    mov R1, #0
    mov R2, #0
    mov R3, #16 ;Loop counter.

hex2bcd_loop:
    mov a, TL2 ;Shift TH0-TL0 left through carry
    rlc a
    mov TL2, a
    
    mov a, TH2
    rlc a
    mov TH2, a
      
	; Perform bcd + bcd + carry
	; using BCD numbers
	mov a, R0
	addc a, R0
	da a
	mov R0, a
	
	mov a, R1
	addc a, R1
	da a
	mov R1, a
	
	mov a, R2
	addc a, R2
	da a
	mov R2, a
	
	djnz R3, hex2bcd_loop
	ret
	
WaitHalfSec:
    mov R2, #89
K3: mov R1, #250
K2: mov R0, #166
K1: djnz R0, K1 ; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, K2 ; 22.51519us*250=5.629ms
    djnz R2, K3 ; 5.629ms*89=0.5s (approximately)
    ret

; Dumps the 5-digit packed BCD number in R2-R1-R0 into the LCD
DisplayBCD_LCD:
	; 5th digit:
    mov a, R2
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 4th digit:
    mov a, R1
    swap a
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 3rd digit:
    mov a, R1
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 2nd digit:
    mov a, R0
    swap a
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
	; 1st digit:
    mov a, R0
    anl a, #0FH
    orl a, #'0' ; convert to ASCII
	lcall ?WriteData
    
    ret

;---------------------------------;
; Hardware initialization         ;
;---------------------------------;
Initialize_All:
	
	lcall InitTimer0
    lcall InitTimer2 
    lcall LCD_4BIT
    Set_Cursor(1,3)
    Send_Constant_String(#PressStart)
    Set_Cursor(2,1)
    Send_Constant_String(#Rules)
    
    
    setb TR2
    jb P2.3, $
    lcall LCD_4BIT
    mov Seed+0, TH2
    mov Seed+1, #0x01
    mov Seed+2, #0x87
    mov Seed+3, TL2
    clr TR2
	ret

;---------------------------------;
; Main program loop               ;
;---------------------------------;
IncP1:
	clr a
	mov a, p1score
	add a, #0x01
	da a
	mov p1score, a
	Set_Cursor(1,5)
	Display_BCD(p1score)
	ret

IncP2:
	clr a
	mov a, p2score
	add a, #0x01
	da a
	mov p2score, a
	Set_Cursor(2,5)
	Display_BCD(p2score)
	ret
	
DecP1:
	clr a
	mov a, p1score
	jz subp1zero
	subb a, #0x01
	da a
	mov p1score, a
	subp1zero:
	Set_Cursor(1,5)
	Display_BCD(p1score)
	ret

DecP2:
	clr a
	mov a, p2score
	jz subp2zero
	subb a, #0x01
	da a
	mov p2score, a
	subp2zero:
	Set_Cursor(2,5)
	Display_BCD(p2score)
	ret
	
;---------------------------------;
; Main program loop               ;
;---------------------------------; 

Random:
	mov x+0, Seed+0
	mov x+1, Seed+1
	mov x+2, Seed+2
	mov x+3, Seed+3
	Load_y(214013)
	lcall mul32
	Load_y(2531011)
	lcall add32
	mov Seed+0, x+0
	mov Seed+1, x+1
	mov Seed+2, x+2
	mov Seed+3, x+3
	ret
	
MyProgram:
    ; Initialize the hardware:

    mov SP, #7FH
    lcall Initialize_All
	lcall LCD_4BIT
    setb EA
    

    ; Make sure the two input pins are configure for input
    clr P2.0
    setb P0.0 ; Pin is used as input
    setb P0.1 ; Pin is used as input
    mov p1score, #0x00
    mov p2score, #0x00

	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message1)
	Set_Cursor(2, 1)
    Send_Constant_String(#Initial_Message2)
    
    
RoundStart:
	mov a, p1score
	cjne a, #5, CheckP2
	sjmp p1win
CheckP2:
	mov a, p2score
	cjne a, #5, Begin
	sjmp p2win
Begin:
	lcall WaitHalfSec
	lcall WaitHalfSec
	lcall Random
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	setb TR0
	mov a, Seed+1
	mov c, acc.3
	mov tone, c
	jb tone, playhigh

playlow:
	clr TR0
	mov RH0, #high(TIMER0_RELOAD1)
	mov RL0, #low(TIMER0_RELOAD1)
	setb TR0
	sjmp forever

playhigh:
	clr TR0
	mov RH0, #high(TIMER0_RELOAD0)
	mov RL0, #low(TIMER0_RELOAD0)
	setb TR0
	sjmp forever

p1win:
	;Player 1 Wins!
	lcall LCD_4BIT ;Clear screen
	Set_Cursor(1, 1)
	Send_Constant_String(#Player1_Win)
	ljmp GameEnd

p2win:
	;Player 2 Wins!
	lcall LCD_4BIT ;Clear screen
	Set_Cursor(1, 1)
	Send_Constant_String(#Player2_Win)
	ljmp GameEnd
    
forever:
	;First Display Score
	Set_Cursor(1,5)
	Display_BCD(p1score)
	Set_Cursor(2,5)
	Display_BCD(p2score)
    ; Measure the period applied to pin P0.0
    clr P2.0
    clr TR2 ; Stop counter 2
    mov TL2, #0
    mov TH2, #0
    jb P0.0, $
    jnb P0.0, $
    mov R0, #100
    setb TR2 ; Start counter 0
meas_loop1:
    jb P0.0, $
    jnb P0.0, $
    djnz R0, meas_loop1 ; Measure the time of 100 periods
    clr TR2 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.0 for later use
    mov Period_A+0, TL2
    mov Period_A+1, TH2
    clr c
    setb player
    mov a, #40
    subb a, Period_A+1
    jc checktone
    clr player
    

	; Convert the result to BCD and display on LCD
;	Set_Cursor(1, 11)
;	lcall hex2bcd2
 ;   lcall DisplayBCD_LCD
    
    ; Measure the period applied to pin P0.1
    clr TR2 ; Stop counter 2
    mov TL2, #0
    mov TH2, #0
    jb P0.1, $
    jnb P0.1, $
    mov R0, #100
    setb TR2 ; Start counter 0
meas_loop2:
    jb P0.1, $
    jnb P0.1, $
    djnz R0, meas_loop2 ; Measure the time of 100 periods
    clr TR2 ; Stop counter 2, TH2-TL2 has the period
    ; save the period of P2.1 for later use
    mov Period_B+0, TL2
    mov Period_B+1, TH2
	mov a, #45
    subb a, Period_B+1
    jc checktone
	lcall Timer1_ISR
	jb one_second_flag, NoSlap
	; Convert the result to BCD and display on LCD
;	Set_Cursor(2, 11)
;	lcall hex2bcd2
 ;   lcall DisplayBCD_LCD
    
    ljmp forever ; Repeat! 

checktone:
	clr TR0
	setb P2.0
	lcall hex2bcd22
	jb tone, addpoint
subbpoint:
    jb player, p1slap1
    lcall DecP2
    sjmp p2slap2
p1slap1:
    lcall DecP1
p2slap2:    
	ljmp RoundStart	
    
addpoint:
    jb player, p1slap
    lcall IncP2
    ljmp RoundStart
p1slap:
    lcall IncP1
p2slap:
	ljmp RoundStart

NoSlap:
	clr TR0
	clr one_second_flag
	ljmp RoundStart
GameEnd:
	;ggez
	clr TR0
mov RH0, #high(TIMER0_RELOAD0+325)
mov RL0, #low(TIMER0_RELOAD0+325)
setb TR0 
lcall WaitHalfSec
clr TR0
mov RH0, #high(TIMER0_RELOAD0+225)
mov RL0, #low(TIMER0_RELOAD0+225)
setb TR0
lcall WaitHalfSec
clr TR0
mov RH0, #high(TIMER0_RELOAD0+125)
mov RL0, #low(TIMER0_RELOAD0+125)
setb TR0 
lcall WaitHalfSec
clr TR0
mov RH0, #high(TIMER0_RELOAD0+25)
mov RL0, #low(TIMER0_RELOAD0+25)
setb TR0 
lcall WaitHalfSec
clr TR0
mov RH0, #high(TIMER0_RELOAD0+125)
mov RL0, #low(TIMER0_RELOAD0+125)
setb TR0
lcall WaitHalfSec
clr TR0
mov RH0, #high(TIMER0_RELOAD0+225)
mov RL0, #low(TIMER0_RELOAD0+225)
setb TR0 
lcall WaitHalfSec
clr TR0
mov RH0, #high(TIMER0_RELOAD0+325)
mov RL0, #low(TIMER0_RELOAD0+325)
setb TR0 
lcall WaitHalfSec
end
