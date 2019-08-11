;This program was written by Jan Derogee
;If you use this program or crucial parts of it for your own project or commercial use,
;please give me some credit and mention my name, it would also be nice if you mention
;my youtube channel: https://www.youtube.com/user/janderogee/videos
;
;Happy coding!
;Jan Derogee


;---------------------------------------------------------------------------------------------------------
;ATTENTION: This code has been developed to be used with the CBM Program Studio compiler
;---------------------------------------------------------------------------------------------------------

;When using CBM prog studio to compile this program, make sure that you
;compile the entire project (Build -> Project -> And Run (CTRL+F5) )

;==========================================================================================================


;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                        KERNAL POINTERS ANS VECTORS
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

KEYCNT          = 198           ;the counter that keeps track of the number of key in the keyboard buffer
KEYBUF          = 631           ;the first position of the keyboard buffer


;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                BASIC STARTER
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; 10 SYS7424:NEW

*=$1001

        BYTE    $0B, $10, $0A, $00, $9E, $37, $34, $32, $34, $3a, $A2, $00, $00, $00



;The NEW command ($A2) is used to wipe the code from memory so that the user can
;type in or LOAD new programs into memory without interference by the driver code
;Without the NEW command you can't even type in a single BASIC line, it would crash
;immediately (you can try it out by leaving out the NEW command and type in 10 print"hoi";:goto10)

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                 INITIALIZE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


; put this at the top of user RAM after reducing the RAM top to protect it

* = $1D00


INIT        
                SEI                     ; disable interrupts
                LDA   #<IRQ_HANDLER     ; get our code start low byte
                STA   $0314             ; set IRQ vector low byte
                LDA   #>IRQ_HANDLER     ; get our code start high byte
                STA   $0315             ; set IRQ vector high byte
                CLI                     ; enable interrupts
                RTS

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                              INTERRUPT HANDLER
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;Interrupt is used to scan the joystick port for signals that come from the rotary keypad
;joystick switch shorts IO-line to ground, meaning that when the switch is closed the IO-line will
;read 0 (so normally when the joystick lines are left alone, they will read 1)
;

;poke 37139,0           ;37139 = $9113  Data direction register A (set all bits to input)
;poke 37154,127         ;37154 = $9122  Data direction register B (set bit 7 to input)
;read joystick values   ;37137 = $9111 bit:     2 - up         <-used for enter button
;                                               3 - down       <-used for dial pulses
;                                               4 - left
;                                               5 - fire       <-used for shift button
;                        37158 = $9126 bit:     7 - right
;poke 37154,255          ;37154 = $9122  Data direction register B (set bit 7 back to original value)


IRQ_HANDLER     
                ;check if the enter-button (bit-0) was pressed (make sure we detect it only once as we don't want any form of keyrepeat)
                ;---------------------------------------------
CHECK_4_ENTER   LDA $9111               ;read gameport
                AND #$04                ;mask, so we can check the enter-button bit
                CMP PREV_STATE_B2       ;check if the value of this bit has changed since the last time that we checked, as we are only interested in the falling edge of the signal
                STA PREV_STATE_B2       ;save the current value, as that will represent the previous value in the next itteration of the loop
                BEQ ENTER_UNCHANGED     ;when the line didn't change, there is nothing to do
                CMP #$00                ;
                BNE ENTER_INACTIVE      ;skip enter handling when the button isn't pressed
ENTER_ACTIVE    LDA #13                 ;13 = <CR>                       
                LDY KEYCNT              ;get keyboard buffer pointer
                STA KEYBUF,Y            ;add character to buffer
                INC KEYCNT              ;increment buffer pointer register
                JMP IRQ_DONE            ;nothing else to do but exit

ENTER_UNCHANGED 
ENTER_INACTIVE


                ;check if there is an incoming pulse of the rotary dial
                ;------------------------------------------------------
CHECK_4_DIAL    LDA $9111               ;read gameport
                AND #$08                ;mask, so we can check the dial bit
                CMP PREV_STATE_B3       ;check if the value of this bit has changed since the last time that we checked, as we are only interested in the falling edge of the signal
                STA PREV_STATE_B3       ;save the current value, as that will represent the previous value in the next itteration of the loop
                BEQ DIAL_UNCHANGED      ;when the line didn't change, there is nothing to do
                CMP #$00                ;
                BNE DIAL_INACTIVE       ;skip enter handling when the button isn't pressed

                INC PULSE_COUNT         ;we received a pulse, so we increment the counter
           ;     INC BORDER              ;
 
                LDA #50                 ;reset the timeout value, if we don;t receive any pulse within this period (timeout value MUST exceed time between pulses)
                STA PULSE_TIMEOUT       ;then the current number of counted pulses is accepted as the dialed value 
                JMP IRQ_DONE            ;nothing else to do but exit

DIAL_UNCHANGED
DIAL_INACTIVE   ;check for timeout, no need to check for the level of the pulse as the timeout value is many times larger then the pulse)
                LDA PULSE_TIMEOUT       ;check if timeout has already expired
                BEQ IRQ_DONE            ;when it has expired, we exit
                DEC PULSE_TIMEOUT       ;decrement timeout counter by 1
                BEQ TIMEOUT_EXCEEDED    ;
                JMP IRQ_DONE            ;nothing else to do but exit

                ;when we had a timeout, therefore we now can process the counted pulses
TIMEOUT_EXCEEDED

CHECK_4_SHIFT   LDA $9111               ;read gameport
                AND #$20                ;mask, so we can check the shift-button bit
                CMP #$00                ;
                BEQ USE_SHIFTED         ;

USE_NORMAL      LDY PULSE_COUNT         ;get the pulse counter value
                LDA TABLE_NORM,Y       ;and use the lookup table to get the corresponding character code
                JMP PUT_IN_KEYBUF       ;

USE_SHIFTED     LDY PULSE_COUNT         ;get the pulse counter value
                LDA TABLE_SHIFT,Y       ;and use the lookup table to get the corresponding character code
               ;JMP PUT_IN_KEYBUF       ;

PUT_IN_KEYBUF   LDY KEYCNT              ;get keyboard buffer pointer
                STA KEYBUF,Y            ;now add the character we've read from the lookup table and add it to the keyboardbuffer
                INC KEYCNT              ;increment buffer pointer register

                LDA #0                  ;reset pulse counter
                STA PULSE_COUNT         ;
            ;    LDA #14         ;14=light blue
            ;    STA BORDER      ;


IRQ_DONE        JMP $EABF               ;Jump to the beginning KERNAL's standard interrupt service routine.

        ;.......................................


PREV_STATE_B2   BYTE $04                ;the default value for bit-2 (when $DC00 is masked with $04)
PREV_STATE_B3   BYTE $08                ;the default value for bit-3 (when $DC00 is masked with $08)
PULSE_COUNT     BYTE $0                 ;
PULSE_TIMEOUT   BYTE $0                 ;

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                   TABLES
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


TABLE_NORM      BYTE    'U', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'   ;the first value U is simply undefined, as it will never be used because the pulse_counter will never be zero (it only be a value from 1 up to and including 10)
TABLE_SHIFT     BYTE    'U',  63,  43,  45,  42,  47,  46,  44,  58,  59,  20   ;63=? 43=+ 45=- 42=* 47=/ 46=. 44=, 58=: 59=; 20=delete


        ;the ? sign is to be used for PRINT, this way the user can do calculation using the keypad and nothing else.
        ;simply dial ? 8 + 1 <enter> and the value shown will be 9


