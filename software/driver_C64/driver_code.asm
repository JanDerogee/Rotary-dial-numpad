;This program was written by Jan Derogee
;If you use this program or crucial parts of it for your own project or commercial use, please give me some credit
;and mention my name, it would also be nice if you mention my youtube channel: https://www.youtube.com/user/janderogee/videos
;
;Happy coding!
;Jan Derogee


;---------------------------------------------------------------------------------------------------------
;ATTENTION: This code has been developed to be used with the CBM Program Studio compiler
;---------------------------------------------------------------------------------------------------------

;When using CBM prog studio to compile this program, make sure that you
;compile the entire project (Build -> Project -> And Run (CTRL+F5) )



;"uncomment" the following line to allow the playback of DTMF tones
;make the following line "comment" to disable the playback of DTMF tones
;PLAY_DTMF = 1

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                            BASIC CALL
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

; 10 SYS49152:NEW
*=$0801

        BYTE    $0B, $08, $0A, $00, $9E, $34, $39, $31, $35, $32, $3a, $A2, $00, $00, $00


;The NEW command ($A2) is used to wipe the code from memory so that the user can
;type in or LOAD new programs into memory without interference by the driver code
;Without the NEW command you can't even type in a single BASIC line, it would crash
;immediately (you can try it out by leaving out the NEW command and type in 10 print"hoi";:goto10)

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                    zeropage RAM registers
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

;n.a.

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                          REGISTERS and KERNAL ROUTINES ADDRESSES
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

KEYCNT          = 198           ;the counter that keeps track of the number of key in the keyboard buffer
KEYBUF          = 631           ;the first position of the keyboard buffer

BORDER          = $D020    ;
BACKGROUND      = $D021    ;



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                        INITIALIZE
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

*=$C000         ;this is very high in memory, so this way the interference with the basic program area is reduced to a minimum

                ;this routine will enable the raster interrupt
RASTERINT_INIT  SEI                     ;prevent an non maskable interrupt     
                LDA #%01111111          ;
                STA $DC0D               ;"Switch off" all interrupts signals from CIA-1
                AND $D011               ;
                STA $D011               ;Clear most significant bit in VIC's raster register
                LDA #40                 ;this may be any value as it will be changed again during the actual interrupt routine
                STA $D012               ;Set the raster line number where interrupt should occur
                LDA #<IRQ               ;
                STA $0314               ;
                LDA #>IRQ               ;
                STA $0315               ;Set the interrupt vector to point to interrupt service routine below
                LDA #%00000001          ;
                STA $D01A               ;Enable raster interrupt signals from VIC
                CLI                     ;enable non maskable interrupt
                RTS                     ;interrupt is set up, we may exit now

;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                       (RASTER) INTERRUPT HANDLER
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 
;===============================================================================
;Raster interrupt is used to scan the joystick port for signals that come from the rotary keypad
;joystick switch shorts IO-line to ground, meaning that when the switch is closed the IO-line will
;read 0 (so normally when the joystick lines are left alone, they will read 1)
;
;    bit-0: joystick up/forward         <-used for enter button
;    bit-1: down/backward               <-used for dial pulses
;    bit-2: left
;    bit-3: right
;    bit-4: fire                        <-used for shift button

;note: normally some key-debouncing routines would be required (as there is nothing in the hardware to prevent or dampen this effect)
;but since the signal scanning is so slow (only 50 times/sec) the high frequency bounce of the contacts is not causing problems, which
;is very nice, since we don't have to do anything about it (hurray!)

IRQ             SEI                     ;prevent non maskable interrupt                          
                INC $D019               ;"acknowledge" the interrupt by clearing the VIC's interrupt flag.


                ;check if the enter-button (bit-0) was pressed (make sure we detect it only once as we don't want any form of keyrepeat)
                ;---------------------------------------------
CHECK_4_ENTER   LDA $DC00               ;read gameport2
                AND #$01                ;mask, so we can check the button bit
                CMP PREV_STATE_B0       ;check if the value of this bit has changed since the last time that we checked, as we are only interested in the falling edge of the signal
                STA PREV_STATE_B0       ;save the current value, as that will represent the previous value in the next itteration of the loop
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
CHECK_4_DIAL    LDA $DC00               ;read gameport2
                AND #$02                ;mask, so we can check the button bit
                CMP PREV_STATE_B1       ;check if the value of this bit has changed since the last time that we checked, as we are only interested in the falling edge of the signal
                STA PREV_STATE_B1       ;save the current value, as that will represent the previous value in the next itteration of the loop
                BEQ DIAL_UNCHANGED      ;when the line didn't change, there is nothing to do
                CMP #$00                ;
                BNE DIAL_INACTIVE       ;skip enter handling when the button isn't pressed

                INC PULSE_COUNT         ;we received a pulse, so we increment the counter
          ;      INC BORDER              ;
 
                ;LDA #50                 ;reset the timeout value, if we don;t receive any pulse within this period (timeout value MUST exceed time between pulses)
                LDA #20                 ;reset the timeout value, if we don;t receive any pulse within this period (timeout value MUST exceed time between pulses)
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

CHECK_4_SHIFT   LDA $DC00               ;read gameport2
                AND #$10                ;mask, so we can check the button bit
                CMP #$00                ;
                BEQ USE_SHIFTED         ;

USE_NORMAL      LDY PULSE_COUNT         ;get the pulse counter value
                LDA TABLE_NORM,Y        ;and use the lookup table to get the corresponding character code
                JMP PUT_IN_KEYBUF       ;

USE_SHIFTED     LDY PULSE_COUNT         ;get the pulse counter value
                LDA TABLE_SHIFT,Y       ;and use the lookup table to get the corresponding character code
               ;JMP PUT_IN_KEYBUF       ;

PUT_IN_KEYBUF   LDY KEYCNT              ;get keyboard buffer pointer
                STA KEYBUF,Y            ;now add the character we've read from the lookup table and add it to the keyboardbuffer
                INC KEYCNT              ;increment buffer pointer register

ifdef PLAY_DTMF
                JSR GEN_DTMF_SOUND      ;generate the DTMF sound for that key (just for the fun of it)
endif   ;this endif belongs to "ifdef PLAY_DTMF"

                LDA #0                  ;reset pulse counter
                STA PULSE_COUNT         ;
         ;       LDA #14         ;14=light blue
         ;       STA BORDER      ;


IRQ_DONE        CLI                     ;enable non maskable interrupt
                JMP $EA31               ;Jump to the beginning KERNAL's standard interrupt service routine.

        ;.......................................


PREV_STATE_B0   BYTE $01                ;the default value for bit-0 (when $DC00 is masked with $01)
PREV_STATE_B1   BYTE $02                ;the default value for bit-1 (when $DC00 is masked with $02)
PULSE_COUNT     BYTE $0                 ;
PULSE_TIMEOUT   BYTE $0                 ;

;...............................................................................


TABLE_NORM      BYTE    'U', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'   ;the first value U is simply undefined, as it will never be used because the pulse_counter will never be zero (it only be a value from 1 up to and including 10)
TABLE_SHIFT     BYTE    'U',  63,  43,  45,  42,  47,  46,  44,  58,  59,  20   ;63=? 43=+ 45=- 42=* 47=/ 46=. 44=, 58=: 59=; 20=delete


        ;the ? sign is to be used for PRINT, this way the user can do calculation using the keypad and nothing else.
        ;simply dial ? 8 + 1 <enter> and the value shown will be 9



;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;                                            SUBROUTINES
;>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


ifdef PLAY_DTMF

;-------------------------------------------------------------------------------
;This routine will produce DTMF tones
;Example:       LDA #8                  ;prepare to produce the sounds belonging to the number 8
;               JSR GEN_DTMF_SOUND      ;generate the DTMF dial tone for the given number sotred in A
;...............................................................................

GEN_DTMF_SOUND  SEC             ;convert PETSCII to a numerical value first
                SBC #48         ;by removal of the offset in the character table (48='0')

CHECK_INPUT     CMP #10         ;check for input beyond 9
                BCC INPUT_OK    ;
                RTS             ;value out of bounds, exit without using it.

INPUT_OK        STA KEYVALUE    ;

                LDA #$15        ;
                STA $D418       ;filter mode and volume

                ;.............

                LDA #$00        ;
                STA $D405       ;voice 1: attack, decay
                LDA #$F0        ;
                STA $D406       ;voice 1: sustain, release

                ;.............

                LDA #$00        ;
                STA $D40C       ;voice 2: attack, decay
                LDA #$F0        ;
                STA $D40D       ;voice 2: sustain, release



                LDA KEYVALUE    ;out tone table is 4 bytes per entry,
                ASL             ;therefore we must multiply the keyvalue by 4
                ASL             ;in order to read from the correct line from the table
                TAY             ;

                LDA TABLE_TONES,Y       ;get value from table
                STA $D401               ;voice 1: freq, high byte
                INY                     ;increase pointer
                LDA TABLE_TONES,Y       ;get value from table
                STA $D400               ;voice 1: freq, low byte
                INY                     ;increase pointer
                LDA TABLE_TONES,Y       ;get value from table
                STA $D408               ;voice 2: freq, high byte
                INY                     ;increase pointer
                LDA TABLE_TONES,Y       ;get value from table
                STA $D407               ;voice 2: freq, low byte



                LDA #$11        ;waveform=triangle, gate=1 (start sound)
                STA $D404       ;voice 1: control reg.

                LDA #$11        ;waveform=triangle, gate=1 (start sound)
                STA $D40B       ;voice 2: control reg.

                ;.............

SOUND_DELAY     LDX #$80        ;
SOUND_DEL_1     LDY #$00        ;
SOUND_DEL_2     PHA             ;
                PLA             ;
                DEY             ;
                BNE SOUND_DEL_2 ;
                DEX             ;
                BNE SOUND_DEL_1 ;

                ;.............

                LDA #$10        ;waveform=triangle, gate=0 (stop sound)
                STA $D404       ;

                LDA #$10        ;waveform=triangle, gate=0 (stop sound)
                STA $D40B       ;voice 2: control reg.

                LDX #$10        ;
SOUND_DEL_3     LDY #$00        ;
SOUND_DEL_4     PHA             ;
                PLA             ;
                DEY             ;
                BNE SOUND_DEL_4 ;
                DEX             ;
                BNE SOUND_DEL_3 ;

                ;.............

                RTS 


KEYVALUE        BYTE $0

                    ;f1H, f1L, f2H, f2L          KEY   f1     f2
TABLE_TONES     BYTE $3E, $98, $58, $DE         ; 0: 941Hz + 1336Hz        
                BYTE $2E, $5D, $50, $6B         ; 1: 679Hz + 1209Hz
                BYTE $2E, $5D, $58, $DE         ; 2: 679Hz + 1336Hz        
                BYTE $2E, $5D, $62, $3F         ; 3: 679Hz + 1477Hz                
                BYTE $33, $38, $50, $6B         ; 4: 770Hz + 1209Hz        
                BYTE $33, $38, $58, $DE         ; 5: 770Hz + 1336Hz        
                BYTE $33, $38, $62, $3F         ; 6: 770Hz + 1477Hz        
                BYTE $38, $AC, $50, $6B         ; 7: 852Hz + 1209Hz        
                BYTE $38, $AC, $58, $DE         ; 8: 852Hz + 1336Hz        
                BYTE $38, $AC, $62, $3F         ; 9: 852Hz + 1477Hz        
              ;  BYTE $3E, $98, $58, $DE         ; 0: 941Hz + 1336Hz        
                BYTE $3E, $98, $50, $6B         ; *: 941Hz + 1209Hz        
                BYTE $3E, $98, $62, $3F         ; #: 941Hz + 1477Hz        
                BYTE $2E, $5D, $6C, $9F         ; A: 697Hz + 1633Hz        
                BYTE $33, $38, $6C, $9F         ; B: 770Hz + 1633Hz        
                BYTE $38, $AC, $6C, $9F         ; C: 852Hz + 1633Hz        
                BYTE $3E, $98, $6C, $9F         ; D: 941Hz + 1633Hz        


;DTMF definition table
;=====================
;
;        | 1209 Hz         1336 Hz         1477 Hz         1633 Hz
; -----------------------------------------------------------------
; 697 Hz |   1               2               3               A
; 770 Hz |   4               5               6               B
; 852 Hz |   7               8               9               C
; 941 Hz |   *               0               #               D        

; 697 Hz = 11869 = 2E5D
; 770 Hz = 13112 = 3338
; 852 Hz = 14508 = 38AC
; 941 Hz = 16024 = 3E98
;1209 Hz = 20587 = 506B
;1336 Hz = 22750 = 58DE
;1477 Hz = 25151 = 623F
;1633 Hz = 27807 = 6C9F


;to check if the frequency table is correctly typed, I used the website
;http://dialabc.com/sound/detect/index.html
;to upload a sample of a sequence of sounds, and they reported the values back
;and hurray... it was right the first time. I'm sure this never happens again


endif   ;this endif belongs to "ifdef PLAY_DTMF"