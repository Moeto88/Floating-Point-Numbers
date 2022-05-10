  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb
  
  .global   fp_exp
  .global   fp_frac
  .global   fp_enc



@ fp_exp subroutine
@ Obtain the exponent of an IEEE-754 (single precision) number as a signed
@   integer (2's complement)
@
@ Parameters:
@   R0: IEEE-754 number
@
@ Return:
@   R0: exponent (signed integer using 2's complement)

@ (0)100 0001 0110 1100 0000 0000 0000 0000
@ (0)|100 0001 0|110 1100 0000 0000 0000 0000

@ (1)|100 0001 1|001 0001 1110 0000 0000 0000
@ 110000011 00100011110000000000000
@ 11111111111111111111111011111111
@                        110000011 

fp_exp:
  PUSH    {R4-R7,LR}                      @ add any registers R4...R12 that you use

  MOV     R4, R0            @ tmp = IEEE-754
  LDR     R5, =23           @ cons1 = 23
  LDR     R6, =127          @ cons2 = 23
While:
  CMP     R5, #0            @ while(cons1 > 0)
  BLE     endWhile          @ {
  MOV     R4, R4, LSR #1    @  shift to the right by 1bit
  SUB     R5, R5, #1        @  cons1--
  B       While             @ }
endWhile: 
  CMP     R0, #0            @ if(IEEE-754 >= 0)
  BLT     ifNegative        @ {
bias:   
  SUB     R4, R4, 127       @  tmp =  tmp - 127
  MOV     R0, R4            @  result = tmp
  B       endBias           @ }

ifNegative:                 @ else {
  LDR     R7, =0b11111111111111111111111011111111 @ mask = 0b11111111111111111111111011111111
  AND     R4, R4, R7        @        clear 8bit
  B       bias              @      }

endBias:

  POP     {R4-R7,PC}                      @ add any registers R4...R12 that you use



@ fp_frac subroutine
@ Obtain the fraction of an IEEE-754 (single precision) number.
@
@ The returned fraction will include the 'hidden' bit to the left
@   of the radix point (at bit 23). The radix point should be considered to be
@   between bits 22 and 23.
@
@ The returned fraction will be in 2's complement form, reflecting the sign
@   (sign bit) of the original IEEE-754 number.
@
@ Parameters:
@   R0: IEEE-754 number
@
@ Return:
@   R0: fraction (signed fraction, including the 'hidden' bit, in 2's
@         complement form)

@ (0)|100 0001 0|110 1100 0000 0000 0000 0000
@                 110 1100 0000 0000 0000 0000
@                 110 1100 0000 0000 0000 0000
@ 0000 0000 1000 0000 0000 0000 0000 0000 
@ 1111 1111 0111 1111 1111 1111 1111 1111
@                1110 1100 0000 0000 0000 0000

@ (1)|100 0001 1|001 0001 1110 0000 0000 0000
@              1.001 0001 1110 0000 0000 0000
@              1.001 0001 1110 0000 0000 0000

@ 11000001100100011110000000000000
@ 11000001100100011110000000000000
@ 01111111111111111111111111111111
@ 11111111111111111111111011111111
@                        110000011 

fp_frac:
  PUSH    {R4-R9,LR}                      @ add any registers R4...R12 that you use

  MOV     R4, R0               @ tmp1 = IEEE-754
  MOV     R9, R0               @ tmp2 = IEEE-754
  CMP     R9, #0               @ if(tmp2 >= 0)
  BLT     ifNegative2          @ {
start:
  LDR     R5, =8               @  cons1 = 8
  MOV     R6, R5               @  copyOfCons1 = 8
While2:
  CMP     R5, #0               @  while(cons1 > 0)
  BLE     endWhile2            @  {
  MOV     R4, R4, LSL #1       @   shift to the left by 1bit 
  SUB     R5, R5, #1           @   cons1--
  B       While2               @  }
endWhile2:  
While3:
  CMP     R6, #0               @  while(copyOfCons1 > 0)
  BLE     endWhile3            @  {
  MOV     R4, R4, LSR #1       @   shift to the right by 1bit
  SUB     R6, R6, #1           @   copyOfCons1--  
  B       While3               @  }
endWhile3:
  LDR     R7, =0b00000000100000000000000000000000  @ mask = 0b00000000100000000000000000000000
  ORR     R4, R4, R7           @  set bit24
  MOV     R0, R4               @  result = tmp1
  CMP     R9, #0               @  if(tmp2 < 0)
  BGE     finish               @  {
  NEG     R0, R0               @   result = -result
  B       finish               @  }

ifNegative2:
  LDR     R8, =0b01111111111111111111111111111111  @ mask = 0b01111111111111111111111111111111
  AND     R4, R4, R8           @ clear 31bit
  B       start                @ }

finish:

  POP     {R4-R9,PC}                      @ add any registers R4...R12 that you use



@ fp_enc subroutine
@ Encode an IEEE-754 (single precision) floating point number given the
@   fraction (in 2's complement form) and the exponent (also in 2's
@   complement form).
@
@ Fractions that are not normalised will be normalised by the subroutine,
@   with a corresponding adjustment made to the exponent.
@
@ Parameters:
@   R0: fraction (in 2's complement form)
@   R1: exponent (in 2's complement form)
@ 
@ Return:
@   R0: IEEE-754 single precision floating point number


@   R0: 111011000000000000000000 (2's)
@   R1: 3
@ 1111 1111 0111 1111 1111 1111 1111 1111
@ (0)|100 0001 0|110 1100 0000 0000 0000 0000
@     100 0001 0110 1100 0000 0000 0000 0000
@                110 1100 0000 0000 0000 0000
@     100 0001 0000  0000 0000 0000 0000 0000
@ (1)|100 0001 1|001 0001 1110 0000 0000 0000
@     100 0001 1001 0001 1110 0000 0000 0000

@ 11 00|01 0000 0000 0000 0000 0000
@ 1100|01000000000000000000000
@  110|00100000000000000000000
@    1|10001000000000000000000
@    111111111111111111111111
@ 00 01|11 1111 1111 1111 1111 1111

@ 110001000000000000000000000
@ 11000100000000000000000000
@ 00011111111111111111111111
@   111111111111111111111111

fp_enc:
  PUSH    {R4-R11,LR}                      @ add any registers R4...R12 that you use
  LDR     R11, =0b111111111111111111111111 @ cons1(for normalisation) = 0b111111111111111111111111
  MOV     R5, R1             @ exp = exponent
  MOV     R4, R0             @ frc1 = fraction
  MOV     R9, R0             @ frc2 = fraction

  CMP     R4, #0             @ if(frc1 < 0)
  BGE     loop               @ {
  NEG     R4, R4             @  frc1 = -frc1
loop:                        @ }
  CMP     R4, R11            @ if(frc > cons1)
  BLE     start2             @ {
  MOV     R4, R4, LSR #1     @  shift to the right by 1bit
  ADD     R5, R5, #1         @  exp++
  B       loop               @ }

start2:
  LDR     R6, =0b11111111011111111111111111111111 @ mask = 0b11111111011111111111111111111111
  AND     R4, R4, R6         @ clear 23bit

  LDR     R7, =127           @ cons2 = 127
  ADD     R5, R5, R7         @ exp = exp + cons2

  LDR     R8, =23            @ cons3 = 23
While4:
  CMP     R8, #0             @ while(cons3 > 0)
  BLE     endWhile4          @ {
  MOV     R5, R5, LSL #1     @  shift to the left by 1bit
  SUB     R8, R8, #1         @  cons3--
  B       While4             @ }
endWhile4:
  ADD     R0, R4, R5         @ result = frc1 + exp
  CMP     R9, #0             @ if(frc2 < 0)
  BGE     finish2            @ {
  LDR     R10, =0b10000000000000000000000000000000 @ mask = 0b10000000000000000000000000000000
  ORR     R0, R0, R10        @  set bit31

finish2:                     @ }
  POP     {R4-R11,PC}                      @ add any registers R4...R12 that you use



.end