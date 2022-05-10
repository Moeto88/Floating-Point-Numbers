  .syntax unified
  .cpu cortex-m4
  .fpu softvfp
  .thumb

  .global Main


  .section  .text

Main:
  STMFD   SP!, {LR}

  LDR     R4, =0x416c0000   @ IEEE-754 test value (+ve number, +ve exponent)

  MOV     R0, R4            @ get the exponent
  BL      fp_exp
  MOV     R5, R0

  MOV     R0, R4            @ get the fraction
  BL      fp_frac
  MOV     R6, R0

  MOV     R0, R6            @ re-encode the exponent and fraction
  MOV     R1, R5
  BL      fp_enc            @ should return the original IEEE-754 test value


  LDR     R4, =0x3f000000   @ IEEE-754 test value (+ve number, -ve exponent)

  MOV     R0, R4            @ get the exponent
  BL      fp_exp
  MOV     R5, R0

  MOV     R0, R4            @ get the fraction
  BL      fp_frac
  MOV     R6, R0

  MOV     R0, R6            @ re-encode the exponent and fraction
  MOV     R1, R5
  BL      fp_enc            @ should return the original IEEE-754 test value


  LDR     R4, =0xbfe00000   @ IEEE-754 test value (-ve number, +ve exponent)

  MOV     R0, R4            @ get the exponent
  BL      fp_exp
  MOV     R5, R0

  MOV     R0, R4            @ get the fraction
  BL      fp_frac
  MOV     R6, R0

  MOV     R0, R6            @ re-encode the exponent and fraction
  MOV     R1, R5
  BL      fp_enc            @ should return the original IEEE-754 test value


  LDR     R0, =0x06200000   @ f=12.25
  LDR     R1, =0x0          @ e=0
  BL      fp_enc

  LDR     R0, =0xF9E00000   @ f=-12.25
  LDR     R1, =0x0          @ e=0
  BL      fp_enc

End_Main:
  LDMFD   SP!, {PC}


.end
