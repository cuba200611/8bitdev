                              1 ;   wide.a65: Generate test output for use with testmc.asxxxx
                              2 ;
                              3 ;   This produces "wide" symbol tables with symbol names truncated
                              4 ;   to 14 (.lst, .sym) or 8 (.map) chars.
                              5 
                              6             .radix h
                              7 
                     F123     8 valF123     .equ     0F123
                              9 
   0800 77 69 64 65 20 6C    10 local0:     .strz   'wide local 0'
        6F 63 61 6C 20 30
        00
   080D 01 02 FE FF          11             .db     01,02, 0FE, 0FF
                             12 
                             13             .globl  g_wide
   0811 77 69 64 65 20 67    14 g_wide:     .strz   'wide global 0'
        6C 6F 62 61 6C 20
        30 00
                             15 
   081F 77 69 64 65 20 6C    16 local1:     .strz   'wide local 1'
        6F 63 61 6C 20 31
        00
                             17 
                             18             .globl  c_wide
   082C AD FE FF      [ 4]   19 c_wide:     lda     0FFFE
   082F 8D 39 08      [ 4]   20             sta     c_vec
   0832 AD FF FF      [ 4]   21             lda     0FFFF
   0835 8D 3A 08      [ 4]   22             sta     c_vec+1
   0838 60            [ 6]   23             rts
   0839                      24 c_vec:      .blkw   1
                             25 
                             26 ; note double-colon here so we don't need to write name again in .globl
   083B                      27 w_longsym_123456789_123456789_123456789_123456789_123456789_123456789_123456789:: .blkw   1
                             28 
                             29             .area   area_abs (abs)
   8000                      30             .org    8000
   8000 61 72 65 61 5F 61    31 aa_local0:  .strz 'area_abs  local 0'
        62 73 20 20 6C 6F
        63 61 6C 20 30 00
                             32 
                             33             .area   area_rel (rel,con)
   1A2B 61 72 65 61 5F 72    34 ar_local0:  .strz 'area_rel local 0'
        65 6C 20 6C 6F 63
        61 6C 20 30 00
ASxxxx Assembler V05.31  (Rockwell 6502/6510/65C02)                     Page 1
Hexadecimal [16-Bits]                                 Fri Oct 11 14:55:12 2019

Symbol Table

    .__.$$$.                                                =   2710 L
    .__.ABS.                                                =   0000 G
    .__.CPU.                                                =   0000 L
    .__.H$L.                                                =   0000 L
  2 aa_local0                                                   8000 R
  3 ar_local0                                                   0000 R
  0 c_vec                                                       0039 R
  0 c_wide                                                      002C GR
  0 g_wide                                                      0011 GR
  0 local0                                                      0000 R
  0 local1                                                      001F R
    valF123                                                 =   F123 
  0 w_longsym_123456789_123456789_123456789_123456789_12345     003B GR


ASxxxx Assembler V05.31  (Rockwell 6502/6510/65C02)                     Page 2
Hexadecimal [16-Bits]                                 Fri Oct 11 14:55:12 2019

Area Table

[_CSEG]
   0 _CODE                                 size   3D   flags C180
   2 area_abs                              size 8012   flags  908
   3 area_rel                              size   11   flags  D00
[_DSEG]
   1 _DATA                                 size    0   flags C0C0

