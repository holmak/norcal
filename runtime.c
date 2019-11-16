
uint8_t _rt_add_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        CLC
        LDA a
        ADC b
    }
}

uint16_t _rt_add_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        CLC
        LDA a
        ADC b
        TAY
        LDA a+1
        ADC b+1
        TAX
        TYA
    }
}

uint8_t _rt_sub_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        SEC
        LDA a
        SBC b
    }
}

uint16_t _rt_sub_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        SEC
        LDA a
        SBC b
        TAY
        LDA a+1
        SBC b+1
        TAX
        TYA
    }
}

uint8_t _rt_mul_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA #$EE
    }
}

uint16_t _rt_mul_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #$EE
        TAX
    }
}

uint8_t _rt_div_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA #$EE
    }
}

uint16_t _rt_div_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #$EE
        TAX
    }
}

uint8_t _rt_mod_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA #$EE
    }
}

uint16_t _rt_mod_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #$EE
        TAX
    }
}

uint8_t _rt_bool_u8(uint8_t a)
{
    __asm
    {
        LDA a
    }
}

uint8_t _rt_bool_u16(uint16_t a)
{
    __asm
    {
        LDA a
        ORA a+1
    }
}

uint8_t _rt_eq_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA #0
        LDX a
        CPX b
        BNE skip
        LDA #1
        skip:
    }
}

uint8_t _rt_eq_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #0
        LDX a
        CPX b
        BNE skip
        LDX a+1
        CPX b+1
        BNE skip
        LDA #1
        skip:
    }
}

uint8_t _rt_ne_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        // This is identical to _rt_eq_u8 except
        // that the constants are flipped.

        LDA #1
        LDX a
        CPX b
        BNE skip
        LDA #0
        skip:
    }
}

uint8_t _rt_ne_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        // This is identical to _rt_eq_u16 except
        // that the constants are flipped.

        LDA #1
        LDX a
        CPX b
        BNE skip
        LDX a+1
        CPX b+1
        BNE skip
        LDA #0
        skip:
    }
}

uint8_t _rt_lt_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA #0
        LDX a
        CPX b
        // The carry flag will be *clear* if a < b.
        BCS skip
        LDA #1
        skip:
    }
}

uint8_t _rt_lt_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        // if (AH == BH)
        //     return AL < BL;
        // else
        //     return AH < BH;

        LDA #0
        LDX a+1
        CPX b+1
        BEQ test_low
        // The carry flag will be *clear* if a+1 < b+1.
        BCS skip
        LDA #1
        JMP skip

        test_low:
        LDX a
        CPX b
        // The carry flag will be *clear* if a < b.
        BCS skip
        LDA #1
        skip:
    }
}

uint8_t _rt_gt_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        // This is identical to _rt_lt_u8 except
        // that the arguments are swapped.

        LDA #0
        LDX b
        CPX a
        BCS skip
        LDA #1
        skip:
    }
}

uint8_t _rt_gt_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        // This is identical to _rt_lt_u16 except
        // that the arguments are swapped.

        LDA #0
        LDX b+1
        CPX a+1
        BEQ test_low
        BCS skip
        LDA #1
        JMP skip
        test_low:
        LDX b
        CPX a
        BCS skip
        LDA #1
        skip:
    }
}

uint8_t _rt_ge_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        // This is identical to _rt_lt_u8 except
        // that the constants are flipped.

        LDA #1
        LDX a
        CPX b
        BCS skip
        LDA #0
        skip:
    }
}

uint8_t _rt_ge_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        // This is identical to _rt_lt_u16 except
        // that the constants are flipped.

        LDA #1
        LDX a+1
        CPX b+1
        BEQ test_low
        BCS skip
        LDA #0
        JMP skip
        test_low:
        LDX a
        CPX b
        BCS skip
        LDA #0
        skip:
    }
}

uint8_t _rt_le_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        // This is identical to _rt_gt_u8 except
        // that the constants are flipped.

        LDA #1
        LDX b
        CPX a
        BCS skip
        LDA #0
        skip:
    }
}

uint8_t _rt_le_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        // This is identical to _rt_gt_u16 except
        // that the constants are flipped.

        LDA #1
        LDX b+1
        CPX a+1
        BEQ test_low
        BCS skip
        LDA #0
        JMP skip
        test_low:
        LDX b
        CPX a
        BCS skip
        LDA #0
        skip:
    }
}

uint8_t _rt_bitwise_and_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA a
        AND b
    }
}

uint16_t _rt_bitwise_and_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA a+1
        AND b+1
        TAX
        LDA a
        AND b
    }
}

uint8_t _rt_bitwise_or_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA a
        ORA b
    }
}

uint16_t _rt_bitwise_or_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA a+1
        ORA b+1
        TAX
        LDA a
        ORA b
    }
}

uint8_t _rt_bitwise_xor_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA a
        EOR b
    }
}

uint16_t _rt_bitwise_xor_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA a+1
        EOR b+1
        TAX
        LDA a
        EOR b
    }
}

uint8_t _rt_bitwise_not_u8(uint8_t a)
{
    __asm
    {
        LDA a
        EOR #$FF
    }
}

uint16_t _rt_bitwise_not_u16(uint16_t a)
{
    __asm
    {
        LDA a+1
        EOR #$FF
        TAX
        LDA a
        EOR #$FF
    }
}
