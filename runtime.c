
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
        STA a
        LDA a+1
        ADC b+1
        TAX
        LDA a
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
        STA a
        LDA a+1
        SBC b+1
        TAX
        LDA a
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
        LDA a
        CMP b
        LDA #0
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
    }
}

uint8_t _rt_ne_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA a
        CMP b
        LDA #0
        BEQ skip
        LDA #1
        skip:
    }
}

uint8_t _rt_ne_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #0
    }
}

uint8_t _rt_lt_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA a
        CMP b
        // The carry flag will be *clear* if T0 < T2.
        // Load the corresponding boolean value:
        LDA #0
        BCS skip
        LDA #1
        skip:
    }
}

uint8_t _rt_lt_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #0
    }
}

uint8_t _rt_gt_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA b
        CMP a
        // The carry flag will be *clear* if T0 > T2.
        // Load the corresponding boolean value:
        LDA #0
        BCS skip
        LDA #1
        skip:
    }
}

uint8_t _rt_gt_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDA #0
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
