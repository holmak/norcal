
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
