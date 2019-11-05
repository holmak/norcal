
uint8_t _rt_add_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        CLC;
        LDA a;
        ADC b;
    }
}

uint16_t _rt_add_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        CLC;
        LDA a;
        ADC b;
        STA a;
        LDA a+1;
        ADC b+1;
        TAX;
        LDA a;
    }
}
