
void _rt_store_u8(__zeropage uint8_t *p, uint8_t n)
{
    __asm
    {
        LDY #0
        LDA n
        STA (p),Y
    }
}

void _rt_store_u16(__zeropage uint16_t *p, uint16_t n)
{
    __asm
    {
        LDY #0
        LDA n
        STA (p),Y
        INY
        LDA n+1
        STA (p),Y
    }
}

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
        LDA #0      // Initialize A (result) to 0
        LDY #8      // Initialize Y (counter) to 8
        loop:
        ASL         // Shift A up    
        ASL b       // Shift b up, storing bit 7 in carry
        BCC skip    // If b.7 was previously 1:
        CLC
        ADC a       // Add a to A without carry
        skip:
        DEY         // Decrement the counter
        BNE loop    // Loop a total of 8 times
    }
}

uint16_t _rt_mul_u16(uint16_t a, uint16_t b)
{
    uint16_t r;
    __asm
    {
        LDA #0      // Initialize r (result) to 0
        STA r
        STA r+1
        LDY #16     // Initialize Y (counter) to 16
        loop:
        ASL r       // Shift r up
        ROL r+1
        ASL b       // Shift b up, storing bit 15 in carry
        ROL b+1
        BCC skip    // If b.15 was previously 1:
        CLC
        LDA r       // Add a to r
        ADC a
        STA r
        LDA r+1
        ADC a+1
        STA r+1
        skip:
        DEY         // Decrement the counter
        BNE loop    // Loop a total of 16 times
        LDA r+1     // Copy r to AX
        TAX
        LDA r
    }
}

uint8_t _rt_div_u8(uint8_t a, uint8_t b)
{
    uint8_t r;
    __asm
    {
        // This is identical to _rt_div_u16 except
        // simplified to 8-bit values.

        LDA #0
        STA r
        LDX #8
        loop:
        ASL a
        ROL r
        LDA r
        SEC
        SBC b
        BCC skip
        STA r
        INC a
        skip:
        DEX
        BNE loop
        LDA a
    }
}

uint16_t _rt_div_u16(uint16_t a, uint16_t b)
{
    uint16_t r;
    __asm
    {
        // From http://nparker.llx.com/a2/mult.html
        
        LDA #0          // Initialize r to 0
        STA r
        STA r+1
        LDX #16         // There are 16 bits in a
        loop:
        ASL a           // Shift hi bit of a into r
        ROL a+1         // (vacating the lo bit, which will be used for the quotient)
        ROL r
        ROL r+1
        LDA r
        SEC             // Trial subtraction
        SBC b
        TAY
        LDA r+1
        SBC b+1
        BCC skip        // Did subtraction succeed?
        STA r+1         // If yes, save it
        STY r
        INC a           // and record a 1 in the quotient
        skip:
        DEX
        BNE loop
        LDA a           // Return the quotient
        LDX a+1                
    }
}

uint8_t _rt_mod_u8(uint8_t a, uint8_t b)
{
    uint8_t r;
    __asm
    {
        // This is identical to _rt_div_u8 except
        // that we return the remainder.

        LDA #0
        STA r
        LDX #8
        loop:
        ASL a
        ROL r
        LDA r
        SEC
        SBC b
        BCC skip
        STA r
        INC a
        skip:
        DEX
        BNE loop
        LDA r
    }
}

uint16_t _rt_mod_u16(uint16_t a, uint16_t b)
{
    uint16_t r;
    __asm
    {
        // This is identical to _rt_div_u16 except
        // that we return the remainder.

        LDA #0
        STA r
        STA r+1
        LDX #16
        loop:
        ASL a
        ROL a+1
        ROL r
        ROL r+1
        LDA r
        SEC
        SBC b
        TAY
        LDA r+1
        SBC b+1
        BCC skip
        STA r+1
        STY r
        INC a
        skip:
        DEX
        BNE loop
        LDA r
        LDX r+1
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
