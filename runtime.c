
uint8_t _rt_load_u8(__zeropage uint8_t *p)
{
    __asm
    {
        LDY #0
        LDA (p),Y
    }
}

uint16_t _rt_load_u16(__zeropage uint16_t *p)
{
    __asm
    {
        LDY #1
        LDA (p),Y
        TAX
        DEY
        LDA (p),Y
    }
}

uint8_t _rt_store_u8(__zeropage uint8_t *p, uint8_t n)
{
    __asm
    {
        LDY #0
        LDA n
        STA (p),Y
    }
}

uint16_t _rt_store_u16(__zeropage uint16_t *p, uint16_t n)
{
    __asm
    {
        LDY #1
        LDA n+1
        STA (p),Y
        TAX
        DEY
        LDA n
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

uint8_t _rt_logical_not_u8(uint8_t a)
{
    __asm
    {
        LDA #1
        LDX a
        BEQ skip
        LDA #0
        skip:
    }
}

uint8_t _rt_predec_u8(__zeropage uint8_t *p)
{
    __asm
    {
        LDY #0
        LDA (p),Y
        SEC
        SBC #1
        STA (p),Y
    }
}

uint16_t _rt_predec_u16(__zeropage uint16_t *p)
{
    // This is identical to _rt_preinc_u16 except
    // that it subtracts instead of adding.
    
    uint8_t r;
    __asm
    {
        SEC
        LDY #0
        LDA (p),Y
        SBC #1
        STA r
        STA (p),Y
        LDY #1
        LDA (p),Y
        SBC #0
        STA (p),Y
        TAX
        LDA r
    }
}

uint8_t _rt_postdec_u8(__zeropage uint8_t *p)
{
    __asm
    {
        LDY #0
        LDA (p),Y
        TAX
        SEC
        SBC #1
        STA (p),Y
        TXA
    }
}

uint16_t _rt_postdec_u16(__zeropage uint16_t *p)
{
    // This is identical to _rt_postinc_u16 except
    // that it subtracts instead of adding.

    uint16_t r;
    __asm
    {
        SEC
        LDY #0
        LDA (p),Y
        STA r
        SBC #1
        STA (p),Y
        LDY #1
        LDA (p),Y
        STA r+1
        SBC #0
        STA (p),Y
        LDA r+1
        TAX
        LDA r
    }
}

uint8_t _rt_preinc_u8(__zeropage uint8_t *p)
{
    __asm
    {
        LDY #0
        LDA (p),Y
        CLC
        ADC #1
        STA (p),Y
    }
}

uint16_t _rt_preinc_u16(__zeropage uint16_t *p)
{
    // TODO: Make this faster by doing less with the high byte
    // when incrementing the low byte doesn't wrap.
    
    uint8_t r;
    __asm
    {
        CLC
        LDY #0          // Increment the low byte
        LDA (p),Y
        ADC #1
        STA r
        STA (p),Y
        LDY #1          // Wrap carry to the high byte
        LDA (p),Y
        ADC #0
        STA (p),Y
        TAX             // Return the new value
        LDA r
    }
}

uint8_t _rt_postinc_u8(__zeropage uint8_t *p)
{
    __asm
    {
        LDY #0
        LDA (p),Y
        TAX
        CLC
        ADC #1
        STA (p),Y
        TXA
    }
}

uint16_t _rt_postinc_u16(__zeropage uint16_t *p)
{
    // TODO: Make this faster by doing less with the high byte
    // when incrementing the low byte doesn't wrap.

    uint16_t r;
    __asm
    {
        CLC
        LDY #0          // Increment the low byte
        LDA (p),Y
        STA r           // ... while remembering the old value
        ADC #1
        STA (p),Y
        LDY #1          // Wrap carry to the high byte
        LDA (p),Y
        STA r+1         // ... while remembering the old value
        ADC #0
        STA (p),Y
        LDA r+1         // Return the old value
        TAX
        LDA r
    }
}

uint8_t _rt_shift_left_u8(uint8_t a, uint8_t b)
{
    __asm
    {
        LDA a
        LDX b           // Load the counter
        BEQ done        // Return a unmodified if b is zero
        loop:
        ASL             // Shift left
        DEX             // Decrement the counter
        BNE loop
        done:
    }
}

uint16_t _rt_shift_left_u16(uint16_t a, uint16_t b)
{
    __asm
    {
        LDY b+1
        BEQ skip        // If b+1 isn't zero, the answer is always zero
        LDA #0          
        LDX #0
        RTS
        skip:
        LDA a
        LDY b           // Load the counter
        BEQ done        // Return a unmodified if b is zero
        loop:
        ASL             // Shift low byte left
        ROL a+1         // Shift high byte left
        DEY             // Decrement the counter
        BNE loop
        done:
        LDX a+1         // a is already in A, but a+1 needs to go to X
    }
}

uint8_t _rt_shift_right_u8(uint8_t a, uint8_t b)
{
    // This is identical to _rt_shift_left_u8 except
    // that it shifts right instead of left.

    __asm
    {
        LDA a
        LDX b
        BEQ done
        loop:
        LSR
        DEX
        BNE loop
        done:
    }
}

uint16_t _rt_shift_right_u16(uint16_t a, uint16_t b)
{
    // This is identical to _rt_shift_right_u16 except
    // that it shifts right instead of left.

    __asm
    {
        LDY b+1
        BEQ skip
        LDA #0          
        LDX #0
        RTS
        skip:
        LDA a
        LDY b
        BEQ done
        loop:
        LSR a+1
        ROR
        DEY
        BNE loop
        done:
        LDX a+1
    }
}
