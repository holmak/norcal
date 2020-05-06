
define u8 FALSE = 0;
define u8 TRUE = 1;

u8 _rt_load_u8(u8 *p)
{
    __asm
    {
        LDA (p)
        STA __result
        LDA #0
        STA __result+1
    }
}

u16 _rt_load_u16(u16 *p)
{
    __asm
    {
        LDA (p)
        TAY
        INC p
        BNE +2
        INC p+1
        LDA (p)
        STY __result
        STA __result+1
    }
}

// This function does something not normally allowed by C; it takes
// one argument -- the address -- and leaves two values on the
// stack -- the unchanged address and the loaded value.
void _rt_load_nondestructive_u16(u16 *p, u16 result)
{
    __asm
    {
        LDA (p)
        STA result+1
        INC p
        BNE +2
        INC p+1
        LDA (p)
        STA result+1
        // Return now without adjusting the stack:
        RTS
    }
}

void _rt_store_u8(u16 *p, u8 a)
{
    __asm
    {
        LDA a
        STA (p)
    }
}

void _rt_store_u16(u16 *p, u16 a)
{
    __asm
    {
        LDA a
        STA (p)
        INC p
        BNE +2
        INC p+1
        LDA a+1
        STA (p)
    }
}

u16 _rt_add_u16(u16 a, u16 b)
{
    __asm
    {
        CLC
        LDA a
        ADC b
        STA __result
        LDA a+1
        ADC b+1
        STA __result+1
    }
}

u16 _rt_sub_u16(u16 a, u16 b)
{
    __asm
    {
        SEC
        LDA a
        SBC b
        STA __result
        LDA a+1
        SBC b+1
        STA __result+1
    }
}

u8 _rt_mul_u8(u8 a, u8 b)
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

u16 _rt_mul_u16(u16 a, u16 b)
{
    u16 r;
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

u8 _rt_div_u8(u8 a, u8 b)
{
    u8 r;
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

u16 _rt_div_u16(u16 a, u16 b)
{
    u16 r;
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

u8 _rt_mod_u8(u8 a, u8 b)
{
    u8 r;
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

u16 _rt_mod_u16(u16 a, u16 b)
{
    u16 r;
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

bool _rt_eq_u16(u16 a, u16 b)
{
    __asm
    {
        LDA a+1
        CMP b+1
        BNE skip
        LDA a
        CMP b

        LDY #1
        STY a
        DEY
        STY a+1
        JMP end

        skip:
        LDY #0
        STY a
        STY a+1

        end:
    }
}

bool _rt_ne_u16(u16 a, u16 b)
{
    __asm
    {
        // This is identical to _rt_eq_u16 except
        // that the constants are flipped.

        // TODO
    }
}

bool _rt_lt_u16(u16 a, u16 b)
{
    __asm
    {
        // if (AH == BH)
        //     return AL < BL;
        // else
        //     return AH < BH;

        // TODO
    }
}

bool _rt_gt_u16(u16 a, u16 b)
{
    __asm
    {
        // This is identical to _rt_lt_u16 except
        // that the arguments are swapped.

        // TODO
    }
}

bool _rt_ge_u16(u16 a, u16 b)
{
    __asm
    {
        // This is identical to _rt_lt_u16 except
        // that the constants are flipped.

        // TODO
    }
}

bool _rt_le_u16(u16 a, u16 b)
{
    __asm
    {
        // This is identical to _rt_gt_u16 except
        // that the constants are flipped.

        // TODO
    }
}

u8 _rt_bitwise_and_u8(u8 a, u8 b)
{
    __asm
    {
        LDA a
        AND b
        RTS
    }
}

u16 _rt_bitwise_and_u16(u16 a, u16 b)
{
    __asm
    {
        LDA a+1
        AND b+1
        TAX
        LDA a
        AND b
        RTS
    }
}

u8 _rt_bitwise_or_u8(u8 a, u8 b)
{
    __asm
    {
        LDA a
        ORA b
        RTS
    }
}

u16 _rt_bitwise_or_u16(u16 a, u16 b)
{
    __asm
    {
        LDA a+1
        ORA b+1
        TAX
        LDA a
        ORA b
        RTS
    }
}

u8 _rt_bitwise_xor_u8(u8 a, u8 b)
{
    __asm
    {
        LDA a
        EOR b
        RTS
    }
}

u16 _rt_bitwise_xor_u16(u16 a, u16 b)
{
    __asm
    {
        LDA a+1
        EOR b+1
        TAX
        LDA a
        EOR b
        RTS
    }
}

u8 _rt_bitwise_not_u8(u8 a)
{
    __asm
    {
        LDA a
        EOR #$FF
        RTS
    }
}

u16 _rt_bitwise_not_u16(u16 a)
{
    __asm
    {
        LDA a+1
        EOR #$FF
        TAX
        LDA a
        EOR #$FF
        RTS
    }
}

u8 _rt_logical_not_u8(u8 a)
{
    __asm
    {
        LDA #1
        LDX a
        BEQ skip
        LDA #0
        skip:
        RTS
    }
}

u8 _rt_shift_left_u8(u8 a, u8 b)
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
        RTS
    }
}

u16 _rt_shift_left_u16(u16 a, u16 b)
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
        RTS
    }
}

u8 _rt_shift_right_u8(u8 a, u8 b)
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
        RTS
    }
}

u16 _rt_shift_right_u16(u16 a, u16 b)
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
        RTS
    }
}

u8 _rt_preinc_u8(u8 *p, u8 a)
{
    u8 r = *p + a;
    *p = r;
    return r;
}

u16 _rt_preinc_u16(u16 *p, u16 a)
{
    u16 r = *p + a;
    *p = r;
    return r;
}

u8 _rt_predec_u8(u8 *p, u8 a)
{
    u8 r = *p - a;
    *p = r;
    return r;
}

u16 _rt_predec_u16(u16 *p, u16 a)
{
    u16 r = *p - a;
    *p = r;
    return r;
}

u8 _rt_postinc_u8(u8 *p, u8 a)
{
    u8 r = *p;
    *p += a;
    return r;
}

u16 _rt_postinc_u16(u16 *p, u16 a)
{
    u16 r = *p;
    *p += a;
    return r;
}

u8 _rt_postdec_u8(u8 *p, u8 a)
{
    u8 r = *p;
    *p -= a;
    return r;
}

u16 _rt_postdec_u16(u16 *p, u16 a)
{
    u16 r = *p;
    *p -= a;
    return r;
}
