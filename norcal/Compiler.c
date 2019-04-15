#include <stdio.h>
#include <string.h>
#include "Common.h"

#define MAX_SYMBOLS 1024

typedef enum SymbolKind
{
    SK_NONE = 0,
    SK_CONSTANT,
} SymbolKind;

typedef struct Symbol
{
    SymbolKind Kind;
    char *Name;
    int32_t Value;
} Symbol;

// type Destination = Discard | Accumulator | Variable name
typedef int32_t Destination;
#define DEST_DISCARD -1
#define DEST_ACC -2

typedef struct Continuation
{
    char *IfTrue;
    char *IfFalse;
} Continuation;

#define CONT_FALLTHROUGH (Continuation){ NULL, NULL }

static Symbol Symbols[MAX_SYMBOLS];

#define RamStart 0x300
#define RamEnd 0x800
static int RamNext = RamStart;

#define SizeOfUInt16 2

// Temporary pseudoregisters for intrinsic operations, such as arithmetic.
// TODO: Once a symbol table is implemented, mark this space as allocated.
#define T0 0x00F0
#define T1 (T0 + 1)
#define T2 (T0 + 2)
#define T3 (T0 + 3)

static uint8_t LowByte(int32_t n)
{
    return n & 0xFF;
}

static uint8_t HighByte(int32_t n)
{
    return (n >> 8) & 0xFF;
}

static void DefineSymbol(SymbolKind kind, char *name, int32_t value)
{
    Symbol *sym = NULL;
    for (int i = 0; i < MAX_SYMBOLS; i++)
    {
        if (Symbols[i].Kind == SK_NONE)
        {
            sym = &Symbols[i];
            break;
        }
    }

    if (!sym) Panic("too many symbols defined");

    sym->Kind = kind;
    sym->Name = name;
    sym->Value = value;
}

static bool FindSymbol(char *name, Symbol **sym)
{
    for (int i = 0; i < MAX_SYMBOLS; i++)
    {
        if (!strcmp(Symbols[i].Name, name))
        {
            *sym = &Symbols[i];
            return true;
        }
    }

    *sym = NULL;
    return false;
}

// Allocate 'size' bytes in RAM and return the address.
static int AllocGlobal(int size)
{
    if (RamNext + size > RamEnd) Error("Not enough RAM to allocate global.");
    int address = RamNext;
    RamNext += size;
    return address;
}

static void BeginTempScope()
{
    // TODO
}

static void EndTempScope()
{
    // TODO
}

// TODO: Temporary variables can be freed when the current temp scope ends.
static int AllocTemp(int size)
{
    return AllocGlobal(size);
}

static int32_t EvaluateConstantExpression(Expr *e)
{
    // TODO: Evaluate more complex constant expressions, too.
    int32_t n;
    if (MatchIntExpr(e, &n))
    {
        return n;
    }
    else
    {
        Error("expression must be constant");
        return 0;
    }
}

static EmitLoadImmediate(Destination dest, int32_t imm)
{
    if (dest == DEST_DISCARD)
    {
        // NOP
    }
    else if (dest == DEST_ACC)
    {
        Emit_U8(LDA_IMM, LowByte(imm));
        Emit_U8(LDX_IMM, HighByte(imm));
    }
    else
    {
        Emit_U8(LDA_IMM, LowByte(imm));
        Emit_U16(STA_ABS, dest);
        Emit_U8(LDA_IMM, HighByte(imm));
        Emit_U16(STA_ABS, dest + 1);
    }
}

static EmitCopyAccTo(Destination dest)
{
    if (dest == DEST_DISCARD || dest == DEST_ACC)
    {
        // NOP
    }
    else
    {
        Emit_U16(STA_ABS, dest);
        Emit_U16(STX_ABS, dest + 1);
    }
}

static void CompileExpression(Expr *e, Destination dest, Continuation cont)
{
    int32_t n;
    if (MatchIntExpr(e, &n))
    {
        EmitLoadImmediate(dest, n);
    }
    else if (e->Type == EXPR_NAME)
    {
        Symbol *sym;
        if (!FindSymbol(e->Name, &sym)) Error("undefined symbol");

        if (sym->Kind == SK_CONSTANT)
        {
            // TODO: Make sure the constant value is not too big.
            n = sym->Value;
            EmitLoadImmediate(dest, n);
        }
        else
        {
            Panic("NYI");
        }
    }
    else if (e->Type == EXPR_CALL)
    {
        if (!e->Args) Panic("no function specified for call");
        if (e->Args->Type != EXPR_NAME) Error("calling via function pointer is not yet implemented");
        char *func = e->Args->Name;
        Expr *firstArg = e->Args->Next;

        // TODO: Make sure that operators and functions are passed the correct number and type of arguments.

        int argCount = 0;
        for (Expr *arg = firstArg; arg; arg = arg->Next) argCount++;

        // Handle certain functions as "intrinsics"; otherwise use the general function call mechanism.
        int32_t addr;
        if (!strcmp(func, "$load") && MatchIntExpr(firstArg, &addr))
        {
            Emit_U16(LDA_ABS, addr);
            Emit_U16(LDX_ABS, addr + 1);
            EmitCopyAccTo(dest);
        }
        else
        {
            // This is a non-intrinsic function call.

            BeginTempScope();

            // Evaluate the arguments and store the results in temporary variables.
            // TODO: (optimization) The first arg doesn't have to be simplified, since we haven't started assembling a call frame yet.
            // TODO: (optimization) Only do this for args that involve a call; otherwise the arg is simple enough to just copy.
            Expr *temps = NULL;
            for (Expr *arg = firstArg; arg; arg = arg->Next)
            {
                int argSize = SizeOfUInt16;
                int temp = AllocTemp(argSize);
                CompileExpression(arg, temp, CONT_FALLTHROUGH);
                AppendExpr(&temps, MakeLoadExpr(temp));
            }

            // Copy all of the argument values from the temporaries into the function's call frame.
            // TODO: Get the call frame address (and type information) from the function's type entry.
            int paramAddress = T0;
            for (Expr *temp = temps; temp; temp = temp->Next)
            {
                int paramSize = SizeOfUInt16;
                CompileExpression(temp, paramAddress, CONT_FALLTHROUGH);
                paramAddress += paramSize;
            }

            // For builtin operations, instead of jumping to a function, emit the code inline.
            if (!strcmp(func, "$load"))
            {
                if (argCount != 1) Panic("wrong number of arguments to unary operator");
                // TODO: This would be more efficient if it loaded the high byte first.
                Emit_U8(LDY_IMM, 0);
                Emit_U8(LDA_ZP_Y_IND, T0);
                Emit_U8(STA_ZP, T2);
                Emit(INY);
                Emit_U8(LDA_ZP_Y_IND, T0);
                Emit(TAX);
                Emit_U8(LDA_ZP, T2);
            }
            else if (!strcmp(func, "$add"))
            {
                if (argCount != 2) Panic("wrong number of arguments to binary operator");
                Emit(CLC);
                Emit_U8(LDA_ZP, T0);
                Emit_U8(ADC_ZP, T2);
                Emit_U8(STA_ZP, T0);
                Emit_U8(LDA_ZP, T1);
                Emit_U8(ADC_ZP, T3);
                Emit(TAX);
                Emit_U8(LDA_ZP, T0);
            }
            else if (!strcmp(func, "$sub"))
            {
                if (argCount != 2) Panic("wrong number of arguments to binary operator");
                Emit(SEC);
                Emit_U8(LDA_ZP, T0);
                Emit_U8(SBC_ZP, T2);
                Emit_U8(STA_ZP, T0);
                Emit_U8(LDA_ZP, T1);
                Emit_U8(SBC_ZP, T3);
                Emit(TAX);
                Emit_U8(LDA_ZP, T0);
            }
            else if (!strcmp(func, "$assign"))
            {
                if (argCount != 2) Panic("wrong number of arguments to binary operator");
                Emit_U8(LDY_IMM, 0);
                Emit_U8(LDA_ZP, T2);
                Emit_U8(STA_ZP_Y_IND, T0);
                Emit(INY);
                Emit_U8(LDA_ZP, T3);
                Emit_U8(STA_ZP_Y_IND, T0);
            }
            else
            {
                // TODO: Look up the function and JSR to it.
                Error("unknown function");
            }

            // Copy the return value from the accumulator to its destination.
            EmitCopyAccTo(dest);

            EndTempScope();
        }
    }
    else if (e->Type == EXPR_SEQUENCE)
    {
        for (Expr *p = e->Args; p; p = p->Next)
        {
            // Drop the result of each expression except the last.
            if (p->Next)
            {
                CompileExpression(p, DEST_DISCARD, CONT_FALLTHROUGH);
            }
            else
            {
                CompileExpression(p, dest, cont);
            }
        }
    }
    else
    {
        NYI();
    }
}

void CompileProgram(Declaration *program)
{
    // First pass: Read all declarations to get type information and global symbols.
    for (Declaration *decl = program; decl; decl = decl->Next)
    {
        if (decl->Type == DECL_FUNCTION)
        {
            // TODO: Record function types so that they can be typechecked later.
        }
        else if (decl->Type == DECL_CONSTANT)
        {
            uint16_t value = EvaluateConstantExpression(decl->Body);
            DefineSymbol(SK_CONSTANT, decl->Name, value);
        }
        else
        {
            Panic("unhandled declaration type");
        }
    }

    // Second pass: Generate code for each function.

    // TODO: The various vectors must jump to appropriate specially-named functions.

    for (Declaration *decl = program; decl; decl = decl->Next)
    {
        if (decl->Type == DECL_FUNCTION)
        {
            CompileExpression(decl->Body, DEST_ACC, CONT_FALLTHROUGH);
            // TODO: Return.
        }
    }
}
