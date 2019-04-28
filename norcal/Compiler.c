#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <string.h>
#include "Common.h"

#define MAX_SYMBOLS 1024

typedef enum SymbolKind
{
    SK_NONE = 0,
    SK_CONSTANT,
    SK_LOCAL,
    SK_FIXUP_REL,
    SK_FIXUP_ABS,
} SymbolKind;

typedef struct Symbol
{
    SymbolKind Kind;
    char *Name;
    int32_t Value;
} Symbol;

// type Destination = Discard | Accumulator | Global Address
typedef int32_t Destination;
#define DEST_DISCARD -1
#define DEST_ACC -2

typedef enum JumpCondition
{
    JUMP_NEVER,
    JUMP_IF_TRUE,
    JUMP_IF_FALSE,
} JumpCondition;

typedef struct Continuation
{
    JumpCondition When;
    char *Target;
} Continuation;

#define CONT_FALLTHROUGH (Continuation){ JUMP_NEVER, NULL }

static int NextLabelNumber = 0;

static Symbol Symbols[MAX_SYMBOLS];

#define RamStart 0x300
#define RamEnd 0x800
static int RamNext = RamStart;

// Temporary pseudoregisters for intrinsic operations, such as arithmetic.
// TODO: Once a symbol table is implemented, mark this space as allocated.
#define T0 0x00F0
#define T1 (T0 + 1)
#define T2 (T0 + 2)
#define T3 (T0 + 3)

static Continuation MakeBranch(JumpCondition when, char *target)
{
    Continuation cont = { when, target };
    return cont;
}

static char *MakeUniqueLabel()
{
    char name[32];
    sprintf(name, "@L%d", NextLabelNumber++);
    return _strdup(name);
}

uint8_t LowByte(int32_t n)
{
    return n & 0xFF;
}

uint8_t HighByte(int32_t n)
{
    return (n >> 8) & 0xFF;
}

static int32_t SizeOf(Type type)
{
    if (type == TYPE_UINT16) return 2;
    else
    {
        NYI();
        return 1;
    }
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

static bool FindSymbol(char *name, Symbol **found)
{
    for (int i = 0; i < MAX_SYMBOLS; i++)
    {
        Symbol *sym = &Symbols[i];
        if (sym->Kind != SK_NONE && !strcmp(sym->Name, name))
        {
            *found = sym;
            return true;
        }
    }

    *found = NULL;
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

// Return true if the expression was constant, and therefore able to be evaluated.
static bool EvaluateConstantExpression(Expr *e, int32_t *value)
{
    // TODO: Evaluate more complex constant expressions, too.
    if (MatchIntExpr(e, value))
    {
        return true;
    }
    else if (e->Type == EXPR_NAME)
    {
        Symbol *sym;
        if (!FindSymbol(e->Name, &sym)) Error("undefined symbol");

        if (sym->Kind == SK_CONSTANT)
        {
            // TODO: Make sure the constant value is not too big.
            *value = sym->Value;
            return true;
        }
    }

    *value = 0;
    return false;
}

// Jump unconditionally.
static void EmitJump(char *target)
{
    Emit_U16(JMP_ABS, 0);
    int32_t fixupAddress = GetCurrentCodeAddress() - 2;
    DefineSymbol(SK_FIXUP_ABS, target, fixupAddress);
}

static void EmitLoadImmediate(int32_t imm, Destination dest, Continuation cont)
{
    EmitComment("load immediate");
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
        Emit_U8(LDX_IMM, HighByte(imm));
        Emit_U16(STA_ABS, dest);
        Emit_U16(STX_ABS, dest + 1);
    }

    if ((cont.When == JUMP_IF_TRUE && imm != 0) ||
        (cont.When == JUMP_IF_FALSE && imm == 0))
    {
        EmitJump(cont.Target);
    }
}

static void EmitStoreAcc(Destination dest)
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

static void EmitBranchOnAcc(Continuation cont)
{
    if (cont.When != JUMP_NEVER)
    {
        // TODO: Values used as branch conditions must have a single-byte type, for easy comparison against zero.
        EmitComment("branch on ACC");
        Emit_U8(ORA_IMM, 0);
        Opcode op = (cont.When == JUMP_IF_TRUE) ? BNE : BEQ;
        Emit_U8(op, 0);
        int32_t fixupAddress = GetCurrentCodeAddress() - 1;
        DefineSymbol(SK_FIXUP_REL, cont.Target, fixupAddress);
    }
}

static void EmitLoad(int32_t address, Destination dest, Continuation cont)
{
    // Even if the value is unused, always read the address; it might be a hardware register.
    Emit_U16(LDA_ABS, address);
    Emit_U16(LDX_ABS, address + 1);
    EmitStoreAcc(dest);
    EmitBranchOnAcc(cont);
}

// Fix up jumps that forward-referenced a label:
static void FixReferencesTo(char *label)
{
    int32_t target = GetCurrentCodeAddress();
    for (int i = 0; i < MAX_SYMBOLS; i++)
    {
        Symbol *sym = &Symbols[i];
        if (sym->Kind != SK_NONE && !strcmp(sym->Name, label))
        {
            if (sym->Kind == SK_FIXUP_REL)
            {
                EmitFix_S8(sym->Value, target);
                memset(sym, 0, sizeof(*sym));
            }
            else if (sym->Kind == SK_FIXUP_ABS)
            {
                EmitFix_U16(sym->Value, target);
                memset(sym, 0, sizeof(*sym));
            }
        }
    }
}

static void CompileExpression(Expr *e, Destination dest, Continuation cont)
{
    int32_t value;
    if (EvaluateConstantExpression(e, &value))
    {
        EmitLoadImmediate(value, dest, cont);
    }
    else if (e->Type == EXPR_NAME)
    {
        Symbol *sym;
        if (!FindSymbol(e->Name, &sym)) Error("undefined symbol");
        if (sym->Kind != SK_LOCAL) NYI();
        int32_t address = sym->Value;
        EmitLoad(address, dest, cont);
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
        if (!strcmp(func, "$load") && EvaluateConstantExpression(firstArg, &addr))
        {
            EmitLoad(addr, dest, cont);
        }
        else if (!strcmp(func, "$assign") && EvaluateConstantExpression(firstArg, &addr))
        {
            if (dest == DEST_DISCARD)
            {
                EmitComment("assign to constant address");
                // Let the sub-expression handle storing the data _and_ any conditional branch.
                CompileExpression(firstArg->Next, addr, cont);
            }
            else
            {
                EmitComment("assign to constant address, and produce the assigned value");
                CompileExpression(firstArg->Next, DEST_ACC, CONT_FALLTHROUGH);
                EmitStoreAcc(addr);
                EmitStoreAcc(dest);
                EmitBranchOnAcc(cont);
            }
        }
        else if (!strcmp(func, "$sequence"))
        {
            for (Expr *p = firstArg; p; p = p->Next)
            {
                EmitComment("begin new statement");
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
        else if (!strcmp(func, "$local"))
        {
            if (argCount != 1) Panic("wrong number of args in local declaration");
            if (firstArg->Type != EXPR_NAME) Panic("invalid local declaration node");
            int address = AllocGlobal(SizeOf(TYPE_UINT16));
            DefineSymbol(SK_LOCAL, firstArg->Name, address);
            if (dest != DEST_DISCARD) Panic("cannot store value of expression of type 'void'");
            if (cont.When != JUMP_NEVER) Panic("cannot branch based on value of type 'void'");
        }
        else if (!strcmp(func, "$addr_of"))
        {
            if (argCount != 1) Panic("wrong number of args");
            if (firstArg->Type == EXPR_NAME)
            {
                Symbol *sym;
                if (!FindSymbol(firstArg->Name, &sym)) Error("undefined symbol");
                if (sym->Kind != SK_LOCAL) NYI();
                int32_t address = sym->Value;
                EmitLoadImmediate(address, dest, cont);
            }
            else
            {
                NYI();
            }
        }
        else if (!strcmp(func, "$switch"))
        {
            if (argCount != 2) Panic("wrong number of items in switch expression");
            Expr *test = firstArg;
            Expr *then = firstArg->Next;
            char *end = MakeUniqueLabel();

            // TODO: Handle any number of clauses. For each clause:
            {
                char *nextClause = MakeUniqueLabel();
                // If this clause's condition is false, try the next clause:
                CompileExpression(test, DEST_DISCARD, MakeBranch(JUMP_IF_FALSE, nextClause));
                // If the condition was true, execute the clause body:
                CompileExpression(then, dest, cont);
                // After executing the body of a clause, skip the rest of the clauses:
                EmitJump(end);
                EmitComment("end of switch clause");
                FixReferencesTo(nextClause);
            }

            EmitComment("end of switch");
            FixReferencesTo(end);
        }
        else
        {
            // This is a non-intrinsic function call.

            BeginTempScope();

            // Evaluate the arguments and store the results in temporary variables.
            // TODO: (optimization) The first arg doesn't have to be simplified, since we haven't started assembling a call frame yet.
            // Optimization: Sufficiently simple arguments (such as literal ints) can skip this
            //   step and be written directly into the call frame.
            Expr *temps = NULL;
            for (Expr *arg = firstArg; arg; arg = arg->Next)
            {
                // When creating the list of "simple expressions", we can't reuse AST nodes, because
                // they form their own list. Instead, create a new copy of the expression,
                // unconnected to the rest of the AST.
                int32_t n;
                Expr *simpleArg;
                if (EvaluateConstantExpression(arg, &n))
                {
                    simpleArg = MakeIntExpr(n);
                }
                else
                {
                    int argSize = SizeOf(TYPE_UINT16);
                    int temp = AllocTemp(argSize);
                    CompileExpression(arg, temp, CONT_FALLTHROUGH);
                    simpleArg = MakeLoadExpr(temp);
                }
                AppendExpr(&temps, simpleArg);
            }

            // Copy all of the argument values from the temporaries into the function's call frame.
            // TODO: Get the call frame address (and type information) from the function's type entry.
            EmitComment("copy arguments to call frame");
            int paramAddress = T0;
            for (Expr *temp = temps; temp; temp = temp->Next)
            {
                int paramSize = SizeOf(TYPE_UINT16);
                CompileExpression(temp, paramAddress, CONT_FALLTHROUGH);
                paramAddress += paramSize;
            }

            // For builtin operations, instead of jumping to a function, emit the code inline.
            EmitComment(func);
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

            // The return value is placed in DEST_ACC.

            EmitStoreAcc(dest);
            EmitBranchOnAcc(cont);

            EndTempScope();
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
            int32_t value;
            if (!EvaluateConstantExpression(decl->Body, &value))
            {
                Error("expression must be constant");
            }
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
            CompileExpression(decl->Body, DEST_DISCARD, CONT_FALLTHROUGH);
            // TODO: Return.
        }
    }
}
