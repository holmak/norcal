#include <stdbool.h>
#include <stdint.h>

typedef enum TokenType
{
    TO_INVALID,
    TO_EOF,

    TO_LPAREN,
    TO_RPAREN,
    TO_STAR,
    TO_PLUS,
    TO_MINUS,
    TO_EQUALS,
    TO_SEMICOLON,
    TO_LBRACE,
    TO_RBRACE,

    TO_INT,
    TO_NAME,
} TokenType;

typedef struct FilePos
{
    int Line, Column;
} FilePos;

typedef enum ExprType
{
    EXPR_INT,
    EXPR_NAME,
    EXPR_CALL,
} ExprType;

typedef struct Expr Expr;

struct Expr
{
    ExprType Type;
    int32_t Int;
    char *Name;
    Expr *Args;
    Expr *Next;
};

typedef enum DeclType
{
    DECL_FUNCTION,
    DECL_CONSTANT,
} DeclType;

typedef struct Declaration Declaration;

struct Declaration
{
    DeclType Type;
    char *Name;
    Expr *Body;
    Declaration *Next;
};

typedef enum Opcode Opcode;
enum Opcode
{
    ORA_ZP       = 0x05,
    ORA_IMM      = 0x09,
    ORA_ABS      = 0x0D,
    CLC          = 0x18,
    SEC          = 0x38,
    JMP_ABS      = 0x4C,
    ADC_ZP       = 0x65,
    ADC_ZP_X     = 0x75,
    STA_ZP       = 0x85,
    STX_ZP       = 0x86,
    DEY          = 0x88,
    STA_ABS      = 0x8D,
    STX_ABS      = 0x8E,
    STA_ZP_Y_IND = 0x91,
    STA_ZP_X     = 0x95,
    LDY_IMM      = 0xA0,
    LDA_ZP_X_IND = 0xA1,
    LDX_IMM      = 0xA2,
    LDA_ZP       = 0xA5,
    LDA_IMM      = 0xA9,
    TAX          = 0xAA,
    LDA_ABS      = 0xAD,
    LDX_ABS      = 0xAE,
    LDA_ZP_Y_IND = 0xB1,
    LDA_ZP_X     = 0xB5,
    INY          = 0xC8,
    DEX          = 0xCA,
    BNE          = 0xD0,
    SBC_ZP       = 0xE5,
    INX          = 0xE8,
    BEQ          = 0xF0,
    SBC_ZP_X     = 0xF5,
};

typedef enum Type
{
    TYPE_UINT16,
} Type;

// Lexer:
void InitLexer(char *filename);
bool TryParseInt(int32_t *n);
bool TryParseAnyName(char **s);
bool TryParseName(char *s);
bool TryParse(TokenType expected);
TokenType ReadNextToken();
FilePos GetNextTokenPosition();

Declaration *ParseFile(char *filename);

void CompileProgram(Declaration *program);

void Emit(Opcode op);
void Emit_U8(Opcode op, uint8_t arg);
void Emit_U16(Opcode op, uint16_t arg);
void EmitFix_S8(int32_t address, int32_t target);
void EmitFix_U16(int32_t address, int32_t target);
void EmitComment(char *comment);
int32_t GetCurrentCodeAddress();
void WriteImage(char *filename);
void Disassemble(char *outputfile);

// Syntax trees:
void AppendExpr(Expr **list, Expr *e);
Expr *MakeIntExpr(int32_t n);
Expr *MakeLoadExpr(int32_t address);
bool MatchIntExpr(Expr *e, int32_t *n);
bool MatchUnaryCall(Expr *e, char *funcName, Expr **arg);
bool MatchBinaryCall(Expr *e, char *funcName, Expr **left, Expr **right);
void PrintProgram(Declaration *program);

// General utility:
uint8_t LowByte(int32_t n);
uint8_t HighByte(int32_t n);

void *XAlloc(size_t size);
void Panic(char *message);
void Error(char *message);
void NYI();
