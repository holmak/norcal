#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "Common.h"

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        fprintf(stderr, "Usage: %s SOURCEFILE OUTPUTFILE", argv[0]);
        exit(1);
    }
    char *sourcefile = argv[1];
    char *outputfile = argv[2];
    Declaration *program = ParseFile(sourcefile);

    printf("Parser output:\n\n");
    PrintProgram(program);
    printf("\n");
    
    CompileProgram(program);
    WriteImage(outputfile);
    Disassemble(outputfile);

    return 0;
}

void *XAlloc(size_t size)
{
    void *p = malloc(size);
    assert(p);
    return p;
}

void Panic(char *message)
{
    fprintf(stderr, "Internal error: %s\n", message);
    exit(1);
}

void Error(char *message)
{
    FilePos pos = GetNextTokenPosition();
    fprintf(stderr, "Error (line %d, column %d): %s\n", pos.Line, pos.Column, message);
    exit(1);
}

void NYI()
{
    Panic("Feature not yet implemented");
}
