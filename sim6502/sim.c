#define _CRT_SECURE_NO_WARNINGS
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "common.h"

#define CYCLE_LIMIT 10000

#define RAM_SIZE 0x0800
#define ROM_SIZE 0x8000
#define ROM_BASE (0x10000 - ROM_SIZE)
#define STOP_PORT 0x6000
#define DEBUG_PORT_LOW 0x6002
#define DEBUG_PORT_HIGH 0x6003
#define DEBUG_PORT_BYTE 0x6004

static bool run;
static uint8_t ram[RAM_SIZE];
static uint8_t rom[ROM_SIZE];
static FILE *input;
static uint8_t outputLow;

uint8_t next_input()
{
    uint8_t b = fgetc(input);
    if (b == EOF) b = 0;
    return b;
}

uint8_t read6502(uint16_t address)
{
    if (address < RAM_SIZE) return ram[address];
    else if (address == DEBUG_PORT_LOW) return next_input();
    else if (address == DEBUG_PORT_HIGH) return next_input();
    else if (address == DEBUG_PORT_BYTE) return next_input();
    else if (address >= ROM_BASE) return rom[address - ROM_BASE];
    else return 0xEE;
}

void write6502(uint16_t address, uint8_t value)
{
    if (address < RAM_SIZE) ram[address] = value;
    else if (address == STOP_PORT) run = false;
    else if (address == DEBUG_PORT_LOW) outputLow = value;
    else if (address == DEBUG_PORT_HIGH) printf("%d ", (value << 8) | outputLow);
    else if (address == DEBUG_PORT_BYTE) printf("%d ", value);
    else if (address >= ROM_BASE) rom[address - ROM_BASE] = value;
}

int main(int argc, char *argv[])
{
    if (argc != 3)
    {
        fprintf(stderr, "Usage: %s IMAGEFILE INPUTFILE", argv[0]);
        exit(1);
    }
    char *imagefilename = argv[1];
    char *inputfilename = argv[2];

    FILE *image = fopen(imagefilename, "rb");
    assert(image);

    uint8_t header[16];
    assert(fread(header, sizeof(header), 1, image) == 1);
    // TODO: Check the file header.

    assert(fread(rom, sizeof(rom), 1, image) == 1);
    fclose(image);

    input = fopen(inputfilename, "rb");
    assert(input);

    reset6502();
    run = true;
    for (int i = 0; run && i < CYCLE_LIMIT; i++)
    {
        //fprintf(stderr, "PC = %04X  A = %02X\n", pc, a);
        //fprintf(stderr, "RAM ");
        //for (int i = 0; i < 16; i++) fprintf(stderr, "%02X ", ram[i]);
        //fprintf(stderr, "\n");
        //fprintf(stderr, "ROM ");
        //for (int i = 0; i < 16; i++) fprintf(stderr, "%02X ", rom[i]);
        //fprintf(stderr, "\n");

        if (!run) break;
        if (clockticks6502 >= CYCLE_LIMIT)
        {
            fprintf(stderr, "sim error: cycle limit exceeded\n");
            exit(0);
        }
        step6502();

        //getchar();
    }

    exit(0);
}
