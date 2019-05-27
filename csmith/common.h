#include <stdio.h>
#include <stdint.h>

void reset();

int main()
{
    reset();
    return 0;
}

void OUT(uint16_t value)
{
    printf("%d ", value);
}

void STOP()
{
}