
extern uint16_t pc;
extern uint8_t sp, a, x, y, status;

extern uint32_t clockticks6502;

void reset6502();
void exec6502(uint32_t tickcount);
void step6502();
void irq6502();
void nmi6502();
void hookexternal(void *funcptr);

uint8_t read6502(uint16_t address);
void write6502(uint16_t address, uint8_t value);
