/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:18
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-17 22:20:52
 * @FilePath: \c-libnes\sources\cpu.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#include "cpu.h"
#include <stdbool.h>
#include <stdio.h>

// 1. register

typedef struct
{
    uint8_t A;   // Accumulator Register
    uint8_t X;   // X Register
    uint8_t Y;   // Y Register
    uint8_t SP;  // Stack Pointer (points to location on bus)
    uint16_t PC; // Program Counter
    union        // Status Register
    {
        struct
        {
            uint8_t C : 1; // Carry Bit
            uint8_t Z : 1; // Zero
            uint8_t I : 1; // Disable Interrupts
            uint8_t D : 1; // Decimal Mode (unused in this implementation)
            uint8_t B : 1; // Break
            uint8_t U : 1; // Unused
            uint8_t V : 1; // Overflow
            uint8_t N : 1; // Negative
        };
        uint8_t P;
    };
} reg_t;

reg_t reg = {0};

struct INSTRUCTION
{
    char *name;
    uint8_t (*operate)(void);
    uint8_t (*addrmode)(void);
    uint8_t cycles;
};
extern struct INSTRUCTION lookup[];

// Assisstive variables to facilitate emulation
uint8_t fetched = 0x00;     // Represents the working input value to the ALU
uint16_t temp = 0x0000;     // A convenience variable used everywhere
uint16_t addr_abs = 0x0000; // All used memory addresses end up in here
uint16_t addr_rel = 0x00;   // Represents absolute address following a branch
uint8_t opcode = 0x00;      // Is the instruction byte
uint8_t cycles = 0;         // Counts how many cycles the instruction has remaining
uint32_t clock_count = 0;   // A global accumulation of the number of clocks

#define a reg.A
#define x reg.X
#define y reg.Y
#define stkp reg.SP
#define pc reg.PC
#define C reg.C
#define Z reg.Z
#define I reg.I
#define D reg.D
#define B reg.B
#define U reg.U
#define V reg.V
#define N reg.N
#define status reg.P

#define read ReadByte
#define write WriteByte

///////////////////////////////////////////////////////////////////////////////
// FLAG FUNCTIONS

// Returns the value of a specific bit of the status register
uint8_t GetFlag(uint8_t bit)
{
    return bit;
}

// Sets or clears a specific bit of the status register
void SetFlag(uint8_t bit, bool v)
{
    bit = (v) ? 1 : 0;
}

// 2. 中断


/**
 * @description: 不可屏蔽中断，该中断不能通过 P 的 I 标志屏蔽，所以它一定能触发。比如 PPU 在进入 VBlank 时就会产生 NMI 中断
 * @param {*}
 * @return {*}
 */
void nmi(void)
{
    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    SetFlag(B, 0);
    SetFlag(U, 1);
    SetFlag(I, 1);
    write(0x0100 + stkp, status);
    stkp--;

    addr_abs = 0xFFFA;
    uint16_t lo = read(addr_abs + 0);
    uint16_t hi = read(addr_abs + 1);
    pc = (hi << 8) | lo;

    cycles = 8;
}

/**
 * @description: 复位中断，RESET 按钮按下后或者系统刚上电时产生
 * @param {*}
 * @return {*}
 */
void reset(void)
{
    // Get address to set program counter to
    addr_abs = 0xFFFC;
    uint16_t lo = read(addr_abs + 0);
    uint16_t hi = read(addr_abs + 1);

    // Set it
    pc = (hi << 8) | lo;

    // Reset internal registers
    a = 0;
    x = 0;
    y = 0;
    stkp = 0xFD;
    status = 0x00 | U;

    // Clear internal helper variables
    addr_rel = 0x0000;
    addr_abs = 0x0000;
    fetched = 0x00;

    // Reset takes time
    cycles = 8;
}


/**
 * @description: 复位中断，RESET 按钮按下后或者系统刚上电时产生
 * @param {*}
 * @return {*}
 */
void irq(void)
{
    // If interrupts are allowed
    if (GetFlag(I) == 0)
    {
        // Push the program counter to the stack. It's 16-bits dont
        // forget so that takes two pushes
        write(0x0100 + stkp, (pc >> 8) & 0x00FF);
        stkp--;
        write(0x0100 + stkp, pc & 0x00FF);
        stkp--;

        // Then Push the status register to the stack
        SetFlag(B, 0);
        SetFlag(U, 1);
        SetFlag(I, 1);
        write(0x0100 + stkp, status);
        stkp--;

        // Read new program counter location from fixed address
        addr_abs = 0xFFFE;
        uint16_t lo = read(addr_abs + 0);
        uint16_t hi = read(addr_abs + 1);
        pc = (hi << 8) | lo;

        // IRQs take time
        cycles = 7;
    }
}



// Perform one clock cycles worth of emulation
void clock(void)
{
    // Each instruction requires a variable number of clock cycles to execute.
    // In my emulation, I only care about the final result and so I perform
    // the entire computation in one hit. In hardware, each clock cycle would
    // perform "microcode" style transformations of the CPUs state.
    //
    // To remain compliant with connected devices, it's important that the
    // emulation also takes "time" in order to execute instructions, so I
    // implement that delay by simply counting down the cycles required by
    // the instruction. When it reaches 0, the instruction is complete, and
    // the next one is ready to be executed.
    if (cycles == 0)
    {
        // Read next instruction byte. This 8-bit value is used to index
        // the translation table to get the relevant information about
        // how to implement the instruction
        opcode = read(pc);

#ifdef LOGMODE
        uint16_t log_pc = pc;
#endif

        // Always set the unused status flag bit to 1
        SetFlag(U, true);

        // Increment program counter, we read the opcode byte
        pc++;

        // Get Starting number of cycles
        cycles = lookup[opcode].cycles;

        // Perform fetch of intermmediate data using the
        // required addressing mode
        uint8_t additional_cycle1 = lookup[opcode].addrmode();

        // Perform operation
        uint8_t additional_cycle2 = lookup[opcode].operate();

        // The addressmode and opcode may have altered the number
        // of cycles this instruction requires before its completed
        cycles += (additional_cycle1 & additional_cycle2);

        // Always set the unused status flag bit to 1
        SetFlag(U, true);

#ifdef LOGMODE
        // This logger dumps every cycle the entire processor state for analysis.
        // This can be used for debugging the emulation, but has little utility
        // during emulation. Its also very slow, so only use if you have to.
        if (logfile == nullptr)
            logfile = fopen("olc6502.txt", "wt");
        if (logfile != nullptr)
        {
            fprintf(logfile, "%10d:%02d PC:%04X %s A:%02X X:%02X Y:%02X %s%s%s%s%s%s%s%s STKP:%02X\n",
                    clock_count, 0, log_pc, "XXX", a, x, y,
                    GetFlag(N) ? "N" : ".", GetFlag(V) ? "V" : ".", GetFlag(U) ? "U" : ".",
                    GetFlag(B) ? "B" : ".", GetFlag(D) ? "D" : ".", GetFlag(I) ? "I" : ".",
                    GetFlag(Z) ? "Z" : ".", GetFlag(C) ? "C" : ".", stkp);
        }
#endif
    }

    // Increment global clock count - This is actually unused unless logging is enabled
    // but I've kept it in because its a handy watch variable for debugging
    clock_count++;

    // Decrement the number of cycles remaining for this instruction
    cycles--;
}

// 3. instruction
///////////////////////////////////////////////////////////////////////////////
// ADDRESSING MODES

// The 6502 can address between 0x0000 - 0xFFFF. The high byte is often referred
// to as the "page", and the low byte is the offset into that page. This implies
// there are 256 pages, each containing 256 bytes.
//
// Several addressing modes have the potential to require an additional clock
// cycle if they cross a page boundary. This is combined with several instructions
// that enable this additional clock cycle. So each addressing function returns
// a flag saying it has potential, as does each instruction. If both instruction
// and address function return 1, then an additional clock cycle is required.

// Address Mode: Implied
// There is no additional data required for this instruction. The instruction
// does something very simple like like sets a status bit. However, we will
// target the accumulator, for instructions like PHA
uint8_t IMP(void)
{
    fetched = a;
    return 0;
}

// Address Mode: Immediate
// The instruction expects the next byte to be used as a value, so we'll prep
// the read address to point to the next byte
uint8_t IMM(void)
{
    addr_abs = pc++;
    return 0;
}

// Address Mode: Zero Page
// To save program bytes, zero page addressing allows you to absolutely address
// a location in first 0xFF bytes of address range. Clearly this only requires
// one byte instead of the usual two.
uint8_t ZP0(void)
{
    addr_abs = read(pc);
    pc++;
    addr_abs &= 0x00FF;
    return 0;
}

// Address Mode: Zero Page with X Offset
// Fundamentally the same as Zero Page addressing, but the contents of the X Register
// is added to the supplied single byte address. This is useful for iterating through
// ranges within the first page.
uint8_t ZPX(void)
{
    addr_abs = (read(pc) + x);
    pc++;
    addr_abs &= 0x00FF;
    return 0;
}

// Address Mode: Zero Page with Y Offset
// Same as above but uses Y Register for offset
uint8_t ZPY(void)
{
    addr_abs = (read(pc) + y);
    pc++;
    addr_abs &= 0x00FF;
    return 0;
}

// Address Mode: Relative
// This address mode is exclusive to branch instructions. The address
// must reside within -128 to +127 of the branch instruction, i.e.
// you cant directly branch to any address in the addressable range.
uint8_t REL(void)
{
    addr_rel = read(pc);
    pc++;
    if (addr_rel & 0x80)
        addr_rel |= 0xFF00;
    return 0;
}

// Address Mode: Absolute
// A full 16-bit address is loaded and used
uint8_t ABS(void)
{
    uint16_t lo = read(pc);
    pc++;
    uint16_t hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;

    return 0;
}

// Address Mode: Absolute with X Offset
// Fundamentally the same as absolute addressing, but the contents of the X Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
uint8_t ABX(void)
{
    uint16_t lo = read(pc);
    pc++;
    uint16_t hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;
    addr_abs += x;

    if ((addr_abs & 0xFF00) != (hi << 8))
        return 1;
    else
        return 0;
}

// Address Mode: Absolute with Y Offset
// Fundamentally the same as absolute addressing, but the contents of the Y Register
// is added to the supplied two byte address. If the resulting address changes
// the page, an additional clock cycle is required
uint8_t ABY(void)
{
    uint16_t lo = read(pc);
    pc++;
    uint16_t hi = read(pc);
    pc++;

    addr_abs = (hi << 8) | lo;
    addr_abs += y;

    if ((addr_abs & 0xFF00) != (hi << 8))
        return 1;
    else
        return 0;
}

// Note: The next 3 address modes use indirection (aka Pointers!)

// Address Mode: Indirect
// The supplied 16-bit address is read to get the actual 16-bit address. This is
// instruction is unusual in that it has a bug in the hardware! To emulate its
// function accurately, we also need to emulate this bug. If the low byte of the
// supplied address is 0xFF, then to read the high byte of the actual address
// we need to cross a page boundary. This doesnt actually work on the chip as
// designed, instead it wraps back around in the same page, yielding an
// invalid actual address
uint8_t IND(void)
{
    uint16_t ptr_lo = read(pc);
    pc++;
    uint16_t ptr_hi = read(pc);
    pc++;

    uint16_t ptr = (ptr_hi << 8) | ptr_lo;

    if (ptr_lo == 0x00FF) // Simulate page boundary hardware bug
    {
        addr_abs = (read(ptr & 0xFF00) << 8) | read(ptr + 0);
    }
    else // Behave normally
    {
        addr_abs = (read(ptr + 1) << 8) | read(ptr + 0);
    }

    return 0;
}

// Address Mode: Indirect X
// The supplied 8-bit address is offset by X Register to index
// a location in page 0x00. The actual 16-bit address is read
// from this location
uint8_t IZX(void)
{
    uint16_t t = read(pc);
    pc++;

    uint16_t lo = read((uint16_t)(t + (uint16_t)x) & 0x00FF);
    uint16_t hi = read((uint16_t)(t + (uint16_t)x + 1) & 0x00FF);

    addr_abs = (hi << 8) | lo;

    return 0;
}

// Address Mode: Indirect Y
// The supplied 8-bit address indexes a location in page 0x00. From
// here the actual 16-bit address is read, and the contents of
// Y Register is added to it to offset it. If the offset causes a
// change in page then an additional clock cycle is required.
uint8_t IZY(void)
{
    uint16_t t = read(pc);
    pc++;

    uint16_t lo = read(t & 0x00FF);
    uint16_t hi = read((t + 1) & 0x00FF);

    addr_abs = (hi << 8) | lo;
    addr_abs += y;

    if ((addr_abs & 0xFF00) != (hi << 8))
        return 1;
    else
        return 0;
}

// This function sources the data used by the instruction into
// a convenient numeric variable. Some instructions dont have to
// fetch data as the source is implied by the instruction. For example
// "INX" increments the X register. There is no additional data
// required. For all other addressing modes, the data resides at
// the location held within addr_abs, so it is read from there.
// Immediate adress mode exploits this slightly, as that has
// set addr_abs = pc + 1, so it fetches the data from the
// next byte for example "LDA $FF" just loads the accumulator with
// 256, i.e. no far reaching memory fetch is required. "fetched"
// is a variable global to the CPU, and is set by calling this
// function. It also returns it for convenience.
uint8_t fetch(void)
{
    if (!(lookup[opcode].addrmode == &IMP))
        fetched = read(addr_abs);
    return fetched;
}

///////////////////////////////////////////////////////////////////////////////
// INSTRUCTION IMPLEMENTATIONS

// Note: Ive started with the two most complicated instructions to emulate, which
// ironically is addition and subtraction! Ive tried to include a detailed
// explanation as to why they are so complex, yet so fundamental. Im also NOT
// going to do this through the explanation of 1 and 2's complement.

// Instruction: Add with Carry In
// Function:    A = A + M + C
// Flags Out:   C, V, N, Z
//
// Explanation:
// The purpose of this function is to add a value to the accumulator and a carry bit. If
// the result is > 255 there is an overflow setting the carry bit. Ths allows you to
// chain together ADC instructions to add numbers larger than 8-bits. This in itself is
// simple, however the 6502 supports the concepts of Negativity/Positivity and Signed Overflow.
//
// 10000100 = 128 + 4 = 132 in normal circumstances, we know this as unsigned and it allows
// us to represent numbers between 0 and 255 (given 8 bits). The 6502 can also interpret
// this word as something else if we assume those 8 bits represent the range -128 to +127,
// i.e. it has become signed.
//
// Since 132 > 127, it effectively wraps around, through -128, to -124. This wraparound is
// called overflow, and this is a useful to know as it indicates that the calculation has
// gone outside the permissable range, and therefore no longer makes numeric sense.
//
// Note the implementation of ADD is the same in binary, this is just about how the numbers
// are represented, so the word 10000100 can be both -124 and 132 depending upon the
// context the programming is using it in. We can prove this!
//
//  10000100 =  132  or  -124
// +00010001 = + 17      + 17
//  ========    ===       ===     See, both are valid additions, but our interpretation of
//  10010101 =  149  or  -107     the context changes the value, not the hardware!
//
// In principle under the -128 to 127 range:
// 10000000 = -128, 11111111 = -1, 00000000 = 0, 00000000 = +1, 01111111 = +127
// therefore negative numbers have the most significant set, positive numbers do not
//
// To assist us, the 6502 can set the overflow flag, if the result of the addition has
// wrapped around. V <- ~(A^M) & A^(A+M+C) :D lol, let's work out why!
//
// Let's suppose we have A = 30, M = 10 and C = 0
//          A = 30 = 00011110
//          M = 10 = 00001010+
//     RESULT = 40 = 00101000
//
// Here we have not gone out of range. The resulting significant bit has not changed.
// So let's make a truth table to understand when overflow has occurred. Here I take
// the MSB of each component, where R is RESULT.
//
// A  M  R | V | A^R | A^M |~(A^M) |
// 0  0  0 | 0 |  0  |  0  |   1   |
// 0  0  1 | 1 |  1  |  0  |   1   |
// 0  1  0 | 0 |  0  |  1  |   0   |
// 0  1  1 | 0 |  1  |  1  |   0   |  so V = ~(A^M) & (A^R)
// 1  0  0 | 0 |  1  |  1  |   0   |
// 1  0  1 | 0 |  0  |  1  |   0   |
// 1  1  0 | 1 |  1  |  0  |   1   |
// 1  1  1 | 0 |  0  |  0  |   1   |
//
// We can see how the above equation calculates V, based on A, M and R. V was chosen
// based on the following hypothesis:
//       Positive Number + Positive Number = Negative Result -> Overflow
//       Negative Number + Negative Number = Positive Result -> Overflow
//       Positive Number + Negative Number = Either Result -> Cannot Overflow
//       Positive Number + Positive Number = Positive Result -> OK! No Overflow
//       Negative Number + Negative Number = Negative Result -> OK! NO Overflow

uint8_t ADC(void)
{
    // Grab the data that we are adding to the accumulator
    fetch();

    // Add is performed in 16-bit domain for emulation to capture any
    // carry bit, which will exist in bit 8 of the 16-bit word
    temp = (uint16_t)a + (uint16_t)fetched + (uint16_t)GetFlag(C);

    // The carry flag out exists in the high byte bit 0
    SetFlag(C, temp > 255);

    // The Zero flag is set if the result is 0
    SetFlag(Z, (temp & 0x00FF) == 0);

    // The signed Overflow flag is set based on all that up there! :D
    SetFlag(V, (~((uint16_t)a ^ (uint16_t)fetched) & ((uint16_t)a ^ (uint16_t)temp)) & 0x0080);

    // The negative flag is set to the most significant bit of the result
    SetFlag(N, temp & 0x80);

    // Load the result into the accumulator (it's 8-bit dont forget!)
    a = temp & 0x00FF;

    // This instruction has the potential to require an additional clock cycle
    return 1;
}

// Instruction: Subtraction with Borrow In
// Function:    A = A - M - (1 - C)
// Flags Out:   C, V, N, Z
//
// Explanation:
// Given the explanation for ADC above, we can reorganise our data
// to use the same computation for addition, for subtraction by multiplying
// the data by -1, i.e. make it negative
//
// A = A - M - (1 - C)  ->  A = A + -1 * (M - (1 - C))  ->  A = A + (-M + 1 + C)
//
// To make a signed positive number negative, we can invert the bits and add 1
// (OK, I lied, a little bit of 1 and 2s complement :P)
//
//  5 = 00000101
// -5 = 11111010 + 00000001 = 11111011 (or 251 in our 0 to 255 range)
//
// The range is actually unimportant, because if I take the value 15, and add 251
// to it, given we wrap around at 256, the result is 10, so it has effectively
// subtracted 5, which was the original intention. (15 + 251) % 256 = 10
//
// Note that the equation above used (1-C), but this got converted to + 1 + C.
// This means we already have the +1, so all we need to do is invert the bits
// of M, the data(!) therfore we can simply add, exactly the same way we did
// before.

uint8_t SBC(void)
{
    fetch();

    // Operating in 16-bit domain to capture carry out

    // We can invert the bottom 8 bits with bitwise xor
    uint16_t value = ((uint16_t)fetched) ^ 0x00FF;

    // Notice this is exactly the same as addition from here!
    temp = (uint16_t)a + value + (uint16_t)GetFlag(C);
    SetFlag(C, temp & 0xFF00);
    SetFlag(Z, ((temp & 0x00FF) == 0));
    SetFlag(V, (temp ^ (uint16_t)a) & (temp ^ value) & 0x0080);
    SetFlag(N, temp & 0x0080);
    a = temp & 0x00FF;
    return 1;
}

// OK! Complicated operations are done! the following are much simpler
// and conventional. The typical order of events is:
// 1) Fetch the data you are working with
// 2) Perform calculation
// 3) Store the result in desired place
// 4) Set Flags of the status register
// 5) Return if instruction has potential to require additional
//    clock cycle

// Instruction: Bitwise Logic AND
// Function:    A = A & M
// Flags Out:   N, Z
uint8_t AND(void)
{
    fetch();
    a = a & fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}

// Instruction: Arithmetic Shift Left
// Function:    A = C <- (A << 1) <- 0
// Flags Out:   N, Z, C
uint8_t ASL(void)
{
    fetch();
    temp = (uint16_t)fetched << 1;
    SetFlag(C, (temp & 0xFF00) > 0);
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, temp & 0x80);
    if (lookup[opcode].addrmode == &IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

// Instruction: Branch if Carry Clear
// Function:    if(C == 0) pc = address
uint8_t BCC(void)
{
    if (GetFlag(C) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Carry Set
// Function:    if(C == 1) pc = address
uint8_t BCS(void)
{
    if (GetFlag(C) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Equal
// Function:    if(Z == 1) pc = address
uint8_t BEQ(void)
{
    if (GetFlag(Z) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

uint8_t BIT(void)
{
    fetch();
    temp = a & fetched;
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, fetched & (1 << 7));
    SetFlag(V, fetched & (1 << 6));
    return 0;
}

// Instruction: Branch if Negative
// Function:    if(N == 1) pc = address
uint8_t BMI(void)
{
    if (GetFlag(N) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Not Equal
// Function:    if(Z == 0) pc = address
uint8_t BNE(void)
{
    if (GetFlag(Z) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Positive
// Function:    if(N == 0) pc = address
uint8_t BPL(void)
{
    if (GetFlag(N) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Break
// Function:    Program Sourced Interrupt
uint8_t BRK(void)
{
    pc++;

    SetFlag(I, 1);
    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    SetFlag(B, 1);
    write(0x0100 + stkp, status);
    stkp--;
    SetFlag(B, 0);

    pc = (uint16_t)read(0xFFFE) | ((uint16_t)read(0xFFFF) << 8);
    return 0;
}

// Instruction: Branch if Overflow Clear
// Function:    if(V == 0) pc = address
uint8_t BVC(void)
{
    if (GetFlag(V) == 0)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Branch if Overflow Set
// Function:    if(V == 1) pc = address
uint8_t BVS(void)
{
    if (GetFlag(V) == 1)
    {
        cycles++;
        addr_abs = pc + addr_rel;

        if ((addr_abs & 0xFF00) != (pc & 0xFF00))
            cycles++;

        pc = addr_abs;
    }
    return 0;
}

// Instruction: Clear Carry Flag
// Function:    C = 0
uint8_t CLC(void)
{
    SetFlag(C, false);
    return 0;
}

// Instruction: Clear Decimal Flag
// Function:    D = 0
uint8_t CLD(void)
{
    SetFlag(D, false);
    return 0;
}

// Instruction: Disable Interrupts / Clear Interrupt Flag
// Function:    I = 0
uint8_t CLI(void)
{
    SetFlag(I, false);
    return 0;
}

// Instruction: Clear Overflow Flag
// Function:    V = 0
uint8_t CLV(void)
{
    SetFlag(V, false);
    return 0;
}

// Instruction: Compare Accumulator
// Function:    C <- A >= M      Z <- (A - M) == 0
// Flags Out:   N, C, Z
uint8_t CMP(void)
{
    fetch();
    temp = (uint16_t)a - (uint16_t)fetched;
    SetFlag(C, a >= fetched);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 1;
}

// Instruction: Compare X Register
// Function:    C <- X >= M      Z <- (X - M) == 0
// Flags Out:   N, C, Z
uint8_t CPX(void)
{
    fetch();
    temp = (uint16_t)x - (uint16_t)fetched;
    SetFlag(C, x >= fetched);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}

// Instruction: Compare Y Register
// Function:    C <- Y >= M      Z <- (Y - M) == 0
// Flags Out:   N, C, Z
uint8_t CPY(void)
{
    fetch();
    temp = (uint16_t)y - (uint16_t)fetched;
    SetFlag(C, y >= fetched);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}

// Instruction: Decrement Value at Memory Location
// Function:    M = M - 1
// Flags Out:   N, Z
uint8_t DEC(void)
{
    fetch();
    temp = fetched - 1;
    write(addr_abs, temp & 0x00FF);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}

// Instruction: Decrement X Register
// Function:    X = X - 1
// Flags Out:   N, Z
uint8_t DEX(void)
{
    x--;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}

// Instruction: Decrement Y Register
// Function:    Y = Y - 1
// Flags Out:   N, Z
uint8_t DEY(void)
{
    y--;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
    return 0;
}

// Instruction: Bitwise Logic XOR
// Function:    A = A xor M
// Flags Out:   N, Z
uint8_t EOR(void)
{
    fetch();
    a = a ^ fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}

// Instruction: Increment Value at Memory Location
// Function:    M = M + 1
// Flags Out:   N, Z
uint8_t INC(void)
{
    fetch();
    temp = fetched + 1;
    write(addr_abs, temp & 0x00FF);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    return 0;
}

// Instruction: Increment X Register
// Function:    X = X + 1
// Flags Out:   N, Z
uint8_t INX(void)
{
    x++;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}

// Instruction: Increment Y Register
// Function:    Y = Y + 1
// Flags Out:   N, Z
uint8_t INY(void)
{
    y++;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
    return 0;
}

// Instruction: Jump To Location
// Function:    pc = address
uint8_t JMP(void)
{
    pc = addr_abs;
    return 0;
}

// Instruction: Jump To Sub-Routine
// Function:    Push current pc to stack, pc = address
uint8_t JSR(void)
{
    pc--;

    write(0x0100 + stkp, (pc >> 8) & 0x00FF);
    stkp--;
    write(0x0100 + stkp, pc & 0x00FF);
    stkp--;

    pc = addr_abs;
    return 0;
}

// Instruction: Load The Accumulator
// Function:    A = M
// Flags Out:   N, Z
uint8_t LDA(void)
{
    fetch();
    a = fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}

// Instruction: Load The X Register
// Function:    X = M
// Flags Out:   N, Z
uint8_t LDX(void)
{
    fetch();
    x = fetched;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 1;
}

// Instruction: Load The Y Register
// Function:    Y = M
// Flags Out:   N, Z
uint8_t LDY(void)
{
    fetch();
    y = fetched;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
    return 1;
}

uint8_t LSR(void)
{
    fetch();
    SetFlag(C, fetched & 0x0001);
    temp = fetched >> 1;
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t NOP(void)
{
    switch (opcode)
    {
    case 0x1C:
    case 0x3C:
    case 0x5C:
    case 0x7C:
    case 0xDC:
    case 0xFC:
        return 1;
        break;
    }
    return 0;
}

// Instruction: Bitwise Logic OR
// Function:    A = A | M
// Flags Out:   N, Z
uint8_t ORA(void)
{
    fetch();
    a = a | fetched;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 1;
}

// Instruction: Push Accumulator to Stack
// Function:    A -> stack
uint8_t PHA(void)
{
    write(0x0100 + stkp, a);
    stkp--;
    return 0;
}

// Instruction: Push Status Register to Stack
// Function:    status -> stack
// Note:        Break flag is set to 1 before push
uint8_t PHP(void)
{
    write(0x0100 + stkp, status | B | U);
    SetFlag(B, 0);
    SetFlag(U, 0);
    stkp--;
    return 0;
}

// Instruction: Pop Accumulator off Stack
// Function:    A <- stack
// Flags Out:   N, Z
uint8_t PLA(void)
{
    stkp++;
    a = read(0x0100 + stkp);
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}

// Instruction: Pop Status Register off Stack
// Function:    Status <- stack
uint8_t PLP(void)
{
    stkp++;
    status = read(0x0100 + stkp);
    SetFlag(U, 1);
    return 0;
}

uint8_t ROL(void)
{
    fetch();
    temp = (uint16_t)(fetched << 1) | GetFlag(C);
    SetFlag(C, temp & 0xFF00);
    SetFlag(Z, (temp & 0x00FF) == 0x0000);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t ROR(void)
{
    fetch();
    temp = (uint16_t)(GetFlag(C) << 7) | (fetched >> 1);
    SetFlag(C, fetched & 0x01);
    SetFlag(Z, (temp & 0x00FF) == 0x00);
    SetFlag(N, temp & 0x0080);
    if (lookup[opcode].addrmode == &IMP)
        a = temp & 0x00FF;
    else
        write(addr_abs, temp & 0x00FF);
    return 0;
}

uint8_t RTI(void)
{
    stkp++;
    status = read(0x0100 + stkp);
    status &= ~B;
    status &= ~U;

    stkp++;
    pc = (uint16_t)read(0x0100 + stkp);
    stkp++;
    pc |= (uint16_t)read(0x0100 + stkp) << 8;
    return 0;
}

uint8_t RTS(void)
{
    stkp++;
    pc = (uint16_t)read(0x0100 + stkp);
    stkp++;
    pc |= (uint16_t)read(0x0100 + stkp) << 8;

    pc++;
    return 0;
}

// Instruction: Set Carry Flag
// Function:    C = 1
uint8_t SEC(void)
{
    SetFlag(C, true);
    return 0;
}

// Instruction: Set Decimal Flag
// Function:    D = 1
uint8_t SED(void)
{
    SetFlag(D, true);
    return 0;
}

// Instruction: Set Interrupt Flag / Enable Interrupts
// Function:    I = 1
uint8_t SEI(void)
{
    SetFlag(I, true);
    return 0;
}

// Instruction: Store Accumulator at Address
// Function:    M = A
uint8_t STA(void)
{
    write(addr_abs, a);
    return 0;
}

// Instruction: Store X Register at Address
// Function:    M = X
uint8_t STX(void)
{
    write(addr_abs, x);
    return 0;
}

// Instruction: Store Y Register at Address
// Function:    M = Y
uint8_t STY(void)
{
    write(addr_abs, y);
    return 0;
}

// Instruction: Transfer Accumulator to X Register
// Function:    X = A
// Flags Out:   N, Z
uint8_t TAX(void)
{
    x = a;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}

// Instruction: Transfer Accumulator to Y Register
// Function:    Y = A
// Flags Out:   N, Z
uint8_t TAY(void)
{
    y = a;
    SetFlag(Z, y == 0x00);
    SetFlag(N, y & 0x80);
    return 0;
}

// Instruction: Transfer Stack Pointer to X Register
// Function:    X = stack pointer
// Flags Out:   N, Z
uint8_t TSX(void)
{
    x = stkp;
    SetFlag(Z, x == 0x00);
    SetFlag(N, x & 0x80);
    return 0;
}

// Instruction: Transfer X Register to Accumulator
// Function:    A = X
// Flags Out:   N, Z
uint8_t TXA(void)
{
    a = x;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}

// Instruction: Transfer X Register to Stack Pointer
// Function:    stack pointer = X
uint8_t TXS(void)
{
    stkp = x;
    return 0;
}

// Instruction: Transfer Y Register to Accumulator
// Function:    A = Y
// Flags Out:   N, Z
uint8_t TYA(void)
{
    a = y;
    SetFlag(Z, a == 0x00);
    SetFlag(N, a & 0x80);
    return 0;
}

// This function captures illegal opcodes
uint8_t XXX(void)
{
    return 0;
}


struct INSTRUCTION lookup[] =
{
    { "BRK",  BRK,  IMM, 7 },{ "ORA",  ORA,  IZX, 6 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 3 },{ "ORA",  ORA,  ZP0, 3 },{ "ASL",  ASL,  ZP0, 5 },{ "???",  XXX,  IMP, 5 },{ "PHP",  PHP,  IMP, 3 },{ "ORA",  ORA,  IMM, 2 },{ "ASL",  ASL,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "???",  NOP,  IMP, 4 },{ "ORA",  ORA,  ABS, 4 },{ "ASL",  ASL,  ABS, 6 },{ "???",  XXX,  IMP, 6 },
    { "BPL",  BPL,  REL, 2 },{ "ORA",  ORA,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 4 },{ "ORA",  ORA,  ZPX, 4 },{ "ASL",  ASL,  ZPX, 6 },{ "???",  XXX,  IMP, 6 },{ "CLC",  CLC,  IMP, 2 },{ "ORA",  ORA,  ABY, 4 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 7 },{ "???",  NOP,  IMP, 4 },{ "ORA",  ORA,  ABX, 4 },{ "ASL",  ASL,  ABX, 7 },{ "???",  XXX,  IMP, 7 },
    { "JSR",  JSR,  ABS, 6 },{ "AND",  AND,  IZX, 6 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "BIT",  BIT,  ZP0, 3 },{ "AND",  AND,  ZP0, 3 },{ "ROL",  ROL,  ZP0, 5 },{ "???",  XXX,  IMP, 5 },{ "PLP",  PLP,  IMP, 4 },{ "AND",  AND,  IMM, 2 },{ "ROL",  ROL,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "BIT",  BIT,  ABS, 4 },{ "AND",  AND,  ABS, 4 },{ "ROL",  ROL,  ABS, 6 },{ "???",  XXX,  IMP, 6 },
    { "BMI",  BMI,  REL, 2 },{ "AND",  AND,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 4 },{ "AND",  AND,  ZPX, 4 },{ "ROL",  ROL,  ZPX, 6 },{ "???",  XXX,  IMP, 6 },{ "SEC",  SEC,  IMP, 2 },{ "AND",  AND,  ABY, 4 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 7 },{ "???",  NOP,  IMP, 4 },{ "AND",  AND,  ABX, 4 },{ "ROL",  ROL,  ABX, 7 },{ "???",  XXX,  IMP, 7 },
    { "RTI",  RTI,  IMP, 6 },{ "EOR",  EOR,  IZX, 6 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 3 },{ "EOR",  EOR,  ZP0, 3 },{ "LSR",  LSR,  ZP0, 5 },{ "???",  XXX,  IMP, 5 },{ "PHA",  PHA,  IMP, 3 },{ "EOR",  EOR,  IMM, 2 },{ "LSR",  LSR,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "JMP",  JMP,  ABS, 3 },{ "EOR",  EOR,  ABS, 4 },{ "LSR",  LSR,  ABS, 6 },{ "???",  XXX,  IMP, 6 },
    { "BVC",  BVC,  REL, 2 },{ "EOR",  EOR,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 4 },{ "EOR",  EOR,  ZPX, 4 },{ "LSR",  LSR,  ZPX, 6 },{ "???",  XXX,  IMP, 6 },{ "CLI",  CLI,  IMP, 2 },{ "EOR",  EOR,  ABY, 4 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 7 },{ "???",  NOP,  IMP, 4 },{ "EOR",  EOR,  ABX, 4 },{ "LSR",  LSR,  ABX, 7 },{ "???",  XXX,  IMP, 7 },
    { "RTS",  RTS,  IMP, 6 },{ "ADC",  ADC,  IZX, 6 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 3 },{ "ADC",  ADC,  ZP0, 3 },{ "ROR",  ROR,  ZP0, 5 },{ "???",  XXX,  IMP, 5 },{ "PLA",  PLA,  IMP, 4 },{ "ADC",  ADC,  IMM, 2 },{ "ROR",  ROR,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "JMP",  JMP,  IND, 5 },{ "ADC",  ADC,  ABS, 4 },{ "ROR",  ROR,  ABS, 6 },{ "???",  XXX,  IMP, 6 },
    { "BVS",  BVS,  REL, 2 },{ "ADC",  ADC,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 4 },{ "ADC",  ADC,  ZPX, 4 },{ "ROR",  ROR,  ZPX, 6 },{ "???",  XXX,  IMP, 6 },{ "SEI",  SEI,  IMP, 2 },{ "ADC",  ADC,  ABY, 4 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 7 },{ "???",  NOP,  IMP, 4 },{ "ADC",  ADC,  ABX, 4 },{ "ROR",  ROR,  ABX, 7 },{ "???",  XXX,  IMP, 7 },
    { "???",  NOP,  IMP, 2 },{ "STA",  STA,  IZX, 6 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 6 },{ "STY",  STY,  ZP0, 3 },{ "STA",  STA,  ZP0, 3 },{ "STX",  STX,  ZP0, 3 },{ "???",  XXX,  IMP, 3 },{ "DEY",  DEY,  IMP, 2 },{ "???",  NOP,  IMP, 2 },{ "TXA",  TXA,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "STY",  STY,  ABS, 4 },{ "STA",  STA,  ABS, 4 },{ "STX",  STX,  ABS, 4 },{ "???",  XXX,  IMP, 4 },
    { "BCC",  BCC,  REL, 2 },{ "STA",  STA,  IZY, 6 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 6 },{ "STY",  STY,  ZPX, 4 },{ "STA",  STA,  ZPX, 4 },{ "STX",  STX,  ZPY, 4 },{ "???",  XXX,  IMP, 4 },{ "TYA",  TYA,  IMP, 2 },{ "STA",  STA,  ABY, 5 },{ "TXS",  TXS,  IMP, 2 },{ "???",  XXX,  IMP, 5 },{ "???",  NOP,  IMP, 5 },{ "STA",  STA,  ABX, 5 },{ "???",  XXX,  IMP, 5 },{ "???",  XXX,  IMP, 5 },
    { "LDY",  LDY,  IMM, 2 },{ "LDA",  LDA,  IZX, 6 },{ "LDX",  LDX,  IMM, 2 },{ "???",  XXX,  IMP, 6 },{ "LDY",  LDY,  ZP0, 3 },{ "LDA",  LDA,  ZP0, 3 },{ "LDX",  LDX,  ZP0, 3 },{ "???",  XXX,  IMP, 3 },{ "TAY",  TAY,  IMP, 2 },{ "LDA",  LDA,  IMM, 2 },{ "TAX",  TAX,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "LDY",  LDY,  ABS, 4 },{ "LDA",  LDA,  ABS, 4 },{ "LDX",  LDX,  ABS, 4 },{ "???",  XXX,  IMP, 4 },
    { "BCS",  BCS,  REL, 2 },{ "LDA",  LDA,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 5 },{ "LDY",  LDY,  ZPX, 4 },{ "LDA",  LDA,  ZPX, 4 },{ "LDX",  LDX,  ZPY, 4 },{ "???",  XXX,  IMP, 4 },{ "CLV",  CLV,  IMP, 2 },{ "LDA",  LDA,  ABY, 4 },{ "TSX",  TSX,  IMP, 2 },{ "???",  XXX,  IMP, 4 },{ "LDY",  LDY,  ABX, 4 },{ "LDA",  LDA,  ABX, 4 },{ "LDX",  LDX,  ABY, 4 },{ "???",  XXX,  IMP, 4 },
    { "CPY",  CPY,  IMM, 2 },{ "CMP",  CMP,  IZX, 6 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "CPY",  CPY,  ZP0, 3 },{ "CMP",  CMP,  ZP0, 3 },{ "DEC",  DEC,  ZP0, 5 },{ "???",  XXX,  IMP, 5 },{ "INY",  INY,  IMP, 2 },{ "CMP",  CMP,  IMM, 2 },{ "DEX",  DEX,  IMP, 2 },{ "???",  XXX,  IMP, 2 },{ "CPY",  CPY,  ABS, 4 },{ "CMP",  CMP,  ABS, 4 },{ "DEC",  DEC,  ABS, 6 },{ "???",  XXX,  IMP, 6 },
    { "BNE",  BNE,  REL, 2 },{ "CMP",  CMP,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 4 },{ "CMP",  CMP,  ZPX, 4 },{ "DEC",  DEC,  ZPX, 6 },{ "???",  XXX,  IMP, 6 },{ "CLD",  CLD,  IMP, 2 },{ "CMP",  CMP,  ABY, 4 },{ "NOP",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 7 },{ "???",  NOP,  IMP, 4 },{ "CMP",  CMP,  ABX, 4 },{ "DEC",  DEC,  ABX, 7 },{ "???",  XXX,  IMP, 7 },
    { "CPX",  CPX,  IMM, 2 },{ "SBC",  SBC,  IZX, 6 },{ "???",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "CPX",  CPX,  ZP0, 3 },{ "SBC",  SBC,  ZP0, 3 },{ "INC",  INC,  ZP0, 5 },{ "???",  XXX,  IMP, 5 },{ "INX",  INX,  IMP, 2 },{ "SBC",  SBC,  IMM, 2 },{ "NOP",  NOP,  IMP, 2 },{ "???",  SBC,  IMP, 2 },{ "CPX",  CPX,  ABS, 4 },{ "SBC",  SBC,  ABS, 4 },{ "INC",  INC,  ABS, 6 },{ "???",  XXX,  IMP, 6 },
    { "BEQ",  BEQ,  REL, 2 },{ "SBC",  SBC,  IZY, 5 },{ "???",  XXX,  IMP, 2 },{ "???",  XXX,  IMP, 8 },{ "???",  NOP,  IMP, 4 },{ "SBC",  SBC,  ZPX, 4 },{ "INC",  INC,  ZPX, 6 },{ "???",  XXX,  IMP, 6 },{ "SED",  SED,  IMP, 2 },{ "SBC",  SBC,  ABY, 4 },{ "NOP",  NOP,  IMP, 2 },{ "???",  XXX,  IMP, 7 },{ "???",  NOP,  IMP, 4 },{ "SBC",  SBC,  ABX, 4 },{ "INC",  INC,  ABX, 7 },{ "???",  XXX,  IMP, 7 },
};

bool complete(void)
{
	return cycles == 0;
}


// This is the disassembly function. Its workings are not required for emulation.
// It is merely a convenience function to turn the binary instruction code into
// human readable form. Its included as part of the emulator because it can take
// advantage of many of the CPUs internal operations to do this.
uint16_t disassemble(uint16_t nStart, uint16_t nStop)
{
    uint32_t addr = nStart;
    uint8_t value = 0x00, lo = 0x00, hi = 0x00;
    uint16_t mapLines;
    uint16_t line_addr = 0;

    // // A convenient utility to convert variables into
    // // hex strings because "modern C++"'s method with
    // // streams is atrocious
    // auto hex = [](uint32_t n, uint8_t d)
    // {
    //     std::string s(d, '0');
    //     for (int i = d - 1; i >= 0; i--, n >>= 4)
    //         s[i] = "0123456789ABCDEF"[n & 0xF];
    //     return s;
    // };

    // Starting at the specified address we read an instruction
    // byte, which in turn yields information from the lookup table
    // as to how many additional bytes we need to read and what the
    // addressing mode is. I need this info to assemble human readable
    // syntax, which is different depending upon the addressing mode

    // As the instruction is decoded, a std::string is assembled
    // with the readable output
    while (addr <= (uint32_t)nStop)
    {
        line_addr = addr;

        // // Prefix line with instruction address
        // std::string sInst = "$" + hex(addr, 4) + ": ";

        // Read instruction, and get its readable name
        uint8_t opcode = read(addr);
        addr++;
        // sInst += lookup[opcode].name + " ";
        printf("%s\n", lookup[opcode].name);

        // Get oprands from desired locations, and form the
        // instruction based upon its addressing mode. These
        // routines mimmick the actual fetch routine of the
        // 6502 in order to get accurate data as part of the
        // instruction
        if (lookup[opcode].addrmode == &IMP)
        {
            // sInst += " {IMP}";
        }
        else if (lookup[opcode].addrmode == &IMM)
        {
            value = read(addr);
            addr++;
            // sInst += "#$" + hex(value, 2) + " {IMM}";
        }
        else if (lookup[opcode].addrmode == &ZP0)
        {
            lo = read(addr);
            addr++;
            hi = 0x00;
            // sInst += "$" + hex(lo, 2) + " {ZP0}";
        }
        else if (lookup[opcode].addrmode == &ZPX)
        {
            lo = read(addr);
            addr++;
            hi = 0x00;
            // sInst += "$" + hex(lo, 2) + ", X {ZPX}";
        }
        else if (lookup[opcode].addrmode == &ZPY)
        {
            lo = read(addr);
            addr++;
            hi = 0x00;
            // sInst += "$" + hex(lo, 2) + ", Y {ZPY}";
        }
        else if (lookup[opcode].addrmode == &IZX)
        {
            lo = read(addr);
            addr++;
            hi = 0x00;
            // sInst += "($" + hex(lo, 2) + ", X) {IZX}";
        }
        else if (lookup[opcode].addrmode == &IZY)
        {
            lo = read(addr);
            addr++;
            hi = 0x00;
            // sInst += "($" + hex(lo, 2) + "), Y {IZY}";
        }
        else if (lookup[opcode].addrmode == &ABS)
        {
            lo = read(addr);
            addr++;
            hi = read(addr);
            addr++;
            // sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + " {ABS}";
        }
        else if (lookup[opcode].addrmode == &ABX)
        {
            lo = read(addr);
            addr++;
            hi = read(addr);
            addr++;
            // sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", X {ABX}";
        }
        else if (lookup[opcode].addrmode == &ABY)
        {
            lo = read(addr);
            addr++;
            hi = read(addr);
            addr++;
            // sInst += "$" + hex((uint16_t)(hi << 8) | lo, 4) + ", Y {ABY}";
        }
        else if (lookup[opcode].addrmode == &IND)
        {
            lo = read(addr);
            addr++;
            hi = read(addr);
            addr++;
            // sInst += "($" + hex((uint16_t)(hi << 8) | lo, 4) + ") {IND}";
        }
        else if (lookup[opcode].addrmode == &REL)
        {
            value = read(addr);
            addr++;
            // sInst += "$" + hex(value, 2) + " [$" + hex(addr + value, 4) + "] {REL}";
        }

        // // Add the formed string to a std::map, using the instruction's
        // // address as the key. This makes it convenient to look for later
        // // as the instructions are variable in length, so a straight up
        // // incremental index is not sufficient.
        // mapLines[line_addr] = sInst;
    }

    return mapLines;
}
