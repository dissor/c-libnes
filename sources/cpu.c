/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:18
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-06 22:03:25
 * @FilePath: \c-libnes\sources\cpu.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#include "cpu.h"
cpu_type_t CPU;
reg_type_t REG;


// This structure and the following vector are used to compile and store
// the opcode translation table. The 6502 can effectively have 256
// different instructions. Each of these are stored in a table in numerical
// order so they can be looked up easily, with no decoding required.
// Each table entry holds:
//    Pneumonic : A textual representation of the instruction (used for disassembly)
//    Opcode Function: A function pointer to the implementation of the opcode
//    Opcode Address Mode : A function pointer to the implementation of the
//                          addressing mechanism used by the instruction
//    Cycle Count : An integer that represents the base number of clock cycles the
//                  CPU requires to perform the instruction
typedef struct
{
    char *name;
    uint8_t (*opcode)(void);
    uint8_t (*addrMode)(void);
    uint8_t cycles;
} instruction_t;

// Addressing Modes =============================================
// The 6502 has a variety of addressing modes to access data in
// memory, some of which are direct and some are indirect (like
// pointers in C++). Each opcode contains information about which
// addressing mode should be employed to facilitate the
// instruction, in regards to where it reads/writes the data it
// uses. The address mode changes the number of bytes that
// makes up the full instruction, so we implement addressing
// before executing the instruction, to make sure the program
// counter is at the correct location, the instruction is
// primed with the addresses it needs, and the number of clock
// cycles the instruction requires is calculated. These functions
// may adjust the number of cycles required depending upon where
// and how the memory is accessed, so they return the required
// adjustment.

uint8_t IMP();
uint8_t IMM();
uint8_t ZP0();
uint8_t ZPX();
uint8_t ZPY();
uint8_t REL();
uint8_t ABS();
uint8_t ABX();
uint8_t ABY();
uint8_t IND();
uint8_t IZX();
uint8_t IZY();

// Opcodes ======================================================
// There are 56 "legitimate" opcodes provided by the 6502 CPU. I
// have not modelled "unofficial" opcodes. As each opcode is
// defined by 1 byte, there are potentially 256 possible codes.
// Codes are not used in a "switch case" style on a processor,
// instead they are repsonisble for switching individual parts of
// CPU circuits on and off. The opcodes listed here are official,
// meaning that the functionality of the chip when provided with
// these codes is as the developers intended it to be. Unofficial
// codes will of course also influence the CPU circuitry in
// interesting ways, and can be exploited to gain additional
// functionality!
//
// These functions return 0 normally, but some are capable of
// requiring more clock cycles when executed under certain
// conditions combined with certain addressing modes. If that is
// the case, they return 1.
//
// I have included detailed explanations of each function in
// the class implementation file. Note they are listed in
// alphabetical order here for ease of finding.

uint8_t ADC();
uint8_t AND();
uint8_t ASL();
uint8_t BCC();
uint8_t BCS();
uint8_t BEQ();
uint8_t BIT();
uint8_t BMI();
uint8_t BNE();
uint8_t BPL();
uint8_t BRK();
uint8_t BVC();
uint8_t BVS();
uint8_t CLC();
uint8_t CLD();
uint8_t CLI();
uint8_t CLV();
uint8_t CMP();
uint8_t CPX();
uint8_t CPY();
uint8_t DEC();
uint8_t DEX();
uint8_t DEY();
uint8_t EOR();
uint8_t INC();
uint8_t INX();
uint8_t INY();
uint8_t JMP();
uint8_t JSR();
uint8_t LDA();
uint8_t LDX();
uint8_t LDY();
uint8_t LSR();
uint8_t NOP();
uint8_t ORA();
uint8_t PHA();
uint8_t PHP();
uint8_t PLA();
uint8_t PLP();
uint8_t ROL();
uint8_t ROR();
uint8_t RTI();
uint8_t RTS();
uint8_t SBC();
uint8_t SEC();
uint8_t SED();
uint8_t SEI();
uint8_t STA();
uint8_t STX();
uint8_t STY();
uint8_t TAX();
uint8_t TAY();
uint8_t TSX();
uint8_t TXA();
uint8_t TXS();
uint8_t TYA();

// I capture all "unofficial" opcodes with this function. It is
// functionally identical to a NOP
uint8_t XXX();

// Assembles the translation table. It's big, it's ugly, but it yields a convenient way
// to emulate the 6502. I'm certain there are some "code-golf" strategies to reduce this
// but I've deliberately kept it verbose for study and alteration

// It is 16x16 entries. This gives 256 instructions. It is arranged to that the bottom
// 4 bits of the instruction choose the column, and the top 4 bits choose the row.

// For convenience to get function pointers to members of this class, I'm using this
// or else it will be much much larger :D

// The table is one big initialiser list of initialiser lists...
instruction_t lookup[] =
    {
        {"BRK", &BRK, &IMM, 7},
        {"ORA", &ORA, &IZX, 6},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 3},
        {"ORA", &ORA, &ZP0, 3},
        {"ASL", &ASL, &ZP0, 5},
        {"???", &XXX, &IMP, 5},
        {"PHP", &PHP, &IMP, 3},
        {"ORA", &ORA, &IMM, 2},
        {"ASL", &ASL, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"???", &NOP, &IMP, 4},
        {"ORA", &ORA, &ABS, 4},
        {"ASL", &ASL, &ABS, 6},
        {"???", &XXX, &IMP, 6},
        {"BPL", &BPL, &REL, 2},
        {"ORA", &ORA, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 4},
        {"ORA", &ORA, &ZPX, 4},
        {"ASL", &ASL, &ZPX, 6},
        {"???", &XXX, &IMP, 6},
        {"CLC", &CLC, &IMP, 2},
        {"ORA", &ORA, &ABY, 4},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 4},
        {"ORA", &ORA, &ABX, 4},
        {"ASL", &ASL, &ABX, 7},
        {"???", &XXX, &IMP, 7},
        {"JSR", &JSR, &ABS, 6},
        {"AND", &AND, &IZX, 6},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"BIT", &BIT, &ZP0, 3},
        {"AND", &AND, &ZP0, 3},
        {"ROL", &ROL, &ZP0, 5},
        {"???", &XXX, &IMP, 5},
        {"PLP", &PLP, &IMP, 4},
        {"AND", &AND, &IMM, 2},
        {"ROL", &ROL, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"BIT", &BIT, &ABS, 4},
        {"AND", &AND, &ABS, 4},
        {"ROL", &ROL, &ABS, 6},
        {"???", &XXX, &IMP, 6},
        {"BMI", &BMI, &REL, 2},
        {"AND", &AND, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 4},
        {"AND", &AND, &ZPX, 4},
        {"ROL", &ROL, &ZPX, 6},
        {"???", &XXX, &IMP, 6},
        {"SEC", &SEC, &IMP, 2},
        {"AND", &AND, &ABY, 4},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 4},
        {"AND", &AND, &ABX, 4},
        {"ROL", &ROL, &ABX, 7},
        {"???", &XXX, &IMP, 7},
        {"RTI", &RTI, &IMP, 6},
        {"EOR", &EOR, &IZX, 6},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 3},
        {"EOR", &EOR, &ZP0, 3},
        {"LSR", &LSR, &ZP0, 5},
        {"???", &XXX, &IMP, 5},
        {"PHA", &PHA, &IMP, 3},
        {"EOR", &EOR, &IMM, 2},
        {"LSR", &LSR, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"JMP", &JMP, &ABS, 3},
        {"EOR", &EOR, &ABS, 4},
        {"LSR", &LSR, &ABS, 6},
        {"???", &XXX, &IMP, 6},
        {"BVC", &BVC, &REL, 2},
        {"EOR", &EOR, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 4},
        {"EOR", &EOR, &ZPX, 4},
        {"LSR", &LSR, &ZPX, 6},
        {"???", &XXX, &IMP, 6},
        {"CLI", &CLI, &IMP, 2},
        {"EOR", &EOR, &ABY, 4},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 4},
        {"EOR", &EOR, &ABX, 4},
        {"LSR", &LSR, &ABX, 7},
        {"???", &XXX, &IMP, 7},
        {"RTS", &RTS, &IMP, 6},
        {"ADC", &ADC, &IZX, 6},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 3},
        {"ADC", &ADC, &ZP0, 3},
        {"ROR", &ROR, &ZP0, 5},
        {"???", &XXX, &IMP, 5},
        {"PLA", &PLA, &IMP, 4},
        {"ADC", &ADC, &IMM, 2},
        {"ROR", &ROR, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"JMP", &JMP, &IND, 5},
        {"ADC", &ADC, &ABS, 4},
        {"ROR", &ROR, &ABS, 6},
        {"???", &XXX, &IMP, 6},
        {"BVS", &BVS, &REL, 2},
        {"ADC", &ADC, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 4},
        {"ADC", &ADC, &ZPX, 4},
        {"ROR", &ROR, &ZPX, 6},
        {"???", &XXX, &IMP, 6},
        {"SEI", &SEI, &IMP, 2},
        {"ADC", &ADC, &ABY, 4},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 4},
        {"ADC", &ADC, &ABX, 4},
        {"ROR", &ROR, &ABX, 7},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 2},
        {"STA", &STA, &IZX, 6},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 6},
        {"STY", &STY, &ZP0, 3},
        {"STA", &STA, &ZP0, 3},
        {"STX", &STX, &ZP0, 3},
        {"???", &XXX, &IMP, 3},
        {"DEY", &DEY, &IMP, 2},
        {"???", &NOP, &IMP, 2},
        {"TXA", &TXA, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"STY", &STY, &ABS, 4},
        {"STA", &STA, &ABS, 4},
        {"STX", &STX, &ABS, 4},
        {"???", &XXX, &IMP, 4},
        {"BCC", &BCC, &REL, 2},
        {"STA", &STA, &IZY, 6},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 6},
        {"STY", &STY, &ZPX, 4},
        {"STA", &STA, &ZPX, 4},
        {"STX", &STX, &ZPY, 4},
        {"???", &XXX, &IMP, 4},
        {"TYA", &TYA, &IMP, 2},
        {"STA", &STA, &ABY, 5},
        {"TXS", &TXS, &IMP, 2},
        {"???", &XXX, &IMP, 5},
        {"???", &NOP, &IMP, 5},
        {"STA", &STA, &ABX, 5},
        {"???", &XXX, &IMP, 5},
        {"???", &XXX, &IMP, 5},
        {"LDY", &LDY, &IMM, 2},
        {"LDA", &LDA, &IZX, 6},
        {"LDX", &LDX, &IMM, 2},
        {"???", &XXX, &IMP, 6},
        {"LDY", &LDY, &ZP0, 3},
        {"LDA", &LDA, &ZP0, 3},
        {"LDX", &LDX, &ZP0, 3},
        {"???", &XXX, &IMP, 3},
        {"TAY", &TAY, &IMP, 2},
        {"LDA", &LDA, &IMM, 2},
        {"TAX", &TAX, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"LDY", &LDY, &ABS, 4},
        {"LDA", &LDA, &ABS, 4},
        {"LDX", &LDX, &ABS, 4},
        {"???", &XXX, &IMP, 4},
        {"BCS", &BCS, &REL, 2},
        {"LDA", &LDA, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 5},
        {"LDY", &LDY, &ZPX, 4},
        {"LDA", &LDA, &ZPX, 4},
        {"LDX", &LDX, &ZPY, 4},
        {"???", &XXX, &IMP, 4},
        {"CLV", &CLV, &IMP, 2},
        {"LDA", &LDA, &ABY, 4},
        {"TSX", &TSX, &IMP, 2},
        {"???", &XXX, &IMP, 4},
        {"LDY", &LDY, &ABX, 4},
        {"LDA", &LDA, &ABX, 4},
        {"LDX", &LDX, &ABY, 4},
        {"???", &XXX, &IMP, 4},
        {"CPY", &CPY, &IMM, 2},
        {"CMP", &CMP, &IZX, 6},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"CPY", &CPY, &ZP0, 3},
        {"CMP", &CMP, &ZP0, 3},
        {"DEC", &DEC, &ZP0, 5},
        {"???", &XXX, &IMP, 5},
        {"INY", &INY, &IMP, 2},
        {"CMP", &CMP, &IMM, 2},
        {"DEX", &DEX, &IMP, 2},
        {"???", &XXX, &IMP, 2},
        {"CPY", &CPY, &ABS, 4},
        {"CMP", &CMP, &ABS, 4},
        {"DEC", &DEC, &ABS, 6},
        {"???", &XXX, &IMP, 6},
        {"BNE", &BNE, &REL, 2},
        {"CMP", &CMP, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 4},
        {"CMP", &CMP, &ZPX, 4},
        {"DEC", &DEC, &ZPX, 6},
        {"???", &XXX, &IMP, 6},
        {"CLD", &CLD, &IMP, 2},
        {"CMP", &CMP, &ABY, 4},
        {"NOP", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 4},
        {"CMP", &CMP, &ABX, 4},
        {"DEC", &DEC, &ABX, 7},
        {"???", &XXX, &IMP, 7},
        {"CPX", &CPX, &IMM, 2},
        {"SBC", &SBC, &IZX, 6},
        {"???", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"CPX", &CPX, &ZP0, 3},
        {"SBC", &SBC, &ZP0, 3},
        {"INC", &INC, &ZP0, 5},
        {"???", &XXX, &IMP, 5},
        {"INX", &INX, &IMP, 2},
        {"SBC", &SBC, &IMM, 2},
        {"NOP", &NOP, &IMP, 2},
        {"???", &SBC, &IMP, 2},
        {"CPX", &CPX, &ABS, 4},
        {"SBC", &SBC, &ABS, 4},
        {"INC", &INC, &ABS, 6},
        {"???", &XXX, &IMP, 6},
        {"BEQ", &BEQ, &REL, 2},
        {"SBC", &SBC, &IZY, 5},
        {"???", &XXX, &IMP, 2},
        {"???", &XXX, &IMP, 8},
        {"???", &NOP, &IMP, 4},
        {"SBC", &SBC, &ZPX, 4},
        {"INC", &INC, &ZPX, 6},
        {"???", &XXX, &IMP, 6},
        {"SED", &SED, &IMP, 2},
        {"SBC", &SBC, &ABY, 4},
        {"NOP", &NOP, &IMP, 2},
        {"???", &XXX, &IMP, 7},
        {"???", &NOP, &IMP, 4},
        {"SBC", &SBC, &ABX, 4},
        {"INC", &INC, &ABX, 7},
        {"???", &XXX, &IMP, 7},
};