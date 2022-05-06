/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:22
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-06 22:11:00
 * @FilePath: \c-libnes\sources\cpu.h
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#pragma once

#include "bus.h"
typedef struct
{
    bus_type_t *bus;
    uint8_t (*read)(uint16_t address);
    void (*write)(uint16_t address, uint8_t data);
} cpu_type_t;
extern cpu_type_t CPU;

// CPU Core registers, exposed as public here for ease of access from external
// examinors. This is all the 6502 has.
typedef struct
{
    uint8_t A;   // Accumulator Register
    uint8_t X;   // X Register
    uint8_t Y;   // Y Register
    uint8_t SP;  // Stack Pointer (points to location on bus)
    uint16_t PC; // Program Counter
    struct       // Status Register
    {
        uint8_t C : 1; // Carry Bit
        uint8_t Z : 1; // Zero
        uint8_t I : 1; // Disable Interrupts
        uint8_t D : 1; // Decimal Mode (unused in this implementation)
        uint8_t B : 1; // Break
        uint8_t U : 1; // Unused
        uint8_t V : 1; // Overflow
        uint8_t N : 1; // Negative
    } P;
} reg_type_t;
extern reg_type_t REG;

// External event functions. In hardware these represent pins that are asserted
// to produce a change in state.
void reset(); // Reset Interrupt - Forces CPU into known state
void irq();   // Interrupt Request - Executes an instruction at a specific location
void nmi();   // Non-Maskable Interrupt Request - As above, but cannot be disabled
void clock(); // Perform one clock cycle's worth of update

// Indicates the current instruction has completed by returning true. This is
// a utility function to enable "step-by-step" execution, without manually
// clocking every cycle
bool complete();
