/*
 * @Author: dissor
 * @Date: 2022-05-05 20:39:57
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-07 21:35:30
 * @FilePath: \c-libnes\sources\bus.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */

#include "bus.h"

static uint8_t ram[0x10000] = {0};

void WriteByte(uint16_t address, uint8_t data)
{
    if (address >= 0x0000 && address <= 0xFFFF)
    {
        ram[address] = data;
    }
}

void WriteWord(uint16_t address, uint16_t data)
{
    WriteByte(address, data & 0xFF);
    WriteByte(address + 1, (data >> 8) & 0xFF);
}

uint8_t ReadByte(uint16_t address)
{
    if (address >= 0x0000 && address <= 0xFFFF)
    {
        return ram[address];
    }

    return 0x00;
}

uint16_t ReadWord(uint16_t address)
{
    return (ReadByte(address + 1) << 8 | ReadByte(address)) & 0xFFFF;
}