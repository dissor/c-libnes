/*
 * @Author: dissor
 * @Date: 2022-05-05 20:39:57
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-15 23:11:46
 * @FilePath: \c-libnes\sources\bus.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */

#include "bus.h"
#include "rom.h"
#include <stdio.h>

/*
CPU 地址空间
+---------+-------+-----------------------+
| 地址    | 大小  |         描述           |
+---------+-------+-----------------------+
| $0000   | $800  | RAM                   |
| $0800   | $800  | RAM 镜像1             |
| $1000   | $800  | RAM 镜像2             |
| $1800   | $800  | RAM 镜像3             |
| $2000   | 8     | PPU Registers         |
| $2008   | $1FF8 | PPU Registers 镜像1   |
| $4000   | $20   | APU Registers         |
| $4020   | $1FDF | Expansion ROM         |
| $6000   | $2000 | SRAM                  |
| $8000   | $4000 | PRG-ROM               |
| $C000   | $4000 | PRG-ROM               |
+---------+-------+-----------------------+
*/
static uint8_t ram[0x800] = {0}; // RAM [$0000, $2000)
static struct
{
    uint8_t ppu[0x08]; // [$2000, $4000)
    uint8_t apu[0x20]; // [$4000, $4020)
} reg = {0};           // register [$2000, $4020)

static uint8_t sram[0x2000] = {0}; // SRAM [$6000, $8000)
#define PRGRom info.PRG.Data       // PRG-ROM [$8000, $FFFF)

void WriteByte(uint16_t address, uint8_t data)
{
    if (address >= 0x0000 && address < 0x2000)
    {
        ram[address & 0x07FF] = data;
    }
    else if (address >= 0x2000 && address < 0x4000)
    {
        reg.ppu[address % 0x08] = data;
    }
    else if (address >= 0x4000 && address < 0x4020)
    {
        reg.apu[address % 0x20] = data;
    }
    else if (address >= 0x4020 && address < 0x6000)
    {
        // Expansion ROM
    }
    else if (address >= 0x6000 && address < 0x8000)
    {
        sram[address - 0x6000] = data;
    }
    else if (address >= 0x8000 && address < 0x10000)
    {
        PRGRom[address - 0x8000] = data;
    }
    else
    {
        printf("read invalid address!!!\n");
    }
}

void WriteWord(uint16_t address, uint16_t data)
{
    WriteByte(address, data & 0xFF);
    WriteByte(address + 1, (data >> 8) & 0xFF);
}

uint8_t ReadByte(uint16_t address)
{
    if (address >= 0x0000 && address < 0x2000)
    {
        return ram[address & 0x07FF];
    }
    else if (address >= 0x2000 && address < 0x4000)
    {
        return reg.ppu[address % 0x08];
    }
    else if (address >= 0x4000 && address < 0x4020)
    {
        return reg.apu[address % 0x20];
    }
    else if (address >= 0x4020 && address < 0x6000)
    {
        // Expansion ROM
        return 0x00;
    }
    else if (address >= 0x6000 && address < 0x8000)
    {
        return sram[address - 0x6000];
    }
    else if (address >= 0x8000 && address < 0xC000)
    {
        return PRGRom[address - 0x8000];
    }
    else if (address >= 0xC000 && address < 0x10000)
    {
        // 如果游戏容量只有 16K，那么 PRG 位于 0xC000 - 0xFFFF，同时 0x8000 - 0xBFFF 为 0xC000 - 0xFFFF 的镜像
        // 如果容量为 32K，那么 RPG 则会塞满 0x8000 - 0xFFFF
        if (head.PRGSize == 1)
        {
            return PRGRom[address - 0xC000];
        }
        return PRGRom[address - 0x8000];
    }

    printf("write invalid address!!!\n");
    return 0x00;
}

uint16_t ReadWord(uint16_t address)
{
    return (ReadByte(address + 1) << 8 | ReadByte(address)) & 0xFFFF;
}