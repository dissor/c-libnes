/*
 * @Author: dissor
 * @Date: 2022-05-13 10:01:34
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-17 22:22:28
 * @FilePath: \c-libnes\sources\rom.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */

#include "rom.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "nes.h"

NesHeader_t head;
NesInfo_t info;

void NesHeaderPrintln(void);

int8_t NesLoad(uint8_t *path)
{
    FILE *file = fopen(path, "rb");
    if (file == NULL)
    {
        printf("OPEN %s ERROR\r\n", path);
        return -1;
    }

    // get head
    // Header 16 bytes
    fread(&head, sizeof(head), 1, file);

    if (memcmp(head.IdStr, "NES\x1A", 4) != 0)
    {
        printf("ERROR: Invalid Id string");
        return -2;
    }

    if (head.Trainer)
    {
        fseek(file, 512, SEEK_CUR);
    }

    // get info
    info.PRG.Size = head.PRGSize * 1024 * 16;
    info.PRG.Data = (uint8_t *)malloc(info.PRG.Size);
    fread(info.PRG.Data, info.PRG.Size, 1, file);
    info.CHR.Size = head.CHRSize * 1024 * 8;
    info.CHR.Data = (uint8_t *)malloc(info.CHR.Size);
    fread(info.CHR.Data, info.CHR.Size, 1, file);

    info.MapperNum = (head.MapperHN << 4) | head.MapperLN;
    info.isVertical = head.MirrType;
    info.isScreen4 = head.Screen4;
    info.isBattery = head.BatMemory;

    NesHeaderPrintln();

    printf(
        "ROM: PRG-ROM: %d x 16kb   CHR-ROM %d x 8kb   Mapper: %03d\n",
        head.PRGSize, head.CHRSize, (head.MapperHN << 4) | (head.MapperLN));

    // reset cpu
    reset();

    printf(
        "ROM: NMI: $%04X  RESET: $%04X  IRQ/BRK: $%04X\n",
        ReadWord(0xFFFA), ReadWord(0xFFFC), ReadWord(0xFFFE));

    disassemble(ReadWord(0xFFFC), ReadWord(0xFFFC)+2);
}

void NesHeaderPrintln(void)
{
    printf("NES File Version: ");
    if (head.NES_20 == 0b10)
    {
        printf("NES 2.0\r\n");
    }
    else
    {
        printf("iNES\r\n");
    }

    printf("PRG-ROM size LSB: %d * 16 kb\r\n", head.PRGSize);
    printf("CHR-ROM size LSB: %d *  8 kb\r\n", head.CHRSize);
    printf("Mapper Number: %d\r\n", (head.MapperHN << 4) | (head.MapperLN));
    if (head.MirrType)
    {
        printf("mirroring type: Vertical\r\n");
    }
    else
    {
        printf("mirroring type: Horizontal or mapper-controlled\r\n");
    }

    if (head.BatMemory)
    {
        printf("Battery: present\r\n");
    }
    else
    {
        printf("Battery: Not present\r\n");
    }

    if (head.Trainer)
    {
        printf("512-byte Trainer: present\r\n");
    }
    else
    {
        printf("512-byte Trainer: Not present\r\n");
    }

    if (head.Screen4)
    {
        printf("Hard-wired four-screen mode: Yes\r\n");
    }
    else
    {
        printf("Hard-wired four-screen mode: No\r\n");
    }

    printf("Console type: ");
    switch (head.ConsoleType)
    {
    case 0:
        printf("Nintendo Entertainment System/Family Computer\r\n");
        break;

    case 1:
        printf("Nintendo Vs. System\r\n");
        break;

    case 2:
        printf("Nintendo Playchoice 10\r\n");
        break;

    case 3:
        printf("Extended Console Type\r\n");
        break;

    default:
        break;
    }
}