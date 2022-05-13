/*
 * @Author: dissor
 * @Date: 2022-05-13 10:01:40
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-13 18:39:51
 * @FilePath: \c-libnes\sources\rom.h
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#pragma once

#include <stdint.h>

/// | Header  | Trainer    | PRG-ROM | CHR-ROM |
/// | ------- | ---------- | ------- | ------- |
/// | 16bytes | 0/512bytes | 16nK    | 8nK     |
typedef struct{
    union
    {
        uint8_t IdStr[4];
        uint32_t Id;
    };

    uint8_t PRGSize;
    uint8_t CHRSize;

    union
    {
        uint8_t Flag6;
        struct{
            uint8_t MirrType:1;
            uint8_t BatMemory:1;
            uint8_t Trainer:1;
            uint8_t Screen4:1;
            uint8_t MapperLN:4;
        };
    };

    union
    {
        uint8_t Flag7;
        struct{
            uint8_t ConsoleType:2;
            uint8_t NES_20:2;
            uint8_t MapperHN:4;
        };
    };
    // todo!
    uint8_t reserved[8];
}NesHeader_t;
extern NesHeader_t head;

typedef struct
{
    struct
    {
        uint8_t *Data;
        uint32_t Size; // bytes
    }PRG,CHR;
    uint8_t MapperNum;
    uint8_t isVertical;
    uint8_t isScreen4;
    uint8_t isBattery;
}NesInfo_t;
extern NesInfo_t info;

int8_t NesLoad(uint8_t *path);
