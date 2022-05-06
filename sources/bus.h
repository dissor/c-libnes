/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:09
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-06 10:46:52
 * @FilePath: \c-libnes\sources\bus.h
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */

#pragma once

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdbool.h>

typedef struct
{
    uint8_t ram[64 * 1024];
    void (*init)(void);
    void (*deinit)(void);
    void (*write)(uint16_t address, uint8_t data);
    uint8_t (*read)(uint16_t address);
} bus_type_t;
extern bus_type_t bus;
