/*
 * @Author: dissor
 * @Date: 2022-05-05 20:39:57
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-05 20:59:31
 * @FilePath: \c-libnes\bus.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */

#include "bus.h"

void bus_init(void);
void bus_deinit(void);
void bus_write(uint16_t address, uint8_t data);
uint8_t bus_read(uint16_t address);

bus_type_t bus = {
    .init = bus_init,
    .deinit = bus_deinit,
    .write = bus_write,
    .read = bus_read,
};

void bus_init(void)
{
    memset(bus.ram, 0, sizeof(bus.ram));
}

void bus_deinit(void)
{
}

void bus_write(uint16_t address, uint8_t data)
{
    if (address >= 0x0000 && address <= 0xFFFF)
    {
        bus.ram[address] = data;
    }
}

uint8_t bus_read(uint16_t address)
{
    if (address >= 0x0000 && address <= 0xFFFF)
    {
        return bus.ram[address];
    }

    return 0x00;
}