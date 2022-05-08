/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:09
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-07 21:35:07
 * @FilePath: \c-libnes\sources\bus.h
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */

#pragma once

#include <stdint.h>

void WriteByte(uint16_t address, uint8_t data);
void WriteWord(uint16_t address, uint16_t data);
uint8_t ReadByte(uint16_t address);
uint16_t ReadWord(uint16_t address);