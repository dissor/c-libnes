/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:22
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-18 19:01:16
 * @FilePath: \c-libnes\sources\cpu.h
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#pragma once

#include <stdbool.h>
#include "bus.h"

void reset(void);
void clock(void);
bool complete(void);
uint16_t disassemble(uint16_t nStart, uint16_t nStop);