/*
 * @Author: dissor
 * @Date: 2022-05-05 20:40:22
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-17 22:19:24
 * @FilePath: \c-libnes\sources\cpu.h
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#pragma once

#include "bus.h"
void reset(void);
uint16_t disassemble(uint16_t nStart, uint16_t nStop);