/*
 * @Author: dissor
 * @Date: 2022-05-05 20:39:43
 * @LastEditors: dissor
 * @LastEditTime: 2022-05-08 22:31:55
 * @FilePath: \c-libnes\examples\main.c
 * @Description:
 * guojianwenjonas@foxmail.com
 * Copyright (c) 2022 by dissor, All Rights Reserved.
 */
#include "main.h"
#include "nes.h"
#include "SDL.h"

const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

int WinMain(int argc, char *argv[])
{
    //将要渲染的窗口
    SDL_Window *window = NULL;

    //窗口含有的surface
    SDL_Surface *screenSurface = NULL;

    //初始化SDL
    if (SDL_Init(SDL_INIT_VIDEO) < 0)
    {
        printf("SDL could not initialize! SDL_Error: %s\n", SDL_GetError());
    }
    else
    {
        //创建 window
        window = SDL_CreateWindow("SDL Tutorial", SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
        if (window == NULL)
        {
            printf("Window could not be created! SDL_Error: %s\n", SDL_GetError());
        }
        else
        {
            //获取 window surface
            screenSurface = SDL_GetWindowSurface(window);

            //用白色填充surface
            SDL_FillRect(screenSurface, NULL, SDL_MapRGB(screenSurface->format, 0xFF, 0xFF, 0xFF));

            //更新surface
            SDL_UpdateWindowSurface(window);

            //延迟两秒
            SDL_Delay(2000);
        }
    }
    //销毁 window
    SDL_DestroyWindow(window);

    //退出 SDL subsystems
    SDL_Quit();

    return 0;
}