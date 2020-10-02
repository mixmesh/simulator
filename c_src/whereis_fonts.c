#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>

int main ( int argc, char *argv[] ) {
  TTF_Font *font;
  char buff[100];
  getcwd(buff,sizeof(buff));
  printf("Here=%s\n",buff);
  if ( TTF_Init() == -1 ) {
    printf("TTF_Init: %s\n", TTF_GetError());
    exit(1);
  }
  font=TTF_OpenFont(argv[1], 16);
  if(!font) {
    printf("TTF_OpenFont: %s\n", TTF_GetError());
    exit(1);
  }
  return 0;
}
