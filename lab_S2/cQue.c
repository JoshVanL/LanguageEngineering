#include <stdio.h>

int main(void) {
  int x,y=4;
  printf("%d\n",x);
  printf("%d\n",y);
  //int y =4;
  while(x-- > 1) y*=x;
  printf("x:%d\n",x);
  printf("y:%d\n",y);
}
