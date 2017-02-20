#include <stdio.h>



#include <limits.h>

int main(void) {
  int x =4;
  int y = x;

  printf("x:%d\n", x);
  printf("y:%d\n\n", y);


  while (x-- > 1) {
    y*=x;
    printf("x:%d\n", x);
    printf("y:%d\n\n", y);
  }
  printf("x:%d\n", x);
  printf("y:%d\n", y);
}
