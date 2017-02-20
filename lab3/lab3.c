#include <stdio.h>

void main () {
	int x =3;
	int z =x;
	int y =4;

	printf("\nx: %d\n", x);
	printf("y: %d\n", y);
   	printf("z: %d\n", z);

	while(y>=2) {
		z*=x;
		y-=1;
		printf("\nx: %d\n", x);
		printf("y: %d\n", y);
   		printf("z: %d\n", z);
	}
	printf("\nx: %d\n", x);
	printf("y: %d\n", y);
   	printf("z: %d\n", z);
}
