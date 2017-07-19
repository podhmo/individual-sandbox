#include <stdio.h>
// cc -c fault.c -o fault.o
// cc -shared -o libfault.dylib fault.o

void disp(int n) {
  int xs[10];
  for (int i=0; i<n; i++){
    xs[i] = i;
  }
  for (int i=0; i<n; i++){
    printf("%d\n", xs[i]);
  }
}
