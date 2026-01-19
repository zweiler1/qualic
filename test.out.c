#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct MyStruct {
  int32_t x, y, z;
  char c;

} MyStruct;
void MyStruct_print(MyStruct *self) asm("MyStruct.print");

void MyStruct_print(MyStruct *self) {
  printf("(%i, %i, %i)", self->x, self->y, self->z);
}

void somefunction() { printf("Hello, World!\n"); }

int main(void) {
  MyStruct *sp = (MyStruct *)malloc(sizeof(MyStruct));

  if (true) {
    // Defer stuff should be inserted here
    free(sp);
    return 0;
  }

  if (true) {
    printf("hello there\n");
    // Defer should *not* insert anything here since the scope does not end
    // explicitely, but the above defer should insert the printf here.
    // So, the printf is inserted but the free is not.
    printf("This prints second!\n");
  }

  somefunction();
  MyStruct s = (MyStruct){.x = 10, .y = 20, .z = 30, .c = '\n'};
  MyStruct_print(&s);
  MyStruct_print(&s);
  printf("\n");

  // Defer stuff should be inserted here
  free(sp);
  return 0;
}
