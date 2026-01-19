#include <stdint.h>
#include <stdio.h>

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
  somefunction();
  MyStruct s = (MyStruct){.x = 10, .y = 20, .z = 30, .c = '\n'};
  MyStruct_print(&s);
  MyStruct_print(&s);
  printf("\n");
  return 0;
}
