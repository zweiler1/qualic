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
  printf("(%i, %i, %i)\n", self->x, self->y, self->z);
}

void someFunction() { printf("Hello, World!\n"); }

int main(void) {
  MyStruct *sp = (MyStruct *)malloc(sizeof(MyStruct));

  if (false) {
    // Defer stuff should be inserted here
    printf("should run second\n");
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

  someFunction();
  MyStruct s = (MyStruct){.x = 0b10, .y = 0xFF, .z = 0o30, .c = '\n'};
  MyStruct_print(&s);
  MyStruct_print(&s);

  // Defer stuff should be inserted here
  printf("should run first\n");
  printf("should run second\n");
  free(sp);
  return 0;
}

