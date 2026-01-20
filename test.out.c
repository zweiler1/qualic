#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct MyStruct {
  int32_t x, y, z;
  char *string;

} MyStruct;
void MyStruct_print(MyStruct *self) asm("MyStruct.print");
void MyStruct_deinit(MyStruct *self) asm("MyStruct.deinit");

void MyStruct_print(MyStruct *self) {
  printf("(%i, %i, %i), %s\n", self->x, self->y, self->z, self->string);
}

void MyStruct_deinit(MyStruct *self) { free(self->string); }

void someFunction() { printf("Hello, World!\n"); }

int main(void) {
  if (false) {
    // This return should not be re-written
    return 0;
  }
  MyStruct *sp = (MyStruct *)malloc(sizeof(MyStruct));

  if (false) {
    // Defer stuff should be inserted here
    int tempvar_wdK5Yxb6 = 0;
    printf("should run second\n");
    free(sp);
    return tempvar_wdK5Yxb6;
  }

  if (true) {
    printf("hello there\n");
    // Defer should *not* insert anything here since the scope does not end
    // explicitely, but the above defer should insert the printf here.
    // So, the printf is inserted but the free is not.
    printf("This prints second!\n");
  }

  someFunction();
  MyStruct s = (MyStruct){
      .x = 0b10,
      .y = 0xFF,
      .z = 0o30,
      .string = (char *)malloc(23),
  };
  memcpy(s.string, "Hello there it's steve", 23);
  MyStruct_print(&s);
  MyStruct_print(&s);

  // Defer stuff should be inserted here
  int tempvar_dLGVVt5v = 0;
  MyStruct_deinit(&s);
  printf("should run first\n");
  printf("should run second\n");
  free(sp);
  return tempvar_dLGVVt5v;
}
