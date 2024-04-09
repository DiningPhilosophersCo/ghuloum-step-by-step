
// No change for this section
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define fixnum_mask       3
#define fixnum_tag        0
#define fixnum_shift      2
#define cons_tag          1
#define cons_mask         1
#define HEAP_SIZE 1024
#define Val uint64_t

Val scheme_entry();
extern void* alloc_ptr = NULL;


void pp(Val val)
{
    if ((val & fixnum_mask) == fixnum_tag)
    {
        printf("fixnum: %d\n", val >> fixnum_shift);
    }
    else if ((val & cons_mask) == cons_tag)
    {
        printf("got a cons\n");
     pp(*(int64_t*)(val - 1));
     pp(*(int64_t*)(val - 1 + (sizeof (int64_t*))));
    }
    else
    {
        printf("undefined\n");
    }
}

int main(int argc, char** argv){
  alloc_ptr = malloc(HEAP_SIZE);
  Val val = scheme_entry(alloc_ptr);
  pp(val);
  return 0;
}
