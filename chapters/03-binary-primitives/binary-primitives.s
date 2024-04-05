.global _scheme_entry
_scheme_entry:
movz x0, #8
str x0, [sp, #0]
movz x0, #4
ldr x1, [sp, #0]
add x0, x0, x1
ret
