.global _scheme_entry
_scheme_entry:
movz x0, #0
cmp x0, #0
b.eq label138
movz x0, #12
b label139
label138:
movz x0, #16
label139:
ret
