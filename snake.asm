[org 0x7c00]
[bits 16]

; Define constants
%define SCRTT_RT 0x0e
%define GRPH_MODE_RT 0x0013

   mov [BOOT_DRIVE], dl      ; BIOS stores the boot drive in dl, storing for best practice.
   jmp init

snake:
   mov di, 50*320 + 100
   mov al, 4
   stosb

   jmp $                     ; Endless loop (code hanging)

init:
   mov dx, STACK_SEG         ; Base address for the stack segment
   mov bp, dx                ; Setting the base pointer the dx
   mov sp, bp                ; Setting the stack pointer to base pointer

   mov dx, EXTRA_SEG         ; Base address of the memory mapped video array
   mov es, dx                ; Setting es segment register to video memory

   mov dx, DATA_SEG          ; Base address for data segment (boot sector address)
   mov ds, dx                ; Setting data segment segment to dx

   mov ax, GRPH_MODE_RT      ; Set graphics mode for bios output
   int 0x10                  ; Calling BIOS interrupt

   jmp snake

; Global Variables
BOOT_DRIVE db 0
EXTRA_SEG  equ 0xA000
DATA_SEG   equ 0x7c00
STACK_SEG  equ 0x9000

times 510 - ($ - $$) db 0    ; Boot sector padding
dw 0xaa55                    ; magic value (recognises sector as boot sector from bios)
