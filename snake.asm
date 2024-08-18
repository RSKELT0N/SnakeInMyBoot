[org 0x7c00]
[bits 16]

; Define constants
%define EXTRA_SEG          0xA000
%define DATA_SEG           0x7c0
%define STACK_BASE_ADDR    0x9000
%define GRID_SIZE          64000
%define START_POS          32160
%define SNAKE_ALLOCATION   100
%define NULL_SNAKE         65535
%define SNAKE_CELL_SIZE    20

%define SCRTT_RT           0x0e
%define GRPH_MODE_RT       0x0013

%define COLOUR_BLACK       0x0
%define COLOUR_LIGHT_GREEN 0x1
%define COLOUR_GREEN       0x2
%define COLOUR_BLUE        0x3
%define COLOUR_RED         0x4
%define COLOUR_PURPLE      0x5
%define COLOUR_ORANGE      0x6
%define COLOUR_WHITE       0x7

init:
   xor ax, ax               ; Base address for the stack segment (0)
   mov ss, ax               ; Setting the stack segment to ax
   mov ax, STACK_BASE_ADDR
   mov sp, ax               ; Setting the stack pointer to stack segment start addr
   mov ax, EXTRA_SEG
   mov es, ax               ; Setting extra segment to the video memory segment base address
   mov ax, DATA_SEG
   mov ds, ax               ; Setting data segment segment to boot sector address

   mov ax, GRPH_MODE_RT     ; Set graphics mode for bios output
   int 0x10                 ; Calling BIOS interrupt

   call start
   jmp $                    ; Endless loop (code hanging)

start:
   push bp
   mov bp, sp

   mov [SNAKE_BASE_ADDR], bp
   sub sp, SNAKE_ALLOCATION
   call clear_snake_data
   mov word [bp - 2], START_POS
   mov word [bp - 4], START_POS - 21
   mov word [bp - 6], START_POS - 42
   mov word [bp - 8], START_POS - 42 + 6720
snake:
   call clear_screen
   call draw_snake
   call update_cells
   call usleep

   jmp snake                  ; Endless loop (code hanging)

draw_snake:
   mov bx, [SNAKE_BASE_ADDR]
   mov al, COLOUR_GREEN
draw_snake_cell:
   mov di, word [ss:bx - 2]
   cmp di, NULL_SNAKE
   je draw_snake_end
   call draw_cell
   sub bx, 2
   jmp draw_snake_cell
draw_snake_end:
   ret

draw_cell:
   push bp
   mov bp, sp
   sub sp, 2
   mov byte [ss:bp - 1], SNAKE_CELL_SIZE
   jmp draw_cell_square
update_pos:
   add di, 300
draw_cell_square:
   mov dl, [ss:bp - 1]
   cmp dl, 0
   je draw_cell_end
   dec dl
   mov byte [ss:bp - 1], dl
   mov byte [ss:bp - 2], SNAKE_CELL_SIZE
draw_cell_line:
   mov cl, [ss:bp - 2]
   cmp cl, 0
   je update_pos
   stosb
   dec cl
   mov byte [ss:bp - 2], cl
   jmp draw_cell_line
draw_cell_end:
   mov sp, bp
   pop bp
   ret

update_cells:
   mov bx, [SNAKE_BASE_ADDR]
   mov si, [SNAKE_BASE_ADDR]
recursive_update:
   sub si, 2
   mov ax, [ss:bx - 2]
   mov cx, [ss:si - 2]
   cmp cx, NULL_SNAKE
   je recursive_update_end
   mov word [ss:si - 2], ax
   sub bx, 2
   jmp recursive_update
recursive_update_end:
   mov ax, [bp - 2]
   add ax, 21
   mov word [bp - 2], ax
   ret

clear_snake_data:
   mov bx, [SNAKE_BASE_ADDR]
clear_snake_data_loop:
   mov word [ss:bx - 2], NULL_SNAKE
   sub bx, 2
   mov ax, [SNAKE_BASE_ADDR]
   sub ax, bx
   cmp ax, SNAKE_ALLOCATION
   jl clear_snake_data_loop

   ret

clear_screen:
   xor di, di
   mov al, COLOUR_BLACK
clear_screen_loop:
   stosb
   cmp di, GRID_SIZE
   jne clear_screen_loop
   ret

usleep:
    mov ax, bp
    mov ah, 0x86       ; BIOS function to sleep
    mov dx, ax         ; Move AX to DX (parameter for BIOS interrupt)
    int 0x15           ; Call BIOS interrupt
    ret

; Global Variables
SNAKE_BASE_ADDR  dw 0

times 510 - ($ - $$) db 0    ; Boot sector padding
dw 0xaa55                    ; magic value (recognises sector as boot sector from bios)
