[org 0x7c00]
[bits 16]

; Define constants
%define EXTRA_SEG          0xA000
%define STACK_BASE_ADDR    0x9000
%define GRID_SIZE          64000
%define GRID_WIDTH         320
%define GRID_HEIGHT        200
%define START_POS          32160
%define SNAKE_ALLOCATION   50
%define NULL_SNAKE         65535
%define SNAKE_CELL_SIZE    15

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

%define KB_SCAN_PORT       0x60
%define KB_SCAN_CODE_W     0x11
%define KB_SCAN_CODE_A     0x1E
%define KB_SCAN_CODE_S     0x1F
%define KB_SCAN_CODE_D     0x20

init:
    xor ax, ax                                       ; Base address for the stack segment (0)
    mov ss, ax                                       ; Setting the stack segment to ax
    mov ax, STACK_BASE_ADDR
    mov sp, ax                                       ; Setting the stack pointer to stack segment start addr
    mov ax, EXTRA_SEG
    mov es, ax                                       ; Setting extra segment to the video memory segment base address

    mov ax, GRPH_MODE_RT                             ; Set graphics mode for bios output
    int 0x10                                         ; Calling BIOS interrupt

    call start                                       ; Jump to entry point
    jmp $                                            ; Endless loop

start:
    push bp
    mov bp, sp

    mov [SNAKE_BASE_ADDR], sp                        ; Move the current stack pointer towards global variable
    sub sp, SNAKE_ALLOCATION * 2                     ; Allocate the space on the stack for the snake coordinates
    call clear_snake_data                            ; Memset the snake cells towards null value
    mov word [bp - 2], START_POS                     ; Set the start cell of the snake to the starting position
    mov word [bp - 4], START_POS - SNAKE_CELL_SIZE - 1
    mov byte [SNAKE_SEGMENT_COUNT], 0x2
    mov byte [KB_SCAN_CODE], KB_SCAN_CODE_W          ; Initialise the keyboard scan code

snake:
    call handle_key_press
    call clear_screen
    call draw_snake
    call update_snake
    call spawn_food
    call handle_oub
    call usleep

    jmp snake                                        ; Refresh frame

snake_end:
    mov sp, bp
    pop bp
    ret

draw_snake:
    mov bx, [SNAKE_BASE_ADDR]
    mov cl, [SNAKE_SEGMENT_COUNT]
    mov al, COLOUR_GREEN
draw_body:
    mov di, word [ss:bx - 2]
    push cx
    call draw_cell
    pop cx
    sub bx, 2
    loop draw_body
draw_snake_end:
    ret

update_snake:
    xor ax, ax
    mov bx, [SNAKE_BASE_ADDR]
    mov al, [SNAKE_SEGMENT_COUNT]
    mov cx, ax
    dec cx
    shl ax, 1
    sub bx, ax
recursive_update:
    mov dx, [ss:bx + 2]
    mov word [ss:bx], dx
    add bx, 2
    loop recursive_update

    mov dl, [KB_SCAN_CODE]
    mov ax, [ss:bx]

    cmp dl, KB_SCAN_CODE_W
    je move_up
    cmp dl, KB_SCAN_CODE_S
    je move_down
    cmp dl, KB_SCAN_CODE_A
    je move_left
    cmp dl, KB_SCAN_CODE_D
    je move_right
    add ax, SNAKE_CELL_SIZE + 1
update_snake_end:
    mov word [ss:bx], ax
    ret

move_up:
    sub ax, GRID_WIDTH * SNAKE_CELL_SIZE + GRID_WIDTH
    jmp update_snake_end
move_down:
    add ax, GRID_WIDTH * SNAKE_CELL_SIZE + GRID_WIDTH
    jmp update_snake_end
move_left:
    sub ax, SNAKE_CELL_SIZE + 1
    jmp update_snake_end
 move_right:
    add ax, SNAKE_CELL_SIZE + 1
    jmp update_snake_end

handle_key_press:
    mov dx, KB_SCAN_PORT
    in al, dx

    cmp al, KB_SCAN_CODE_W
    je key_press
    cmp al, KB_SCAN_CODE_S
    je key_press
    cmp al, KB_SCAN_CODE_A
    je key_press
    cmp al, KB_SCAN_CODE_D
    je key_press
    ret
key_press:
    mov [KB_SCAN_CODE], al
    ret

spawn_food:
    ;; xor ax, ax

    ;; mov bx, [SNAKE_BASE_ADDR]
    ;; mov al, [SNAKE_SEGMENT_COUNT]
    ;; shl ax, 1
    ;; sub bx, ax
    ;; mov di, [ss:bx]
    ret

handle_oub:
    ret

clear_snake_data:
    mov bx, [SNAKE_BASE_ADDR]
    mov cx, SNAKE_ALLOCATION
    shr cx, 1
clear_snake_data_loop:
    mov word [ss:bx - 2], NULL_SNAKE
    sub bx, 2
    loop clear_snake_data_loop
    ret

draw_cell:
    mov cx, SNAKE_CELL_SIZE
    mov dx, SNAKE_CELL_SIZE
draw_square:
    push cx                                         ; Save row counter
    mov cx, dx                                      ; Reset column counter for each row

draw_row:
    stosb                                           ; Store byte at ES:DI and increment DI
    loop draw_row                                   ; Repeat for 20 columns

    add di, GRID_WIDTH - SNAKE_CELL_SIZE            ; Move to the next line (320 - 20)
    pop cx                                          ; Restore row counter
    loop draw_square                                ; Repeat for 20 rows
    ret

clear_screen:
    xor di, di
    mov cx, GRID_SIZE
    mov al, COLOUR_BLACK
clear_screen_loop:
    stosb                                           ; Store byte at ES:DI and increment DI
    loop clear_screen_loop
    ret

usleep:
    mov ah, 0x86                                    ; BIOS function to sleep
    mov dx, ax                                      ; Move AX to DX (parameter for BIOS interrupt)
    int 0x15                                        ; Call BIOS interrupt
    int 0x15
    ret

; Global Variables
SNAKE_BASE_ADDR     dw 0                            ; 16 bit value of base address for the snake cell coordinates
SNAKE_SEGMENT_COUNT db 0                            ; 8 bit value of snake cell length
KB_SCAN_CODE        db 0                            ; 8 bit value of keyboard scan code
FOOD_PRESENT        db 0                            ; 8 bit value of the food is present
FOOD_LOCATION       dw 0                            ; 16 bit value of the food position

times 510 - ($ - $$) db 0                           ; Boot sector padding
dw 0xaa55                                           ; magic value (recognises sector as boot sector from bios)
