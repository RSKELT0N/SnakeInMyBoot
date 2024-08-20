[org 0x7c00]
[bits 16]

; Define constants
%define EXTRA_SEG          0xA000
%define STACK_BASE_ADDR    0x9000
%define GRID_SIZE          64000
%define GRID_WIDTH         320
%define GRID_HEIGHT        200
%define SNAKE_START_POS    32150
%define SNAKE_ALLOCATION   50
%define NULL_SNAKE         65535
%define SNAKE_CELL_SIZE    15
%define BOUNDARY_BIAS      10

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
%define KB_SCAN_CODE_SPACE 0x39

    xor ax, ax                                       ; Base address for the stack segment (0)
    mov ss, ax                                       ; Setting the stack segment to ax
    mov ax, STACK_BASE_ADDR
    mov sp, ax                                       ; Setting the stack pointer to stack segment start addr
    mov ax, EXTRA_SEG
    mov es, ax                                       ; Setting extra segment to the video memory segment base address

    mov ax, GRPH_MODE_RT                             ; Set graphics mode for bios output
    int 0x10                                         ; Calling BIOS interrupt

_entry:
    call _start                                      ; Jump to entry point
    jmp _entry                                       ; Endless loop

_start:
    push bp
    mov bp, sp

    mov [SNAKE_BASE_ADDR], sp                        ; Move the current stack pointer towards global variable
    sub sp, SNAKE_ALLOCATION * 2                     ; Allocate the space on the stack for the snake coordinates
    call _clear_snake_data                           ; Memset the snake cells towards null value

    mov word [bp - 2], SNAKE_START_POS               ; Set the start cell of the snake to the starting position
    mov byte [SNAKE_SEGMENT_COUNT], 0x1              ; Initalise the current segment count
    mov byte [KB_SCAN_CODE], KB_SCAN_CODE_W          ; Initialise the keyboard scan code

    call _clear_screen
    call _draw_snake
    call _idle

_loop:
    call _clear_screen
    call _spawn_food
    call _draw_snake
    call _handle_collisions
    call _handle_key_press
    call _update_snake
    call _usleep

    mov al, [EXIT]
    test al, al
    jnz _end

    jmp _loop                                        ; Refresh frame

_end:
    mov byte [EXIT], 0x0
    mov sp, bp
    pop bp
    ret

_draw_snake:
    mov bx, [SNAKE_BASE_ADDR]
    mov cl, [SNAKE_SEGMENT_COUNT]
    mov al, COLOUR_GREEN
_draw_body:
    sub bx, 2
    mov di, word [ss:bx]

    push cx
    call _draw_cell
    pop cx

    loop _draw_body
_draw_snake_end:
    ret

_update_snake:
    xor ax, ax
    mov bx, [SNAKE_BASE_ADDR]
    mov al, [SNAKE_SEGMENT_COUNT]

    mov cx, ax
    dec cx
    shl ax, 1
    sub bx, ax

    cmp cx, 0x0
    je _update_cell
_recursive_update:
    mov dx, [ss:bx + 2]
    mov word [ss:bx], dx
    add bx, 2

    loop _recursive_update
_update_cell:
    mov dl, [KB_SCAN_CODE]
    mov ax, [ss:bx]

    cmp dl, KB_SCAN_CODE_W
    je _move_up
    cmp dl, KB_SCAN_CODE_S
    je _move_down
    cmp dl, KB_SCAN_CODE_A
    je _move_left
    cmp dl, KB_SCAN_CODE_D
    je _move_right
_update_snake_end:
    mov word [ss:bx], ax
    ret

_move_up:
    sub ax, GRID_WIDTH * (SNAKE_CELL_SIZE)
    jmp _update_snake_end
_move_down:
    add ax, GRID_WIDTH * (SNAKE_CELL_SIZE)
    jmp _update_snake_end
_move_left:
    sub ax, SNAKE_CELL_SIZE
    jmp _update_snake_end
_move_right:
    add ax, SNAKE_CELL_SIZE
    jmp _update_snake_end

_handle_key_press:
    mov dx, KB_SCAN_PORT
    in al, dx

    cmp al, KB_SCAN_CODE_W
    je _key_press
    cmp al, KB_SCAN_CODE_S
    je _key_press
    cmp al, KB_SCAN_CODE_A
    je _key_press
    cmp al, KB_SCAN_CODE_D
    je _key_press
    ret
_key_press:
    mov [KB_SCAN_CODE], al
    ret

_spawn_food:
    mov al, [FOOD_PRESENT]
    test al, al
    jnz _spawn_food_end

    xor cx, cx
    mov bx, [SNAKE_BASE_ADDR]
    mov cl, [SNAKE_SEGMENT_COUNT]
    shl cx, 0x1
    sub bx, cx
    mov di, [ss:bx]

    mov word [FOOD_LOCATION], di
    mov byte [FOOD_PRESENT], 0x1
_spawn_food_end:
    mov di, [FOOD_LOCATION]
    mov al, COLOUR_RED
    call _draw_cell
    ret

_handle_collisions:
    xor cx, cx
    mov bx, [SNAKE_BASE_ADDR]
    mov ax, [ss:bx - 2]
    mov bx, [FOOD_LOCATION]

    ; Compare snake head against known food location
    cmp ax, bx
    je _add_snake_cell
_wall_check:
    push ax
    xor dx, dx
    mov bx, GRID_WIDTH
    div bx
    cmp dx, BOUNDARY_BIAS
    pop ax
    jb _handle_exit

    cmp ax, 0x0 + BOUNDARY_BIAS
    jb _handle_exit
    cmp ax, GRID_SIZE - BOUNDARY_BIAS
    ja _handle_exit
    ret
_handle_exit:
    mov byte [EXIT], 0x1
    ret

_add_snake_cell:
    mov byte [FOOD_PRESENT], 0x0

    mov si, [SNAKE_BASE_ADDR]
    mov cl, [SNAKE_SEGMENT_COUNT]
    inc cl
    push cx

    shl cx, 1
    sub si, cx
    mov ax, [ss:si + 2]
    sub si, SNAKE_CELL_SIZE + 1
    mov word [ss:si], ax

    pop cx
    mov byte [SNAKE_SEGMENT_COUNT], cl
    jmp _wall_check

_clear_snake_data:
    mov bx, [SNAKE_BASE_ADDR]
    mov cx, SNAKE_ALLOCATION
    shr cx, 1
_clear_snake_data_loop:
    mov word [ss:bx - 2], NULL_SNAKE
    sub bx, 2
    loop _clear_snake_data_loop
    ret

_draw_cell:
    mov cx, SNAKE_CELL_SIZE
    mov dx, SNAKE_CELL_SIZE
_draw_square:
    push cx                                         ; Save row counter
    mov cx, dx                                      ; Reset column counter for each row

_draw_row:
    stosb                                           ; Store byte at ES:DI and increment DI
    loop _draw_row                                  ; Repeat for 20 columns

    add di, GRID_WIDTH - SNAKE_CELL_SIZE            ; Move to the next line (320 - 20)
    pop cx                                          ; Restore row counter
    loop _draw_square                               ; Repeat for 20 rows
    ret

_clear_screen:
    xor di, di
    mov cx, GRID_SIZE
    mov al, COLOUR_BLACK
_clear_screen_loop:
    stosb                                           ; Store byte at ES:DI and increment DI
    loop _clear_screen_loop
    ret

_idle:
    mov dx, 0x60                                    ; Set DX to the keyboard data port (0x60)
    in al, dx                                       ; Read a byte from the port 0x60 into AL
    cmp al, KB_SCAN_CODE_SPACE                      ; Test if AL is zero
    jne _idle                                       ; If AL is zero, keep idling (no key pressed)
    ret                                             ; Otherwise, return (a key was pressed)

_usleep:
    mov ah, 0x86                                    ; BIOS function to sleep
    mov dx, ax                                      ; Move AX to DX (parameter for BIOS interrupt)
    int 0x15                                        ; Call BIOS interrupt
    int 0x15                                        ; Call BIOS interrupt
    int 0x15                                        ; Call BIOS interrupt
    ret

; Global Variables
EXIT                db 0                            ; 8 bit value of the programs conditional to keep looping
SNAKE_SEGMENT_COUNT db 0                            ; 8 bit value of snake cell length
KB_SCAN_CODE        db 0                            ; 8 bit value of keyboard scan code
FOOD_PRESENT        db 0                            ; 8 bit value of the food is present

SNAKE_BASE_ADDR     dw 0                            ; 16 bit value of base address for the snake cell coordinates
FOOD_LOCATION       dw 0                            ; 16 bit value of the food position

times 510 - ($ - $$) db 0                           ; Boot sector padding
dw 0xaa55                                           ; magic value (recognises sector as boot sector from bios)
