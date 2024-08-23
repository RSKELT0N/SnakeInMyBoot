[org 0x7c00]
[bits 16]

; Define constants
%define EXTRA_SEG          0xA000
%define STACK_BASE_ADDR    0x9000
%define GRID_SIZE          64000
%define GRID_WIDTH         320
%define GRID_HEIGHT        200
%define SNAKE_START_POS    30545
%define FOOD_START_POS     14545
%define SNAKE_ALLOCATION   50
%define NULL_SNAKE         65535
%define SNAKE_CELL_SIZE    10
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

    xor ax, ax                                                        ; Base address for the stack segment (0)
    mov ss, ax                                                        ; Setting the stack segment to ax
    mov ax, STACK_BASE_ADDR
    mov sp, ax                                                        ; Setting the stack pointer to stack segment start addr
    mov ax, EXTRA_SEG
    mov es, ax                                                        ; Setting extra segment to the video memory segment base address

    mov ax, GRPH_MODE_RT                                              ; Set graphics mode for bios output
    int 0x10                                                          ; Calling BIOS interrupt

_entry:
    call _start                                                       ; Jump to entry point
    jmp _entry                                                        ; Endless loop

_start:
    push bp                                                           ; subroutine preamble
    mov bp, sp

    mov word [SNAKE_BASE_ADDR], sp                                    ; Move the current stack pointer towards global variable
    sub sp, SNAKE_ALLOCATION * 2                                      ; Allocate the space on the stack for the snake coordinates
    call _clear_snake_data                                            ; Memset the snake cells towards null value

    mov byte [EXIT], 0x0                                              ; Initalise the exit status of the game to be zero
    mov byte [KB_SCAN_CODE], 0x0                                      ; Intialise the scan code of the keyboard to be zero upon startup
    mov byte [FOOD_PRESENT], 0x1                                      ; Initalise the food present byte to 1 (not collected)
    mov word [FOOD_LOCATION], FOOD_START_POS                          ; Set the food to the starting position of program
    mov word [bp - 2], SNAKE_START_POS                                ; Set the start cell of the snake to the starting position
    mov byte [SNAKE_SEGMENT_COUNT], 0x1                               ; Initalise the current segment count

; Startup screen
    call _clear_screen                                                ; Clear the screen within the GRID_SIZE range to be completely black
    call _draw_snake                                                  ; Draw the current snake on the screen with its default starting position
    call _draw_food                                                   ; Draw the current food on the screen with its default starting positon
    call _idle                                                        ; Idle until an appropiate key has been pressed (WASD)

_loop:
    call _clear_screen                                                ; Clear the screen within the GRID_SIZE range to be completely black
    call _draw_food                                                   ; Draw the current snake on the screen with its default starting position
    call _draw_snake                                                  ; Draw the current food on the screen with its default starting positon
    call _handle_collisions                                           ; Check the bounds of the snake against itself, food and walls to trigger an event
    call _handle_key_press                                            ; Check the keyboard port mapping for any input key to update the snake's direction
    call _update_snake                                                ; On each interation, update each segment of the snake to be n - 1 = n + 1. Including update the new position of the snake's head
    call _usleep                                                      ; Sleep the program - call the wait interrupt within the bios

    mov al, [EXIT]                                                    ; Load the current value of global variable exit into lower half of ax
    test al, al                                                       ; AND operation to ensure if exit status has been set to 1
    jnz _end                                                          ; Jump to exit game frame based on lower half of ax

    jmp _loop                                                         ; Refresh frame

_end:
    mov sp, bp                                                        ; subroutine epilogue
    pop bp
    ret

_draw_snake:                                                          ; Draw the body of snake
    mov bx, [SNAKE_BASE_ADDR]                                         ; Load the base address of the snake's memory within the stack
    mov cl, [SNAKE_SEGMENT_COUNT]                                     ; Load the current length of the snake
    mov al, COLOUR_GREEN                                              ; Set the lower half of ax to the colour green
_draw_body:
    sub bx, 2                                                         ; Set the bx register to point to the first snake segment
    mov di, word [ss:bx]                                              ; Retrieve the coordinates of the snake segment
    sub di, GRID_WIDTH * (SNAKE_CELL_SIZE / 2) - (SNAKE_CELL_SIZE / 2)
    push cx                                                           ; Store the snake length on the stack
    call _draw_cell                                                   ; Draw the current segment at its appropiate coordinates
    pop cx                                                            ; Load the snake length back into cx

    loop _draw_body                                                   ; Repeat until all snake segments are drawn onto screen
_draw_snake_end:
    ret

_update_snake:
    xor ax, ax                                                        ; Clear the ax register (set to zero)
    mov bx, [SNAKE_BASE_ADDR]                                         ; Load the base address of the snake's memory within the stack
    mov al, [SNAKE_SEGMENT_COUNT]                                     ; Load the current length of the snake

    mov cx, ax                                                        ; Update the bx register to point to the address of last segment of the snake
    dec cx
    shl ax, 1
    sub bx, ax

    cmp cx, 0x0                                                       ; Edge case to jump directly to update the snake's head, if it has a current length of one
    je _update_cell
_recursive_update:
    mov dx, [ss:bx + 2]                                               ; Load the current coordinates of the snake segment + 1
    mov word [ss:bx], dx                                              ; Store the coordinates to the current segment
    add bx, 2                                                         ; Update bx address to next segment

    loop _recursive_update                                            ; Repeat until cx is zero
_update_cell:                                                         ; Retrieve the current keyboard scan code and update the snake's head position
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
    mov word [ss:bx], ax                                              ; Store the updated head location at the start of the snake's segment memory
    ret

_move_up:                                                             ; Based on the current key press, update the head accordingly
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

_handle_key_press:                                                    ; Read the current keyboard scan code
    mov dx, KB_SCAN_PORT                                              ; Move 0x60 into dx
    in al, dx                                                         ; Read the dx port and store the current scan code into the lower half of ax

    cmp al, KB_SCAN_CODE_W                                            ; Based on the scan code, jump to the appropiate label
    je _key_press
    cmp al, KB_SCAN_CODE_S
    je _key_press
    cmp al, KB_SCAN_CODE_A
    je _key_press
    cmp al, KB_SCAN_CODE_D
    je _key_press
    ret
_key_press:
    mov [KB_SCAN_CODE], al                                            ; If a valid key press, update the global variable the newly pressed key
    ret

_draw_food:
    mov al, [FOOD_PRESENT]                                            ; Retrieve the state if food is present (not collected)
    test al, al
    jnz _draw_food_end                                                ; If present, re-draw the food at it's known location

    xor cx, cx
    mov bx, [SNAKE_BASE_ADDR]                                         ; Load the current base address of the snake on the stack
    mov cl, [SNAKE_SEGMENT_COUNT]                                     ; Load the current snake segment count
    shl cx, 0x1                                                       ; Calculate the offset of the last segment position
    sub bx, cx                                                        ; Calculate the effective address
    mov di, [ss:bx]                                                   ; Retrieve the coordinates of the last segment

    sub di, SNAKE_CELL_SIZE                                           ; Subtract a segment length from the last segment coordinates
    mov word [FOOD_LOCATION], di                                      ; Store the location of the food to di
    mov byte [FOOD_PRESENT], 0x1                                      ; Set the food present byte to 1
_draw_food_end:
    mov di, [FOOD_LOCATION]                                           ; Draw the food to the screen
    mov al, COLOUR_RED
    sub di, GRID_WIDTH * (SNAKE_CELL_SIZE / 2) - (SNAKE_CELL_SIZE / 2)
    call _draw_cell
    ret

_handle_collisions:                                                   ; Check the head of the snake to the position of food, itself and walls
    mov bx, [SNAKE_BASE_ADDR]
    mov ax, [ss:bx - 2]                                               ; Store the position of the snakes head
    mov bx, [FOOD_LOCATION]                                           ; Store the position of the food

    ; Compare snake head against known food location
    cmp ax, bx
    je _add_snake_cell

_wall_check:
    ; Check the snakes head position to the left and right side walls

    push ax                                                           ; Push the snakes head position on the stack
    xor dx, dx
    mov bx, GRID_WIDTH
    div bx
    cmp dx, 0x0                                                       ; Compare the modular of the snakes position to the GRID_WIDTH
    pop ax                                                            ; Pop the snakes head position off the stack
    je _handle_exit                                                   ; With zero bias in the modular operation, handle the exit

    ; Check the snakes head position to check if its above the GRID_SIZE
    cmp ax, GRID_SIZE                                                 ; Compare the snake's head to last accessible position before the GRID_SIZE
    ja _handle_exit

    ; Check the snakes head against the rest of it's body
    xor cx, cx
    mov bx, [SNAKE_BASE_ADDR]
    mov cl, [SNAKE_SEGMENT_COUNT]
    mov ax, [ss:bx - 2]                                               ; Retrieve the position of the snake's head
_check_segment_collision:
    sub bx, 2
    mov si, [ss:bx - 2]                                               ; Get the next segment coordinates
    cmp ax, si                                                        ; Compare if equal, handle exit if true
    je _handle_exit
    loop _check_segment_collision                                     ; Loop until all segments are checked
    ret
_handle_exit:
    mov byte [EXIT], 0x1                                              ; Set the exit status to 1
    ret

_add_snake_cell:                                                      ; Upon collision with food
    mov byte [FOOD_PRESENT], 0x0                                      ; Set the food present variable to 0

    mov si, [SNAKE_BASE_ADDR]                                         ; Retrieve the base address and snake segment count
    mov cl, [SNAKE_SEGMENT_COUNT]
    inc cl                                                            ; Add 1 to the current length and push on the stack until needed
    push cx

    shl cx, 1                                                         ; Calculate the needed offset for the new segment
    sub si, cx                                                        ; Calculate the effective address
    mov ax, [ss:si + 2]
    sub si, SNAKE_CELL_SIZE + 1
    mov word [ss:si], ax                                              ; Store the new coordinates of the new snake segment

    pop cx                                                            ; Pop the length back into cx
    mov byte [SNAKE_SEGMENT_COUNT], cl                                ; Load the lower half of ax into snake segment count
    jmp _wall_check                                                   ; Jump back for other collision checks

_clear_snake_data:
    mov bx, [SNAKE_BASE_ADDR]                                         ; Load the current base address of the snake's memory within the stack
    mov cx, SNAKE_ALLOCATION                                          ; Move the allocation space for the snake's memory
    shr cx, 1                                                         ; Shift bits right by one to fit counting for word size
_clear_snake_data_loop:
    mov word [ss:bx - 2], NULL_SNAKE                                  ; Move NULL_SNAKE into the appropiate offset into snake's memory
    sub bx, 2                                                         ; Update address to next segment
    loop _clear_snake_data_loop                                       ; Loop until cx is zero
    ret

_draw_cell:
    mov cx, SNAKE_CELL_SIZE                                           ; Store SNAKE_CELL_SIZE for the rows
    mov dx, SNAKE_CELL_SIZE                                           ; Store SNAKE_CELL_SIZE for the columns
_draw_square:
    push cx                                                           ; Save row counter
    mov cx, dx                                                        ; Reset column counter for each row

_draw_row:
    stosb                                                             ; Store byte at ES:DI and increment DI
    loop _draw_row                                                    ; Repeat for 20 columns

    add di, GRID_WIDTH - SNAKE_CELL_SIZE                              ; Move to the next line (320 - 20)
    pop cx                                                            ; Restore row counter
    loop _draw_square                                                 ; Repeat for 20 rows
    ret

_clear_screen:
    xor di, di                                                        ; Clear the di register
    mov cx, GRID_SIZE                                                 ; Move the GRID_SIZE to cx (counter)
    mov al, COLOUR_BLACK                                              ; Set the lower 8 bits of ax to COLOUR_BLACK
_clear_screen_loop:
    stosb                                                             ; Store byte at ES:DI and increment DI
    loop _clear_screen_loop                                           ; Loop towards _clear_screen_loop until cx is 0
    ret

_idle:
    call _handle_key_press                                            ; Call handle key press to check if any of WASD is pressed
    cmp al, 0x0                                                       ; Test if AL is zero
    je _idle                                                          ; If AL is zero, keep idling (no valid key pressed)
    ret                                                               ; Otherwise, return (a key was pressed)

_usleep:
    mov ah, 0x86                                                      ; BIOS function to sleep
    mov dx, ax                                                        ; Move AX to DX (parameter for BIOS interrupt)
    int 0x15                                                          ; Call BIOS interrupt
    int 0x15                                                          ; Call BIOS interrupt
    ret

; Global Variables
EXIT                db 0                                              ; 8 bit value of the programs conditional to keep looping
SNAKE_SEGMENT_COUNT db 0                                              ; 8 bit value of snake cell length
KB_SCAN_CODE        db 0                                              ; 8 bit value of keyboard scan code
FOOD_PRESENT        db 0                                              ; 8 bit value of the food is present

SNAKE_BASE_ADDR     dw 0                                              ; 16 bit value of base address for the snake cell coordinates
FOOD_LOCATION       dw 0                                              ; 16 bit value of the food position

times 510 - ($ - $$) db 0                                             ; Boot sector padding
dw 0xaa55                                                             ; magic value (recognises sector as boot sector from bios)
