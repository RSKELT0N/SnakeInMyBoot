# SnakeInMyBoot
A simple snake game written within the first sector (512 bytes) of a hard drive; the boot sector located by the BIOS in 16-bit mode.

## Project Information
- Snake game is bounded by 512 bytes (first sector of the hard drive), this is due to the BIOS loading this sector into memory for us. Utilising more disk space, requires loading additional sectors from the hard drive through the hard drive interrupt within the BIOS. As it is possible, the challenge of this project is to only use the default allocated space.
- CPU is still within real mode (16-bit).
- As the boot sector is located within the first sector of the hard drive, the BIOS will typically load the sector into address 0x7c00 of the main memory.

## Project usage
|  Usage  | Shell Command |
| ------- | ------------- |
| compile | make          |
| clean   | make clean    |
| rebuild | make rb       |
| execute | make run      |

## Usage
- The program is controlled through W, A, S, and D keys for Up, Left Right and Down movements, respectively.
- Upon startup, the game is in an idle state, waiting for one of the valid key presses.

## Binary generated
![snake](https://github.com/user-attachments/assets/0a82a22a-4f12-4201-a522-02ce66d5a59a)

## Example 
![snake](https://github.com/user-attachments/assets/3baf47b1-5077-4c0f-949d-3340320e1db9)
