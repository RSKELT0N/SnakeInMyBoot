# SnakeInMyBoot
A simple snake game written within the first sector (512 bytes) of a hard drive; the boot sector located by the BIOS in 16-bit mode.

## Project Information
- Snake game is bounded by 512 bytes (first sector of the hard drive), this is due to the BIOS loading this sector into memory for us. Utilising more disk space, requires loading additional sectors from the hard drive through the hard drive interrupt within the BIOS. As it is possible, the challenge of this project is to only use the default allocated space.
- CPU is still within real mode (16-bit).
- Boot sector will located
- As the boot sector is located within the first sector of the hard drive, the BIOS will typically load the sector into address 0x7c00 of the main memory.

## Project usage

- Compile
```
make
```

- Clean
```
make clean
```

- Rebuild
```
make rb
```

## Execute
```
make run
```
