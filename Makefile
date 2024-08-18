# Author: Ryan Skelton
# Date Created: 18/08/2024
#
# Makefile to build project, write 512 bytes into first sector towards the first
# sector of a floppy disk and load into a qemu VM.

# Constants
ASM 	  := nasm
ASM_FLAGS := -f bin
TARGET    := snake.bin

# Path and file collectives
SRC_FOLDER 	 := ./
BUILD_FOLDER := ./out
SRC_FILES    = $(shell find $(SRC_FOLDER) -name "*.asm")

# Entry point
.PHONY: all
all: setup install

setup:
	mkdir $(BUILD_FOLDER)

$(TARGET): $(SRC_FILES)
	$(ASM) $(ASM_FLAGS) -o $@ $<

install: $(TARGET)
	mv $(TARGET) $(BUILD_FOLDER)

.PHONY: run
run:
	qemu-system-x86_64 -drive file=$(BUILD_FOLDER)/$(TARGET),format=raw,if=floppy

.PHONY: rb
rb: clean all

.PHONY: clean
clean:
	rm $(TARGET) || true
	rm -rf $(BUILD_FOLDER) || true
