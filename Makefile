NASM = nasm
QEMU = qemu-system-i386
RM = rm -rf

all: os.bin

kernel.bin: kernel.asm
	nasm -fbin kernel.asm -o kernel.bin

bootloader.bin: bootloader.asm
	nasm -fbin bootloader.asm -o bootloader.bin

os.bin: kernel.bin bootloader.bin
	cat bootloader.bin kernel.bin > os.bin

clean:
	$(RM) os.bin kernel.bin bootloader.bin

run: os.bin
	$(QEMU) -curses -drive file=os.bin,format=raw
