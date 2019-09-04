org 0x7C00
%define SECTOR_AMOUNT 0x10  	; precompiler defined value to easily adjust the size needed for the kernel
jmp short start
nop

; dis is absolutely stolen from http://mikeos.sourceforge.net/, but that dude a beer

								; BGP 
OEMLabel		db "Example "	; Disk label
BytesPerSector		dw 512		; Bytes per sector
SectorsPerCluster	db 1		; Sectors per cluster
ReservedForBoot		dw 1		; Reserved sectors for boot record
NumberOfFats		db 2		; Number of copies of the FAT
RootDirEntries		dw 224		; Number of entries in root dir
LogicalSectors		dw 2880		; Number of logical sectors
MediumByte		db 0F0h		    ; Medium descriptor byte
SectorsPerFat		dw 9		; Sectors per FAT
SectorsPerTrack		dw 18		; Sectors per track (36/cylinder)
Sides			dw 2		    ; Number of sides/heads
HiddenSectors		dd 0		; Number of hidden sectors
LargeSectors		dd 0		; Number of LBA sectors
DriveNo			dw 0		    ; Drive No: 0
Signature		db 41		    ; Drive signature: 41 for floppy
VolumeID		dd 00000000h	; Volume ID: any number
VolumeLabel		db "Example    "; Volume Label: any 11 chars
FileSystem		db "FAT12   "	; File system type: don't change!
start: 

;##########################################################
; initiliazing registers

cli
xor ax, ax
mov ds, ax
mov ss, ax
mov es, ax
mov fs, ax
mov gs, ax
mov sp, 0x6ef0					; setting up stack
sti

mov ah, 0						; resetting disk 
int 0x13 						; ah register is equal to 0, driver number is handled by BIOS
jc errorpart

; preparing for loading kernel by reading from the harddrive and writing to RAM

mov bx, 0x8000     				; bx as address to write the kernel to, choosing memory space we know is free and in range
mov al, SECTOR_AMOUNT			; al sets amount of sectors to read, 1 sector = 512 
mov ch, 0          				; setting cylinder/track equal to 0, necessary for used cylinder-head-sector (CHS) format 
mov dh, 0          				; setting head equal to 0, necessary for used cylinder-head-sector (CHS) format
mov cl, 2				        ; setting the sector equal to 2
mov ah, 2          				; setting ah equal to 2 to read from drive
int 0x13   		   				; => ah = status, al = amount read
jc errorpart
jmp 0x8000

errorpart:            			; to make sure you have *some* error handling
mov si, errormsg
mov bh, 0x00          			; sets page to 0
mov bl, 0x07          			; handles text attribute
mov ah, 0x0E          			; communicating to BIOS that we wanna print a char
.part:
lodsb
sub al, 0
jz end
int 0x10 
jmp .part
end:
jmp $

errormsg db "lol u fucced it up again"
times 510-($-$$) db 0

; MBR signature
db 0x55 ;byte 511 = 0x55
db 0xAA ;byte 512 = 0xAA
