org 0x8000 ; mapping memory to a space that we know is free
bits 16 ; letting nasm know we want to write with 32 bit layout

; making sure everything is cleared before initial operation 
push main
jmp clrCmd

;##########################################################
; 
; loading "command to run" string into si, then calling printStr to print it
; afterwards calls readStr to read user input
; comparing user input with known commands and jumping to the function for the cmd entered by user
; if no known command is entered printing "command not found" error string

main:										; ret point for all commands other than clear
	call printNL
skip_NL:									; ret point for clear
	push main 								; pushes main on top of stack to return to when coming from clear command
	mov si, input 							; moves "enter your input string" into si to print
	call printStr 							; prints above mentioned string
	call readStr 							; reads in user input
	mov si, cmdhlp 							; moves "help" into si
	call strCmp								; compares content of si to user input
	je hlpCmd 								; jumps to help command if user input equal to "help"
	mov si, cmdsub 							; moves "sub" into si
	call strCmp 							; compares content of si to user input
	je subNum 								; jumps to sub command if user input equal to "sub"
	mov si, cmdadd 							; moves "add" into si
	call strCmp 							; compares content of si to user input
	je addNum 								; jumps to add command if user input equal to "add"
	mov si, cmdmul							; moves "mul" into si
	call strCmp								; compares content of si to user input
	je mulNum								; jumps to mul command if user input equal to "mul"
	mov si, cmddiv 							; moves "div" into si
	call strCmp 							; compares content of si to user input
	je divNum 								; jumps to div command if user input equal to "div"
	mov si, cmdclr 							; moves "clear" into si
	call strCmp								; compares content of si to user input
	je clrCmd								; jumps to clear command if user input is equal to "clear"
	.commandN: 								; if no known command is entered;
		mov si, errStr						; moves error string (command not found) to si, then prints it
		jmp printStr
jmp $   ; in theory program never calls this


;########################################################## 
; maff functions

addNum:
call mathBase								; prepping numbers and putting them into the right registers for the operations
	add dx, cx								; adding numbers
	call printHexW
	ret
subNum:
	call mathBase							; prepping numbers and putting them into the right registers for the operations
	sub dx, cx								; substracting numbers
	call printHexW
	ret
mulNum:
	call mathBase							; prepping numbers and putting them into the right registers for the operations
	mov ax, dx								; moving value from dx to ax to prep for mul instruction (which takes ax value)
	xor dx, dx								; clearing out dx to be ready to enter the final value in afterwards
	mul cx									; actual multiplication 
	mov dx, ax								; moving back result from mul cx (with ax) to dx
	call printHexW
	ret
divNum:
	call mathBase							; prepping numbers and putting them into the right registers for the operations
	mov ax, dx								; moving value from dx to ax to prep for mul instruction (which takes ax value)
	xor dx, dx								; clearing out dx to be ready to enter the final value in afterwards
	div cx									; actual division 
	mov dx, ax								; moving back result from div cx (with ax) to dx
	call printHexW
	ret
clrCmd:
	mov cx, 24 								; adds number resembling amount of loops to be done by the loop to counter register
	.loop:
	call printNL							; function to print a new line
	loop .loop 								; loops through function n times (n is the value stored in cx, 24 in this case)
	mov ah, 0x02 							; sys call to set cursor
	mov bh, 0x00 							; sys call to set page to 0
	mov dh, 0x00 							; sys call to set row to 0th row
	mov dl, 0x00 							; sys call to set char to 0th char
	int 0x10 								; interruption call to set cursor
	pop cx									; puts top of stack and puts value into cx
	add cx, (skip_NL-main)					; adds a pointer to skip_NL, by taking cx value (last one pushed is main) and adding skipNL-main
	push cx									; pushes cx to stack to update value, results into jumping to skipNL instead of main which would add another NL
; it is doing the above mentioned because it already printed 24 new lines, therefore there is no need for the additional nl that gets added in main
; all other commands ret to main because it is what is on top of the stack, hence why we have to change that value
; as soon as skipNL is called, pointer to main gets pushed to top of the stack again
	ret

hlpCmd:
	mov si, hlpStr							; moves help string (commands available) to si for printing
	call printStr							; prints above mentioned string
	ret


mathBase:
	call readNums							; reading numbers
	; calculates and print the result
	mov [num2], dx
	mov si, numRStr							; moving "result:" string to si for printing
	call printStr							; printing string loaded into si
	mov dx, [num1]							; moving num1 into dx, prep for following mathematical operation
	mov cx, [num2]							; moving num2 into cx, prep for following mathematical operation
	ret
readNums:
	; reading first number
	mov si, num1Str							; moving text (enter first number) to si register
	call printStr							; call printStr to print the message mentioned above
	call readStr							; call readStr to read in user input
	mov si, di 								; moving di (buffer from readStr) to si to prepare for operations in hextoNum
	call hextoNum
	mov [num1], dx							; adds calculated num from hex str to memory space of num1 because dx will be used for operation on num2
	; reading second number
	mov si, num2Str							; moving text (enter second number) to si register
	call printStr							; call printStr to print the message mentioned above
	call readStr							; call readStr to read in user input
	mov si, di 								; moving di (buffer from readStr) to si to prepare for operations in hextoNum
	call hextoNum
	ret
;##########################################################
num1 dd 0
num2 dd 0

input db "command to run: ",0
num1Str db "1st number >    ",0
num2Str db "2nd number >    ",0
numRStr db "result >>       ",0
errStr db "command not found",0
hlpStr db "commands available: help, sub, add, mul, div and clear",0
cmdhlp db "help",0
cmdsub db "sub",0
cmdadd db "add",0
cmdmul db "mul",0
cmddiv db "div",0
cmdclr db "clear",0

;##########################################################
; sets zero flag if equal

strCmp:
	pusha                                 ; saving all registers by pushing them to stack 
	or cx, -1                             ; setting the counter register to biggest unsigned number
	xor al, al                            ; clears out al register
	repne scasb                           ; scans through destination index register until terminated by nullbyte, decreasing cx for each scanned char
	neg cx                                ; calculating length of di by negating cx which returns the length of the string including the zero terminator
	sub di, cx                            ; resets destination index by substracting length 
	inc di
	repe cmpsb                            ; check if character from di match with si (including zero terminator)
	; compares two values by subtracting the byte pointed to by ES:DI, from the byte pointed to by DS:SI, and sets the flags according to the results
	test cx, cx                           ; test if amount of matching equals size of string, set zero flag if equals
	popa                                  ; restores all registers by popping them from stack
	ret

;##########################################################
;=> dx = hex => zf (zf = FAILED), si = input str |

hextoNum:
	push ax                                ; pushes si and ax to stack to save content of registers 
	push si
	xor dx, dx                             ; zeroes out ax and dx register to prepare for operations, dx will contain resulting number
	xor ax, ax
	.loop:
		lodsb                              ; loads first ASCII character from inputed string in si into al and increase si
		test al, al                        ; jumps to end if the string loaded in in previous instruction is a null byte that terminates the string
		jz .end
		shl dx, 4                          ; bit shifting left by 4, equals multiplying with 16 to prepare for following compare operations
		cmp al, '0'
			jl .error                      ; checks if character is less than 0x30, which concludes that it cannot be a number or character
		cmp al, '9'
			jle .num                       ; checks if character is in the range between 0x30 and 0x39; concludes that it's a number
		cmp al, 'A'
			jl .error                      ; checks if character is bigger than 0x39 and smaller than 0x42, concludes that it's not a character
		cmp al, 'F'
			jle .clet                      ; checks if character is in the range between 0x42 and 0x46, concludes that it's a uppercased hex character
		cmp al, 'a'
			jl .error                      ; checks if character is bigger than 0x46 and smaller than 0x61, concludes that it's not a lowercased character
		cmp al, 'f'
			jle .slet                      ; checks if character is in the range between 0x61 and 0x66, concludes that it's a lowercased hex character
		jmp .error                         ; jumps to .error if it's not a number or a hex character (a-f)
		.num:
			sub al, '0'                    ; subtracts 0x30 from ASCII number to get the value
			jmp .continue
		.clet:
			sub al, 'A'-0xA                ; subtracts 0x42 and adds 0xA to ASCII uppercased hex character to get the value
			jmp .continue
		.slet:
			sub al, 'a'-0xA                ; subtracts 0x61 and add 0xA to ASCII lowercased hex character to get the value
		.continue:
		add dx, ax                         ; lastResult = (lastResult * 16) + currentNum;
;
; Example:
; A    = 10                           = 10
; AB   = 10*16+11                     = 171
; ABF  = (10*16+11)*16+15             = 2751
; ABFF = ((10*16+11)*16+15)*16+15     = 44031
;
		jmp .loop	                       ; jump back to the loop to perform operations on next character
	xor ax, ax							   ; zeroes out ax
	cmp ax, 1                              ; tests if ax is unequal to 1, concludes that the zero flag not set
	jmp .end
	.error:
		xor dx, dx 						   ; zeroes out dx
		test dx, dx                        ; if dx is equal to 0, concludes that zero flag set
	.end:
	pop si                                 ; restores si and ax that got pushed to stack in beginning of function by popping both values
	pop ax
	ret

;##########################################################
%define readStr_size 8 ;buffer max size

readStr:
	mov di, .buffer						  ; moves buffer into destination index register
	.inner:
		call readChar
		jz .inner                         ; if input equals 0, repeat this until length exceeded or enter hit
		cmp ah, 0x1C                      ; scancode for enter
		je .end
		cmp ah, 0x0E                      ; scancode for backspace
		je .remove
		stosb
		; implementation of live feedback
		pusha 							  ; saved register values by pushing all general purpose registers to the stack 
		call printChar					  ; calling function to print single character
		popa 							  ; restores stack from before previous pusha was executed by popping all general purpose values from stack
		cmp di, (.buffer+readStr_size) ; if length of buffer is bigger or equal to readStr_size, jump to end
										  ; assembly version of bof protection
		jge .end
	jmp .inner							  ; else jump back to loop and wait for backspace, enter or a string that's too long to get out of loop
	.remove:
		cmp di, .buffer                   ; if buffer is at index 0, don't remove any characters (cause there are none)
		jle .inner						  ; in that case, jump back to .inner
		dec di                            ; else: decrement index by one
		; implementation of live feedback
		pusha 							  ; saved register values by pushing all general purpose registers to the stack 
		call printChar 					  ; goes one character back, empties it with space and then goes back again
		mov al, ' '						  ; moves space into al for printChar function
		call printChar
		mov al, 0x8 					  ; 0x8 is the ascii code for a backspace
		call printChar
		popa 							  ; restores stack from before previous pusha was executed by popping all general purpose values from stack
		jmp .inner						  ; jumps back to .inner
	.end:
		xor al, al 						  ; zeroes out al register
		stosb                             ; zero terminate string
		; implementation of live feedback
		pusha 							  ; saved register values by pushing all general purpose registers to the stack
		call printNL 					  ; prints a new line
		popa 							  ; restores stack from before previous pusha was executed by popping all general purpose values from stack
		mov di, .buffer                   ; sets output to return to the beginning of the string
		ret
.buffer resb (readStr_size+1)
;##########################################################
;
; |----int----|--ah--|----Zero flag---|----Returned al----|---Returned ah---|--------------Description--------|
; |  int 0x16 | 0x0  |    Unchanged   |     Scan code     |    ASCII Code   | Halts until key is pressed      |
; |  int 0x16 | 0x1  | No Key pressed |     Scan code     |    ASCII Code   | Read Keyboard status            |
; |-----------|------|----------------|-------------------|-----------------|---------------------------------|

readChar:
	mov ah, 1                             ; int 0x16, 0 (used after) should be enough, added 0x16, 1 for consistency purposes
	int 0x16                              ; int 0x16, 1 - checks if a key is pressed, sets zero flag if not
	jz .end
	mov ah, 0                             ; int 0x16, 0 - halts until key is pressed, returns key (scancode) into al and (ASCII code) into ah
	int 0x16
	ret
	.end:
	mov ax, 0                             ; returns 0 for al and ah if no key was pressed
	ret


;##########################################################
; value to be printed out is loaded into dl
; converting a number into a Hex string works by printing out digits one by one
; in our case we print out content of 8bit register which can store 2 hex digits
; first 4 bit and last 4 bit are independent numbers and both represent one of the digits stored

printHexW:
	xchg dl, dh 						 ; exchanges contents of the two opperants
	call printHex 						 ; calls the actual loop to print the hexadecimal strings
	xchg dl, dh 						 ; exchanges contents of the two opperants
	call printHex 						 ; calls the actual loop to print the hexadecimal strings
	ret

;##########################################################

printHex:
	call .print           				 ; called twice to print out the first 4 bit and the last 4 bit
										 ; it is called twice through printHexW, not inside this function
.print:
	ror dl, 4        				     ; we want to print the last 4 bit first, so we rotate 
	pusha                 				 ; pushes all general registers to stack to save them
										 ; as dl has 8 bit and gets rotated twice it will end up unchanged
	and dl, 0x0F						 ; reading first 4 bit into dl
	; "0x0F is a hexadecimal number which equals 15 in decimal. It represents the lower four bits and translates the the bit-pattern 00001111"
	mov al, ('0'+0xA)     				 ; NUM BASE = '0' if dl < 10
	mov bl, 'A'           				 ; NUM BASE = 'A' if dl >= 10
	; handles the fact that hex represent the values from 10-15 as letters (A, B, C, D, E, F)
	sub dl, 0xA							 ; subtracting 10 from dl to set the correct flags
	cmovae ax, bx         				 ; if dl >= 0xA, setting NUM BASE = 'A'
	add al, dl            				 ; dl+NUM BASE = ASCII Character
	call printChar						 ; call to function that prints the character stored in al register
	popa								 ; reverses previous pusha by popping all general registers from stack again
	ret

;##########################################################


printBin:
	pusha 								 ; saving general purpose registers by pushing them to the stack
	mov cx, 8             				 ; setting counter register to 8 to prepare for loop instruction
	rol dx, 1    						 ; offsets everything by one
	.loop:
	bt dx, cx             				 ; tests bit at position cx, which gets decreased in loop, copies a bit from a given register to the carry flag
	setc al               				 ; sets the byte in the operand to 1 if carry flag is set
	or al, 30h							 ; performs OR operation to number stored in al with 0x30 to convert it to an ASCII character
	call printChar 						 ; prints ASCII value stored in al
	loop .loop 							 ; loops through the loop 8 times as this is the value initially stored in cx
										 ; 8 times for each bit in a binary number, loop instruction decrements cx by one each time
	popa 								 ; resets previous pusha by popping all general registers from stack
	ret


;##########################################################

printNL:
	mov al, 0Dh           ; 0Dh in ah is instruction for read graphic pixels (see int 0x10 man)
	call printChar		  
	mov al, 0Ah           ; 0Ah sets writes character at cursor position (see int 0x10 man)
	call printChar
	ret	


;##########################################################
;
; |----int----|--ah--|------------al------------|---------bh-------|-------bl-------|-----------Description-----------|
; |  int 0x10 | 0xE  | ASCII Character to print | Page to write to | Color Attribute| Print a character to the screen |
; |-----------|------|--------------------------|------------------|----------------|---------------------------------|


printChar:
    mov bh, 0x00 ; page to write to, page 0 is displayed by default
    mov bl, 0x00 ; setting color to 0 as we are not dealing with gui 
    mov ah, 0x0E 
    int 0x10 ; int 0x10, 0x0E = print character in al
    ret



;##########################################################

printStr:
	pusha                 				; pushes all general purpose registers to the stack
	.loop:
		lodsb           				; loads byte from si into al and increases si
		test al, al       				; tests if al is 0 which would indicate that string reached its end
		jz .end 		  				; if string is terminated by nullbyte (previous instructions returns 0), jumps to end
		call printChar    				; prints the character in al register
	jmp .loop             				; continues loop
	.end:
	popa               					; restores state before pusha was executed by popping all general purpose registers from stack
	ret

;##########################################################

%assign usedMemory ($-$$)
%assign usableMemory (512*16)
%warning [usedMemory/usableMemory] Bytes used
times (512*16)-($-$$) db 0 				; adding padding since kernel size needs to be multiple of 512
