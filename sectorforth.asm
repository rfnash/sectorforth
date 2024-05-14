;; sectorforth - a 512-byte, bootable x86 Forth.
;; Copyright (c) 2020 Cesar Blum
;; Distributed under the MIT license. See LICENSE for details.

bits 16
cpu 386

jmp 0x0050:start

org 0x7700

TIB             equ 0x0000      ; terminal input buffer (TIB)
STATE           equ 0x1000      ; current state (0=interpret, 1=compile)
TOIN            equ 0x1002      ; current read offset into TIB (>IN)
RP0             equ 0x76fe      ; bottom of return stack
SP0             equ 0xfffe      ; bottom of data stack

F_IMMEDIATE     equ 0x80
F_HIDDEN        equ 0x40
LENMASK         equ 0x1f

%define link 0

%macro defword 2-3 0            ; name, label, flags
word_%2:
        dw link                 ; link to previous word
%define link word_%2
%strlen %%len %1
        db %3+%%len             ; flags+length
        db %1                   ; name
%2:                             ; code starts here
%endmacro

%define NEXT jmp next

defword "@",FETCH
pop bx
push word [bx]
NEXT

defword "!",STORE
pop bx
pop word [bx]
NEXT

defword "sp@",SPFETCH
push sp
NEXT

defword "rp@",RPFETCH
push bp
NEXT

defword "0=",ZEROEQUALS
pop ax
test ax,ax
setnz al                ; AL=0 if ZF=1, else AL=1
dec ax                  ; AL=ff if AL=0, else AL=0
cbw                     ; AH=AL
push ax
NEXT

defword "+",PLUS
pop bx
pop ax
add ax,bx
push ax
NEXT

defword "nand",NAND
pop bx
pop ax
and ax,bx
not ax
push ax
NEXT

defword "exit",EXIT
xchg sp,bp              ; swap SP and BP, SP controls return stack
pop si                  ; pop address to next word
xchg sp,bp              ; restore SP and BP
NEXT

defword "tib",TIBVAR
push word TIB
NEXT

defword "state",STATEVAR
push word STATE
NEXT

defword ">in",TOINVAR
push word TOIN
NEXT

next:
        lodsw                   ; load next word's address into AX
        jmp ax                  ; jump directly to it

defword "here",HEREVAR
        push word HERE
        NEXT
HERE:   dw start_HERE

        defword "latest",LATESTVAR
        push word LATEST
        NEXT
LATEST: dw word_SEMICOLON       ; initialized to last word in dictionary

defword "key",KEY
mov ah,0
int 0x16
push ax
NEXT

defword "emit",EMIT
pop ax
call writechar
NEXT

defword ":",COLON
call token              ; parse word from input
push si
mov si,di               ; set parsed word as string copy source
mov di,[HERE]           ; set current value of HERE as destination
mov ax,[LATEST]         ; get pointer to latest defined word
mov [LATEST],di         ; update LATEST to new word being defined
stosw                   ; link pointer
mov al,cl
or al,F_HIDDEN          ; hide new word while it's being defined
stosb                   ; word length
rep movsb               ; word name
mov ax,0x26ff
stosw                   ; compile near jump, absolute indirect...
mov ax,DOCOL.addr
stosw                   ; ...to DOCOL
mov [HERE],di           ; update HERE to next free position
mov byte [STATE],1      ; switch to compilation state
pop si
NEXT

DOCOL:
        xchg sp,bp              ; swap SP and BP, SP controls return stack
        push si                 ; push current "instruction pointer"
        xchg sp,bp              ; restore SP and BP
        add ax,4                ; skip word's code field
        mov si,ax               ; point "instruction pointer" to word body
        NEXT                    ; start executing the word

.addr:  dw DOCOL

defword ";",SEMICOLON,F_IMMEDIATE
        mov bx,[LATEST]
        and byte [bx+2],~F_HIDDEN       ; reveal new word
        mov byte [STATE],0      ; switch to interpretation state
        mov ax,EXIT             ; prepare to compile EXIT
compile:
        mov di,[HERE]
        stosw                   ; compile contents of AX to HERE
        mov [HERE],di           ; advance HERE to next cell
        NEXT

start:
        cld                     ; clear direction flag

push cs
push cs
push cs
pop ds
pop es
pop ss

jmp init

error:
        mov ax,0x0921           ; write '!'
        mov bx,0x0004           ; black background, red text
        mov cx,2                ; twice
        int 0x10

init:
        mov bp,RP0              ; BP is the return stack pointer
        mov sp,SP0              ; SP is the data stack pointer

mov al,0
mov cx,STATE+4
mov di,TIB
rep stosb

interpreter:
        call token              ; parse word from input
        mov bx,[LATEST]         ; start searching for it in the dictionary
.1:     test bx,bx              ; zero?
        jz error                ; not found, reset interpreter state
        mov si,bx
        lodsw                   ; skip link
        lodsb                   ; read flags+length
        mov dl,al               ; save those for later use
        test al,F_HIDDEN        ; entry hidden?
        jnz .2                  ; if so, skip it
        and al,LENMASK          ; mask out flags
        cmp al,cl               ; same length?
        jne .2                  ; if not, skip entry
        push cx
        push di
        repe cmpsb              ; compare strings
        pop di
        pop cx
        je .3                   ; if equal, search is over
.2:     mov bx,[bx]             ; skip to next entry
        jmp .1                  ; try again
.3:     mov ax,si               ; after comparison, SI points to code field
        mov si,.loop            ; set SI so NEXT loops back to interpreter

and dl,F_IMMEDIATE      ; isolate IMMEDIATE flag
        or dl,[STATE]           ; OR with state
        dec dl                  ; decrement
        jz compile              ; if result is zero, compile
        jmp ax                  ; otherwise, interpret (execute) the word
.loop:  dw interpreter

token:
        mov di,[TOIN]           ; starting at the current position in TIB
        mov cx,-1               ; search "indefinitely"
        mov al,32               ; for a character that's not a space
        repe scasb
        dec di                  ; result is one byte past found character
        cmp byte [di],0         ; found a 0?
        je .readline            ; if so, read more input
        mov cx,-1               ; search "indefinitely" again
        repne scasb             ; this time, for a space
        dec di                  ; adjust DI again
        mov [TOIN],di           ; update current position in TIB
        not cx                  ; after ones' complement, CX=length+1
        dec cx                  ; adjust CX to correct length
        sub di,cx               ; point to start of parsed word
        ret
.readline:
        mov al,13
        call writechar          ; CR
        mov al,10
        call writechar          ; LF
        mov di,TIB              ; read into TIB
.1:     mov ah,0                ; wait until a key is pressed
        int 0x16
        cmp al,13               ; return pressed?
        je .3                   ; if so, finish reading
        cmp al,8                ; backspace pressed?
        je .2                   ; if so, erase character
        call writechar          ; otherwise, write character to screen
        stosb                   ; store character in TIB
        jmp .1                  ; keep reading
.2:     cmp di,TIB              ; start of TIB?
        je .1                   ; if so, there's nothing to erase
        dec di                  ; erase character in TIB
        call writechar          ; move cursor back one character
        mov ax,0x0a20           ; erase without moving cursor
        mov cx,1
        int 0x10                ; (BH already set to 0 by writechar)
        jmp .1                  ; keep reading
.3:     mov ax,0x0020
        stosw                   ; put final delimiter and 0 in TIB
        call writechar          ; write a space between user input and
                                ; execution output
        mov word [TOIN],0       ; point >IN to start of TIB
        jmp token               ; try parsing a word again

writechar:
        push ax                 ; INT 10h/AH=03h clobbers AX in some BIOSes
        mov bh,0                ; video page 0 for all BIOS calls
        mov ah,3                ; get cursor position (DH=row, DL=column)
        int 0x10
        pop ax                  ; restore AX
        mov ah,0x0e             ; teletype output
        mov bl,0x7              ; black background, light grey text
        int 0x10
        cmp al,8                ; backspace?
        jne .1                  ; if not, nothing else to do
        test dl,dl              ; was cursor in first column?
        jnz .1                  ; if not, nothing else to do
        mov ah,2                ; move cursor
        mov dl,79               ; to last column
        dec dh                  ; of previous row
        int 0x10
.1:     ret

        times 510-($-$$) db 0
        db 0x55, 0xaa

start_HERE:
