; Name: Basil Haj Yehia
; Date: 16/5/2020
; Project: Final Assembly Project

IDEAL
MODEL small
STACK 100h

DATASEG

;-------------------
maxrad dw ? ; Variable for squaredist proc
birdy dw 120 ; Bird's y position
birdx equ 50; Bird's x position
birdcol equ 14 ; Bird's color
birdrad equ 10 ; Bird's radius
grav equ 1 ; Gravity constant
vel dw 0 ; Velocity variable
last db 0 ; Last key press
bgcolor equ 3 ; Background color
spacelen equ 75 ; Constant space between pipes
pipe1h dw 0 ; Pipe 1's height
pipe2h dw 0 ; Pipe 2's height
pipex dw 400 ; Pipe's x position
pspeed equ 9 ; Pipe's constant speed
pipe2sy dw 110 ; Pipe 2's y position
pipelen equ 20 ; Pipe's length/width
crnt dw 0 ; Variable to store random value
temp dw 0 ; Temporary variable
jvel equ -10 ; Velocity for jumping
colb equ birdrad ; Collision distance
pipecol equ 2 ; Pipe's color
score dw 0 ; Score variable
;Messages
msg1 db "Flappy Bird",10,13,"$"
msg2 db "by $"
msg3 db "Basil Haj Yehia",10,13,10,13,"$"
msg4 db "Rules:",10,13,"$"
msg5 db "Jump using the space bar and be careful not to hit the pipes or floor!",10,13,10,13,"$"
msg6 db "-Press any key to start-$"
msg7 db "Your score is: $"
msg8 db "Better luck next time!",10,13,"$"
msg9 db "Well done!",10,13,"$"

buffone dw 0
;-------------------

CODESEG

proc toText ; set console mode to text, 
	push ax ; save ax
	; BIOS settings for text mode interrupt
	mov ah, 0
	mov al, 2
	int 10h ; BIOS interrupt
	pop ax ; restore ax
	ret
endp toText

proc toGraphics ; set console mode to graphics
	push ax ; save ax
	; BIOS settings for graphic mode interrupt
	mov ax, 13h
	int 10h ; BIOS interrupt
	pop ax ; restore ax
	ret
endp toGraphics

proc printDigits
  valueParam equ [bp + 4] ; Use EQU to point to parameter

  push bp ; Push base pointer
  mov bp, sp ; Move the base pointer to the stack pointer

  push ax ; Push ax so that it can be used
  push bx ; Push bx so that it can be used
  push cx ; Push cx so that it can be used
  push dx ; Push dx so that it can be used

  cmp valueParam, 0
  je zero

  ; Initialize counter
  mov cx, 0 
  mov dx, 0 
  mov ax, valueParam

  printLoop: ; Initialize label

  cmp ax, 0 ; If AX = 0
  je print ; Jump to label

  mov bx, 10 ; Initialize BX as 10
  div bx ; Extract last digit
  push dx ; Push it to the stack
  inc cx ; Increment the counter             
  xor dx, dx ; Clear the register

  jmp printLoop ; Jump up to label

  print: ; Initialize label

  cmp cx, 0 ; Check if counter is equal than zero
  je leavePrint ; If it is, leave the function

  pop dx ; Pop the top of the stack
  add dx, 48 ; Add character code
  mov ah, 02h ; Prepare for DOS Interrupt
  int 21h ; Print interrupt

  dec cx ; Decrease the counter
  jmp print ; Jump to label

  zero:
  mov dx, 48 ; Add character code
  mov ah, 02h ; Prepare for DOS Interrupt
  int 21h 

  leavePrint: 
  mov dx, 10 ; Add character code
  mov ah, 02h ; Prepare for DOS Interrupt
  int 21h 

  mov dx, 13 ; Add character code
  mov ah, 02h ; Prepare for DOS Interrupt
  int 21h 

  pop dx ; Take back the value of dx
  pop cx ; Take back the value of cx
  pop bx ; Take back the value of bx
  pop ax ; Take back the value of ax
  pop bp ; Take back the value of bp
  ret 2 ; 2 * 1 Parameters
endp printDigits

proc randInt
	push ax ; Save register
	push bx ; Save register
	push cx ; Save register

	xor ax,ax
	mov ah, 2ch
	int 21h ; Initialization for time data

	

	; Randomization Algorithm
	add ch, cl ; Random functions that mix around the different values of the time
	mov al, dh ; (Hours, Minutes, Seconds, Deciseconds)
	mul ch 
	sub al, dl
	mul dl
	cwd

	; Extracting Last Two Digits
	mov bx, 10 
	div bx
	mov [temp], ax
	mov ax, dx
	mul bx
	mov dx, ax
	mov ax, [temp]
	mov [temp], dx
	cwd
	div bx
	add dx, [temp] ; Move values around in registers and variables until left with 2 digits

	add dx, 10
	
	pop cx ; pop register
	pop bx ; pop register
	pop ax ; pop register
	ret
endp randInt


x equ [bp + 8] ; x parameter
y equ [bp + 6] ; y parameter
col equ [bp + 4] ; color parameter
proc pixel
	;save registers
	push bp ; Save register
	mov bp, sp 
	push ax ; Save register
	push bx ; Save register
	push cx ; Save register
	push dx ; Save register
	
	xor bh, bh ; bh = 0
	mov cx, x ; cx = x
	mov dx, y ; dx = y
	mov al, col ; al = col
	mov ah, 0Ch ; BIOS interrupt setting to paint pixel
	int 10h ; BIOS interrupt
	
	; restore registers
	pop dx
	pop cx
	pop bx
	pop ax
	pop bp
	ret 6 ; restore stack before parameters
endp pixel


x   equ [bp + 10]
y   equ [bp + 8]
len equ [bp + 6]
col equ [bp + 4]
proc lineDraw
  push bp
  mov bp,sp
  push ax  ; Save
  push bx  ; Save
  push cx  ; Save
  push dx  ; Save
  
  mov bx,len
  xor bh,bh
  lpDraw:
    mov bh,0h 
    mov cx,x ; x cor
    mov dx,y ; y cor
    mov ax,col
    mov ah,0Ch 
    int 10h ; Draw
    inc x 
    
    dec len 
    cmp len,0
    jne lpDraw
    
  fin:
    pop dx ; Load
    pop cx ; Load
    pop bx ; Load
    pop ax ; Load
    pop bp
    ret 8
endp

; PROC rectDraw
x   equ [bp + 12]
y   equ [bp + 10]
w   equ [bp + 8]
h   equ [bp + 6]
col equ [bp + 4]
proc rectDraw
  push bp ; Save register
  mov bp,sp
  push cx ; save
  
  mov cx,h ; draw lines
  
  drawLoop:
    push x 
    push y 
    inc y  ;Next line
    push w 
    push col 
    call lineDraw 
    loop drawLoop
  
  pop cx ; load
  pop bp ;load
  ret 10
endp rectDraw



x1 equ [bp + 10]
y1 equ [bp + 8]
x2 equ [bp + 6]
y2 equ [bp + 4]
; OVERRIDE dx
; dx - the square distance between two points
proc squaredist
	push bp ; Save register
	mov bp, sp
	push ax ; Save register
	push bx ; Save register
	push cx ; Save register
	
	;Get x distance from center to corner of square surrounding circle
	mov ax, x1
	cmp ax, x2
	ja abs1
	mov ax, x2
	sub ax, x1
	jmp abs2
	abs1: ; Different labels for which ever value is bigger
	sub ax, x2
	abs2:

	;Get y distance from center to corner of square surrounding circle
	mov bx, y1
	cmp bx, y2
	ja babs1
	mov bx, y2
	sub bx, y1
	jmp babs2
	babs1:
	sub bx, y2
	babs2:

	;Multiply and add values using pythagoras
	mov cx, ax
	mul cx
	xchg ax, bx
	mov cx, ax
	mul cx
	
	mov dx, ax
	add dx, bx
	
	; Pop registers
	pop cx
	pop bx
	pop ax
	pop bp
	ret 8
endp squaredist


x equ [bp + 10]
y equ [bp + 8]
rad equ [bp + 6]
col equ [bp + 4]
proc circle ; proc to draw a circle using squaredist
	push bp ; Save register
	mov bp, sp
	push ax ; Save register
	push bx ; Save register
	push cx ; Save register
	push dx ; Save register
	
	mov ax, rad
	mov cx, rad
	mul cx
	mov [maxrad], ax ; Get rad squared so it can be compared with squared distances
	mov ax, x
	sub ax, cx
	mov bx, y
	sub bx, cx
	shl cx, 1
	xs:
		push bx ; Push registers to save
		push cx ; Push registers to save
		mov cx, rad
		mov bx, y
		sub bx, cx
		shl cx, 1
		ys:
		
		push x ; push value for squaredist
		push y ; push value for squaredist
		push ax ; push value for squaredist
		push bx ; push value for squaredist
		call squaredist
		
		cmp dx, [maxrad] ; Compare square distance of center to chosen point (first time is bottom left corner)
		ja notincirc ; If the distance is bigger than the radius (outside the circle) dont color
		push ax
		push bx
		push col
		call pixel ; If its in the circle, color
		notincirc:
		inc bx ; Increase the y2 value
		loop ys ; Loop for a full line
		pop cx
		pop bx
		inc ax ; Increase x2 value
	loop xs ; Loop across the full square (the one surrounding the circle)
	
	pop dx ; Pop back register
	pop cx ; Pop back register
	pop bx ; Pop back register
	pop ax ; Pop back register
	pop bp ; Pop back register
	ret 8
endp circle



proc wloop ; The while loop (the entire repeating game loop)

	begin:


		add [vel], grav ; change velocity by the gravity
		mov ax, [vel] ; set ax as velocity
		add [birdy], ax ; change y pos by the velocity

		cmp [birdy], 13 ; Compare bird y to the top of the screen
		jb topjmp1 ; goto label

		cmp [birdy], 40000 ; Compare bird y to the top of the screen (Around 60 thousand)
		ja topjmp1 ; goto label

		cpnt1: 

		cmp [birdy], 188 ; Check if the bird hit the bottom of the screen
		ja exitjmp1

		call clrScreen ; Clear the screen

		cpnt2:

		call drawBird ; Draw the bird 

		;-------Buffer once--------
	    cmp [buffone], 0 ; proc to buffer once at the start of the game 
	    jne nobuff;        in order to leave some time for the player

	    mov al, 0 ; set al
	    mov ah, 86h ; set ah
	    mov cx, 3h ; set cx as beginning of time value
	    mov dx, 0D40h ; set dx as end of time value 
	    int 15h ; buffer interrupt (for the duration of CX:DX)

	    inc [buffone] ; increment 

	    nobuff:
	    ;-------------------------


		;-------CheckPoint--------
	    jmp abc

	    topjmp1:
	    	jmp topjmp2

	    exitjmp1:
	    	jmp exitjmp2

	    beginjmp3:
	    	jmp begin

	    abc:
	    ;-------------------------


	    ;---------Pipes-----------


	    sub [pipex], pspeed ; Move the pipes by a constant value

	    cmp [pipex], 300 ; See if the pipes should be drawn on screen
	    ja pskipjmp ; If not, skip the drawing phase

	    mov ax, 9
	    cmp [pipex], ax ; Compare pipe x value to 9
	    jb rset ; If its smaller, teleport the pipe back to its starting point

	    ; Calculations on where to draw the pipes
	    mov ax, [crnt] ; Random 2 digit number
	    mov [pipe1h], ax ; set the height of the first pipe
	    mov ax, [pipe1h] ; set ax to the height
	    add ax, spacelen ; add the pre-determined space (between the pipes)
	    mov [pipe2sy], ax ; set the value as pipe 2's starting y
		mov ax, 200 ; use calculated values to see how much space is left 
		sub ax, [pipe1h] ; subtract
		sub ax, spacelen ; subtract
	    mov [pipe2h], ax ; set remaining space to pipe 2's height

	    ;-------CheckPoint2--------
	    jmp def

	    topjmp2:
	    	jmp topjmp3

	    exitjmp2:
	    	jmp exitjmp3

	    beginjmp2:
	    	jmp beginjmp3

	    pskipjmp:
	    	jmp pskip

	    def:
	    ;-------------------------

	    call drawPipes ; draw the pipes
	    call colCheck ; check for collision
	    call scrCheck ; check for score increment
		sub [pipex], pspeed ; move pipes again
		mov ax, 9 ; set ax to 9
	    cmp [pipex], ax ; compare
	    jb rset ; if value is too small, teleport the pipes back

	    add [vel], grav ; change velocity by the gravity
		mov ax, [vel] ; set ax as velocity
		add [birdy], ax ; change y pos by the velocity

	    mov al, 0 ; set al
	    mov ah, 86h ; set ah
	    mov cx, 0h ; set cx as beginning of time value
	    mov dx, 07724h ; set dx as end of time value 
	    int 15h ; buffer interrupt (for the duration of CX:DX)

		call clrScreen ; clear the screen 
	    call drawBird ; draw the bird
		call drawPipes	; draw the pipes 
		call colCheck ; check for collision
	    call scrCheck ; check for score increment


	    jmp bskip

	    rset:
	    	mov [pipex], 300 ; move back pipes
	    	call randInt ; call proc for randInt
	    	mov [crnt], dx ; set crnt as new random integer
	    	jmp bskip ; goto label

	    pskip:

		add [vel], grav ; change velocity by the gravity
		mov ax, [vel] ; set ax as velocity
		add [birdy], ax ; change y pos by the velocity

		mov al, 0 ; set al
	    mov ah, 86h ; set ah
	    mov cx, 0h ; set cx as beginning of time value
	    mov dx, 07530h ; set dx as end of time value 
	    int 15h ; buffer interrupt (for the duration of CX:DX)

		call clrScreen ; clear the screen
	    call drawBird ; draw the bird

	    ;-------CheckPoint3--------
	    jmp ghi

	    topjmp3:
	    	jmp top

	    exitjmp3:
	    	jmp exit

	    beginjmp1:
	    	jmp beginjmp2

	    ghi:
	    ;-------------------------

	    bskip:

	    ;Buffer
	    mov al, 0 ; set al
	    mov ah, 86h ; set ah
	    mov cx, 0h ; set cx as beginning of time value
	    mov dx, 0EA60h ; set dx as end of time value 
	    int 15h ; buffer interrupt (for the duration of CX:DX)

	    ;-------Key Presses--------
	    in al,64h ; get keyboard inut
	    cmp al,10b ; check for new data
	    je beginjmp1 ; if none go to start
	    in al,60h ; Key
	    
	    cmp [last],al ; Check if there was no change in the key
	    je beginjmp1 ; go back to start
	    
	    mov [last],al ; set key pressed as last key
	    
	    cmp al,39h ; scan the code for Space key
	    je spc ; goto spc label
	    
	    
	    cmp al,81h ; scan code for ESC key
	    je exitjmp ; exit code

	    jmp beginjmp1 ; go back to start

	    spc:
	    	mov [vel], jvel ; set velocity to a pre-determined value for a jump

	    jmp beginjmp1 ; loop until termination

	    top: ; if bird hits the top
	    	mov [vel], 0 ; set velocity to zero
	    	mov [birdy], 11 ; move bird a bit down
	    	jmp cpnt1 ; goto label

	   
    	
    ret
endp wloop

proc clrScreen
	push 0 ; Push parameter
	push 0 ; Push parameter
	push 320 ; Push parameter
	push 200 ; Push parameter
	push bgcolor ; Push parameter
	call rectDraw ; Color a rectangle which covers the screen
	ret
endp clrScreen

;-------CheckPoint4--------
jmp jkl

exitjmp:
	jmp exit

jkl:
;-------------------------

proc drawPipes
	push [pipex] ; X
    push 0 ; Y
    push pipelen ; Width
    push [pipe1h] ; Height
    push pipecol ; Color
    call rectDraw

    push [pipex] ; X
    push [pipe2sy] ; Y
    push pipelen ; Width
    push [pipe2h] ; Height
    push pipecol ; Color
    call rectDraw
   	ret
endp drawPipes

proc drawBird
	push birdx ; Push parameter
	push [birdy] ; Push parameter
	push birdrad ; Push parameter
	push birdcol ; Push parameter
	call circle ; Draw circle
	ret
endp drawBird

proc colCheck
	push ax ; Save register
	cmp [pipex], birdx ; Compare to see if colision is from the right or from the left
	ja lt ; jump to label (if collision is from the left)

	rt: ; else, check for collision from the right
	mov ax, birdx ; set ax as bird x
	sub ax, [pipex] ; subtract the pipe x to see the distance
	cmp ax, colb ; check if distance is smaller than a pre-determined collision check distance
	ja cont ; if it isnt, continue normally

	; If it is:
	mov ax, [pipe1h] ; set ax to pipe1h
	add ax, birdrad ; add the birds radius
	cmp [birdy], ax ; compare calculated value to birds y pos
	jb exitjmpp ; if bird is above that position there has been a collision

	; If bird is above that position:
	mov ax, 200 ; set ax to 200
	sub ax, [pipe2h] ; subtract pipe 2's height
	sub ax, birdrad ; and birds radius to get remaining height
	cmp [birdy], ax ; compare calculated value to birds y pos
	ja exitjmpp ; if bird is below that point there has been a collision

	ja cont ; if none of the above happened, continue normally

	lt: ; check for collision from the left
	mov ax, [pipex] ; set ax as bird x
	sub ax, birdx ; subtract the pipe x to see the distance
	cmp ax, colb ; check if distance is smaller than a pre-determined collision check distance
	ja cont ; if it isnt, continue normally

	; If it is:
	mov ax, [pipe1h] ; set ax to pipe1h
	add ax, birdrad ; add the birds radius
	cmp [birdy], ax ; compare calculated value to birds y pos
	jb exitjmpp ; if bird is above that position there has been a collision

	; If bird is above that position:
	mov ax, 200 ; set ax to 200
	sub ax, [pipe2h] ; subtract pipe 2's height
	sub ax, birdrad ; and birds radius to get remaining height
	cmp [birdy], ax ; compare calculated value to birds y pos
	ja exitjmpp ; if bird is below that point there has been a collision

	; if none of the above happened, continue normally

	cont:
	pop ax ; Pop register
	ret

endp colCheck

;-------CheckPoint4--------
jmp mno

exitjmpp:
	jmp exit

mno:
;-------------------------

proc scrCheck ; Proc for checking if score should be increased
	push ax ; Save register
	cmp [pipex], birdx ; Compare to see if bird is to the right or to the left of the pipe
	ja left ; jump to label (if bird is to the left of the pipe)

	right:
	mov ax, birdx ; set ax as bird x
	sub ax, [pipex] ; subtract the pipe x to see the distance
	cmp ax, 5 ; check if bigger than 5
	ja cnt ; if it is, continue normally

	inc [score] ; Increase score by 1

	jmp cnt


	left:
	mov ax, [pipex] ; set ax as pipe x
	sub ax, birdx ; subtract the bird x to see the distance
	cmp ax, 5 ; check if bigger than 5
	ja cnt ; if it is, continue normally

	inc [score] ; Increase score by 1

	cnt:
	pop ax ; Pop register
	ret

endp scrCheck



;-------------------
start:
mov ax, @data
mov ds, ax
;-------------------



call randInt ; call random int proc
mov [crnt], dx ; set value to variable

call toText

startScrn:
	xor ax,ax
	mov dl, 33 ; Column
	mov dh, 1 ; Row
	mov bh, 0h ; set as 0 for interrupt
	mov ah, 02h ; set as 2 for interrupt
	int 10h ; Interrupt

	mov ah, 9h ; Value for interrupt
	mov bl, 10 ; Color (green)
	mov cx, 11 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg1 ; set as offset of message
	int 21h ; Interrupt

	xor ax,ax
	mov dl, 29 ; Column
	mov dh, 2 ; Row
	mov bh, 0h ; set as 0 for interrupt
	mov ah, 02h ; set as 2 for interrupt
	int 10h ; Interrupt

	mov ah, 9h ; Value for interrupt
	mov bl, 8 ; Color (grey)
	mov cx, 3 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg2 ; set as offset of message
	int 21h ; Interrupt

	mov ah, 9h ; Value for interrupt
	mov bl, 9 ; Color (blue)
	mov cx, 15 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg3 ; set as offset of message
	int 21h ; Interrupt

	mov ah, 9h ; Value for interrupt
	mov bl, 4 ; Color (red)
	mov cx, 6 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg4 ; set as offset of message
	int 21h ; Interrupt

	mov ah, 9h ; Value for interrupt
	mov bl, 7 ; Color (light grey)
	mov cx, 70 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg5 ; set as offset of message
	int 21h ; Interrupt

	xor ax,ax
	mov dl, 27 ; Column
	mov dh, 16 ; Row
	mov bh, 0h ; set as 0 for interrupt
	mov ah, 02h ; set as 2 for interrupt
	int 10h ; Interrupt

	mov ah, 9h ; Value for interrupt
	mov bl, 13 ; Color (green)
	mov cx, 24 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg6 ; set as offset of message
	int 21h ; Interrupt



;Wait for key input
xor ax,ax ; Clear ax
mov ah,01h ; set as 1 for interrupt
int 21h; Interrupt

call toGraphics ; Go to graphics mode
call wloop ; start the while loop

xor ah, ah
int 16h

call toText ; goto text mode

;-------------------
exit:
call clrScreen; clear the screen
call toText ; goto text mode

endScrn:
	cmp [score], 5
	ja goodScr

	badScr:
	mov dx,10 ; Beginning of line	
	mov ah,2h
	int 21h
	mov dx,13 ; New line
	mov ah,2h
	int 21h
	
	mov ah, 9h ; Value for interrupt
	mov bl, 15 ; Color (white)
	mov cx, 15 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg7 ; set as offset of message
	int 21h ; Interrupt

	mov ax, [score]
	push ax
	call printDigits

	mov ah, 9h ; Value for interrupt
	mov bl, 12 ; Color (green)
	mov cx, 22 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg8 ; set as offset of message
	int 21h ; Interrupt


	jmp finend

	goodScr:
	mov dx,10 ; Beginning of line	
	mov ah,2h
	int 21h
	mov dx,13 ; New line
	mov ah,2h
	int 21h

	mov ah, 9h ; Value for interrupt
	mov bl, 15 ; Color (white)
	mov cx, 15 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg7 ; set as offset of message
	int 21h ; Interrupt

	mov ax, [score]
	push ax
	call printDigits ; Print score

	mov ah, 9h ; Value for interrupt
	mov bl, 2 ; Color (green)
	mov cx, 10 ; Number of chars
	int 10h ; Interrupt
	mov dx, offset msg9 ; set as offset of message
	int 21h ; Interrupt


	finend:


mov ax, 4c00h
int 21h
END start