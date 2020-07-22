 ;=============================================================================
 ; Scrolling starfield demo
 ;
 ; By Dennis Katsonis, July 2020
 ; Written for DOS.  Compile with FASM.
 ;
 ; This is a basic demonstration of VGA graphics.  It draws
 ; colourful stars that scroll accross the screen.
 ; Demonstrates use of writing to the video buffer,
 ; setting the VGA pallet, waiting for a vertical retrace
 ; and other stuff.
 ;
 ; Should run on a 286 processor or higher.
 ;
 ; Version 1.0
 ;=============================================================================

  use16
  
  rightpos equ 319 ; X position of rightmost pixel
  inputlen equ 8 ; We are going to ask for the number or stars.  6 digit max
  ; (two bytes are required for the CR/LF
  defaultnum equ 500 ; The default number of stars.
  maxstars equ 5000 ; Setting this much more than 5000 may
  ; end up with this program overwriting the stack.
  ; Don't do it without finding a better place for the stack and relocating
  ; it first.
  numcol equ 16 ; How many bands of colours

struc STAR {
  .x dw ?	; X position
  .y dw ?	; Offset (position on screen)
  .colour dw ?	; Colour
  .speed dw ?	; This is how many cycles it takes to move one space
  .count dw ?	; This is how many cycles have completed since last move
}

virtual at 0
 STAR STAR
 sizeof.STAR = $ - STAR
end virtual

 ;=============================================================================
 ; PROGRAM CODE
 ;=============================================================================

org 100h
	mov ah, 00h   ; interrupt to get system timer in CX:DX 
	int 1ah
	mov [PRN], dx
  
	mov ax,0A000h	; Start address of VGA mode 13h buffer

	mov es,ax

	mov ah,09h
	mov dx,intro
	int 21h  ; Print introductory text.

	mov cx,inputlen ; Input length
	xor ax,ax
	mov ah,3fh
	mov bx,0
	mov dx,input  ; Where to store
	int 21h
	jc endp
	cmp ax,6
	jg inputerror  ; Anything more than 6 characters we reject

	mov bx, ax ; BX will store the offset as we go through the input data.
	cmp [bx+input-1],0ah ; If this is not the last character
			     ; It means we didn't fill all the input
			     ; and some may be lost.  Best to end with
			     ; an error.  Besides, if it was a valid
			     ; number, it would have been well over the
			     ; maximum number of stars we could handle.
	jne inputerror

	xor bx, bx  ; Start index at 0.
	sub ax, 2 ; We don't want the CR/LF duo at the end.
	mov cx, ax   ; CX will be used to compare the index
	; to determine whether we have reached the end of the user data
	; we want to convert.
	xor ax, ax ; Start tally at 0.
	mov si, 10   ; Multiply each number we get by 10, as we push it up
		    ; as a significant digit.
	mov di, input
.getnum:
	cmp byte [di+bx], '0'  ; If less than '0', its not a numeral.
	jl inputerror
	cmp byte [di+bx], '9'  ; If more than '9', its not a numeral.'
	jg inputerror
	sub byte [di+bx], 30h  ; Sub 30h to convert the ascii numeral
			       ; to the number
	mul si		       ; Multiply what we have added so far by 10
	push dx
	mov dl, byte [di+bx]
	add ax,dx	       ; And add the next digit
	pop dx
	inc bx
	cmp bx, cx	     
	jl .getnum	      ; If more digits, go again.
	cmp ax, maxstars      ; Error if more than maxstars
			    ; If there are too many stars, the buffer
			    ; of star information will overwrite the stack.
			    ; Set 5000 max to ensure the stack
			    ; is not overwritten.
			    ; We could relocate the stack at the start, and
			    ; store star data after, not limiting us.  But
			    ; having that number of stars doesn't look that good
			    ; anyway.
	jg inputerror
	mov [numstars], ax

	; This will set the speeds of the stars.  The speeds are weighted
	; to prefer slower star, so there are more "background" stars than
	; "foreground" stars.
	; We are setting a table with the speeds 1 - 16 distributed
	; the way we want.  We will refer to this table to select
	; a random star speed.	Note that 16 is the slowest star, 1 the fastest.

begindemo:
	mov di,buffer ; Point to buffer
	mov ax,10     ; We start with 10 repetitions
	mov bx,1      ; And we write speed of 1
	mov cx,16     ; Sixteen speed gradients
.cloop1:
	push cx
	mov cx,ax    ; Store the number of times this speed is repeated.
@@:
	mov [di],bx  ; And write to buffer
	inc di	     ; Next buffer position
	loop @b
	pop cx	     
	inc ax	     ; Increase the number of repetitions by 1.
	inc bx	     ; As well as the speed value
	loop .cloop1
	    ;  There is now a table of 306 bytes.

	mov cx, [numstars]
	mov si, stars	; Set SI to start of stars buffer
.setstars:
	mov bx,319	; Random number between 1 and 319
	call GetRandom
	mov [ex],dx	; This is its x position.
	mov bx,199	; Random number between 1 and 199
	call GetRandom
	shl dx,6	; Use shifts to multiply by 320.
	mov ax,dx
	shl dx,2
	add ax,dx

	add ax, [ex]
	mov [why],ax	; This is its offset in the video buffer.

	mov bx,309
	call GetRandom
	mov bx,dx	    ; Pick a random position from the buffer
			    ; of distributed speed values.
	mov byte al, [ds:buffer+bx]
	mov byte [spe], al  ; Save as speed.  Because the values in
			    ; the buffer are weighted to have more
			    ; higher value  (more slower stars)
			    ; the likelihood of this number being
			    ; lower is increased.

	call setstar		; Initialise star
	add si, sizeof.STAR	; Increment to next star

	loop .setstars	; Stars are initialised.

	mov ax,1200h	; Test for VGA card
	mov bl,36h
	int 10h
	cmp al,12h	
	jne novga	; Exit if not detected

	mov ax,0013h
	int 10h 	; Set 320 x 200 x 256
			; Mode 13h, good ol' VGA.

	call InitColours

startloop:
.loops1:
	call WAIT_RETRACE

	mov si, stars
	mov cx, [numstars]
@@: ; Update position of stars
	call updatestar
	add si,sizeof.STAR
	loop @b

	mov cx, [numstars]
	mov si, stars
@@: ; Erase stars one by one
	call erasestar

	add si, sizeof.STAR
	loop @b
  
	mov cx, [numstars]
	mov si, stars
@@:
	call drawstar

	add si,sizeof.STAR

	loop @b
	sti ; Enable interrupts
	mov ah,01h
	int 16h
	cli ; Disable interrupts
;	call delay
	jz startloop

	xor ah,ah
	mov al,03h
	int 10h ; Set back to text video mode 80x25
endp:
	mov ax,4c00h	     ; Send exit code to dos
	int 21h 	    ; Send command to DOS

 ;=============================================================================
 ; No VGA reports that a VGA card could not be found and exits.
 ;=============================================================================

novga:
	mov dx,novgamsg ;and tell the user
	mov ah,09h
	int 21h
	jmp endp     ; Exit

 ;=============================================================================
 ; Handles invalid input from operator.   Just displays a warning.
 ; Use the default number of stars if the input is not valid.
 ;=============================================================================

inputerror:
	mov dx,invalid ;Tell user about invalid input
	mov ah,09h
	int 21h
	mov ah,10h
	int 16h   ; Wait for keypress
	jmp begindemo

 ;=============================================================================
 ; Updates star pointed at by register SI.
 ; First determines whether a star is due to move in this frame, and if so,
 ; updates its position.
 ;=============================================================================

updatestar:
	mov dx,[si+STAR.speed]
	mov bx,[si+STAR.count]
	cmp bx,dx ; If count is less than speed, we do NOT change position
	jle .dontdrawyet
	  ; Ready to draw!

	mov [si+STAR.count],0 ; Reset the count to zero
	mov ax,[si+STAR.y]
	dec ax ; Move left one pixel
	jnz .n1 ; And if we have moved to offset zero?
	mov bx,199 ; Get new offset
	call GetRandom
	xor ax,ax  ; Multiply by 320 to get absolute offset in buffer
	shl dx,6
	add ax,dx
	shl dx,2
	add ax,dx
	mov bx, 319 ; An add x offset
	call GetRandom
	add al,dl
.n1:
	mov word [si+STAR.x],rightpos ; Store the new pixel value
	mov word [si+STAR.y],ax ; Store new offset.
	inc [si+STAR.count] ; Increment count.
	ret
.dontdrawyet:
	inc [si+STAR.count] ; Increment count.
	ret

 ;=============================================================================
 ; Draws the star pointed at by register SI.
 ;=============================================================================

drawstar:
	mov bx,[si+STAR.y] ; Get y position.
	mov al, byte [si+STAR.colour] ; Make pixel the selected colour
	mov byte [es:bx], al	      ; and store...
	ret

 ;=============================================================================
 ; Erase the star pointed at by resgister SI by setting the pixel to black.
 ; A more sophisticated variant would restore the original colour underneath.
 ;=============================================================================

erasestar:
	mov bx, [si+STAR.y]  ; Get star position
	mov byte[es:bx+1], 0 ; Set to black
	ret

 ;=============================================================================
 ; Wait for vertial retrace, for flicker free animation when drawing.
 ;=============================================================================

WAIT_RETRACE:
	mov dx,3dah
@@:
	in al,dx
	test al,8h
	jnz @b
@@:
	in al,dx
	test al,8h
	jz @b
	ret

 ;=============================================================================
 ; Set up parameters for a new star.
 ; Variables are...
 ; ex = x offset.
 ; why = offset in buffer (ex * 200) + ex
 ; colo = pixel colour
 ; spe = Speed, or more accurated, number of frames to wait before moving one
 ; position to the left.
 ;=============================================================================

setstar:
	mov word ax, [ex]
	mov word [si], ax  ; Store 'ex' as new x value

	mov word ax, [why]
	mov word [si+STAR.y], ax ; And 'why' as y value.

	mov word ax, [colo]
	mov word [si+STAR.colour], ax ; and colour

	mov word ax, [spe]
	mov word [si+STAR.speed], ax ; and speed
	mov word [si+STAR.colour],ax ; Colour = speed

	mov ax,16
	mov bx,5
	 ; and a random number between 1 and 5
	call GetRandom

	mul dl ; Multiply by that random number to get an offset
	add [si+STAR.colour], ax ; And add that to the ax value which is speed
	; We now have a shade which is based on the speed of the star
	; With slower stars darker to appear further away.
	mov word [si+STAR.count], 0
	ret

 ;=============================================================================
 ; Get somewhat random number.
 ; BX contains the upper limit.
 ; DX will return the random number.
 ;=============================================================================

GetRandom:
	push ax
	mov ax, 25173	       ; LCG Multiplier
	mul word [PRN]	   ; DX:AX = LCG multiplier * seed
	add ax, 13849	       ; Add LCG increment value
	; Modulo 65536, AX = (multiplier*seed+increment) mod 65536
	mov [PRN], ax	       ; Update seed = return value
	xor dx,dx
	div bx
	pop ax
	ret	

 ;=============================================================================
 ; Initialises the colour pallette.
 ;=============================================================================

InitColours:	       ; Set up the colours we will use.
  
	mov dx,3c8h	; Output to video card we as setting
	mov al,1	; colours from colour 0 up
	out dx,al
	inc dx		;change dx to point to pallete
	xor bx,bx
	xor ax,ax
	mov cx,numcol
	mov al,63
.setgrey:	     ;Set 16 shades of gray
	out dx,al
	out dx,al
	out dx,al
	sub al,4
	loop .setgrey

	mov cx,numcol
	mov al,63
	xor bl,bl
.setyellow:	      ;Set 16 shades of yellow
	out dx,al
	out dx,al
	push ax
	mov al,bl
	out dx,al
	pop ax
	sub al,4
	loop .setyellow

	mov cx,numcol
	mov al,63
.setcyan:	      ;Set 16 shades of cyan
	push ax
	mov al,bl
	out dx,al
	out dx,al
	pop ax
	out dx,al
	sub al,4
	loop .setcyan

	mov cx,numcol
	mov al,63
.setred: 	     ;Set 16 shades of red
	out dx,al
	push ax
	mov al,bl
	out dx,al
	out dx,al
	pop ax
	sub al,4
	loop .setred
	ret

;=============================================================================
; PROGRAM DATA
;=============================================================================

novgamsg  db "You need a VGA card!  Sorry!",0ah,0dh,024h
intro	  db "**** Simple Scrolling Star Demo ****",0ah,0dh,\
	     "By Dennis Katsonis.  July, 2020.",0ah,0ah,0dh,\
	     "Press any key during the demo to exit",0ah,0ah,0dh,\
	     "How many stars (Max 5000) ? ",24h
invalid   db "Invalid input or number too high.  ",\
	     "Using default number of stars.",0ah,0dh,\
	     "Press any key.",0ah,0dh,24h
input	  db	8
align	  2
ex	  dw	1
why	  dw	5
colo	  dw	1
spe	  dw	1
PRN	  dw	0
numstars  dw	defaultnum
align	  4
stars	  rb	sizeof.STAR*numstars
buffer	  rb	384 ; This buffer is for a numerical distribution.
  ; It is a little larger than we need.
	
