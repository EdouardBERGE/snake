
RELEASE_DSK    = 1
RELEASE_TAPE   = 1
RELEASE_AMSDOS = 1

org #100
run #100

di
ld bc,#7F00+%10001101 : out (c),c ; MODE 1 !!!
ld bc,#FA7E : out (c),0 ; Floppy MOTOR OFF
ld hl,mycode
ld de,#1000

;**************************************************
;**************************************************
; official LZ48 decrunch routine, modified to jump
; into application after decrunch
;**************************************************
;**************************************************
LZ48_decrunch
        ldi
        ld b,0

nextsequence
        ld a,(hl)
        inc hl
        cp #10
        jr c,lzunpack ; no literal bytes
        ld ixl,a
        and #f0
        rrca
        rrca
        rrca
        rrca

        cp 15 ; more bytes for literal length?
        jr nz,copyliteral
getadditionallength
        ld c,(hl) ; get additional literal length byte
        inc hl
        add a,c ; compute literal length total
        jr nc,lengthNC
        inc b
lengthNC
        inc c
        jr z,getadditionallength ; if last literal length byte was 255, we have more bytes to process
copyliteral
        ld c,a
        ldir
        ld a,ixl
        and #F
lzunpack
        add 3
        cp 18 ; more bytes for match length?
        jr nz,readoffset
getadditionallengthbis
        ld c,(hl) ; get additional match length byte
        inc hl
        add a,c ; compute match length size total
        jr nc,lengthNCbis
        inc b
lengthNCbis
        inc c
        jr z,getadditionallengthbis ; if last match length byte was 255, we have more bytes to process
readoffset
        ld c,a
; read encoded offset
        ld a,(hl)
        inc a
        ;ret z ; LZ48 end with zero offset
	jp z,init
        inc hl
        push hl
; source=dest-copyoffset
        ; A != 0 here
        neg
        ld l,a
        ld h,#ff
        add hl,de
copykey
        ldir

        pop hl
        jr nextsequence


mycode

;**************************************************
;**************************************************
;         game code relocated and crunched
;**************************************************
;**************************************************

org #1000,$
lz48

;*******************************************
; print routines
;*******************************************
SCREEN_WIDTH equ 64
DISPLAY_REFERENCE equ #C000
displaylineadr defw DISPLAY_REFERENCE
displaycharadr defw DISPLAY_REFERENCE

macro locate posx,posy
 ld hl,DISPLAY_REFERENCE+{posy}*SCREEN_WIDTH
 ld (displaylineadr),hl
 ld hl,DISPLAY_REFERENCE+{posx}+{posy}*SCREEN_WIDTH
 ld (displaycharadr),hl
mend


; A=char to display
displaychar
ld h,0
add a
ld l,a
add hl,hl
add hl,hl
ld de,minifonte
add hl,de
ex hl,de ; sprite char ADR
ld hl,(displaycharadr)
ld bc,#808
displaycharline
ld a,(de) ; donnee du caractere
inc e
ld (hl),a
ld a,h
add c
ld h,a
djnz displaycharline

ld hl,(displaycharadr)
inc hl
ld (displaycharadr),hl
ret

; a=hex value
displayhexa
push af
rrca : rrca : rrca : rrca : and 15
call displaychar
pop af
and 15
call displaychar
ret

; hl=string adr
; 255 -> EOS
; 254 -> CR
displaystring
ld a,(hl)
cp 255
ret z
push hl
call displaychar
pop hl
inc hl
jr displaystring

align 8
minifonte
incbin 'minifonte.bin'

;****************************************************
;      initialisation CRTC, colors, PPI
;****************************************************
crtc_data defb 63,32,42,#8E,38,0,32,34,0,7,0,0,#30,00

init
xor a
ld bc,#F782 ; PPI output A, output C
out (c),c
ld d,a
ld e,a
ld b,#bc
ld hl,crtc_data

; loop is useless when we have memory and a good cruncher
repeat 14
out (c),a : inc b : inc b : outi : inc a : dec b
rend

ld sp,0
ld bc,4096
.razscreen
push de
push de
push de
push de
cpi
jp pe,.razscreen

;****************************************************
;    draw white borders around the entire screen
;****************************************************
ld hl,#C000 : ld de,64 : exx : ld hl,#C000+63 : ld de,64 : exx : ld b,0
.vert ld (hl),%10001000 : res 6,h : ld (hl),%10001000 : set 6,h : add hl,de : exx : ld (hl),%10001 : res 6,h : ld (hl),%10001000 : set 6,h : add hl,de : exx : djnz .vert

ld hl,#C000 : ld de,#FFFF-63 : ld b,64 : ld a,d
.horiz ld (hl),a : ld (de),a : res 6,h : res 6,d : ld (hl),a : ld (de),a : set 6,h : set 6,d : inc l : inc e : djnz .horiz

call player.init

ld bc,#7F03 : out (c),c : ld a,64+11 : out (c),a
      dec c : out (c),c : ld a,64+21 : out (c),a
      dec c : out (c),c : ld a,64+31 : out (c),a
      dec c : out (c),c : ld a,64+20 : out (c),a
    set 4,c : out (c),c : out (c),a

;****************************************************
;     display some messages for startup screen
;****************************************************
locate 32-4,12
ld hl,str_title
call displaystring

locate 32-12,14
ld hl,str_player1
call displaystring
locate 32-11,15
ld hl,str_nbplayer2
call displaystring
locate 32-11,16
ld hl,str_nbplayer3
call displaystring
locate 32-11,17
ld hl,str_nbplayer4
call displaystring
locate 32-12,18
ld hl,str_any
call displaystring

locate 32-14,30
ld hl,str_copy
call displaystring

.waitnbplayer
call scankeyboard
ld hl,keyboardbuffer : ld b,10 : ld de,.nbplayertab
.testnbplayer
ld a,(hl) : cp #FF : jr nz,.nbplayerok
inc hl : inc de : djnz .testnbplayer
jr .waitnbplayer

; keyboard line pressed gives the number of players
; player1 keyboard lines 0,1
; player2 keyboard line  7
; player3 keyboard line  5
; player4 keyboard line  4
.nbplayertab defb 1,1,1,1,4,3,1,2,1,1

.nbplayerok
ld a,(de)
ld (player.manage+1),a

; raz messages before play
ld hl,#C000+64
inc l
ld a,30
.raznbline
exa : ld a,8
.raznbplayer
push hl
ld de,hl : inc e
ld bc,61
ld (hl),0 : ldir
pop hl
ld bc,#800
add hl,bc
dec a
jr nz,.raznbplayer
ld bc,64-#4000
add hl,bc
exa
dec a
jr nz,.raznbline

ld bc,#7F01 : out (c),c : ld a,64+4 : out (c),a
;-----------------------------------------------------
                        main
;-----------------------------------------------------
; wait VBL and NO-VBL because the game is consumming
; less than 3% of CPU with one player...
;-----------------------------------------------------
ld b,#F5
.vbl in a,(c) : rra : jr nc,.vbl
.novbl in a,(c) : rra : jr c,.novbl

call player.manage
call scankeyboard
;-------------------------------------------------
;   change angle modifier according to keyboard
;-------------------------------------------------
ld a,(keyboardbuffer) : bit 1,a : jr nz,.skipleft ; cursor
ld a,1 : ld (player.coordz+1),a
jr .endkey
.skipleft
ld a,(keyboardbuffer+1) : rra : jr c,.endkey ; cursor
ld a,#FF : ld (player.coordz+1),a
.endkey

ld a,(keyboardbuffer+7) : bit 6,a : jr nz,.skipleft2 ; C
ld a,1 : ld (player.coordz+1+14),a
jr .endkey2
.skipleft2
ld a,(keyboardbuffer+7) : bit 7,a : jr nz,.endkey2 ; X
ld a,#FF : ld (player.coordz+1+14),a
.endkey2

ld a,(keyboardbuffer+5) : bit 5,a : jr nz,.skipleft3 ; J
ld a,1 : ld (player.coordz+1+28),a
jr .endkey3
.skipleft3
ld a,(keyboardbuffer+5) : bit 4,a : jr nz,.endkey3 ; H
ld a,#FF : ld (player.coordz+1+28),a
.endkey3

ld a,(keyboardbuffer+4) : bit 2,a : jr nz,.skipleft4 ; J
ld a,1 : ld (player.coordz+1+42),a
jr .endkey4
.skipleft4
ld a,(keyboardbuffer+4) : bit 3,a : jr nz,.endkey4 ; H
ld a,#FF : ld (player.coordz+1+42),a
.endkey4


;------------------------------------
; this counter let us know where
; snakes leave holes
;------------------------------------
ld hl,.scronch+1 : inc (hl)


;-----------------------------------
; loop each player
;-----------------------------------
ld ix,player.coordz
ld a,(player.manage+1)
ld yh,a
.pushdisplay
push ix
	ld a,(ix+13) : or a : jr z,.nochange
	ld a,(ix+10) : or a : jr z,.nochange
	ld e,(ix+2+1)
	ld d,(ix+6+1)
	;--------------------------------------------------------------
	; according to player coordinates, get screen adress and masks
	;--------------------------------------------------------------
	call prepare_plot
	push af ; backup pixel mask
	and #FF ; pen 3
	ld e,a
	exa
	or e
.scronch ld e,0
	rl e : rl e : jr nc,.display : rl e : jr nc,.display ; <192 is ok
	and #F ; pen 2
	.display
	ld c,a    ; backup color
	ld (hl),a ; push pixel on screen
	;
	res 6,h ; => transcription to #8000 page
	pop af  ; retrieve back pixel mask

	ld d,a
	ld e,(hl)
	and e
	cp d ; is it PEN 3 == collision?
	jr nz,.notdead
	ld (ix+13),0
	jr .nochange

	.notdead
	; E still contains the 2 bits of the pixel
	ld a,d : and #F : ld d,a ; we keep only low bits which represent high bit of the mask
	ld a,e : and d : cp d ; is the pixel using PEN 2?
	jr nz,.nopoint
	;---------------------------
	; increase score only if
	; the previous position is
	; not a score position
	;---------------------------
	ld a,(ix+11)
	or a
	jr nz,.nopoint
	ld (ix+11),1
	inc (ix+12) ; score++
	jr .afternopoint

	.nopoint
	ld (ix+11),0
	.afternopoint
	;---------------------------------
	; apply the pixel to collision map
	;---------------------------------
	ld a,(hl)
	or c
	ld (hl),a
	.nochange
pop ix
ld de,player.coordz_end-player.coordz
add ix,de
dec yh
jr nz,.pushdisplay

;*** all dead? ***
ld ix,player.coordz
ld a,(player.manage+1)
ld b,a
xor a
.testdead
or (ix+13)
add ix,de
djnz .testdead

or a
jp nz,main

;-----------------------------------------------------
; display score of active players
;-----------------------------------------------------

ld bc,#7F01 : out (c),c : ld a,64+31 : out (c),a

ld hl,#C000+15*64+26
ld ix,player.coordz
exx
ld a,(player.manage+1)
ld b,a : ld c,0
ld de,player.coordz_end-player.coordz
.affichescores
exx
push hl
ld (displaycharadr),hl

ld hl,str_player : call displaystring
exx : inc c : ld a,c : exx
call displayhexa
ld hl,displaycharadr : inc (hl) : inc (hl)

ld a,(ix+12)
call displayhexa

pop hl
ld de,64
add hl,de
exx
add ix,de
djnz .affichescores

ld bc,#F540
.wvbl in a,(c) : rra : jr nc,.wvbl
.wnovbl in a,(c) : rra : jr c,.wnovbl
dec c : jr nz,.wvbl

; press a key (scan all lines)
.wkey
call scankeyboard
ld hl,keyboardbuffer
ld bc,#AFF
ld a,c
.tkey and (hl) : inc hl : djnz .tkey
cp c : jr z,.wkey

jp init


; DE=player coordz
; return
; HL=screen adress
; A=background with mask applied
; A'=pixel mask
prepare_plot
ld a,d ; Y first
and %11111000
ld h,#C0>>3
ld l,a
add hl,hl : add hl,hl : add hl,hl
ld a,d
and %111 : add a : add a : add a
or h : ld h,a
; Y line adress in HL
ld a,e
srl a
srl a
add l
ld l,a ; there is never a carry with a screen that size
; HL contains pixel adress
ld a,e
and %11
add a
ld e,a
ld d,hi(pixel)
ld a,(de) ; get background mask
and (hl)
exa
inc e
ld a,(de) ; pixel mask
ret


player
.init
ld hl,player.coordz
ld de,player.coordz+1
ld bc,14*4-1
ld (hl),0
ldir
;
ld ix,player.coordz
ld (ix+0),64 ; to the right
ld (ix+1),1
ld (ix+2+1),20
ld (ix+6+1),20
ld (ix+13),1

ld bc,player.coordz_end-player.coordz
add ix,bc

ld (ix+0),192 ; to the left
ld (ix+1),1
ld (ix+2+1),236
ld (ix+6+1),236
ld (ix+13),1

add ix,bc

ld (ix+0),128 ; downward
ld (ix+1),1
ld (ix+2+1),236
ld (ix+6+1),20
ld (ix+13),1

add ix,bc

;ld (ix+0),0 ; upward
ld (ix+1),1
ld (ix+2+1),20
ld (ix+6+1),236
ld (ix+13),1
ret

;---------------------------------------------------
; for each player, apply sinus to position according
; to the current angle. Also apply current direction
; to the current angle
;---------------------------------------------------
.manage
ld b,0
ld de, .coordz_end-.coordz
ld ix,.coordz
.manaloop

exx
ld a,(ix+0) : add (ix+1) : ld e,a : ld (ix+0),a
ld d,hi(sinus)
;********
ld a,(de)
ld c,a
add a
sbc a
ld b,a
ld hl,(ix+2)
ld (ix+4),hl ; set previous value 
ld a,h
add hl,bc
sub h : ld (ix+10),a ; monitor coordinates changes
ld (ix+2),hl
;********
ld a,192
add e
ld e,a
ld a,(de)
ld c,a
add a
sbc a
ld b,a
ld hl,(ix+6)
ld (ix+8),hl ; set previous value
ld a,h
add hl,bc
sub h : or (ix+10) : ld (ix+10),a ; monitor coordinates changes
ld (ix+6),hl

exx
add ix,de
djnz .manaloop
ret


.coordz
defb 0,0 ; ang,increment
defw 0,0,0,0 ; x,y,prevx,prevy
defb 0,0 ; change/previouspixeltype
defb 0 ; score
defb 0 ; alive!
.coordz_end

nop 14 ; player 2
nop 14 ; player 3
nop 14 ; player 4


;******************************************
               scankeyboard
;******************************************
ld bc,#F40E ; PPI register 14
out (c),c
ld bc,#F6C0 ; we want to select a register
out (c),c
out (c),0   ; finish operation

ld bc,#F792 ; PPI configuration input A, output C
out (c),c
ld hl,keyboardbuffer

ld de,#40F4 ; d=#40 counter / e=#F4 read port

; loop is useless when we have memory and good cruncher
repeat 10,x
if x==1
  dec b
  ld a,b
else
  ld b,a
endif
out (c),d ; write keyboard line
ld b,e
ini       ; read value to (hl++)
if x<10
  inc d
endif
rend

ld bc,#F782 ; PPI output A, output C
out (c),c
ret

keyboardbuffer: DEFS 10,#C9 ; initialized to "ret" to icnrease compression ratio


charset '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-:.! =/\\[]_^<>@()',0

str_title     defb 'SNAKE v1.0',255
str_mauvais   defb 'GAME OVER',255
str_nbplayer2 defb 'Press X/C for 2 players',255
str_nbplayer3 defb 'Press H/J for 3 players',255
str_nbplayer4 defb 'Press I/O for 4 players',255
str_any      defb 'Any other key to play solo',255
str_player1  defb 'Player 1 uses cursor keys',255
str_copy     defb 'Resistance game department 2021',255
str_player defb 'Player ',255

charset


align 256
sinus
ang=0
repeat 256
defb sin(ang)*127
ang=ang+360/256
rend

pixel
defb %01110111,%10001000
defb %10111011,%01000100
defb %11011101,%00100010
defb %11101110,%00010001
lzclose
org $

lafin

if RELEASE_DSK
save"snake.bin",#100,lafin-#100,DSK,"snake.dsk"
endif

if RELEASE_TAPE
save"snake.bin",#100,lafin-#100,TAPE,"snake.cdt"
endif

if RELEASE_AMSDOS
save"snake.bin",#100,lafin-#100,AMSDOS
endif


