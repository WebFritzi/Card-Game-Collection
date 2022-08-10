!to "FCGC.d64",d64

!zone Constants
NO_OF_GAMES = 2; Currently only 2 titles [0-1] available
std_irq = $ea31
joy_delay = 13
VIC = $d000
sid = $d400
xPos0 = VIC
yPos0 = VIC+1
xPos1 = VIC+2
yPos1 = VIC+3
xPos2 = VIC+4
yPos2 = VIC+5
xPos3 = VIC+6
yPos3 = VIC+7
xPos4 = VIC+8
yPos4 = VIC+9
xPos5 = VIC+10
yPos5 = VIC+11
xPos6 = VIC+12
yPos6 = VIC+13
xPos7 = VIC+14
yPos7 = VIC+15
xposmsb  = VIC+16
col0  = VIC+39
col1  = VIC+40
col2  = VIC+41
col3  = VIC+42
col4  = VIC+43
col5  = VIC+44
col6  = VIC+45
col7  = VIC+46
CHAR_BASE = $2000
CHAR_NUMBERS_BASE = CHAR_BASE + $180
CL_BLACK = 0
CL_WHITE = 1
CL_RED = 2
CL_CYAN = 3
CL_MAGENTA = 4
CL_DARKGREEN = 5
CL_DARKBLUE = 6
CL_YELLOW = 7
CL_LIGHTBROWN = 8
CL_DARKBROWN = 9
CL_ROSE = 10
CL_DARKGREY = 11
CL_MIDGREY = 12
CL_LIGHTGREEN = 13
CL_LIGHTBLUE = 14
CL_LIGHTGREY = 15
SPRPTR_0 = 2040
SPRPTR_1 = 2041
SPRPTR_2 = 2042
SPRPTR_3 = 2043
SPRPTR_4 = 2044
SPRPTR_5 = 2045
SPRPTR_6 = 2046
SPRPTR_7 = 2047
DM_EXIT = 0
DM_WON = 1
DM_STATS = 2
DM_TOOMANYCARDS = 3
DM_INSTRUCTS = 4
DM_ABOUT = 5
DM_NEW = 6
GM_NORMAL = 0
GM_MENU = 1
GM_DIALOG = $ff
MM_GAME = 0
MM_HELP = 1
GM_MI_NEW = 0
GM_MI_UNDO = 1
GM_MI_STATS = 2
GM_MI_EXIT = 3
HM_MI_HELP = 0
HM_MI_ABOUT = 1
;Sprite Blocks
SP_Mouse0 = Mousepointer0Addr/64
SP_Mouse1 = Mousepointer1Addr/64
SP_MenuItemHighliter = MenuItemHighliterAddr/64


!zone BASIC_stub
*=$0801
;** BASIC-Zeile: 0 SYS2061
 !word main-2, 0
 !byte $9e
 !text "2061"
 !byte $00,$00,$00

!zone Main
main            lda #0
                sta $9d; no kernal messages ("SEARCHING FOR ..." etc)
                jsr MouseJoyChoice
                jsr Welcome
start_over      jsr InstallIRQ
                jsr SetGlobals
                jsr DrawMainMenu

!zone MainLoop
;Exit codes:
; 00000001 released fire button
; 00000010 moved mouse pointer
; 00000100 pushed fire button
; 00001000 load game
; 00010000 exit game to BASIC
MainLoop        lda bExit
                asl
                ora bLoadGame
                asl
                ora bFire
                asl
                ora bMoved
                asl
                ora bFireReleased
                beq MainLoop

                sta exit_code
                and #%00000001
                bne FireReleased;<--
                lda exit_code
                and #%00000010
                bne Moving      ;<--
                lda exit_code
                and #%00000100
                bne Fired       ;<--
                lda exit_code
                and #%00001000
                bne GoLoadGame  ;<--
                lda exit_code
                and #%00010000
                bne Exit        ;<--

FireReleased    lda #0
                sta bFireReleased
                jsr OnFireRelease
                jmp MainLoop

Moving          lda #0
                sta bMoved
                jsr OnMoved
                jmp MainLoop

Fired           lda #0
                sta bFire
                jsr OnFirePressed
                jmp MainLoop
                
GoLoadGame      lda #0
                sta bLoadGame
                jsr LoadGame
                lda MouseOn
                sta $4c00
                ; Restore standard graphics mode
                ; in particular: multicolor OFF
                jsr $4e00 ;<--- Start the game
                lda #0
                sta SelectedGame
                jmp start_over

Exit            lda #128
                sta $9d; kernal messages on
                lda #0
                sta bExit
                sta 198
                sta VIC+21
                lda #21
                sta $d018
                jsr $e544; clear screen
                jsr RestoreIRQ
                jsr MultiColorOff
                lda #CL_LIGHTBLUE
                sta $d020
                lda #CL_DARKBLUE
                sta $d021
                rts
;=============================================
!zone ____________Events______________
;============================================= OnFirePress
OnFirePressed   rts
;============================================= OnFireRelease
OnFireRelease   jsr IsInPlayBtnRect
                lda res
                bne btn_pressed
                jsr IsInTitlesRect
                lda res
                bne title_select
                rts
btn_pressed     lda SelectedGame
                cmp #NO_OF_GAMES
                bne +
                lda #1
                sta bExit
                rts
+               lda #1
                sta GameToLoad
                sta bLoadGame
                rts
title_select    lda VIC+1
                sec
                sbc #94
                cmp #239
                bcs no_select
                lsr
                lsr
                lsr
                lsr
                cmp #NO_OF_GAMES+1
                bcs no_select
                sta SelectedGame
                asl
                asl
                asl
                asl
                clc
                adc #94
                sta VIC+5
                sta VIC+7
                ;cmp #110 bcc FreecellSnap
                lda SelectedGame                
                cmp #NO_OF_GAMES
                bne +
                jsr ShowBASIC
                rts
+               ; No BASIC
                lda SelectedGame
                beq FreecellSnap
                jsr ShowSolitaire
                rts
FreecellSnap    jsr ShowFreecell
no_select       rts
;============================================= OnMouseMove
OnMoved         jsr CheckTitlesRect
                jsr CheckPlayBtn
                rts
;=============================================


!zone Behavior
ShowSolitaire   lda #<SnapSolitairMap
                sta $fb
                lda #>SnapSolitairMap
                sta $fc
                lda #24
                sta MapWidth
                lda #13
                sta MapHeight
                ldy #2
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                lda #1
                sta $d022
                rts
                
ShowFreecell    lda #<SnapFreecellMap
                sta $fb
                lda #>SnapFreecellMap
                sta $fc
                lda #24
                sta MapWidth
                lda #13
                sta MapHeight
                ldy #2
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                lda #1
                sta $d022
                rts

ShowBASIC       lda #<SnapBASICMap
                sta $fb
                lda #>SnapBASICMap
                sta $fc
                lda #24
                sta MapWidth
                lda #13
                sta MapHeight
                ldy #2
                ldx #6
                jsr PosToScrMem
                jsr DrawMap
                lda #14
                sta $d022
                rts

CheckPlayBtn    jsr IsInPlayBtnRect
                lda res
                eor bIsInPlayBtn
                bne changed
                rts
changed         lda res
                sta bIsInPlayBtn
                bne fill_btn
                jsr ClearPlayBtn
                rts
fill_btn        jsr FillPlayBtn
                rts
                
CheckTitlesRect jsr IsInTitlesRect
                lda res
                bne HighlightTitle
                lda #%00001111
                sta VIC+21
                rts
HighlightTitle  lda #%00111111
                sta VIC+21
                lda VIC+1
                sec
                sbc #94
                lsr
                lsr
                lsr
                lsr
                cmp #NO_OF_GAMES+1
                bcs no_highlight
                asl
                asl
                asl
                asl
                clc
                adc #94
                sta VIC+9
                sta VIC+11
no_highlight    rts

!zone Joystick
Joystick        jsr JoyDecoder
                bcs NoFire
                ;Fire pressed
                lda bFirePressed
                bne OnlyMove;fire was also pressed before
                lda #1;fire not pressed before
                sta bFirePressed
                sta bFire
                jmp OnlyMove
NoFire          ;Fire not pressed
                lda bFirePressed
                beq OnlyMove;fire was also not pressed before
                lda #0;fire was pressed before
                sta bFirePressed
                lda #1
                sta bFireReleased
OnlyMove        lda dx
                ora dy
                bne move
                rts
move            lda #1
                sta bMoved
                lda dx
                asl
                asl
                clc
                adc VIC
                sta VIC
                sta VIC+2
                lda dx
                bpl right
                ;left
                bcc ovf_lr
                lda $d010
                and #%00000011
                bne change_y
                lda VIC
                cmp #24
                bcs change_y
                lda #24
                sta VIC
                sta VIC+2
                jmp change_y
right           bcs ovf_lr

                lda $d010
                and #%00000011
                beq change_y
                lda VIC
                cmp #88
                bcc change_y
                lda #87
                sta VIC
                sta VIC+2
                jmp change_y
                
                jmp change_y
ovf_lr          lda $d010
                eor #%00000011
                sta $d010
change_y        lda dy
                asl
                asl
                clc
                adc VIC+1
                sta VIC+1
                sta VIC+3
                cmp #50
                bcc correct_yup
                cmp #250
                bcs correct_ydown
                rts
correct_yup     lda #50
                sta VIC+1
                sta VIC+3
                rts
correct_ydown   lda #249
                sta VIC+1
                sta VIC+3
                rts


JoyDecoder      lda $dc00     ; get input from port 2 only
                ldy #0        ; this routine reads and decodes the
                ldx #0        ; joystick/firebutton input data in
                lsr           ; the accumulator. this least significant
                bcs djr0      ; 5 bits contain the switch closure
                dey           ; information. if a switch is closed then it
djr0            lsr           ; produces a zero bit. if a switch is open then
                bcs djr1      ; it produces a one bit. The joystick dir-
                iny           ; ections are right, left, forward, backward
djr1            lsr           ; bit3=right, bit2=left, bit1=backward,
                bcs djr2      ; bit0=forward and bit4=fire button.
                dex           ; at rts time dx and dy contain 2's compliment
djr2            lsr           ; direction numbers i.e. $ff=-1, $00=0, $01=1.
                bcs djr3      ; dx=1 (move right), dx=-1 (move left),
                inx           ; dx=0 (no x change). dy=-1 (move up screen),
djr3            lsr           ; dy=1 (move down screen), dy=0 (no y change).
                stx dx        ; the forward joystick position corresponds
                sty dy        ; to move up the screen and the backward
                rts           ; position to move down screen.
                              ;
                              ; at rts time the carry flag contains the fire
                              ; button state. if c=1 then button not pressed.
                              ; if c=0 then pressed.

!zone Mouse
potx     = sid+$19
poty     = sid+$1a

maxx     = 319 ;Screen Width
maxy     = 199 ;Screen Height
offsetx  = 24  ;Sprite left border edge
offsety  = 50  ;Sprite top  border edge
acc      = 1   ;accelaration
musposx  !word 160
musposy  !word 100
old_yPos !byte 0
old_xPos !byte 0

Mouse           jsr GetClicks
                jsr scanmovs
                jsr boundmus
                rts

;---------------------------------------
GetClicks       lda $dc01
                cmp #$ef
                bne noFire
                ;Fire pressed
                lda bFirePressed
                bne out_here;fire was also pressed before
                lda #1;fire not pressed before
                sta bFirePressed
                sta bFire
                rts
noFire          ;Fire not pressed
                lda bFirePressed
                beq out_here;fire was also not pressed before
                lda #0;fire was pressed before
                sta bFirePressed
                lda #1
                sta bFireReleased
out_here        rts
;---------------------------------------

scanmovs        ;--- X Axis ---
                lda potx
oldpotx         ldy #0
                jsr movechk
                beq noxmove

                sty oldpotx+1

                clc
                adc musposx
                sta musposx
                txa            ;upper 8-bits
                adc musposx+1
                sta musposx+1
noxmove         ;--- Y Axis ---
                lda poty
oldpoty         ldy #0
                jsr movechk
                beq noymov

                sty oldpoty+1

                clc
                eor #$ff       ;Reverse Sign
                adc #1

                clc
                adc musposy
                sta musposy
                txa            ;Upper 8-bits
                eor #$ff       ;Reverse Sign
                adc musposy+1
                sta musposy+1
noymov          rts
;---------------------------------------
movechk         ;Y -> Old Pot Value
                ;A -> New Pot Value
                sty oldvalue+1
                tay
                sec

oldvalue        sbc #$ff
                and #%01111111
                cmp #%01000000
                bcs neg

                lsr a   ;remove noise bit
                beq nomove

                cmp #acc ;Acceleration Speed
                bcc *+3
                asl a   ;X2

                ldx #0
                cmp #0

                ;A > 0
                ;X = 0 (sign extension)
                ;Y = newvalue
                ;Z = 0

                rts

neg             ora #%10000000
                cmp #$ff
                beq nomove

                sec    ;Keep hi negative bit
                ror a  ;remove noise bit

                cmp #256-acc ;Acceleration Speed
                bcs *+3
                asl a       ;X2

                ldx #$ff

                ;A < 0
                ;X = $ff (sign extension)
                ;Y = newvalue
                ;Z = 0

                ;fallthrough

nomove          ;A = -
                ;X = -
                ;Y = -
                ;Z = 1
                rts

;---------------------------------------
boundmus        ldx musposx+1
                bmi zerox
                beq chky

                ldx #maxx-256
                cpx musposx
                bcs chky

                stx musposx
                bcc chky

zerox           ldx #0
                stx musposx
                stx musposx+1

chky            ldy musposy+1
                bmi zeroy
                beq loychk

                dec musposy+1
                ldy #maxy
                sty musposy
                bne movemus

loychk          ldy #maxy
                cpy musposy
                bcs movemus

                sty musposy
                bcc movemus

zeroy           ldy #0
                sty musposy
                sty musposy+1

movemus         lda xPos0
                sta old_xPos
                
                clc
                lda musposx
                adc #offsetx
                sta xPos0
                sta xPos1
                
                lda musposx+1
                adc #0
                beq clearxhi

                ;set x sprite pos high
                lda xposmsb
                ora #%00000011         
                bne *+7
         
clearxhi        ;set x sprite pos low
                lda xposmsb
                and #%11111100
                sta xposmsb
                
                lda xPos0
                cmp old_xPos
                beq make_ymove
                lda #1
                sta bMoved

make_ymove      lda yPos0
                sta old_yPos
                
                clc
                lda musposy
                adc #offsety
                sta yPos0
                sta yPos1

                cmp old_yPos
                beq no_ymove
                lda #1
                sta bMoved
no_ymove        rts

;---------------------------------------


!zone Preparations
;Once called, never changes
SetGlobals      ; Char set at $2000
                lda $d018
                and #%11110001
                ora #%00001000
                sta $d018

                ; Install mouse pointer sprites
                lda #SP_Mouse0
                sta SPRPTR_0
                lda #SP_Mouse1
                sta SPRPTR_1
                lda #CL_BLACK
                sta col0
                lda #CL_WHITE
                sta col1
                lda #100
                sta VIC
                sta VIC+2
                sta VIC+1
                sta VIC+3
                
                ; Install menu item highliter sprite
                lda #SP_MenuItemHighliter
                sta SPRPTR_2
                sta SPRPTR_3
                sta SPRPTR_4
                sta SPRPTR_5
                lda #CL_RED
                sta col4
                sta col5
                lda #CL_DARKBLUE
                sta col2
                sta col3
                ; x coords
                lda #246
                sta VIC+4
                sta VIC+8
                lda #26; +36 mod 256
                sta VIC+6
                sta VIC+10
                ; y coords
                lda #94
                sta VIC+5
                sta VIC+7
                sta VIC+9
                sta VIC+11
                ;stretch
                lda #%00111100
                sta VIC+29; stretch X
                ;select box behind text
                lda #%00111100
                sta VIC+27
                ;sprites high byte
                lda #%00101000
                sta VIC+16
                ; show sprites
                lda #%00111111
                sta VIC+21
                
                ; Bkg and frame colors
                lda #CL_DARKGREEN;#CL_LIGHTGREY
                sta $d021
                lda #CL_BLACK;LIGHTGREY;#CL_DARKGREEN
                sta $d020
                
                ; Set MC on
                jsr MultiColorOn
                
                ; Fill GameNamesLo/Hi
                lda #<GameNames
                sta $fb
                lda #>GameNames
                sta $fc
                ldx #0
game_names_loop lda $fb
                sta GameNamesLo,x
                lda $fc
                sta GameNamesHi,x
                ldy StrLen,x
                jsr AddYtoFBFC
                inx
                cpx #NO_OF_GAMES
                bcc game_names_loop
                rts

DrawMainMenu    lda #<MainMenuMap
                sta $fb
                lda #>MainMenuMap
                sta $fc
                lda #40
                sta MapWidth
                lda #25
                sta MapHeight
                lda #$00
                sta $fd
                lda #$04
                sta $fe
                jsr DrawMap
                rts

Welcome         ; Bkg and frame colors
                lda #CL_WHITE
                sta $d021
                lda #CL_DARKGREEN
                sta $d020

                lda #<WhosTheKing
                sta $fb
                lda #>WhosTheKing
                sta $fc
                lda #40
                sta MapWidth
                lda #25
                sta MapHeight
                lda #$00
                sta $fd
                lda #$04
                sta $fe
                jsr DrawMap
input           jsr $ffe4
                beq input
                cmp #32
                bne input
-               lda $cb
                cmp #$40
                bne -
                rts


!zone IRQ
CIA_IRQ         ; Mouse and Joystick
                lda MouseOn
                bne mouse
                jsr Joystick
                jmp std_irq
mouse           jsr Mouse
                jmp std_irq

InstallIRQ      sei
                lda #<CIA_IRQ
                sta $0314
                lda #>CIA_IRQ
                sta $0315
                cli
                rts

RestoreIRQ      sei
                lda #$31
                sta $0314
                lda #$ea
                sta $0315
                cli
                rts

!zone Graphics
MultiColorOn    lda $d016
                ora #%00010000
                sta $d016
                lda #1
                sta $d022
                rts

MultiColorOff   lda $d016
                and #%11101111
                sta $d016
                rts
                
ClearPlayBtn    lda #<PlayBtnNoHovMap
                sta $fb
                lda #>PlayBtnNoHovMap
                sta $fc
                lda #12
                sta MapWidth
                lda #10
                sta MapHeight
                ldy #27
                ldx #13
                jsr PosToScrMem
                jsr DrawMap
                rts
                
FillPlayBtn     lda #<PlayBtnHoverMap
                sta $fb
                lda #>PlayBtnHoverMap
                sta $fc
                lda #12
                sta MapWidth
                lda #10
                sta MapHeight
                ldy #27
                ldx #13
                jsr PosToScrMem
                jsr DrawMap
                rts

IsInTitlesRect  jsr MouseToScr
                lda #1
                sta res
                ldx Point
                cpx TitlesRect
                bcc not_in_rect2
                dex
                cpx TitlesRect+2
                bcs not_in_rect2
                ldx Point+1
                cpx TitlesRect+1
                bcc not_in_rect2
                dex
                cpx TitlesRect+3
                bcs not_in_rect2
                rts
not_in_rect2    lda #0
                sta res
                rts
                
IsInPlayBtnRect jsr MouseToScr
                lda #1
                sta res
                ldx Point
                cpx PlayBtnRect
                bcc not_in_rect
                dex
                cpx PlayBtnRect+2
                bcs not_in_rect
                ldx Point+1
                cpx PlayBtnRect+1
                bcc not_in_rect
                dex
                cpx PlayBtnRect+3
                bcs not_in_rect
                rts
not_in_rect     lda #0
                sta res
                rts

;Returns Mouse pos in char coords in Point
MouseToScr      lda $d000
                sec
                sbc #24
                sta Point
                lda $d010
                sbc #0
                lsr
                lda Point
                ror
                lsr
                lsr
                sta Point
                
                lda $d001
                sec
                sbc #50
                lsr
                lsr
                lsr
                sta Point+1
                rts
                
;Expects scr pos in Y,X
;Output: scr mem adr in FDFE
PosToScrMem     sty dummy
                lda ScrTabLo,x
                clc
                adc dummy
                sta $fd
                lda ScrTabHi,x
                adc #0
                sta $fe
                rts

; Uses: $0203 (for color mem ptr)
; Expects:
;  map mem pos in $fb
;  map width in MapWidth
;  map height in MapHeight
;  upper left screen mem location in $fd
DrawMap         ; Put color mem addr to $02
                lda $fd
                sta $02
                lda $fe; add $d4 to $fe
                clc
                adc #$d4
                sta $03
                ; Start loop
                ldx MapHeight
                dex
                stx counter
outer_draw      lda MapWidth
                tay
                dey
draw_map        lda ($fb),y
                sta ($fd),y
                ; Adjust color---------
                tax
                lda Colors,x
                sta ($02),y 
                ;----------------------
                dey
                bpl draw_map
                
                ldy MapWidth
                jsr AddYtoFBFC
                ldy #40
                jsr AddYtoFDFE
                jsr AddYto0203
                
                dec counter
                bpl outer_draw
                
                rts



!zone Math
AddYto0203      tya
                clc
                adc $02
                sta $02
                lda $03
                adc #0
                sta $03
                rts
                
AddYtoFBFC      tya
                clc
                adc $fb
                sta $fb
                lda $fc
                adc #0
                sta $fc
                rts
                
AddYtoFDFE      tya
                clc
                adc $fd
                sta $fd
                lda $fe
                adc #0
                sta $fe
                rts

!zone MouseJoystickChoice
SCREEN_LOCATION = $0428
COLOR_LOCATION = $d828

MouseJoyChoice  ; Char set at $2000
                lda $d018
                and #%11110001
                ora #%00001000
                sta $d018

                jsr $e544; clear screen
                
                ldy #0
                lda #1
do_colors       sta COLOR_LOCATION,y
                sta COLOR_LOCATION + 200,y
                iny
                cpy #200
                bne do_colors
                
new_choice      ldy #0
                lda MouseOn
                beq choice_joy

choice_mouse    lda ChoiceMouse,y
                sta SCREEN_LOCATION,y
                lda ChoiceMouse + 200,y
                sta SCREEN_LOCATION + 200,y
                iny
                cpy #200
                bne choice_mouse
                jmp input_get
                
choice_joy      lda ChoiceJoy,y
                sta SCREEN_LOCATION,y
                lda ChoiceJoy + 200,y
                sta SCREEN_LOCATION + 200,y
                iny
                cpy #200
                bne choice_joy
                
                lda #0
                sta 198
input_get       jsr $ffe4
                beq input_get
                cmp #13
                beq ok
                cmp #145
                beq up
                cmp #17
                beq down
                jmp input_get
up              lda MouseOn
                beq input_get
                lda #0
                sta MouseOn
                jmp new_choice
down            lda MouseOn
                bne input_get
                lda #1
                sta MouseOn
                jmp new_choice
ok              rts


!zone GameLoading
; Expects game no in A  ;
LoadGame        ldx SelectedGame
                lda StrLen,x
                sta $fb
                ldy GameNamesHi,x
                lda GameNamesLo,x
                tax
                lda $fb
                ;LDA #fname_end-fname
                ;LDX #<fname
                ;LDY #>fname
                JSR $FFBD     ; call SETNAM
                LDA #$01
                LDX $BA       ; last used device number
                BNE skip
                LDX #$08      ; default to device 8
skip            LDY #$01      ; not $01 means: load to address stored in file
                JSR $FFBA     ; call SETLFS

                LDA #$00      ; $00 means: load to memory (not verify)
                JSR $FFD5     ; call LOAD
                BCS error     ; if carry set, a load error has happened
                RTS
error           ; Accumulator contains BASIC error code
                ; Most likely errors:
                ; A = $05 (DEVICE NOT PRESENT)
                ; A = $04 (FILE NOT FOUND)
                ; A = $1D (LOAD ERROR)
                ; A = $00 (BREAK, RUN/STOP has been pressed during loading)
                RTS

!zone Data
SelectedGame    !byte 0
dummy           !byte 0
GameNames       !text "FREECELL","SOLITAIRE"
StrLen          !byte 8,9
GameNamesLo     !byte 0,0
GameNamesHi     !byte 0,0
;TitlesRect      !byte 2,4,11,23
TitlesRect      !byte 28,5,37,11
bIsInPlayBtn    !byte 0
PlayBtnRect     !byte 27,13,37,21
counter         !byte 0
MouseOn         !byte 0
exit_code       !byte 0
GameToLoad      !byte 0
;--------------------------------
bLoadGame       !byte 0; the
bMoved          !byte 0; five
bFire           !byte 0; exit
bFireReleased   !byte 0; cod-
bExit           !byte 0; es
;--------------------------------
bFirePressed    !byte 0
dx              !byte 0 ;joystick
dy              !byte 0 ;directions
res             !byte 0 ;return value for various functions
Point           !byte 0,0
MapWidth        !byte 0
MapHeight       !byte 0
ScrTabLo        !byte $00,$28,$50,$78,$a0,$c8,$f0,$18,$40,$68,$90,$b8
                !byte $e0,$08,$30,$58,$80,$a8,$d0,$f8,$20,$48,$70,$98,$c0
ScrTabHi        !byte $04,$04,$04,$04,$04,$04,$04,$05,$05,$05,$05,$05
                !byte $05,$06,$06,$06,$06,$06,$06,$06,$07,$07,$07,$07,$07

*=$2000         
                !bin "MainMenuCharset.bin"

*=$2800
Mousepointer0Addr
!byte $c0,$00,$00,$a0,$00,$00,$90,$00
!byte $00,$88,$00,$00,$84,$00,$00,$82
!byte $00,$00,$8e,$00,$00,$a8,$00,$00
!byte $e4,$00,$00,$14,$00,$00,$12,$00
!byte $00,$12,$00,$00,$0c,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00

Mousepointer1Addr
!byte $c0,$00,$00,$e0,$00,$00,$f0,$00
!byte $00,$f8,$00,$00,$fc,$00,$00,$fe
!byte $00,$00,$fe,$00,$00,$f8,$00,$00
!byte $fc,$00,$00,$1c,$00,$00,$1e,$00
!byte $00,$1e,$00,$00,$0c,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$01

MenuItemHighliterAddr
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff
!byte $00,$00,$00,$00,$00,$00,$00,$00
!byte $00,$00,$00,$00,$00,$00,$00,$02

Colors          !bin "MainMenuAttribs.bin"; per char, thus 256 bytes
MainMenuMap     !bin "MainMenuMap.bin"
PlayBtnHoverMap !bin "PlayBtnHoverMap.bin"
PlayBtnNoHovMap !bin "PlayBtnNoHoverMap.bin"
ChoiceJoy       !bin "ChoiceJoy.bin"
ChoiceMouse     !bin "ChoiceMouse.bin"
SnapFreecellMap !bin "Snap_FreecellMap.bin"
SnapSolitairMap !bin "Snap_SolitaireMap.bin"
SnapBASICMap    !bin "Snap_BASICMap.bin"
WhosTheKing     !bin "WhosTheKing.bin"