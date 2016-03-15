;show txt from .txt file

title auto read .txt file
page 60, 132

;set cursor position    
setcsrpos macro row, col
     push ax
     push bx
     push dx

     mov dh, row
     mov dl, col
     mov ah, 02h
     mov bh, 00h         ;page number (0..7).
     int 10h
     
     pop dx
     pop bx
     pop ax
     endm
     
;AH = 03h
;BH = page number
;0-3 in modes 2&3
;0-7 in modes 0&1
;0 in graphics modes

;Return:
;AX = 0000h (Phoenix BIOS)
;CH = start scan line
;CL = end scan line
;DH = row (00h is top)
;DL = column (00h is left)
getcsrpos macro
     mov ah, 03h
     mov bh, 0h
     int 10h
     endm     
 
;set cursor attribution
;int 10h
;AH = 01h
;CH = cursor start and options (see #00013)
;CL = bottom scan line containing cursor (bits 0-4)
;Bit(s)  Description     (Table 00013)
;7      should be zero
;6,5    cursor blink.
;(00=normal, 01=invisible, 10=erratic, 11=slow).
;(00=normal, other=invisible on EGA/VGA)
;4-0    topmost scan line containing cursor
setca macro attr, bsl
     mov ch, attr
     mov cl, bsl
     mov ah, 01h
     int 10h
     endm

;display the string terminated by '$'
dsplstr macro msg
     ;push ax
     ;push dx
     
     lea  dx, msg
     mov  ah, 09h
     int  21h
     
     ;pop  dx
     ;pop  ax
     endm
     
;open a file
;in:  al = mode
;     dx = file_name ;pointer to ascii zero     
;out: 
;    if success      ax = file_handle
;                    cf = 0   
;    if failure      ax = error_code
;                    cf = 1
fopen macro file_name
     push dx
     lea  dx, file_name
     mov  ax, 3d02h ;read/write
     int  21h
     pop  dx   
     endm
     
;read a file
;in: bx = file handle     
;    dx = buffer
;    cx = read byte num < buff size  
;out: 
;    if success      ax = the number of bytes read
;                    cf = 0   
;    if failure      ax = error_code
;                    cf = 1
fread macro file_handle, bufptr, bufsize
     push bx
     push cx
     push dx
     mov  bx, file_handle
     lea  dx, bufptr
     mov  cx, bufsize
     mov  ah, 3fh
     int  21h
     pop  dx
     pop  cx
     pop  bx
     endm
     
;write buffer to file
;in: bx = file handle     
;    dx = buffer
;    cx = buff size
;out: 
;    if success      ax = the number of bytes written
;                    cf = 0   
;    if failure      ax = error_code
;                    cf = 1
fwrite macro file_handle, bufptr, bufsize
     push bx
     push cx
     push dx
     mov  bx, file_handle
     lea  dx, bufptr
     mov  cx, bufsize
     mov  ah, 40h
     int  21h
     pop  dx
     pop  cx
     pop  bx
     endm
     
;close a file
;in: bx = file handle     ;points to ascii zero
;out: 
;    if success      ax = 0
;                    cf = 0   
;    if failure      ax = error_code
;                    cf = 1
fclose macro file_handle
     push bx
     mov  bx, file_handle
     mov  ah, 3eh
     int  21h
     pop  bx
     endm

;LSEEK - SET CURRENT FILE POSITION
;AH = 42h
;AL = origin of move
;00h start of file
;01h current file position
;02h end of file
;BX = file handle
;CX:DX = (signed) offset bytes from origin of new file position
;Return:
;CF clear if successful
;DX:AX = new file position in bytes from start of file
;CF set on error
;AX = error code (01h,06h) (see #01680 at AH=59h/BX=0000h)
;offset the beginning of file   
fseek macro file_handle, offset_num
     push bx
     push cx
     mov  bx, file_handle
     mov  cx, word ptr ds:[offset_num+2]
     mov  dx, word ptr ds:[offset_num]
     mov  ah, 42h
     mov  al, 00h
     int  21h
     pop  cx
     pop  bx   
     endm 
     
;seek end of file 
fsk_eof macro file_handle
     push bx
     push cx
     mov  bx, file_handle
     sub  cx, cx
     sub  dx, dx
     mov  ah, 42h
     mov  al, 02h
     int  21h
     pop  cx
     pop  bx   
     endm

;seek begin of file 
fsk_bof macro file_handle
     push bx
     push cx
     mov  bx, file_handle
     sub  cx, cx
     sub  dx, dx
     mov  ah, 42h
     mov  al, 00h
     int  21h
     pop  cx
     pop  bx   
     endm
     
dsub macro double_word, i_num 
     sub word ptr ds:[double_word], i_num
     sbb word ptr ds:[double_word+2], 0
     endm 
     
dadd macro double_word, i_num
     add word ptr ds:[double_word], i_num
     adc word ptr ds:[double_word+2],0       ;adjust cx
     endm
     
;check for key press
;zf = 1, no key press
chkkey macro
     mov ah, 01
     int 16h
     endm
     
;get key 
;ah = 0
;al = ascii char press
getkey macro
     mov ah, 0
     int 16h
     endm



;locate vedio memory
;es:[di]
lctvm macro mrow, mcol
     mov dh, mrow
     mov dl, mcol   ;
     mov bl, 07h    ;71h    ;
          
     mov ax,0B800H  ;0a000h;
     mov es,ax

     ;计算es段偏移地址 
     sub di,di
     mov ax,0A0H 
     mul dh 
     add di,ax 
     mov ax,2H 
     mul dl 
     add di,ax 
     endm
     
;debug print
;mov ax = 
printax macro
     push si
     lea si, dbgbuf
     call itoa
     dsplstr dbgbuf
     pop si 
;_dieloop:jmp _dieloop   
     endm
stop macro
_dieloop:jmp _dieloop    
     endm

               
bufsize equ 192
filestamp equ 0bcfeh
dataseg segment
     txtbuf    db bufsize dup(?),0,0    ;must be ended by two zero
     row       db 4
     col       db 0
     count     db 0      ;for '-/|\' speed ctrl
     pspds	dw 0     
     fname     db 'c:\masm\exe\file.txt',0 ;db 128 dup(?),0;zero terminating string
     fhandle   dw ?
     fsize     dd ?
     fstamp    dw 0
     foffset   dd 0
     frprgrt   db 0      ;file reading progress rate % = foffset/fsize
     
     lstsec    db 0
     thesec    db 0
     encnflag  db 0       ;english or chinese word flag, 2->cn; 1->en, 0->no word
     quelfnws  db 0,0,0   ;queue made of last four number of words read from file
     quetail   db 0
     
     ddbuffer  dd 0CAA4C7EFH
     nrrcd     label byte
     rdctrl    db 10000100B   ;bit0->pause; bit1->esc ;bit2->line spacing; bit4->col spacing; bit6->finish; bit7->start flag;
     speed     db 7      	;[2~17]
     ttlwords  dd 4294967295
     ttltimes  dd 4294967295  ;second
     ddstrbuf  db 10 dup (?),0
     oldint09  dw 0,0
     oldint1c  dw 0,0
;==reader==user==interface==
;user interface of novel reader
;                             novel reader
;  reading speed:                                           time: 00:00:00
;  <================>                                       data: mm-dd-yy, wk
;0=========1=========2=========3=========4=========5=========6=========7=========
;...16lines [4-19]
;0=========1=========2=========3=========4=========5=========6=========7=========
;  progress rate: 100%                              total word: ============     
;  |>>>>>>>>>>>>>>>>>>>>|                           total time: ============
;  status: normal, pause, finish                    designer: light2nd@gmail.com                    
;  tips: <space> -> pause | <, .> -> adjust speed | <esc> -> quit | ? -> more

     nvlrd db 044h,00h,'NOVEL READER',0h                    ;row=0,  col=34
     rdspd db 0a4h,00h,'reading speed:',0h             ;row=1,  col=2
     rstar db 0c4h,00h,'-/|\',0h;                      ;row=1,  col=18
     ctime db 018h,01h,'time: 00:00:00',0h                  ;row=1,  col=60
     cdate db 0b8h,01h,'date: mmm.dd.yy,wkd',0h             ;row=2,  col=60
     prgrt db 024h,0dh,'progress rate:',0h                  ;row=21, col=2
     ttlwd db 088h,0dh,'total word:',0h; 0123456789',0h          ;row=21, col=52
     ttltm db 028h,0eh,'total time:',0h; 9876543210',0h          ;row=22, col=52
     statu db 064h,0eh,'status:',0h,'normal',0h,'pause ',0h,'finish',0h    ;row=23, col=2
     abtme db 0c8h,0eh,'designer: light2nd@gmail.com',0h              ;row=23, col=52
     nrtip db 004h,0fh,'tips: <space> -> pause | <, .> -> adjust speed | <esc> -> quit | ? -> more',0h ;;row=24, col=2
     array dw offset rdspd, offset ctime, offset cdate, offset prgrt, offset ttlwd 
           dw offset ttltm, offset statu, offset abtme, offset nrtip, 00h
     mths  db 'Jan','Feb','Mat','Apr','May','Jun','Jul','Oct','Sep','Nov','Dec'
     wkds  db 'Sun','Mon','Tue','Wed','Thu','Fri','Sat'     
     
;==error==message== 
     err_open  db 0dh,0ah,'err fopn$'
     err_close db 0dh,0ah,'err fcls$'
     err_read  db 0dh,0ah,'err frd$'
     err_write db 0dh,0ah,'err fwr$'
     err_seek  db 0dh,0ah,'err fsk$'
     ;exit_msg       db 0dh,0ah,'press any key to exit!$'

     dbgbuf db 8 dup(' '),0dh,0ah,'$'
     
dataseg ends

stckseg segment stack
     db 32 dup (?)
stckseg ends

codeseg segment
     assume cs:codeseg, ds:dataseg, ss:stckseg
main proc near 
     ;get the full path name of file to read
     call    getFFPN       

     mov     ax, dataseg
     mov     ds, ax
    
     call    insint09vct

     ;get file handle
     fopen   fname
     jnc     _fopen_ok
     jmp     _fopen_err
_fopen_ok:
     mov     fhandle, ax

     ;calc file size
     fsk_eof fhandle
     jnc     _fseek_ok1
     jmp     _fseek_err               
_fseek_ok1:
     
     mov     word ptr ds:[fsize+2],   dx
     mov     word ptr ds:[fsize],     ax

     mov     word ptr ds:[foffset+2], dx
     mov     word ptr ds:[foffset],   ax

     ;check file stamp
     dsub    foffset, 6
     fseek   fhandle, foffset
     jnc     _fseek_ok2
     jmp     _fseek_err               
_fseek_ok2:

     fread   fhandle, fstamp, 2
     jnc     _fread_ok1
     jmp     _fread_err
_fread_ok1:

     cmp     word ptr fstamp, filestamp
     jne     _no_file_stamp
     ;get last reading position
     fread   fhandle, foffset, 4
     jnc     _fread_ok2
     jmp     _fread_err
_fread_ok2:
     jmp     short _clear_screen   
     
_no_file_stamp:     
     ;if this file has no stamp then mark it
     dadd    foffset, 6
     fseek   fhandle, foffset
     jnc     _fseek_ok3
     jmp     _fseek_err            
_fseek_ok3:

     mov     word ptr ds:[fstamp],    filestamp
     mov     word ptr ds:[foffset],   0
     mov     word ptr ds:[foffset+2], 0
     
     fwrite  fhandle, fstamp, 6
     jnc     _fwrite_ok1
     jmp     _fwrite_err
_fwrite_ok1:        
     ;update file size
     dadd    fsize, 6    

_clear_screen:
     setca   20h, 0fh
     call    clrscr 
     call    nrstaui
     call    uispdbar
     call    uidate
     call    dsplsta
     mov     byte ptr row, 04h
     mov     byte ptr col, 00h
     
_reading_start:
_load_txtbuf:
     ;read file to text buffer
     fseek   fhandle, foffset
     jnc     _fseek_ok4
     jmp     _fseek_err               
_fseek_ok4:
     
     fread   fhandle, txtbuf, bufsize
     jnc     _fread_ok3
     jmp     _fread_err
_fread_ok3:
     lea     si, txtbuf
     call    adjtxtbuf
     dadd    foffset, ax
     call    inqueue   

_show_txtbuf:
     call    wrvm 
     jnc     _next_col
     call    clrlines  ;go to a new line
_next_col:

     call    staword
     call    calcpgrt
     call    drawpgrt  
     
     call    statime
     call    delay

_reading_finish:    
     call    chkstatus
     jc      _reading_esc

     inc     count 
     mov     ax, [si]
     or      al, al
     jnz     _show_txtbuf

     call    chkfrf
     jc      _reading_finish
     jmp     _load_txtbuf ;next text buffer

_reading_esc:
     ;save the position of reading
     dsub    fsize, 4
     fseek   fhandle, fsize
     jnc     _fseek_ok5
     jmp     _fseek_err           
_fseek_ok5:    
     call    sumqueue
     dsub    foffset, ax
          
     fwrite  fhandle, foffset, 4
     jnc     _fwrite_ok2
     jmp     _fwrite_err
_fwrite_ok2:

     ;close file
     fclose  fhandle
     jnc     _fclose_ok
     jmp     _fclose_err
_fclose_ok:
     
;update reader.exe self
     ;push ds
     ;pop es
     ;lea di, ddbuffer              ;restore ddbuffer for check
     mov     word ptr [ddbuffer],   0c7efh
     mov     word ptr [ddbuffer]+2, 0caa4h
     and     byte ptr rdctrl, 0fch     ;clear esc flag, pause flag    
     or      byte ptr rdctrl, 80h      ;restore next start flag

     mov	   ax, seg fname
     mov     es, ax
     lea     di, fname          
	mov ax, pspds		;restore ds of psp
	mov ds, ax
     call    getxfdn

	mov 	   ax, dataseg
	mov     ds, ax
    
     fopen   fname         ;exe file self
     jnc     _fopen_exe
     jmp     _fopen_err
_fopen_exe:
     mov     fhandle, ax
     
     fread   fhandle, quelfnws, 4
     jnc     _fread_exe
     jmp     _fread_err
_fread_exe:
     
     cld
_scan_exe_next_byte:     
     xor     al, al
     call    inqueue               
     fread   fhandle, quetail, 1  
     lea     si, quelfnws
     lea     di, ddbuffer
     mov     cx, 2
     repe    cmpsw
     jne     _scan_exe_next_byte
     
	;save nr setting    
     fwrite  fhandle, nrrcd, 10
     jnc     _fwrite_exe
     jmp     _fwrite_err
_fwrite_exe:

     ;close file
     fclose  fhandle
     jnc     _fclose_exe
     jmp     _fclose_err
_fclose_exe:
     
     
;deal with error
     jmp     short _main_exit
_fopen_err:
     dsplstr err_open
     jmp     short _main_exit      
_fseek_err:
     dsplstr err_seek
     jmp     short _main_exit 
_fread_err:
     dsplstr err_read
     jmp     short _main_exit 
_fwrite_err:
     dsplstr err_write
     jmp     short _main_exit
_fclose_err:
     dsplstr err_close
      
_main_exit:
     call    clrscr
     call    rstint09vct
     mov     ax, 4c00h
     int     21h
main endp


;display current time
timeint1c proc near
     push ax
     push bx
     push cx
     push dx
     push ds   
     push es
     push si
     push di
          
     mov ax, 0b800h
     mov es, ax
     mov bl, 07h
     
     mov ax, seg ctime
     mov ds, ax     
     lea si, ctime
     mov di, [si]
     add di, 12
     
     mov ax,0200h  
     int 1ah
     mov thesec, dh      ;save current second
     mov al,ch      ;hour(BCD)     
     call altovm
     add di, 2      ;pass :signal  
     mov al, cl     ;mintue(BCD)
     call altovm
     add di, 2      ;pass :signal
     mov al,dh      ;second(BCD) 
     call altovm
     
     cli                 ;turn off interrupt
     mov al, 20h         
     out 20h, al         ;issue EOI (end of interrupt) to 8259 chip
          
     pop di
     pop si
     pop es    
     pop ds
     pop dx
     pop cx
     pop bx
     pop ax    
          
     iret
timeint1c endp


;esc      for quit ;cf=1 if  key pressed
;spacebar for pausing
; <,      for slow  speed
; .>      for quick speed
checkkey  proc near
     push ax

     in al, 60h

     ;check space key
     cmp al, 039h        ;see if it is space key  ;scan code: 039h(down), 0b9h(up)        
     jne _chkkey_2c
     mov ah, rdctrl      ;rdctrl = ((~rdctrl)&0x1h)|(rdctrl&0feh)
     test ah, 40h        
     jnz _chkkey_spc_end
     not ah
     and ah,01h
     and byte ptr rdctrl, 0feh
     or  byte ptr rdctrl, ah
     call dsplsta
_chkkey_spc_end:    
     jmp _chkkey_over
     
     ;check <, key
_chkkey_2c:         
     cmp al, 33h      ;scan code: 033h(down), 0b3h(up)
     jne _chkkey_2e
     mov ah, speed
     dec ah           ;sub ah, 02h ;inc ah
     cmp ah, 010h
     jb _chkkey_2c2e          ;_chkkey_ltf    
     mov ah, 0fh
     jmp _chkkey_2c2e
                              ;_chkkey_ltf:  ;less than 15
                              ;    add ah, 02h
                              ;    mov speed, ah
                              ;    call uispdbar
                              ;    jmp _chkkey_over
     
     ;check .> key
_chkkey_2e:
     cmp al, 34h    ;scan code: 034h(down), 0b4h(up)
     jne _chkkey_n
     mov ah, speed
     sub ah, 03h     ;sub ah, 02h; dec ah
     test ah, 80h
     jz _chkkey_2c2e          ;_chkkey_mtz
     mov ah, 0h
_chkkey_2c2e:                 ;_chkkey_mtz:;more than zero
     add ah, 02h
     mov speed, ah
     call uispdbar
     jmp _chkkey_over
     
     ;check n key   ;for line spacing
_chkkey_n:
     cmp al, 31h    ;scan code: 031h(down), 0b1h(up)
     jne _chkkey_b
     mov ah, rdctrl      ;rdctrl = ((~rdctrl)&0x4h)|(rdctrl&~04h)
     not ah
     and ah,04h
     and byte ptr rdctrl, 0fbh ;~04h
     or  byte ptr rdctrl, ah
     jmp _chkkey_over

     ;check b key   ;for reading again
_chkkey_b:
     cmp al, 30h    ;scan code: 030h(down), 0b0h(up)
     jne _chkkey_esc
     mov ah, rdctrl      ;rdctrl = ((~rdctrl)&0x4h)|(rdctrl&~04h)
     test ah, 40h        ;if has finish
     jz _chkkey_over
     not ah
     and ah,20h
     and byte ptr rdctrl, 0dfh ;~20h
     or  byte ptr rdctrl, ah
     jmp _chkkey_over

               
     ;check esc key 
_chkkey_esc:
     cmp al, 1h     ;scan code: 01h(down), 081h(up)
     jne _chkkey_over
     or byte ptr rdctrl, 02h
     stc

_chkkey_over:
     pop ax
     
     cli                 ;turn off interrupt
     ;in al, 20h         ;发送EOI中断结束命令
     ;or al, 00100000b   ;20h
     mov al, 20h         
     out 20h, al         ;issue EOI (end of interrupt) to 8259 chip

     iret
checkkey endp


;install new interrupt vector
insint09vct proc near
     push ax
     push bx
     push es
     push ds
      
     mov al, 09h         ;Int 09h - IRQ1 - KEYBOARD DATA READY
     mov ah, 35h         ;get interrupt vector
     int 21h             ;get current interrupt handler -> es:bx 
     mov oldint09, bx    ;save current interrupt handler
     mov oldint09+2, es
               
     mov dx, seg checkkey     ;set new interrupt handler
     mov ds, dx     
     mov dx, offset checkkey
     mov al, 09h         ;interrupt number
     mov ah, 25h         ;set interrupt vector
     int 21h             ;set ds:dx->new interrupt handler

     cli                ;turn off interrupt                      
     in al, 21h
     and al, 11111101b
     out 21h, al                      
     sti

     pop ds    
     pop es
     pop bx
     pop ax
     ret
insint09vct endp 

;restore interrupt 09 handler
rstint09vct    proc near
     push dx
     push ds
     push ax
     
     mov dx, oldint09
     mov ds, oldint09+2 
     mov ah, 25h 
     mov al, 09h 
     int 21h 
     
     pop ax
     pop ds
     pop dx
     ret
rstint09vct endp 


;check rdctrl flag status
;cf=1 if exit soft
chkstatus proc near
     push ax
     
     mov ah, rdctrl 
     test ah, 40h             
     jz _csf_stop             
     call dsplsta        ;finish the file
     
     _csf_finish:
     	call delay
          mov ah, rdctrl      
          test ah, 20h
          jnz _csf_rerd             ;reading again 
          test ah, 02h             ;escaped finish
          jz _csf_finish
          jmp _csf_exit  ;exit soft               
     _csf_rerd:
          call reprd               
          call dsplsta            
          jmp _csf_end        
     
_csf_stop:     
     test ah, 01h
     jz _csf_esc         
     
     _csf_pause:         ;pause reading
     	call statime
     	call delay
          mov ah, rdctrl
          test ah, 01h
          jz _csf_esc    
          test ah, 02h
          jz _csf_pause       
          jmp _csf_exit  ;exit soft
                    
_csf_esc:
     test ah, 02h
     jz _csf_end
_csf_exit:               ;exit software                
     stc
     
_csf_end:
     pop ax
     ret
chkstatus endp

;read again
reprd proc near
     push ax
     push si
     
     ;mov  ah, rdctrl
     ;test ah, 20h
     ;jz _repr_end
     lea si, foffset
     mov word ptr [si], 0
     mov word ptr [si+2], 0
     and byte ptr rdctrl, 1ch
     or byte ptr rdctrl, 80h
     
     call clrqueue
_repr_end:     
     pop si
     pop ax
     ret
reprd endp


;update display status normal, pause , finish
dsplsta proc near
     push es
     push si
     push ax

     mov ax,0B800H  ;0a000h;
     mov es,ax 
     lea si, statu
     mov di, [si]   
     mov ah, rdctrl
     
     test ah, 40h
     jz   _dsta_no_finish
     add si, 10+7+7
     add di, 2*8
     mov ah, 0ch    ;bl attr light red
     call pstrz               ;finish
     jmp _dsta_end
_dsta_no_finish:
     
     test ah, 01h
     jz   _dsta_no_pause
     add si, 10+7
     add di, 2*8
     mov ah, 0eh    	;bl attr  yellow
     call pstrz               ;pause 
     jmp _dsta_end       
_dsta_no_pause:
     add si, 10
     add di, 2*8              ;normal
     mov ah, 07h    ;bl attr 
     call pstrz
     
_dsta_end:
     pop ax
     pop si
     pop es
     ret
dsplsta endp  


;check if finish reading file
;rdctrl bit6=1 if yes
chkfrf proc near
     push ax
     push dx
     push si
     push di
     
     lea si, foffset
     mov ax, [si]        ;foffset
     mov dx, [si+2]      ;foffset+2
     lea si, ddbuffer
     mov [si], ax
     mov [si+2], dx
     dadd ddbuffer, 8
     
     lea di, fsize  
     mov ax, [si+2]      ;foffset+2
     mov dx, [di+2]      ;fsize+2
     cmp dx, ax 
     jne _chkfrf_sr
     mov ax, [si]
     mov dx, [di]
     cmp dx, ax
     
_chkfrf_sr: ;set rdctrl
     jnc _chkfrf_end
     or rdctrl, 01000000b
     stc  
_chkfrf_end:

     pop di
     pop si
     pop dx
     pop ax    
     ret
chkfrf endp

;import: al = input to queue's tail
inqueue proc near
     push ax
     push cx
     push si
     
     lea  si, quelfnws
     mov  cx, 3               ;queue size
_iq_mov:
     mov  ah, [si]+1
     mov  [si], ah
     inc  si
     loop _iq_mov
     mov  byte ptr [si], al

     pop  si
     pop  cx
     pop  ax
     ret
inqueue endp

;export: al = output from queue
outqueue proc near
     push cx
     push si
     
     mov al, byte ptr [si]
     
     lea si, quelfnws
     mov cx, 3           ;queue size
_oq_mov:
     mov ah, [si]+1
     mov [si], ah
     inc si
     loop _oq_mov
     xor ah, ah
     mov [si], ah
     
     pop si
     pop cx
     ret
outqueue endp

;export ax = sum of queue
sumqueue proc near
     push cx
     push dx
     push si
     
     xor dx, dx
     xor ax, ax
     lea si, quelfnws
     
     mov cx, 4           ;queue size
_sq_scan:
     add dl, byte ptr [si]
     add ax, dx
     loop _sq_scan
     
     pop si
     pop dx
     pop cx
     ret
sumqueue endp

;clear queue
clrqueue proc near
     push si
     lea si, quelfnws
     mov word ptr [si], 0
     mov word ptr [si+2], 0
     pop si    
     ret
clrqueue endp
;import:
;    al = packed BCD
;    es:[di] = address of video memory
;export: none
altovm proc near
     push dx

     mov ah, al  
     shr ah, 1  
     shr ah, 1  
     shr ah, 1  
     shr ah, 1  
     and al, 0fh 
     add ax, '00'
     
     mov dh, bl
     mov dl, ah
     mov es:[di], dx
     add di, 2

     mov dl, al
     mov es:[di], dx
     add di, 2
     
     pop dx
     ret  
altovm endp

;double * word
;si = double word
;cx = word
;return: 
;dx:ax = double word
;cf=1 if overflow
dmul proc near
     push bx
     
     mov ax, [si]
     mov dx, [si]+2
     
     or dx, dx
     jz _dmul_wxw
     
     ;double word * word
     xor ax, ax
     push ax
     push ax
     mov bp, sp
     
     xor bl, bl
_dmul_sht:
     test cx, 01h
     jz _dmul_cx_shr
     
     mov ax, [si]
     mov dx, [si]+2
     
     push cx
     xor ch, ch
     mov cl, bl

     jcxz _dmul_cx_shr        
_dmul_cl_sh:
     shl dx, 1
     shl ax, 1      
     adc dx, 0 
     loop _dmul_cl_sh
     pop cx
     
     add [bp], ax
     adc [bp+2], dx
     jc  _dmul_cx_shr_e  ;over flow
_dmul_cx_shr:
     inc bl
     shr cx, 1
     or cx, cx
     jnz _dmul_sht
     
_dmul_cx_shr_e:     
     pop ax
     pop dx
     clc  
     jmp short _dmul_end
          
_dmul_wxw:
     mul cx
     clc
_dmul_end:
     pop bx    
     ret
dmul endp


;si = double word
;bx = base
;di = quotient
;dx = remainer
ddiv proc near
     push ax
     push cx
     
     mov ax, [si]
     mov dx, [si]+2
     mov word ptr ds:[di], 0
     mov word ptr ds:[di]+2,0
          
     or dx, dx
     jz _ddiv_2_wdiv
     push bx                  ;save bx
     
     push dx
     push ax
     xor dx, dx
     mov ax, 0ffffh
     div bx
     mov cx, ax          ;save quotient
     xor dx, dx
     mul bx
     mov bx, ax     ;bx=n*bx
     pop ax
     pop dx
     
_ddiv_sub_lp:
     sub ax, bx          
     sbb dx, 0
     dadd di, cx         ;use quotient
     or dx, dx
     jnz _ddiv_sub_lp

     pop bx                   ;store bx
_ddiv_2_wdiv:
     xor dx, dx
     div bx
     dadd di, ax

     pop cx
     pop ax
     ret
ddiv endp

;si = double word
;di = pointer to string's tail
;addbuf = temp double word buffer to save quotient
dtoa proc near 
     ;add di, LENGTH ddstrbuf -1;9
     push di
     lea di, ddbuffer
     
_dtoa_lp: 
     mov bx, 10     
     call ddiv
     
     or dl, 30h          ;mask to ascii
     pop di
     mov [di], dl
     dec di
     push di
     
     lea si, ddbuffer         ;addbuf is the quotient of ddiv / bx
     mov di, si
     
     mov ax, ds:[si]     ;check addbuf is zero 
     or ax, ax
     jnz _dtoa_lp
     mov ax, ds:[si]+2
     or ax, ax
     jnz _dtoa_lp        
     
     pop di
     ret
dtoa endp


;display string 
;import:
;    ds:[si] = address of string
;    es:[di] = address of video memory
;    cx = length of string
;    bl = attr
;export: none
strtovm proc near
     mov ah, bl     
_strtovm_cx_lp:
     mov al, ds:[si]
     mov es:[di], ax
     inc si
     add di, 2
     loop _strtovm_cx_lp
     ret
strtovm endp

;display speed bar
uispdbar proc near
     ;push ax
     push bx
     push cx
     
     mov ax, 0b800h
     mov es, ax
     mov di, 0144h       ;row=2; col=2
     mov bl, 07h
          
     mov ah, bl
     mov al, 0eh
     mov es:[di], ax
     add di, 36          ;speed bar 16 byte
     mov ah, bl
     mov al, 0fh
     mov es:[di], ax
     sub di, 34
     
     mov  cx, 18
     sub  cl, speed
_ui_spd_bar1:  
     mov al, ' '
     mov ah, bl
     mov es:[di], ax
     add di, 2
     add bl, 10h
     loop _ui_spd_bar1
     
     mov ah, 0h     ;black
     mov cl, speed
     dec cx
_ui_spd_bar2:
     dec cx
     jcxz _ui_spd_bar_over    
     mov al, ' '
     mov es:[di], ax
     add di, 2
     jmp _ui_spd_bar2    
_ui_spd_bar_over:
     
     pop cx
     pop bx
     ;pop ax
     ret
uispdbar endp

;display current date
uidate proc near
     push bx
     push dx
     
     mov bl, 07h
          
     mov ax, 0b800h
     mov es, ax

     lea si, cdate
     mov di, [si]
     add di, 12

     mov ax,0400h  
     int 1ah
     
     push cx   ;CL = year (BCD) ;DH = month (BCD) ;DL = day (BCD)
     push dx
     
     xor  ax, ax
     test dh, 10h   ;DH = month (BCD)
     jz _ui_mth_less_ten
     mov ax, 10
_ui_mth_less_ten:
     and dh, 0fh
     add al, dh
     dec ax
     mov bh, 3
     mul bh    
     lea si, mths
     add si, ax
     
     mov cx, 3
     call strtovm

     add di, 2      ;pass .signal

     pop dx
     mov al, dl          ;day(BCD) 
     call altovm    
     add di, 2      ;pass .signal
     
     pop cx
     mov al, cl          ;CL = year (BCD)    
     call altovm         
     add di, 2           ;pass ,signal
     
     mov ax, 2a00h       ;get week day
     int 21h
     
     xor ah, ah     ;AL = day of week (00h=Sunday)
     mov bh, 3
     mul bh    
     lea si, wkds
     add si, ax
     
     mov cx, 3
     call strtovm
     
     pop dx
     pop bx    
     ret
uidate endp

;static ui
nrstaui proc near
     push si
     push di
     
     mov bl, 07h    ;71h
     mov ax,0B800H  ;0a000h;
     mov es,ax 
;==edge==up==row:3==col:0==    
     mov di, 01e0h  ; 0a0h*row + 02h*col     
     mov cx, 2
_ui_edge_dn:     
     push cx
     mov cx, 80     
_ui_edge_up:
     mov al, '='
     mov es:[di], al
     inc di
     mov es:[di], bl
     inc di
     loop _ui_edge_up
;==edge==down==row:20==col:0==    
     mov di, 0c80h; 0a0h*row + 02h*col  
     pop cx
     loop _ui_edge_dn
     
;draw static strings
     cld
     mov ah, bl
     lea bx, array
     sub bx, 2 

_ui_load_str:  
     add bx, 2
     mov si, ds:[bx]
     or si, si
     jz _ui_array_exit        
     mov di, [si]
     add si, 2

_ui_load_char:                ;call pstrz
     mov al, ds:[si]          
     or al, al           
     jz _ui_load_str
     mov es:[di], ax
     add di, 2
     inc si    
     jmp _ui_load_char        ;jc _ui_load_str
_ui_array_exit:

     pop di
     pop si
     ret
nrstaui endp


;ui every word update once
drawpgrt proc near
     push si
     
     mov ax, 0b800h
     mov es, ax
     mov bl, 07h

     lea si, prgrt
     mov di, [si]
     add di, 36      ;progress rate:    
     mov ah, bl
     mov al, '%'
     mov es:[di], ax
          
     mov di, 0dc4h  ;row=22, col=2
     mov ah, bl
     mov al, '|'
     mov es:[di], ax
     add di, 36
     mov es:[di], ax

     lea si, prgrt
     mov di, [si]
     add di, 30     
     lea si, ddstrbuf
     add si, 5
     xor ah, ah
     mov al, frprgrt;73;

     call itoa
     add si, 3
     mov ah, bl
     call pstrz

     mov di, 0dc4h+2     ;row=22, col=3
     mov ax, 17
     mul byte ptr frprgrt     
     
     mov bh, 100
     div bh         ;byte/byte
     
     mov bh, al     
     xor ah, ah
     mov cx, ax
     jcxz _dpr_bhp  
     mov ah, 09h	;bl light blue
     mov al, '>'	;front half part
	call _dpr_fill
	
_dpr_bhp:			;back half part	
	mov cx, 17
	sub cl, bh
	jcxz _dpr_end
     mov ax, 0h	;
     call _dpr_fill
     jmp _dpr_end
     
_dpr_fill:
     mov es:[di], ax
     add di, 2
     loop _dpr_fill
	ret
	
_dpr_end:
     pop si
     ret
drawpgrt endp

dspltime proc near
     mov ax, 0b800h
     mov es, ax
     mov bl, 07h
     
     mov ax, seg ctime
     mov ds, ax     
     lea si, ctime
     mov di, [si]
     add di, 12
     
     mov ax,0200h  
     int 1ah

     mov al,ch      ;hour(BCD)     
     call altovm
     add di, 2      ;pass :signal  
     mov al, cl     ;mintue(BCD)
     call altovm
     add di, 2      ;pass :signal
     mov al,dh      ;second(BCD) 
     call altovm
     
     mov thesec, dh      ;save current second     
     test byte ptr rdctrl, 80h
     jz _dt_end
     and byte ptr rdctrl, 7fh
     mov lstsec, dh		;first initialize lstsec
_dt_end:
         
     ret
dspltime endp

dsplspdstar proc near
     mov ax, 0b800h
     mov es, ax
     mov bl, 07h

     mov bl, count
     shr bl, 1
     shr bl, 1
     mov bh, dh
     ;get char index (dx)
     mov dl, bl
     and dl, 03h
     xor dh, dh
     add dx, 02h
     ;get char color (bl)
     mov bl, bh
     add bl, 6
     and bl, 0fh
     jnz _ui_rstar_show
     mov bl, 6
_ui_rstar_show:
     ;display char (-/|\)     
     lea si, rstar
     mov di, ds:[si]
     add si, dx
     lodsb
     stosb
     mov es:[di], bl
     ;inc di
     
     ret
dsplspdstar endp

dsplnvlrd proc near
     mov ax, 0b800h
     mov es, ax
     mov bl, 07h

     mov bl, cl;2;color ;count
     
     lea si, nvlrd
     mov di, ds:[si]
     add si, 2
     
_ui_load_nrstr:
     lodsb
     or al, al
     jz _ui_nrstr_over
     stosb
     
     inc bl    
     and bl, 0fh
     or  bl, bl
     jnz _ui_nrstr_color
     inc bl
_ui_nrstr_color:
     
     mov es:[di], bl
     inc di    
     jmp _ui_load_nrstr
     
_ui_nrstr_over:
     
     ret
dsplnvlrd endp

;display total words, and time
dspltwt proc near
     mov ax, 0b800h
     mov es, ax
     mov bl, 07h

;word     
     lea si, ddstrbuf
     mov cx, length ddstrbuf
     call clrbuf
     
     lea si, ttlwords
     lea di, ddstrbuf
     add di, length ddstrbuf - 1
     call dtoa
     
     lea si, ttlwd
     mov di, ds:[si]
     add di, 24     ;length of 'total word: '
     lea si, ddstrbuf
     mov bl, 07h
     mov ah, bl
     call pstrz
     
;time
     lea si, ddstrbuf
     mov cx, length ddstrbuf
     call clrbuf
     
     lea si, ttltimes
     lea di, ddstrbuf
     add di, length ddstrbuf - 1
     call dtoa
     
     lea si, ttltm
     mov di, ds:[si]
     add di, 24
     lea si, ddstrbuf
     mov bl, 07h
     mov ah, bl
     call pstrz
          
     ret
dspltwt endp

;real time UI
;update quickly
nrdynui proc
     push ax
     push cx
     push si
          
     ;display current time
     call dspltime
     
     ;display speed star
     call dsplspdstar
     
     ;display novel reader
     call dsplnvlrd
     
     ;draw word and time
     call dspltwt
     
     pop  si
     pop  cx
     pop  ax
     ret
nrdynui endp

;clear buffer with space
;import: ds:[si] = str
;        cx = buffer size
;export: none
clrbuf proc near
     push ax
     push es
     push di
     
     push ds
     pop es
     mov di, si
     ;xor al, al
     mov al, ' '
     rep stosb
     
     pop di
     pop es
     pop ax
     ret
clrbuf endp



;print string terminated by zero
;import:
;ds:[si] = str
;es:[di] = pos of video memory
;ah = attr
;export: none
pstrz proc near
_psz_str2vm_lp:
     mov al, ds:[si]     
     or al, al
     jz _psz_exit
     mov es:[di], ax
     add di,2
     inc si    
     jmp _psz_str2vm_lp
     
_psz_exit:       
     ret
pstrz endp

;get exe file's dir and file's name
;import: es = seg string
;        di = offset string
;export: string
;ds is the value when code start
getxfdn proc near
     push di                  ;save string es:[di]
     push es
     
     mov ax,ds:[2ch]          ;取环境块段地址
     mov es,ax 
     xor di,di                ;es:[di]定位到环境块
     xor ax,ax
     mov cx,7fffh             ;环境块的最大长度   
     cld

_gxfdn_scan:                  ;扫描环境块 to find the full pathname of exe
     repnz scasb              ;es:di  
     jnz _gxfdn_over
     scasb                    ;找到两个连续的0 byte
     jnz _gxfdn_scan        
     
     add di, 2                ;找到 00 00 则加 2 字节地址就是文件名
     mov si, di
     push es
     pop ds                   ;DS:SI指向当前执行的程序名
     
     ;save to string
     pop es                   ;restore string address to es:di
     pop di
     
_gxfdn_save:
     lodsb                    ;ds:si
     stosb                    ;es:di
     or al, al
     jz _gxfdn_over      ;terminated by zero
     jmp short _gxfdn_save
_gxfdn_over:   

     ret
getxfdn endp

;check if file name is a full path name
;import:  ds =  ds of code start
;export:
;    cf = 1 if a full path name 
chkfullpath proc near
; restore ds of code start
     push ax
     push cx   
     push di

     push ds
     pop es    
     mov di,81H          ; argv offset address    

     cld  
     mov cx, 128
     mov al, ':'    
     repnz scasb    
     jnz _chkfp_exit
     stc       ;is a full path name 
_chkfp_exit:
          
     pop di
     pop cx
     pop ax
     ret
chkfullpath endp


;get the full path name of file to read
getFFPN proc near
     mov ax, seg pspds
     mov es, ax
     lea di, pspds 
     mov es:[di], ds		;save ds of psp 

     call chkfullpath         ;check if argument is a full path name
     jc _gffpn_oth_dir

     mov ax, seg fname        ;file in the cur dir or sub dir 
     mov es, ax
     lea di, fname
     call getthisdirfname
     jmp _gffpn_exit
     
_gffpn_oth_dir:               ;argument is a full path name
     mov ax, seg fname
     mov es, ax
     lea di, fname
     call getothdirfname
_gffpn_exit:
     
     ret
getFFPN endp

;get a full path name in other dir
;string terminated by zero
;import: es = seg string
;        di = offset string
;export: string
;ds : ds of code start
getothdirfname proc near
     mov si,81H          ; argv offset address
     cld
_gdfn_lp:                ;copy file name arg to dirfname
     lodsb               ;ds:si
     cmp al,' '          ;跳过空格
     jz  _gdfn_lp
     cmp al,9            ;跳过横向制表符
     jz  _gdfn_lp
     cmp al, 13          ;cr
     jz _gdfn_exit
     stosb               ;es:di
     jmp _gdfn_lp        
_gdfn_exit:
     xor al, al          ;end by zero
     stosb
               
     ret
getothdirfname endp

;get full path and file name
;file in current dir or sub dir
;string terminated by zero
;import: es = seg string
;        di = offset string
;export: string
;note:
;    mov ax, seg string
;    mov es, ax
;    lea di, string
;    call getdirfname 
getthisdirfname proc near
     push ds                  ;save ds

     call getxfdn

     std
     mov cx, 128
     mov al, '\'
     repnz scasb              ;定位到最后一个'\'  
     add di, 2

     pop ds
     call getothdirfname      ;join argument part to string 
     
     ret
getthisdirfname endp


;adjust txt buffer to prevent cuting a chinese char
;import:  si =  index of buffer 
;export:  ax =  buffer byte size
adjtxtbuf proc near
     push bx
     push dx
               
     push si
     xor  dx, dx
     xor  bx, bx
_adjbuf_scan_buf:   
     mov  al, [si]
     test al, 80h
     jz   _adjbuf_westen_char
     mov  dx, si
     inc  si                  ;just check the first byte of chinese char
     jmp  short _adjbuf_next_byte
_adjbuf_westen_char:
     mov  bx, si
_adjbuf_next_byte:  
     inc  si
     or   byte ptr [si], 0
     jnz  _adjbuf_scan_buf

     cmp  dx, bx    
     ja   _adjbuf_result
     xchg dx, bx         ;dx get the bigger
_adjbuf_result:     
     mov  ax, dx
     pop  si
     sub  ax, si         ;the real byte size of buf
     
     or ax, ax
     jnz _adjbuf_end
     inc ax              ;pass the file's hole of zero
     
_adjbuf_end:
     pop  dx
     pop  bx
     ret
adjtxtbuf endp

;import: al = packed bcd
;export: al = bin
bcd2bin proc near
     push bx
     
     push ax
     mov bh, al
     and bh, 0fh
     
     shr al, 1
     shr al, 1
     shr al, 1
     shr al, 1 
     mov bl, 10
     mul bl
          
     add al, bh
     pop bx
     mov ah, bh     ;restore ah
     
     pop bx
     ret
bcd2bin endp

;statistic all words read and time token
staword proc near
     push ax
;statistic word
     xor ax, ax     
     mov al, encnflag
     test al, 02h
     jz _sta_wd_end
     dadd ttlwords, 1         ;is a word
     mov encnflag, ah
_sta_wd_end:
     pop ax    
     ret
staword endp


statime proc near
     push ax

;statistic time
     ;test byte ptr rdctrl, 80h
     ;jz _sta_cmp_tls
     ;and byte ptr rdctrl, 7fh
     ;jmp short _sta_tm_update_lstsec
;_sta_cmp_tls:       ;compare thesec with lstsec
     mov al, lstsec
     call bcd2bin   
     mov ah, al
     
     mov al, thesec
     call bcd2bin

     cmp al, ah
     je _sta_tm_end
     cmp al, ah
          
     ja _sta_tm_dt  
     add al, 60          ;thesec < lstsec
_sta_tm_dt:              ;delta time(ax) = thesec - lstsec
     sub al, ah     
     xor ah, ah
     dadd ttltimes, ax
     
;_sta_tm_update_lstsec:
     mov al, thesec
     mov lstsec, al 

_sta_tm_end:   
     pop ax
     ret
statime endp

;calacute progress rat
calcpgrt proc near
     push ax
     push dx
     push cx
     push si
 
     lea dx, txtbuf
     mov cx, si
     sub cx, dx
     lea si, foffset
     mov dx, [si+2]
     mov ax, [si]
     add ax, cx          ;dx:ax current byte position  
     adc dx, 0           
     
     lea si, ddbuffer
     mov [si], ax
     mov [si+2], dx
     mov cx, 100
     call dmul           ;dx:ax*100
     jc _cpr_100    ;over flow, rate = 100%

     lea si, fsize  
     xor cx, cx
_cpr_times:
     inc cx
     sub ax, [si]
     sbb dx, [si+2]                          
     jnc _cpr_times
     dec cx         ;cx = dx:ax / fsize
     cmp cl, 100
     jae _cpr_100    
     mov frprgrt, cl
     jmp _cpr_end
     
_cpr_100:
     mov frprgrt, 100

_cpr_end:
     pop si    
     pop cx
     pop dx
     pop ax         
     ret
calcpgrt endp

;write a word or byte to video memory
;and adjust the row and col pos
;import : dh = row
;         dl = col
;export: cf = 1 if get a new row
wrvm proc near
     mov dh, row
     mov dl, col 
     mov bl, 07h    ;71h    ;
          
     mov ax,0B800H  ;0a000h;
     mov es,ax

     ;计算es段偏移地址 
     sub di,di
     mov ax,0A0H 
     mul dh 
     add di,ax 
     mov ax,2H 
     mul dl 
     add di,ax 
     
     ;deal with cr and lf char
     mov al,ds:[si]           
     cmp al, 0dh              ;get enter char
     jne _not_enter_char
     inc si
     jmp _update_row
_not_enter_char:   
     cmp al, 0ah              ;get lf char
     jne _not_cr_and_lf
     inc si
     jmp _not_new_row    
_not_cr_and_lf:
     
     ;es:[di] video memory
     mov al, ds:[si]
     mov es:[di], al
     inc di
     mov es:[di], bl
     inc di
     inc si    
                         
     test al, 80h             ;check if en or cn char
     jz _is_westen_char
     mov byte ptr encnflag, 02h    ;is chinese character
     mov al,ds:[si]                ;es:[di] video memory
     mov es:[di],al
     inc di
     mov es:[di],bl
     inc di
     inc si    
     inc col
     jmp _chk_en_cn_over
_is_westen_char:
     cmp al, ' '              ;check if a english word
     jne _chk_en_cn_over
     test byte ptr encnflag, 01h
     jnz _is_westen_word
     mov byte ptr encnflag, 01h
     jmp _chk_en_cn_over
_is_westen_word:
     mov byte ptr encnflag, 02h
     
_chk_en_cn_over:    
     inc col

     ;adjust col to show chinese char
     cmp byte ptr col, 79     
     jb _not_new_row
     mov byte ptr col, 0
_update_row:
     test byte ptr rdctrl, 04h
     jz  _no_row_gap               
     inc row
_no_row_gap:   
     inc row
               
     cmp byte ptr row, 20
     jb _get_new_row
     mov byte ptr row, 04h
     
_get_new_row:
     mov word ptr col, 0 
     stc
     jmp _next_exit
_not_new_row:
     clc  
_next_exit:         
     ret
wrvm endp

;clear next four rows below row
;import:  dh = start row [0~24]
;         dl = start col [0~79]
;         cx = row num   [1~24]
clrrows proc near
     mov bl, 07h    ;71h    ;
          
     mov ax,0B800H  ;0a000h;
     mov es,ax

     ;计算es段偏移地址 
     sub di,di
     mov ax,0A0H 
     mul dh 
     add di,ax 
     mov ax,2H 
     mul dl 
     add di,ax      
     
     cld      
_ss_wr2vm:          ;copy to video memory
     mov al, 0      ;space
     stosb          ;to es:di
     mov al, bl     ;attr
     stosb
     loop _ss_wr2vm
     
     ret
clrrows endp

;clear the following three lines
clrlines proc near
     mov ax, 20
     sub al, row
     cmp ax, 3
     jae _clear_stl 
     push ax
     
     mov dx, 3
     sub dx, ax
     mov ax, dx
     mov bh, 80 ;80 char/row
     mul bh
     mov cx, ax
     
     mov dh, 04h
     xor dl, dl
     call clrrows   ;[4~7]lines
               
     pop ax
     jmp _clear_left
_clear_stl:    ;following sequence three lines
     mov ax, 3      
_clear_left:
     mov bh, 80
     mul bh
     mov cx, ax     ;80 char/row

     mov dh, row
     mov dl, col
     call clrrows 
     
     ret
clrlines endp

;export: cx
fibonacci proc near
	push ax
	push bx
	
	xor ch, ch
	mov cl, speed
	shr cx, 1
	mov ax, 0
	mov bx, 1
_f_lp:
	xchg ax, bx
	add bx, ax
	loop _f_lp
	mov cx, bx	
_f_end:
	
	pop bx
	pop ax	
	ret
fibonacci endp 

;delay
;import:  dx = delay time unit ;6629*15.085microsec = 100ms=0.1s
;         cx = count
delay proc
     push ax
     push cx
     push dx
     
     mov     dx, 1979
     call    fibonacci     
_dly_cxlp:
     push dx
_dly_wait:
     ;call checkkey
     ;jc  _dly_esc   ;esc key press
     in   al, 61h
     and  al, 10h    ;check pb4
     cmp  al, ah     ;did it just change?
     je   _dly_wait
     mov  ah, al     ;save the new pb4 status
     dec  dx
     jnz  _dly_wait  ;delay one unit time
     
     call nrdynui
         
     pop  dx   
     loop _dly_cxlp
     jmp  _dly_over
_dly_esc:
     pop dx
_dly_over:
     
     pop  dx
     pop  cx    
     pop  ax
     ret
delay endp
;clear entire screen
;import: none
;export: none     
clrscr proc near
     push ax
     push bx
     push cx
     push dx
     
     mov ax, 0600h
     mov bh, 07h
     mov cx, 0000h
     mov dx, 184fh
     int 10h

     pop dx
     pop cx
     pop bx
     pop ax
     ret
clrscr endp


itoa proc near
     push cx
     push dx
     push bx

     add si, 4
     mov bx, 10  
     mov cx, 5
_i2a_lp:
     cmp ax, 0
     je  _i2a_zero
     
     xor dx, dx  ;dx = 0
     div bx
     add dl, 30h  ;'0'
     mov [si], dl
     dec si
     loop _i2a_lp    
     jmp _i2a_exit
     
_i2a_zero:
     mov byte ptr [si], ' ' ;space
     dec si
     loop _i2a_lp
     
_i2a_exit:
     pop bx
     pop dx
     pop cx
     ret
itoa endp


codeseg ends

     end main