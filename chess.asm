;extern Filename:byte
public callpiece

.model Small
.stack 64
.data

Filename db 'bbk.bin', 0h;
DIRECTORY       DB      'D:\Pieces',0h
filehandle dw ?
chessData db  625d dup(?);
.code
callpiece PROC far
mov ax , @data ;
mov ds , ax ;

MOV AH, 3BH
MOV DX, OFFSET DIRECTORY
INT 21H

call OpenFile;
call ReadData;

LEA bx , chessData
mov cx , 30h ;
mov dx , 0h ;
mov ah ,0ch ;


drawingloop :
mov al ,[Bx] ;
int 10h;
inc cx;
inc bx;
cmp cx , 49h;
JNE drawingloop ;
mov cx , 30H ;
inc dx ;
cmp dx, 19h;
JNE drawingloop;

mov ah , 0h ;
int 16h ;

call closeFile ;
;mov ah , 0h ;
;;mov al , 3h ;
;int 10h ;

;mov ah , 4ch ;
;int 21h;


;hlt
ret
callpiece ENDP

OpenFile proc
mov ah , 3dh ;
mov al ,0h ;
LEA dx,Filename ;
int 21h ;   
mov [filehandle], ax;

RET ;
OpenFile ENDP ;

ReadData proc
mov ah , 3fh ;
mov bx , [filehandle];
mov cx , 625d;
LEA dx ,chessData;
int 21h;
;mov ah , 3fh;
RET;
ReadData ENDP;


closeFile proc;
mov ah , 3eh;
mov bx , [filehandle];
int 21h;
RET ;
closeFile ENDP;
End 


