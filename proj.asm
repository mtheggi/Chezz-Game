.model Small
.stack 64
.data
boardFile   db   'chess.bin', 0h;
firstState  db   'board.txt', 0h;
DIRECTORY       DB      'D:\Pieces',0h
filehandle dw ?

Squares     DB     07h, 05h, 03h, 02h, 01h, 04h, 06h, 08h, 09h, 0ah, 0bh, 0ch, 0dh, 0eh, 0fh, 10h
            DB     32 DUP(0h)
            DB     19h, 1ah, 1bh, 1ch, 1dh, 1eh, 1fh, 20h, 17h, 15h, 13h, 12h, 11h, 14h, 16h, 18h 

Pieces      DB    8h DUP(0), 0ffh DUP(0), 20h

countX      DW    ?
countY      DW    ?

chessData db  9C40h dup(?);


DrawInitialState     MACRO
    MOV DX, OFFSET firstState
    MOV CX, 9C40H
    MOV BX, OFFSET Pieces + 8
    PUSH DX
    PUSH CX
    PUSH BX
    CALL HandleFile

    ADD SP, 6H 

    mov bx, 0ffffh
    DrawPiecesLoop:     inc bx; 
                        mov cl, Squares[bx]
                        cmp cl, 0H
                        jz DrawPiecesLoop
                    mov al, 08h
                    mul cl
                    mov si, offset Pieces
                    add si, ax
                    mov byte ptr [si + 7], 0h
                    ;;;MOV DX, OFFSET firstState
                    MOV CX, 271H
                    MOV DI, OFFSET chessData
                    PUSH SI
                    PUSH CX
                    PUSH DI
                    CALL HandleFile
                    ADD SP, 6H
                    mov byte ptr [si + 7], 20h
                    
                    mov dx, 0h
                    mov ax, bx
                    mov cx, 8h
                    div cx
                    mov si, dx
                    mov dx, 0h
                    mov cx, 19h
                    mul cx
                    push ax

                    mov ax, si
                    mov cx, 19h
                    mul cx
                    push ax
                    CALL DrawPiece

                    add sp, 4h

                    cmp bx, 03fh
                    Jnz DrawPiecesLoop


ENDM

.code

main PROC far
mov ax , @data ;
mov ds , ax ;

MOV AH, 3BH
MOV DX, OFFSET DIRECTORY
INT 21H

mov ah,0;
mov al,13h;
int 10h;

MOV DX, OFFSET boardFile
MOV CX, 9C40H
MOV BX, OFFSET chessData
PUSH DX
PUSH CX
PUSH BX
CALL HandleFile
ADD SP, 6H

CALL CloseFile

LEA bx , chessData
mov cx , 30h ;
mov dx , 0c8h ;
mov ah ,0ch ;


drawingloop :
mov al ,[Bx] ;
int 10h;
inc cx;
inc bx;
cmp cx , 0f8h;
JNE drawingloop ;
mov cx , 30H ;
dec dx ;
cmp dx, 0h;
JNE drawingloop;


mov ah , 0h ;
int 16h ;

DrawInitialState
;CALL OpenFile

mov ah , 0h ;
int 16h ;

;mov ah , 0h ;
;mov al , 3h ;
;int 10h ;

mov ah , 4ch ;
int 21h;


hlt
main ENDP

; In order for this procedure to work you have to put in DX the OFFSET of your file name variable
OpenFile PROC
mov ah , 3dh ;
mov al ,0h ;
;mov dx, offset boardFile
MOV BP, SP
mov dx, [BP]+2 ; It was a trial to make it a macro but changed
int 21h ;   
mov [filehandle], ax;

RET
OpenFile ENDP

; In order for this procedure to work you have to put in DX the OFFSET of your DataLocation
ReadData PROC ; DataLoc
mov ah , 3fh ;
mov bx , [filehandle];
MOV BP, SP
mov cx , [BP+6];
;LEA dx, offset chessData
mov dx, [BP+4]; It was a trial to make it a macro but changed
int 21h;
;mov ah , 3fh;

RET
ReadData ENDP;

; The Procedure HandleFile wants by pushing OFFset of Filename then number of bytes, then address of writing to the stack
HandleFile      PROC
    MOV BP, SP

    mov dx, [BP+6]
    PUSH DX
    CALL OpenFile
    add sp, 2h;

    MOV BP, SP
    mov dx, [BP+2]
    mov cx, [BP+4]
    push cx
    PUSH DX
    push bx ; 
    CALL ReadData
    CALL CloseFile
    pop bx;

    add sp, 4h
    RET
HandleFile ENDP

DrawPiece     PROC  FAR ; To be noticed. May cause an error due to Jump Far
    mov bp, sp
    LEA si , chessData
    mov cx , 30h ; 
    ADD CX, [BP + 4] ;
    mov dx , [BP + 6] ;
    mov ah, 0ch ;

    ADD CX, 19H
    MOV [countX], CX
    SUB CX, 19H

    add dx, 19h
    MOV [countY], dx
    sub dx, 19h
    
    piecesloop :
                    mov al ,[si] ;
                    cmp al, 06h
                    jz NODRAW
                    int 10h;
                    NODRAW: inc cx;
                    inc si;
                    cmp cx , [countX];
                    JNE piecesloop ;
                    mov cx , [bp+4] ;
                    add cx, 30h
                    inc dx ;
                    cmp dx, [countY];
                    JNE piecesloop;

RET
DrawPiece   ENDP

closeFile proc;
mov ah , 3eh;
mov bx , [filehandle];
int 21h;
RET ;
closeFile ENDP;
End main