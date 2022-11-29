showmain Macro message1,message2,message3
    mov ah,2h
    mov dl,10
    mov dh,5
    int 10h  
    
    mov ah,9h
    mov dx,offset message1
    int 21h
    
    mov ah,2h
    mov dl,10
    mov dh,7
    int 10h
    
    mov ah,9h
    mov dx,offset message2
    int 21h
    
    mov ah,2h
    mov dl,10
    mov dh,9
    int 10h
    
    mov ah,9h
    mov dx,offset message3
    int 21h
    
ENDM



.model small
.stack 64
.data

message1   db   'To Start Chatting Press F1','$'
message2   db   'To Start the game Press F2','$'
message3   db   'To End The Program Press ESC','$'
chatmessage db  'Chat mode','$'
gamemode db     'Gamemode','$'
.code
main proc far
    mov ax,@data
    mov ds,ax
    
    mov ax,0003h
    int 10h

    showmain message1,message2,message3    
    
    mov ah,0h
    int 16h   

    cmp ah,3bH
    jz Chat
    cmp ah,3ch
    jz play
    

    chat:
    mov ax,0003h
    int 10h

    mov ah,9
    mov dx, offset chatmessage
    int 21h
    


    play:
    mov ax,0003h
    int 10h

    mov ah,9
    mov dx, offset gamemode
    int 21h   
    
    mov ah,0h
    int 16h  

    cmp ah,3dh
    jz again



    again:
    mov ax,0003h
    int 10h

    showmain message1,message2,message3

    HLT

main ENDP
end main         

   ;;we can write the read button as a macro 