org 0x7C00
bits 16

%define ENDL 0x0D, 0x0A

; Jump to the code after the header
jmp short start
nop

; ===== FAT12 BIOS Parameter Block (BPB) =====
bdb_oem:                    db 'MSWIN4.1'        ; 8 bytes
bdb_bytes_per_sector:       dw 512
bdb_sectors_per_cluster:    db 1
bdb_reserved_sectors:       dw 1
bdb_fat_count:              db 2
bdb_dir_entries_count:      dw 224               ; 224 entries x 32 bytes = 7168 bytes = 14 sectors
bdb_total_sectors:          dw 2880              ; 1.44MB = 2880 sectors
bdb_media_descriptor_type:  db 0xF0              ; Standard for floppy
bdb_sectors_per_fat:        dw 9
bdb_sectors_per_track:      dw 18
bdb_heads:                  dw 2
bdb_hidden_sectors:         dd 0
bdb_large_sector_count:     dd 0

; ===== Extended Boot Record (EBR) =====
ebr_drive_number:           db 0
ebr_reserved:               db 0
ebr_signature:              db 0x29
ebr_volume_id:              dd 0x12345678        ; Random serial
ebr_volume_label:           db 'NBOS       '     ; 11 bytes, padded with spaces
ebr_system_id:              db 'FAT12   '        ; 8 bytes


start:
	jmp main

puts:
	push si
	push ax
.loop:
	lodsb
	or al,al
	jz .done

	mov ah,0x0e
	mov bh,0
	int 0x10
	
	jmp .loop
.done:
	pop ax
	pop si
	ret

main:
	mov ax, 0
	mov ds,ax
	mov es,ax

	mov ss,ax
	mov sp,0x7C00

	mov [ebr_drive_number],dl

	mov ax, 1
	mov cl, 1
	mov bx,0x7E00
	call disk_read

    mov si,msg_hello
	call puts
	cli
	hlt
    
; Error handlers
floppy_error:
    mov si, msg_read_failed   ; Load the address of the error message into SI
    call puts                 ; Print the error message
    jmp wait_key_and_reboot  ; Jump to function that waits for key press and reboots

wait_key_and_reboot:
    mov ah, 0                 ; BIOS function to wait for key press
    int 16h                   ; BIOS interrupt for keyboard input
    jmp 0FFFFh:0              ; Jump to the beginning of BIOS, effectively reboots

.halt:
    cli                       ; Clear interrupts (disable them)
    hlt                       ; Halt the CPU (wait forever)

; ax: LBA address
; Returns:
;   cl (bits 0-5): sector number
;   ch (bits 0-7): cylinder (lower 8 bits)
;   cl (bits 6-7): cylinder (upper 2 bits)
;   dh: head

lba_to_chs:
    push ax
    push dx

    xor dx, dx
    div word [bdb_sectors_per_track]  ; dx = 0 → ax = LBA / sectors per track, dx = LBA % sectors per track
    inc dx                            ; sector = (LBA % sectors per track) + 1
    mov cx, dx                        ; cx = sector

    xor dx, dx
    div word [bdb_heads]              ; ax = (LBA / sectors per track) / heads → cylinder, dx = head
    mov dh, dl                        ; dh = head
    mov ch, al                        ; ch = cylinder (lower 8 bits)
    shl ah, 6                         ; shift upper 2 bits of cylinder to high bits
    or cl, ah                         ; put upper 2 bits of cylinder into cl

    pop dx
    pop ax
    ret


disk_read:

	push ax
	push bx
	push cx
	push dx
	push di
	

    push cx                 ; Save the number of sectors (CL) on the stack

    call lba_to_chs         ; Convert LBA in AX to CHS values
                            ; Sets CH, CL, DH for BIOS
    pop ax                  ; Get back the number of sectors from the stack
                            ; Now AX = number of sectors to read

    mov ah, 02h             ; BIOS function 0x02: Read sectors
    mov di, 3               ; We will retry reading 3 times if it fails

.retry:
    pusha                   ; Save all registers (just in case BIOS changes any)
    stc                     ; Set carry flag (forces int 13h to reset it on success)
    int 13h                 ; Call BIOS disk read
    jnc .done               ; If carry not set = success, jump to done

    ; If read failed:
    popa                    ; Restore registers
    call disk_reset         ; Try to reset the disk controller

    dec di                  ; Reduce retry count
    test di, di             ; Did we try 3 times?
    jnz .retry              ; If not zero, try again

.fail:
    jmp floppy_error        ; All 3 retries failed, show error

.done:
    popa                    ; Restore registers (from success path)
	
	
	pop ax
	pop bx
	pop cx
	pop dx
	pop di
	ret


disk_reset:
	pusha
	mov ah,0
	stc
	int 13h
	jc floppy_error
	popa
	ret

msg_hello:			db 'Hello World',ENDL,0
msg_read_failed:	db 'read from disk failed',ENDL,0	
times 510-($-$$) db 0
dw 0AA55h
