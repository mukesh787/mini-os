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
	mov ax,0
	mov ds,ax
	mov es,ax

	mov ss,ax
	mov sp,0x7C00

    mov si,msg_hello
	call puts

	hlt
    

.halt:
	jmp .halt
    
msg_hello: db 'Hello World',ENDL,0
times 510-($-$$) db 0
dw 0AA55h
