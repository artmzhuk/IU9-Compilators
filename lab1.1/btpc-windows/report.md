% Лабораторная работа № 1.1. Раскрутка самоприменимого компилятора
% 18 февраля 2025 г.
% Артём Жук, ИУ9-62Б

# Цель работы
Целью данной работы является ознакомление с раскруткой самоприменимых компиляторов на примере модельного компилятора.

# Индивидуальный вариант
Компилятор BeRo. Заменить шестнадцатеричные константы вида $12ABcd на константы вида 0x12ABcd.

# Реализация

Различие между файлами `btpc.pas` и `btpc2.pas`:

```diff
Сравнение файлов btpc.pas и BTPC2.PAS
***** btpc.pas

function ReadNumber:integer;
var Num:integer;
begin
 Num:=0;
 if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
  while ('0'<=CurrentChar) and (CurrentChar<='9') do begin
   Num:=(Num*10)+(ord(CurrentChar)-ord('0'));
   ReadChar;
  end;
 end else if CurrentChar='$' then begin
  ReadChar;
  while (('0'<=CurrentChar) and (CurrentChar<='9')) or
        (('a'<=CurrentChar) and (CurrentChar<='f')) or
        (('A'<=CurrentChar) and (CurrentChar<='F')) do begin
   if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('0'));
   end else if ('a'<=CurrentChar) and (CurrentChar<='f') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('a')+10);
   end else if ('A'<=CurrentChar) and (CurrentChar<='F') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('A')+10);
   end;
   ReadChar;
  end;
 end;
 ReadNumber:=Num;
end;
***** BTPC2.PAS

function ReadNumber: integer;
var Num: integer;IsHex: boolean;
begin
  Num := 0;
  IsHex := False;

  if CurrentChar = '0' then begin
    ReadChar;
    if (CurrentChar = 'x') or (CurrentChar = 'X') then begin
      IsHex := True;
      ReadChar;
    end;
  end;

  if IsHex then begin
    while (('0' <= CurrentChar) and (CurrentChar <= '9')) or
          (('a' <= CurrentChar) and (CurrentChar <= 'f')) or
          (('A' <= CurrentChar) and (CurrentChar <= 'F')) do begin
      if ('0' <= CurrentChar) and (CurrentChar <= '9') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('0'))
      else if ('a' <= CurrentChar) and (CurrentChar <= 'f') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('a') + 10)
      else if ('A' <= CurrentChar) and (CurrentChar <= 'F') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('A') + 10);
      ReadChar;
    end;
  end else begin
    while ('0' <= CurrentChar) and (CurrentChar <= '9') do begin
      Num := (Num * 10) + (Ord(CurrentChar) - Ord('0'));
      ReadChar;
    end;
  end;
  ReadNumber := Num;
end;
*****


C:\Users\artm\Downloads\lab1.1\btpc-windows>fc btpc.pas btpc2.pas
Сравнение файлов btpc.pas и BTPC2.PAS
***** btpc.pas

function ReadNumber:integer;
var Num:integer;
begin
 Num:=0;
 if ('0'<=CurrentChar) and (CurrentChar<='9') then begin  
  while ('0'<=CurrentChar) and (CurrentChar<='9') do begin
   Num:=(Num*10)+(ord(CurrentChar)-ord('0'));
   ReadChar;
  end;
 end else if CurrentChar='$' then begin
  ReadChar;
  while (('0'<=CurrentChar) and (CurrentChar<='9')) or
        (('a'<=CurrentChar) and (CurrentChar<='f')) or
        (('A'<=CurrentChar) and (CurrentChar<='F')) do begin
   if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('0'));
   end else if ('a'<=CurrentChar) and (CurrentChar<='f') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('a')+10);
   end else if ('A'<=CurrentChar) and (CurrentChar<='F') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('A')+10);
   end;
   ReadChar;
  end;
 end;
 ReadNumber:=Num;
end;
***** BTPC2.PAS

function ReadNumber: integer;
var Num: integer;IsHex: boolean;
begin
  Num := 0;
  IsHex := False;

  if CurrentChar = '0' then begin
    ReadChar;
    if (CurrentChar = 'x') or (CurrentChar = 'X') then begin
      IsHex := True;
      ReadChar;
    end;
  end;

  if IsHex then begin
    while (('0' <= CurrentChar) and (CurrentChar <= '9')) or
          (('a' <= CurrentChar) and (CurrentChar <= 'f')) or
          (('A' <= CurrentChar) and (CurrentChar <= 'F')) do begin
      if ('0' <= CurrentChar) and (CurrentChar <= '9') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('0'))
      else if ('a' <= CurrentChar) and (CurrentChar <= 'f') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('a') + 10)
      else if ('A' <= CurrentChar) and (CurrentChar <= 'F') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('A') + 10);
      ReadChar;
    end;
  end else begin
    while ('0' <= CurrentChar) and (CurrentChar <= '9') do begin
      Num := (Num * 10) + (Ord(CurrentChar) - Ord('0'));
      ReadChar;
    end;
  end;
  ReadNumber := Num;
end;
*****


C:\Users\artm\Downloads\lab1.1\btpc-windows>fc btpc.pas btpc2.pas
Сравнение файлов btpc.pas и BTPC2.PAS
***** btpc.pas

function ReadNumber:integer;
var Num:integer;
begin
 Num:=0;
 if ('0'<=CurrentChar) and (CurrentChar<='9') then begin  
  while ('0'<=CurrentChar) and (CurrentChar<='9') do begin
   Num:=(Num*10)+(ord(CurrentChar)-ord('0'));
   ReadChar;
  end;
 end else if CurrentChar='$' then begin
  ReadChar;
  while (('0'<=CurrentChar) and (CurrentChar<='9')) or
        (('a'<=CurrentChar) and (CurrentChar<='f')) or
        (('A'<=CurrentChar) and (CurrentChar<='F')) do begin
   if ('0'<=CurrentChar) and (CurrentChar<='9') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('0'));
   end else if ('a'<=CurrentChar) and (CurrentChar<='f') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('a')+10);
   end else if ('A'<=CurrentChar) and (CurrentChar<='F') then begin
    Num:=(Num*16)+(ord(CurrentChar)-ord('A')+10);
   end;
   ReadChar;
  end;
 end;
 ReadNumber:=Num;
end;
***** BTPC2.PAS

function ReadNumber: integer;
var Num: integer;IsHex: boolean;
begin
  Num := 0;
  IsHex := False;

  if CurrentChar = '0' then begin
    ReadChar;
    if (CurrentChar = 'x') or (CurrentChar = 'X') then begin
      IsHex := True;
      ReadChar;
    end;
  end;

  if IsHex then begin
    while (('0' <= CurrentChar) and (CurrentChar <= '9')) or
          (('a' <= CurrentChar) and (CurrentChar <= 'f')) or
          (('A' <= CurrentChar) and (CurrentChar <= 'F')) do begin
      if ('0' <= CurrentChar) and (CurrentChar <= '9') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('0'))
      else if ('a' <= CurrentChar) and (CurrentChar <= 'f') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('a') + 10)
      else if ('A' <= CurrentChar) and (CurrentChar <= 'F') then
        Num := (Num * 16) + (Ord(CurrentChar) - Ord('A') + 10);
      ReadChar;
    end;
  end else begin
    while ('0' <= CurrentChar) and (CurrentChar <= '9') do begin
      Num := (Num * 10) + (Ord(CurrentChar) - Ord('0'));
      ReadChar;
    end;
  end;
  ReadNumber := Num;
end;
*****

```

Различие между файлами `btpc2.pas` и `btpc3.pas`:

```diff
*****

***** btpc2.pas
    OCPopEAX;
    EmitByte($ff); EmitByte($30); { PUSH DWORD PTR [EAX] }
    LastOutputCodeValue:=locNone;
***** BTPC3.PAS
    OCPopEAX;
    EmitByte(0xff); EmitByte(0x30); { PUSH DWORD PTR [EAX] }
    LastOutputCodeValue:=locNone;
*****

***** btpc2.pas
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($6a); EmitByte(Value); { PUSH BYTE Value }
    end else begin
     EmitByte($68); EmitInt32(Value); { PUSH DWORD Value }
    end;
***** BTPC3.PAS
    if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x6a); EmitByte(Value); { PUSH BYTE Value }
    end else begin
     EmitByte(0x68); EmitInt32(Value); { PUSH DWORD Value }
    end;
*****

***** btpc2.pas
    if Value=0 then begin
     EmitByte($8b); EmitByte($c5); { MOV EAX,EBP }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($8d); EmitByte($45); EmitByte(Value); { LEA EAX,[EBP+BYTE Value] }
    end else begin
     EmitByte($8d); EmitByte($85); EmitInt32(Value); { LEA EAX,[EBP+DWORD Value] }
    end;
***** BTPC3.PAS
    if Value=0 then begin
     EmitByte(0x8b); EmitByte(0xc5); { MOV EAX,EBP }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x8d); EmitByte(0x45); EmitByte(Value); { LEA EAX,[EBP+BYTE Value] }
    end else begin
     EmitByte(0x8d); EmitByte(0x85); EmitInt32(Value); { LEA EAX,[EBP+DWORD Value] }
    end;
*****

***** btpc2.pas
    if Value=0 then begin
     EmitByte($8b); EmitByte($c4); { MOV EAX,ESP }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($8d); EmitByte($44); EmitByte($24); EmitByte(Value); { LEA EAX,[ESP+BYTE Value] }
    end else begin
     EmitByte($8d); EmitByte($84); EmitByte($24); EmitInt32(Value); { LEA EAX,[ESP+DWORD Value] }
    end;
***** BTPC3.PAS
    if Value=0 then begin
     EmitByte(0x8b); EmitByte(0xc4); { MOV EAX,ESP }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x8d); EmitByte(0x44); EmitByte(0x24); EmitByte(Value); { LEA EAX,[ESP+BYTE Value] }
    end else begin
     EmitByte(0x8d); EmitByte(0x84); EmitByte(0x24); EmitInt32(Value); { LEA EAX,[ESP+DWORD Value] }
    end;
*****

***** btpc2.pas
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($8b); EmitByte($44); EmitByte($24); EmitByte(Value);
      { MOV EAX,DWORD PTR [ESP+BYTE Value] }
***** BTPC3.PAS
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x8b); EmitByte(0x44); EmitByte(0x24); EmitByte(Value);
      { MOV EAX,DWORD PTR [ESP+BYTE Value] }
*****

***** btpc2.pas
    end else begin
     EmitByte($8b); EmitByte($84); EmitByte($24); EmitInt32(Value);
      { MOV EAX,DWORD PTR [ESP+DWORD Value] }
***** BTPC3.PAS
    end else begin
     EmitByte(0x8b); EmitByte(0x84); EmitByte(0x24); EmitInt32(Value);
      { MOV EAX,DWORD PTR [ESP+DWORD Value] }
*****

***** btpc2.pas
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($8b); EmitByte($45); EmitByte(Value); { MOV EAX,DWORD PTR [EBP+BYTE Value] }
    end else begin
     EmitByte($8b); EmitByte($85); EmitInt32(Value); { MOV EAX,DWORD PTR [EBP+DWORD Value] }
    end;
***** BTPC3.PAS
    if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x8b); EmitByte(0x45); EmitByte(Value); { MOV EAX,DWORD PTR [EBP+BYTE Value] }
    end else begin
     EmitByte(0x8b); EmitByte(0x85); EmitInt32(Value); { MOV EAX,DWORD PTR [EBP+DWORD Value] }
    end;
*****

***** btpc2.pas
    if Value=0 then begin
     EmitByte($89); EmitByte($04); EmitByte($24); { MOV DWORD PTR [ESP],EAX }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte($89); EmitByte($44); EmitByte($24); EmitByte(Value);
      { MOV DWORD PTR [ESP+BYTE Value],EAX }
***** BTPC3.PAS
    if Value=0 then begin
     EmitByte(0x89); EmitByte(0x04); EmitByte(0x24); { MOV DWORD PTR [ESP],EAX }
    end else if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x89); EmitByte(0x44); EmitByte(0x24); EmitByte(Value);
      { MOV DWORD PTR [ESP+BYTE Value],EAX }
*****

***** btpc2.pas
    end else begin
     EmitByte($89); EmitByte($84); EmitByte($24); EmitInt32(Value);
      { MOV EAX,DWORD PTR [ESP+DWORD Value] }
***** BTPC3.PAS
    end else begin
     EmitByte(0x89); EmitByte(0x84); EmitByte(0x24); EmitInt32(Value);
      { MOV EAX,DWORD PTR [ESP+DWORD Value] }
*****

***** btpc2.pas
    if (Value>=-128) and (Value<=127) then begin
     EmitByte($89); EmitByte($45); EmitByte(Value); { MOV DWORD PTR [EBP+BYTE Value],EAX }
    end else begin
     EmitByte($89); EmitByte($85); EmitInt32(Value); { MOV EAX,DWORD PTR [EBP+DWORD Value] }
    end;
***** BTPC3.PAS
    if (Value>=-128) and (Value<=127) then begin
     EmitByte(0x89); EmitByte(0x45); EmitByte(Value); { MOV DWORD PTR [EBP+BYTE Value],EAX }
    end else begin
     EmitByte(0x89); EmitByte(0x85); EmitInt32(Value); { MOV EAX,DWORD PTR [EBP+DWORD Value] }
    end;
*****
```

# Тестирование

Тестовый пример:

```pascal
program Hello;

begin
  WriteLn(0x10);
  WriteLn(10);
  WriteLn(0030);
  WriteLn(0);
end.
```

Вывод тестового примера на `stdout`

```
16
10
30
0
```

# Вывод
В рамках выполнения данной лабораторной работы я научился раскручивать самоприменимый компилятор, вспомнил синтаксис языка языка Pascal (с уроков информатики 7 класса).