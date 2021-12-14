(*
 *      CentrEd+ (c) 2013 by StaticZ <uoquint.ru>
 *		Base on Wyatt algoritm published on www.ruosi.org
 *)
unit UUopFile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, UMulBlock;

function HashFileName(string s) : QWord;

function UopContainer()

type

  TUopHeader = packed record;
    Format:    DWord;
    Version:   DWord;
    Signature: DWord;
    Offset:    QWord;
    Capacity:  DWord;
    Count:     DWord;
  end;

  TUopIndex = packed record;
    Offset:    DWord;
    Length:    DWord;
    Uncomp:    DWord;
    Append:    DWord;
  end;

implementation

{ THue }

function HashFileName(string s) : QWord;
var
  i, stl, eax, ecx, edx, ebx, esi, edi : DWord;
begin
    i := 0;  stl := Length(s);
  eax := 0;  ebx := $DEADBEEF + stl;
  ecx := 0;  edi := $DEADBEEF + stl;
  edx := 0;  esi := $DEADBEEF + stl;
  while i < stl do begin
    edi := (uint)((ord(s[i+ 7])shl 24)or(ord(s[i+ 6])shl 16)or(ord(s[i+ 5])shl 8)or ord(s[i+ 4]))+edi;
    esi := (uint)((ord(s[i+11])shl 24)or(ord(s[i+10])shl 16)or(ord(s[i+ 9])shl 8)or ord(s[i+ 8]))+esi;
    edx := (uint)((ord(s[i+ 3])shl 24)or(ord(s[i+ 2])shl 16)or(ord(s[i+ 1])shl 8)or ord(s[i   ]))-esi;

    edx := (edx + ebx)xor(esi shr 28)xor(esi shl  4);
    inc(esi, edi);
    edi := (edi - edx)xor(edx shr 26)xor(edx shl  6);
    inc(edx, esi);
    esi := (esi - edi)xor(edi shr 24)xor(edi shl  8);
    inc(edi, edx);
    ebx := (edx - esi)xor(esi shr 16)xor(esi shl 16);
    inc(esi, edi);
    edi := (edi - ebx)xor(ebx shr 13)xor(ebx shl 19);
    inc(ebx, esi);
    esi := (esi - edi)xor(edi shr 28)xor(edi shl  4);
    inc(edi, ebx);

    inc(i, 12);
  end;

  if (stl - i) > 0 then begin
    if (stl - i) <= 12 then inc(esi, DWord(ord(s[i+11])shl 24));
    if (stl - i) <= 11 then inc(esi, DWord(ord(s[i+10])shl 16));
    if (stl - i) <= 10 then inc(esi, DWord(ord(s[i+ 9])shl  8));
    if (stl - i) <=  9 then inc(esi, DWord(ord(s[i+ 8])      ));

    if (stl - i) <=  8 then inc(edi, DWord(ord(s[i+ 7])shl 24));
    if (stl - i) <=  7 then inc(edi, DWord(ord(s[i+ 6])shl 16));
    if (stl - i) <=  6 then inc(edi, DWord(ord(s[i+ 5])shl  8));
    if (stl - i) <=  5 then inc(edi, DWord(ord(s[i+ 4])      ));

    if (stl - i) <=  4 then inc(ebx, DWord(ord(s[i+ 3])shl 24));
    if (stl - i) <=  3 then inc(ebx, DWord(ord(s[i+ 2])shl 16));
    if (stl - i) <=  2 then inc(ebx, DWord(ord(s[i+ 1])shl  8));
    if (stl - i) <=  1 then inc(ebx, DWord(ord(s[i+ 0])      ));

    esi := (esi xor edi) - ((edi shr 18)xor(edi shl 14));
    ecx := (esi xor ebx) - ((esi shr 21)xor(esi shl 11));
    edi := (edi xor ecx) - ((ecx shr  7)xor(ecx shl 25));
    esi := (esi xor edi) - ((edi shr 16)xor(edi shl 16));
    edx := (esi xor ecx) - ((esi shr 28)xor(esi shl  4));
    edi := (edi xor edx) - ((edx shr 18)xor(edx shl 14));
    eax := (esi xor edi) - ((edi shr  8)xor(edi shl 24));

    Result := (QWord(edi) shl 32) or eax;
  end else begin
    Result := (QWord(esi) shl 32) or eax;
  end;

end;



end.

