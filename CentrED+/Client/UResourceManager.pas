(*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").  You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license at
 * http://www.opensource.org/licenses/cddl1.php.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at
 * http://www.opensource.org/licenses/cddl1.php.  If applicable,
 * add the following below this CDDL HEADER, with the fields enclosed
 * by brackets "[]" replaced with your own identifying * information:
 *      Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 *
 *
 *      Portions Copyright 2009 Andreas Schneider
 *)
unit UResourceManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Crc32Hash;
  
type

  { TResourceManager }

  TResourceManager = class
    constructor Create(AFileName: string);
    destructor Destroy; override;
  protected
    FFileStream: TFileStream;
    FCount: Integer;
    FLookupTable: array of Cardinal;
    FfnHashTable: array of Cardinal;
    FCurrentResource: Integer;
    FResourceStream: TMemoryStream;
  public
    function GetResource(AIndex: Integer): TStream;
    function GetResource(AFileName: String): TStream;
  end;
  
var
  ResourceManager: TResourceManager;

implementation

{ TResourceManager }

constructor TResourceManager.Create(AFileName: string);
begin
  inherited Create;
  FFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  FFileStream.Position := 0;
  FFileStream.Read(FCount, SizeOf(Integer));
  SetLength(FLookupTable, FCount);
  SetLength(FfnHashTable, FCount);
  FFileStream.Read(FLookupTable[0], FCount * SizeOf(Cardinal));
  FFileStream.Read(FfnHashTable[0], FCount * SizeOf(Cardinal));
  FCurrentResource := -1;
end;

destructor TResourceManager.Destroy;
begin
  FreeAndNil(FFileStream);
  FreeAndNil(FResourceStream);
  inherited Destroy;
end;

function TResourceManager.GetResource(AFileName: String): TStream;
var
  i    : Integer;
  hash : DWORD;
begin
  if not CalcStringCRC32(AnsiLowerCase(AFileName), hash)
  then Result := nil
  else begin
    for i := 0 to FCount do
      if FfnHashTable[i] = hash then begin
        Result := GetResource(i);
        break;
      end;
  end;
end;

function TResourceManager.GetResource(AIndex: Integer): TStream;
var
  size: Cardinal;
begin
  if AIndex <> FCurrentResource then
  begin
    FFileStream.Position := FLookupTable[AIndex];
    FResourceStream.Free;
    FResourceStream := TMemoryStream.Create;
    FFileStream.Read(size, SizeOf(Cardinal));
    FResourceStream.CopyFrom(FFileStream, size);
    FCurrentResource := AIndex;
  end;
  FResourceStream.Position := 0;
  Result := FResourceStream;
end;

initialization
begin
  ResourceManager := TResourceManager.Create(ExtractFilePath(ParamStr(0)) + '../Assetspack.dat');
end;

finalization
begin
  if ResourceManager <> nil then FreeAndNil(ResourceManager);
end;

end.

