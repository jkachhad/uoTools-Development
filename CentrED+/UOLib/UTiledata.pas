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
unit UTiledata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMulBlock;

const
  LandOldTileDataSize = 26;
  LandOldTileGroupSize = 4 + 32 * LandOldTileDataSize;
  StaticOldTileDataSize = 37;
  StaticOldTileGroupSize = 4 + 32 * StaticOldTileDataSize;

  LandTileDataSize = 30;
  LandTileGroupSize = 4 + 32 * LandTileDataSize;
  StaticTileDataSize = 41;
  StaticTileGroupSize = 4 + 32 * StaticTileDataSize;

type
  TTileDataFlag = (tdfBackground, tdfWeapon, tdfTransparent, tdfTranslucent,
                   tdfWall, tdfDamaging, tdfImpassable, tdfWet, tdfUnknown1,
                   tdfSurface, tdfBridge, tdfGeneric, tdfWindow, tdfNoShoot,
                   tdfArticleA, tdfArticleAn, tdfInternal, tdfFoliage,
                   tdfPartialHue, tdfUnknown2, tdfMap, tdfContainer,
                   tdfWearable, tdfLightSource, tdfAnimation, tdfNoDiagonal,
                   tdfArtUsed, tdfArmor, tdfRoof, tdfDoor, tdfStairBack,
                   tdfStairRight);
  TTileDataFlags = set of TTileDataFlag;

  { TTiledata }

  TTiledata = class(TMulBlock)
  protected
    FFlags: TTileDataFlags;
    FFlags2 : LongWord;
    FTileName: string;
  public
    property Flags: TTileDataFlags read FFlags write FFlags;
    property TileName: string read FTileName write FTileName;
  end;

  { TLandTiledata }

  TLandTiledata = class(TTiledata)
    constructor Create(AData: TStream); virtual;
    destructor Destroy; override;
    function Clone: TLandTiledata; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FTextureID: Word;
  public
    property TextureID: Word read FTextureID write FTextureID;
  end;

  { TLandOldTiledata }

  TLandOldTiledata = class(TLandTiledata)
    constructor Create(AData: TStream); overload; override;
    destructor Destroy; overload; override;
    function Clone: TLandTiledata; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  end;

  { TStaticTiledata }

  TStaticTiledata = class(TTiledata)
    constructor Create(AData: TStream); virtual;
    destructor Destroy; override;
    function Clone: TStaticTiledata; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FWeight: Byte;
    FQuality: Byte;
    FUnknown1: Word;
    FUnknown2: Byte;
    FQuantity: Byte;
    FAnimID: Word;
    FUnknown3: Byte;
    FHue: Byte;
    FUnknown4: Word;
    FHeight: Byte;
  public
    property Weight: Byte read FWeight write FWeight;
    property Quality: Byte read FQuality write FQuality;
    property Unknown1: Word read FUnknown1 write FUnknown1;
    property Unknown2: Byte read FUnknown2 write FUnknown2;
    property Quantity: Byte read FQuantity write FQuantity;
    property AnimID: Word read FAnimID write FAnimID;
    property Unknown3: Byte read FUnknown3 write FUnknown3;
    property Hue: Byte read FHue write FHue;
    property Unknown4: Word read FUnknown4 write FUnknown4;
    property Height: Byte read FHeight write FHeight;
  end;

  { TStaticOldTiledata }
  TStaticOldTiledata = class(TStaticTiledata)
    constructor Create(AData: TStream); overload; override;
    destructor Destroy; overload; override;
    function Clone: TStaticTiledata; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  end;

  { TLandTileGroup }

  TLandTileGroup = class(TMulBlock)
    constructor Create(AData: TStream); virtual;
    destructor Destroy; override;
    function Clone: TLandTileGroup; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FUnknown: LongInt;
  public
    LandTileData: array[0..31] of TLandTiledata;
    property Unknown: LongInt read FUnknown write FUnknown;
  end;

  { TLandOldTileGroup }
  TLandOldTileGroup = class(TLandTileGroup)
    constructor Create(AData: TStream); overload; override;
    destructor Destroy; overload; override;
    function Clone: TLandTileGroup; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  end;

  { TStaticTileGroup }

  TStaticTileGroup = class(TMulBlock)
    constructor Create(AData: TStream); virtual;
    destructor Destroy; override;
    function Clone: TStaticTileGroup; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FUnknown: LongInt;
  public
    StaticTileData: array[0..31] of TStaticTiledata;
    property Unknown: LongInt read FUnknown write FUnknown;
  end;

  { TStaticOldTileGroup }

  TStaticOldTileGroup = class(TStaticTileGroup)
    constructor Create(AData: TStream); overload; override;
    destructor Destroy; overload; override;
    function Clone: TStaticTileGroup; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  end;


function GetTileDataOffset(ABlock: Integer; OldFormat: Boolean = False): Integer;

implementation

function GetTileDataOffset(ABlock: Integer; OldFormat: Boolean = False): Integer;
var
  group, tile: Integer;
begin
  if ABlock > $3FFF then
  begin
    ABlock := ABlock - $4000;
    group := ABlock div 32;
    tile := ABlock mod 32;

    if OldFormat
      then Result := 512 * LandOldTileGroupSize + group * StaticOldTileGroupSize
                     + 4 + tile * StaticOldTileDataSize
      else Result := 512 * LandTileGroupSize + group * StaticTileGroupSize
                     + 4 + tile * StaticTileDataSize;
  end else
  begin
    group := ABlock div 32;
    tile := ABlock mod 32;

    if OldFormat
      then Result := group * LandOldTileGroupSize + 4 + tile * LandOldTileDataSize
      else Result := group * LandTileGroupSize + 4 + tile * LandTileDataSize;
  end;
end;

{ TLandTiledata }

constructor TLandTiledata.Create(AData: TStream);
begin
  SetLength(FTileName, 20);
  if assigned(AData) then
  begin
    AData.Read(FFlags, SizeOf(LongWord));
    AData.Read(FFlags2, SizeOf(LongWord));
    AData.Read(FTextureID, SizeOf(Word));
    AData.Read(PChar(FTileName)^, 20);
  end;
  FTileName := Trim(FTileName);
end;

destructor TLandTiledata.Destroy;
begin
  SetLength(FTileName, 0);
  inherited;
end;

function TLandTiledata.Clone: TLandTiledata;
begin
  Result := TLandTiledata.Create(nil);
  Result.FFlags := FFlags;
  Result.FFlags2:= FFlags2;
  Result.FTextureID := FTextureID;
  Result.FTileName := FTileName;
end;

procedure TLandTiledata.Write(AData: TStream);
var
  i: Integer;
begin
  if Length(FTileName) < 20 then
    for i := Length(FTileName) to 20 do
      FTileName := FTileName + #0;
  AData.Write(FFlags, SizeOf(LongWord));
  AData.Write(FFlags2, SizeOf(LongWord));
  AData.Write(FTextureID, SizeOf(Word));
  AData.Write(PChar(FTileName)^, 20);
end;

function TLandTiledata.GetSize: Integer;
begin
  GetSize := LandTileDataSize;
end;

{ TLandOldTiledata }

constructor TLandOldTiledata.Create(AData: TStream);
begin
  SetLength(FTileName, 20);
  if assigned(AData) then
  begin
    AData.Read(FFlags, SizeOf(LongWord));
    FFlags2 := 0;
    AData.Read(FTextureID, SizeOf(Word));
    AData.Read(PChar(FTileName)^, 20);
  end;
  FTileName := Trim(FTileName);
end;

destructor TLandOldTiledata.Destroy;
begin
  SetLength(FTileName, 0);
  inherited;
end;

function TLandOldTiledata.Clone: TLandTiledata;
begin
  Result := TLandOldTiledata.Create(nil);
  Result.FFlags := FFlags;
  Result.FFlags2:= FFlags2;
  Result.FTextureID := FTextureID;
  Result.FTileName := FTileName;
end;

procedure TLandOldTiledata.Write(AData: TStream);
var
  i: Integer;
begin
  if Length(FTileName) < 20 then
    for i := Length(FTileName) to 20 do
      FTileName := FTileName + #0;
  AData.Write(FFlags, SizeOf(LongWord));
  AData.Write(FTextureID, SizeOf(Word));
  AData.Write(PChar(FTileName)^, 20);
end;

function TLandOldTiledata.GetSize: Integer;
begin
  GetSize := LandOldTileDataSize;
end;

{ TStaticTiledata}

constructor TStaticTiledata.Create(AData: TStream);
begin
  SetLength(FTileName, 20);
  if AData <> nil then
  begin
    AData.Read(FFlags, SizeOf(LongWord));
    AData.Read(FFlags2, SizeOf(LongWord));
    AData.Read(FWeight, SizeOf(Byte));
    AData.Read(FQuality, SizeOf(Byte));
    AData.Read(FUnknown1, SizeOf(Word));
    AData.Read(FUnknown2, SizeOf(Byte));
    AData.Read(FQuantity, SizeOf(Byte));
    AData.Read(FAnimID, SizeOf(Word));
    AData.Read(FUnknown3, SizeOf(Byte));
    AData.Read(FHue, SizeOf(Byte));
    AData.Read(FUnknown4, SizeOf(Word));
    AData.Read(FHeight, SizeOf(Byte));
    AData.Read(PChar(FTileName)^, 20);
  end;
  FTileName := Trim(FTileName);
end;

destructor TStaticTiledata.Destroy;
begin
  SetLength(FTileName, 0);
  inherited;
end;

function TStaticTiledata.Clone: TStaticTiledata;
begin
  Result := TStaticTiledata.Create(nil);
  Result.FFlags := FFlags;
  Result.FFlags2:= FFlags2;
  Result.FWeight := FWeight;
  Result.FQuality := FQuality;
  Result.FUnknown1 := FUnknown1;
  Result.FUnknown2 := FUnknown2;
  Result.FQuantity := FQuantity;
  Result.FAnimID := FAnimID;
  Result.FUnknown3 := FUnknown3;
  Result.FHue := FHue;
  Result.FUnknown4 := FUnknown4;
  Result.FHeight := FHeight;
  Result.FTileName := FTileName;
end;

procedure TStaticTiledata.Write(AData: TStream);
var
  i: Integer;
begin
  if Length(FTileName) < 20 then
    for i := Length(FTileName) to 20 do
      FTileName := FTileName + #0;
  AData.Write(FFlags, SizeOf(LongWord));
  AData.Write(FFlags2, SizeOf(LongWord));
  AData.Write(FWeight, SizeOf(Byte));
  AData.Write(FQuality, SizeOf(Byte));
  AData.Write(FUnknown1, SizeOf(Word));
  AData.Write(FUnknown2, SizeOf(Byte));
  AData.Write(FQuantity, SizeOf(Byte));
  AData.Write(FAnimID, SizeOf(Word));
  AData.Write(FUnknown3, SizeOf(Byte));
  AData.Write(FHue, SizeOf(Byte));
  AData.Write(FUnknown4, SizeOf(Word));
  AData.Write(FHeight, SizeOf(Byte));
  AData.Write(PChar(FTileName)^, 20);
end;

function TStaticTiledata.GetSize: Integer;
begin
  GetSize := StaticTileDataSize;
end;

{ TStaticOldTiledata }

constructor TStaticOldTiledata.Create(AData: TStream);
begin
  SetLength(FTileName, 20);
  if AData <> nil then
  begin
    AData.Read(FFlags, SizeOf(LongWord));
    FFlags2 := 0;
    AData.Read(FWeight, SizeOf(Byte));
    AData.Read(FQuality, SizeOf(Byte));
    AData.Read(FUnknown1, SizeOf(Word));
    AData.Read(FUnknown2, SizeOf(Byte));
    AData.Read(FQuantity, SizeOf(Byte));
    AData.Read(FAnimID, SizeOf(Word));
    AData.Read(FUnknown3, SizeOf(Byte));
    AData.Read(FHue, SizeOf(Byte));
    AData.Read(FUnknown4, SizeOf(Word));
    AData.Read(FHeight, SizeOf(Byte));
    AData.Read(PChar(FTileName)^, 20);
  end;
  FTileName := Trim(FTileName);
end;

destructor TStaticOldTiledata.Destroy;
begin
  SetLength(FTileName, 0);
  inherited;
end;

function TStaticOldTiledata.Clone: TStaticTiledata;
begin
  Result := TStaticOldTiledata.Create(nil);
  Result.FFlags := FFlags;
  Result.FFlags2:= FFlags2;
  Result.FWeight := FWeight;
  Result.FQuality := FQuality;
  Result.FUnknown1 := FUnknown1;
  Result.FUnknown2 := FUnknown2;
  Result.FQuantity := FQuantity;
  Result.FAnimID := FAnimID;
  Result.FUnknown3 := FUnknown3;
  Result.FHue := FHue;
  Result.FUnknown4 := FUnknown4;
  Result.FHeight := FHeight;
  Result.FTileName := FTileName;
end;

procedure TStaticOldTiledata.Write(AData: TStream);
var
  i: Integer;
begin
  if Length(FTileName) < 20 then
    for i := Length(FTileName) to 20 do
      FTileName := FTileName + #0;
  AData.Write(FFlags, SizeOf(LongWord));
  AData.Write(FWeight, SizeOf(Byte));
  AData.Write(FQuality, SizeOf(Byte));
  AData.Write(FUnknown1, SizeOf(Word));
  AData.Write(FUnknown2, SizeOf(Byte));
  AData.Write(FQuantity, SizeOf(Byte));
  AData.Write(FAnimID, SizeOf(Word));
  AData.Write(FUnknown3, SizeOf(Byte));
  AData.Write(FHue, SizeOf(Byte));
  AData.Write(FUnknown4, SizeOf(Word));
  AData.Write(FHeight, SizeOf(Byte));
  AData.Write(PChar(FTileName)^, 20);
end;

function TStaticOldTiledata.GetSize: Integer;
begin
  GetSize := StaticOldTileDataSize;
end;

{ TLandTileGroup }

constructor TLandTileGroup.Create(AData: TStream);
var
  i: Integer;
begin
  if assigned(AData) then
  begin
    AData.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 31 do
    LandTileData[i] := TLandTiledata.Create(AData);
end;

destructor TLandTileGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 31 do
    LandTileData[i].Free;
  inherited;
end;

function TLandTileGroup.Clone: TLandTileGroup;
var
  i: Integer;
begin
  Result := TLandTileGroup.Create(nil);
  Result.FUnknown := FUnknown;
  for i := 0 to 31 do
    Result.LandTileData[i] := LandTileData[i].Clone;
end;

procedure TLandTileGroup.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FUnknown, SizeOf(LongInt));
  for i := 0 to 31 do
    LandTileData[i].Write(AData);
end;

function TLandTileGroup.GetSize: Integer;
begin
  GetSize := LandTileGroupSize;
end;

{ TLandOldTileGroup }

constructor TLandOldTileGroup.Create(AData: TStream);
var
  i: Integer;
begin
  if assigned(AData) then
  begin
    AData.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 31 do
    LandTileData[i] := TLandOldTiledata.Create(AData);
end;

destructor TLandOldTileGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 31 do
    LandTileData[i].Free;
  inherited;
end;

function TLandOldTileGroup.Clone: TLandTileGroup;
var
  i: Integer;
begin
  Result := TLandOldTileGroup.Create(nil);
  Result.FUnknown := FUnknown;
  for i := 0 to 31 do
    Result.LandTileData[i] := LandTileData[i].Clone;
end;

procedure TLandOldTileGroup.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FUnknown, SizeOf(LongInt));
  for i := 0 to 31 do
    LandTileData[i].Write(AData);
end;

function TLandOldTileGroup.GetSize: Integer;
begin
  GetSize := LandOldTileGroupSize;
end;

{ TStaticTileGroup }

constructor TStaticTileGroup.Create(AData: TStream);
var
  i: Integer;
begin
  if assigned(AData) then
  begin
    AData.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 31 do
    StaticTileData[i] := TStaticTiledata.Create(AData);
end;

destructor TStaticTileGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 31 do
    StaticTileData[i].Free;
  inherited;
end;

function TStaticTileGroup.Clone: TStaticTileGroup;
var
  i: Integer;
begin
  Result := TStaticTileGroup.Create(nil);
  Result.FUnknown := FUnknown;
  for i := 0 to 31 do
    Result.StaticTileData[i] := StaticTileData[i].Clone;
end;

procedure TStaticTileGroup.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FUnknown, SizeOf(LongInt));
  for i := 0 to 31 do
    StaticTileData[i].Write(AData);
end;

function TStaticTileGroup.GetSize: Integer;
begin
  GetSize := StaticTileGroupSize;
end;

{ TStaticOldTileGroup }

constructor TStaticOldTileGroup.Create(AData: TStream);
var
  i: Integer;
begin
  if assigned(AData) then
  begin
    AData.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 31 do
    StaticTileData[i] := TStaticOldTiledata.Create(AData);
end;

destructor TStaticOldTileGroup.Destroy;
var
  i: Integer;
begin
  for i := 0 to 31 do
    StaticTileData[i].Free;
  inherited;
end;

function TStaticOldTileGroup.Clone: TStaticTileGroup;
var
  i: Integer;
begin
  Result := TStaticOldTileGroup.Create(nil);
  Result.FUnknown := FUnknown;
  for i := 0 to 31 do
    Result.StaticTileData[i] := StaticTileData[i].Clone;
end;

procedure TStaticOldTileGroup.Write(AData: TStream);
var
  i: Integer;
begin
  AData.Write(FUnknown, SizeOf(LongInt));
  for i := 0 to 31 do
    StaticTileData[i].Write(AData);
end;

function TStaticOldTileGroup.GetSize: Integer;
begin
  GetSize := StaticOldTileGroupSize;
end;

end.

