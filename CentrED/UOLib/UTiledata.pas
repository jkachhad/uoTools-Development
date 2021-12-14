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
 *      Portions Copyright 2012 Andreas Schneider
 *)
unit UTiledata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMulBlock;

const
  LandTileDataSize = 26;
  LandTileGroupSize = 4 + 32 * LandTileDataSize;
  StaticTileDataSize = 37;
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

  TTileDataVersion = (tdvLegacy, tdvHighSeas);

  { TTiledata }

  TTiledata = class(TMulBlock)
  protected
    FVersion: TTileDataVersion;
    FFlags: TTileDataFlags;
    FNewFlags: LongWord; //HighSeas added new flags
    FTileName: string;
    procedure ReadFlags(AData: TStream);
    procedure WriteFlags(AData: TStream);
    procedure PopulateClone(const AClone: TTiledata);
  public
    property Flags: TTileDataFlags read FFlags write FFlags;
    property TileName: string read FTileName write FTileName;
  end;

  { TLandTiledata }

  TLandTiledata = class(TTiledata)
    constructor Create(AData: TStream; AVersion: TTileDataVersion = tdvLegacy);
    destructor Destroy; override;
    function Clone: TLandTiledata; override;
    function GetSize: Integer; override;
    procedure Write(AData: TStream); override;
  protected
    FTextureID: Word;
    procedure PopulateClone(const AClone: TLandTiledata);
  public
    property TextureID: Word read FTextureID write FTextureID;
  end;

  { TStaticTiledata }

  TStaticTiledata = class(TTiledata)
    constructor Create(AData: TStream; AVersion: TTileDataVersion = tdvLegacy);
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
    procedure PopulateClone(const AClone: TStaticTiledata);
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

  { TLandTileGroup }

  TLandTileGroup = class(TMulBlock)
    constructor Create(AData: TStream; AVersion: TTileDataVersion = tdvLegacy);
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

  { TStaticTileGroup }

  TStaticTileGroup = class(TMulBlock)
    constructor Create(AData: TStream; AVersion: TTileDataVersion = tdvLegacy);
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

function GetTileDataOffset(ABlock: Integer): Integer;

implementation

function GetTileDataOffset(ABlock: Integer): Integer;
var
  group, tile: Integer;
begin
  if ABlock > $3FFF then
  begin
    ABlock := ABlock - $4000;
    group := ABlock div 32;
    tile := ABlock mod 32;

    Result := 512 * LandTileGroupSize + group * StaticTileGroupSize + 4
      + tile * StaticTileDataSize;
  end else
  begin
    group := ABlock div 32;
    tile := ABlock mod 32;

    Result := group * LandTileGroupSize + 4 + tile * LandTileDataSize;
  end;
end;

{ TTiledata }

procedure TTiledata.ReadFlags(AData: TStream);
begin
  AData.Read(FFlags, SizeOf(LongWord));
  if FVersion >= tdvHighSeas then
    AData.Read(FNewFlags, SizeOf(LongWord));
end;

procedure TTiledata.WriteFlags(AData: TStream);
begin
  AData.Write(FFlags, SizeOf(LongWord));
  if FVersion >= tdvHighSeas then
    AData.Write(FNewFlags, SizeOf(LongWord));
end;

procedure TTiledata.PopulateClone(const AClone: TTiledata);
begin
  AClone.FVersion := FVersion;
  AClone.FFlags := FFlags;
  AClone.FNewFlags := FNewFlags;
  AClone.FTileName := FTileName;
end;

{ TLandTiledata }

constructor TLandTiledata.Create(AData: TStream;
  AVersion: TTileDataVersion = tdvLegacy);
var
  legacyFlags: LongWord;
  highSeasFlags: QWord;
begin
  FVersion := AVersion;
  SetLength(FTileName, 20);
  if assigned(AData) then
  begin
    ReadFlags(AData);
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
  PopulateClone(Result);
end;

procedure TLandTiledata.Write(AData: TStream);
var
  i: Integer;
begin
  if Length(FTileName) < 20 then
    for i := Length(FTileName) to 20 do
      FTileName := FTileName + #0;

  WriteFlags(AData);
  AData.Write(FTextureID, SizeOf(Word));
  AData.Write(PChar(FTileName)^, 20);
end;

procedure TLandTiledata.PopulateClone(const AClone: TLandTiledata);
begin
  inherited PopulateClone(AClone);
  AClone.FTextureID := FTextureID;
end;

function TLandTiledata.GetSize: Integer;
begin
  GetSize := LandTileDataSize;
end;

{ TStaticTiledata}

constructor TStaticTiledata.Create(AData: TStream;
  AVersion: TTileDataVersion = tdvLegacy);
begin
  FVersion := AVersion;
  SetLength(FTileName, 20);
  if AData <> nil then
  begin
    ReadFlags(AData);
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
  PopulateClone(Result);
end;

procedure TStaticTiledata.Write(AData: TStream);
var
  i: Integer;
begin
  if Length(FTileName) < 20 then
    for i := Length(FTileName) to 20 do
      FTileName := FTileName + #0;

  WriteFlags(AData);
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

procedure TStaticTiledata.PopulateClone(const AClone: TStaticTiledata);
begin
  inherited PopulateClone(AClone);
  AClone.FWeight := FWeight;
  AClone.FQuality := FQuality;
  AClone.FUnknown1 := FUnknown1;
  AClone.FUnknown2 := FUnknown2;
  AClone.FQuantity := FQuantity;
  AClone.FAnimID := FAnimID;
  AClone.FUnknown3 := FUnknown3;
  AClone.FHue := FHue;
  AClone.FUnknown4 := FUnknown4;
  AClone.FHeight := FHeight;
end;

function TStaticTiledata.GetSize: Integer;
begin
  GetSize := StaticTileDataSize;
end;

{ TLandTileGroup }

constructor TLandTileGroup.Create(AData: TStream;
  AVersion: TTileDataVersion = tdvLegacy);
var
  i: Integer;
begin
  if assigned(AData) then
  begin
    AData.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 31 do
    LandTileData[i] := TLandTiledata.Create(AData, AVersion);
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

{ TStaticTileGroup }

constructor TStaticTileGroup.Create(AData: TStream;
  AVersion: TTileDataVersion = tdvLegacy);
var
  i: Integer;
begin
  if assigned(AData) then
  begin
    AData.Read(FUnknown, SizeOf(LongInt));
  end;
  for i := 0 to 31 do
    StaticTileData[i] := TStaticTiledata.Create(AData, AVersion);
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

end.

