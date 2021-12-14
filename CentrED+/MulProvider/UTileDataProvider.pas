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
unit UTileDataProvider;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, UMulProvider, UMulBlock, UTiledata;

type
  TLandTileDataArray = array[$0..$3FFF] of TLandTileData;
  TStaticTileDataArray = array of TStaticTileData;

  { TTiledataProvider }

  TTiledataProvider = class(TMulProvider)
    constructor Create(OldFormat: Boolean; AData: TStream; AReadOnly: Boolean = False); overload;
    constructor Create(OldFormat: Boolean; AData: string; AReadOnly: Boolean = False); overload;
    destructor Destroy; override;
  protected
    FLandTiles: TLandTileDataArray;
    FEmptyLandTile: TLandTileData;
    FStaticTiles: TStaticTileDataArray;
    FEmptyStaticTile: TStaticTileData;
    FStaticCount: Cardinal;
    UseOldTileDataFormat: Boolean;
    procedure InitArray;
    function CalculateOffset(AID: Integer): Integer; override;
    function GetData(AID, AOffset: Integer): TMulBlock; override;
    procedure SetData(AID, AOffset: Integer; ABlock: TMulBlock); override;
    function GetTileData(AID: Integer): TTiledata;
    function GetLandTileData(AID: Integer): TLandTileData;
    function GetStaticTileData(AID: Integer): TStaticTileData;
  public
    function GetBlock(AID: Integer): TMulBlock; override;
    property LandTiles[AID: Integer]: TLandTileData read GetLandTileData;
    property StaticTiles[AID: Integer]: TStaticTileData read GetStaticTileData;
    property TileData[AID: Integer]: TTiledata read GetTileData; //all tiles, no cloning
    property StaticCount: Cardinal read FStaticCount;
  end;

implementation

uses
  Logging;

{ TTiledataProvider }

function TTiledataProvider.CalculateOffset(AID: Integer): Integer;
begin
  Result := GetTileDataOffset(AID, UseOldTileDataFormat);
end;

constructor TTiledataProvider.Create(OldFormat: Boolean; AData: TStream; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AReadOnly);
  UseOldTileDataFormat := OldFormat; //(FData.Size <= 1644544);
  if UseOldTileDataFormat
    then Logger.Send([lcInfo], 'Using Old TileData Format')
    else Logger.Send([lcInfo], 'Using New TileData Format');
  InitArray;
end;

constructor TTiledataProvider.Create(OldFormat: Boolean; AData: string; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AReadOnly);
  UseOldTileDataFormat := OldFormat; //(FData.Size <= 1644544);
  if UseOldTileDataFormat
    then Logger.Send([lcInfo], 'Using Old TileData Format')
    else Logger.Send([lcInfo], 'Using New TileData Format');
  InitArray;
end;

destructor TTiledataProvider.Destroy;
var
  i: Integer;
begin
  for i := $0 to $3FFF do
    FreeAndNil(FLandTiles[i]);
  for i := 0 to FStaticCount - 1 do
    FreeAndNil(FStaticTiles[i]);

  inherited Destroy;
end;

function TTiledataProvider.GetBlock(AID: Integer): TMulBlock;
begin
  Result := GetData(AID, 0);
end;

function TTiledataProvider.GetData(AID, AOffset: Integer): TMulBlock;
begin
  if AID < $4000 then
    Result := TMulBlock(FLandTiles[AID].Clone)
  else
    Result := TMulBlock(FStaticTiles[AID - $4000].Clone);
  Result.ID := AID;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

procedure TTiledataProvider.InitArray;
var
  i: Integer;
begin
  FData.Position := 0;
  Logger.Send([lcInfo], 'Loading $4000 LandTiledata Entries');
  if UseOldTileDataFormat
    then for i := $0 to $3FFF do
      begin
        if i mod 32 = 0 then
          FData.Seek(4, soFromCurrent);
        FLandTiles[i] := TLandOldTileData.Create(FData);
      end
    else for i := $0 to $3FFF do
      begin
        if i mod 32 = 0 then
          FData.Seek(4, soFromCurrent);
        FLandTiles[i] := TLandTileData.Create(FData);
      end;

  if UseOldTileDataFormat
    then FStaticCount := ((FData.Size - FData.Position) div StaticOldTileGroupSize) * 32
    else FStaticCount := ((FData.Size - FData.Position) div StaticTileGroupSize) * 32;
  Logger.Send([lcInfo], 'Loading $%x StaticTiledata Entries', [FStaticCount]);
  SetLength(FStaticTiles, FStaticCount);

  if UseOldTileDataFormat
    then for i := 0 to FStaticCount - 1 do
      begin
        if i mod 32 = 0 then
          FData.Seek(4, soFromCurrent);
        FStaticTiles[i] := TStaticOldTileData.Create(FData);
      end
    else for i := 0 to FStaticCount - 1 do
      begin
        if i mod 32 = 0 then
          FData.Seek(4, soFromCurrent);
        FStaticTiles[i] := TStaticTileData.Create(FData);
      end;

  // empty
  FEmptyLandTile := FLandTiles[0].Clone;
  FEmptyLandTile.Flags      := [];
  FEmptyLandTile.TextureID  := 0;

  FEmptyStaticTile := FStaticTiles[0].Clone;
  FEmptyStaticTile.Flags    := [];
  //FEmptyStaticTile.Flags2   := 0;
  FEmptyStaticTile.TileName := '!! NOT EXISTING !!';
  FEmptyStaticTile.Weight   := 0;
  FEmptyStaticTile.Quality  := 0;
  FEmptyStaticTile.Unknown1 := 0;
  FEmptyStaticTile.Unknown2 := 0;
  FEmptyStaticTile.Quantity := 0;
  FEmptyStaticTile.AnimID   := 0;
  FEmptyStaticTile.Unknown3 := 0;
  FEmptyStaticTile.Hue      := 0;
  FEmptyStaticTile.Unknown4 := 0;
  FEmptyStaticTile.Height   := 0;

end;

procedure TTiledataProvider.SetData(AID, AOffset: Integer;
  ABlock: TMulBlock);
begin
  if AID >= $4000 + FStaticCount then
    Exit;

  if AID < $4000 then
  begin
    FreeAndNil(FLandTiles[AID]);
    if UseOldTileDataFormat
      then FLandTiles[AID] := TLandOldTileData(ABlock.Clone)
      else FLandTiles[AID] := TLandTileData(ABlock.Clone);
  end else
  begin
    FreeAndNil(FStaticTiles[AID - $4000]);
    if UseOldTileDataFormat
      then FStaticTiles[AID - $4000] := TStaticOldTileData(ABlock.Clone)
      else FStaticTiles[AID - $4000] := TStaticTileData(ABlock.Clone);
  end;

  if not FReadOnly then
  begin
    FData.Position := AOffset;
    ABlock.Write(FData);
  end;
end;

function TTiledataProvider.GetTileData(AID: Integer): TTiledata;
begin
  if AID < $4000 then
    Result := FLandTiles[AID]
  else
    Result := StaticTiles[AID - $4000];
end;

function TTiledataProvider.GetLandTileData(AID: Integer): TLandTileData;
begin
  if (AID >= 0) and (AID < $4000) then
    Result := FLandTiles[AID]
  else
    Result := FEmptyLandTile;
end;

function TTiledataProvider.GetStaticTileData(AID: Integer): TStaticTileData;
begin
  if (AID >= 0) and (AID < FStaticCount) then
    Result := FStaticTiles[AID]
  else
    Result := FEmptyStaticTile;
end;

end.

