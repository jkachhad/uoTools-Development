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
unit UArtProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, UMulProvider, UMulBlock, UGenericIndex, UArt, UHue, types;

type
  TArtProvider = class(TIndexedMulProvider)
    constructor Create(OldFormat: Boolean; AData, AIndex: TStream; AReadOnly: Boolean = False); overload;
    constructor Create(OldFormat: Boolean; AData, AIndex: string; AReadOnly: Boolean = False); overload;
  protected
    UseOldArtFormat: Boolean;
    function GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock; override;
    function GetArtData(AID: Integer; AIndex: TGenericIndex; AColor: Word;
      AHue: THue; APartialHue: Boolean): TArt;
  public
    function GetArt(AID: Integer; AColor: Word; AHue: THue; APartialHue: Boolean): TArt;
    function GetFlatLand(AID: Integer): TArt;
    function GetArtSize(AID: Integer): TSize;
  end;

implementation

uses
  Logging;


{ TArtProvider }

constructor TArtProvider.Create(OldFormat: Boolean; AData, AIndex: TStream; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AIndex, AReadOnly);
  UseOldArtFormat := OldFormat;
  if UseOldArtFormat
    then Logger.Send([lcInfo], 'Using pre-Alpha ArtData Format')
    else Logger.Send([lcInfo], 'Using common ArtData Format');
end;

constructor TArtProvider.Create(OldFormat: Boolean; AData, AIndex: string; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AIndex, AReadOnly);
  UseOldArtFormat := OldFormat;
  if UseOldArtFormat
    then Logger.Send([lcInfo], 'Using pre-Alpha ArtData Format')
    else Logger.Send([lcInfo], 'Using common ArtData Format');
end;

function TArtProvider.GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock;
begin
  Result := GetArtData(AID, AIndex, clBlack, nil, False);
end;

function TArtProvider.GetArtData(AID: Integer; AIndex: TGenericIndex;
  AColor: Word; AHue: THue; APartialHue: Boolean): TArt;
begin
  if (AIndex.Lookup > -1) and (AIndex.Size > 0) then
  begin
    if AID < $4000 then
      Result := TArt.Create(FData, AIndex, atLand, AColor, AHue, APartialHue)
    else
      Result := TArt.Create(FData, AIndex, atStatic, AColor, AHue, APartialHue, UseOldArtFormat);
  end
  else
  begin
    if AID < $4000 then
      Result := TArt.Create(nil, nil, atLand, AColor, AHue, APartialHue)
    else
      Result := TArt.Create(nil, nil, atStatic, AColor, AHue, APartialHue, UseOldArtFormat);
  end;
  Result.ID := AID;
end;

function TArtProvider.GetArt(AID: Integer; AColor: Word; AHue: THue;
  APartialHue: Boolean): TArt;
var
  genericIndex: TGenericIndex;
begin
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex := TGenericIndex.Create(FIndex);
  Result := GetArtData(AID, genericIndex, AColor, AHue, APartialHue);
  genericIndex.Free;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

function TArtProvider.GetFlatLand(AID: Integer): TArt;
var
  genericIndex: TGenericIndex;
begin
  FIndex.Position := CalculateIndexOffset(AID);
  genericIndex := TGenericIndex.Create(FIndex);
  Result := TArt.Create(FData, genericIndex, atLandFlat, UseOldArtFormat);
  genericIndex.Free;
  Result.OnChanged := @OnChanged;
  Result.OnFinished := @OnFinished;
end;

function TArtProvider.GetArtSize(AID: Integer): TSize;
var
  genericIndex: TGenericIndex;
  //genericBlock: TMemoryStream;
  value: SmallInt;
begin
  if (AID < $4000)
  then Result := Size(44, 44)
  else begin
    FIndex.Position := CalculateIndexOffset(AID);
    genericIndex := TGenericIndex.Create(FIndex);
    if (genericIndex.Lookup <> -1) and (genericIndex.Size > 0) then begin
      FData.Position := genericIndex.Lookup + 4;
      //genericBlock := TMemoryStream.Create;
      //genericBlock.CopyFrom(FData, 8);
      //genericBlock.Position := 4;

      FData.Read(value, SizeOf(SmallInt));   Result.cx := value;
      FData.Read(value, SizeOf(SmallInt));   Result.cy := value;

      //if Assigned(genericBlock) then genericBlock.Free;
    end else
      Result := Size(0, 0);
    genericIndex.Free;
  end;
end;

end.

