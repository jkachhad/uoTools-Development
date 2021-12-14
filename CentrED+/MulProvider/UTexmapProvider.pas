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
unit UTexmapProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, UMulProvider, UMulBlock, UGenericIndex, UTexture;

type
  TTexmapProvider = class(TIndexedMulProvider)
    constructor Create(OldFormat: Boolean; AData, AIndex: TStream; AReadOnly: Boolean = False); overload;
    constructor Create(OldFormat: Boolean; AData, AIndex: string; AReadOnly: Boolean = False); overload;
  protected
    UseOldArtFormat: Boolean;
    function GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock; override;
  end;

implementation

uses
  Logging;

{ TTexmapProvider }

constructor TTexmapProvider.Create(OldFormat: Boolean; AData, AIndex: TStream; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AIndex, AReadOnly);
  UseOldArtFormat := OldFormat;
  if UseOldArtFormat
    then Logger.Send([lcInfo], 'Using textures in ArtData')
    else Logger.Send([lcInfo], 'Using textures in TexMaps');
end;

constructor TTexmapProvider.Create(OldFormat: Boolean; AData, AIndex: string; AReadOnly: Boolean = False);
begin
  inherited Create(AData, AIndex, AReadOnly);
  UseOldArtFormat := OldFormat;
  if UseOldArtFormat
    then Logger.Send([lcInfo], 'Using textures in ArtData')
    else Logger.Send([lcInfo], 'Using textures in TexMaps');
end;

function TTexmapProvider.GetData(AID: Integer; AIndex: TGenericIndex): TMulBlock;
begin
  if (AIndex.Lookup > -1) and (AIndex.Size > 0) then begin
    //Result := TTexture.Create(FData, AIndex)
    if not UseOldArtFormat
      then Result := TTexture.Create(FData, AIndex)
      else Result := TOldTexture.Create(Cardinal(AID));
  end else
    Result := TTexture.Create(-1);
  Result.ID := AID;
end;

end.

 
