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
unit UGameResources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, UArtProvider, UTileDataProvider, UTexmapProvider,
  ULandscape, UHueProvider, UAnimDataProvider, ULightProvider;

type

  { TGameResourceManager }

  TGameResourceManager = class
    constructor Create(ADataDir: String; Flags: Cardinal; out Success: Boolean);
    destructor Destroy; override;
  protected
    { Members }
    FDataDir: String;
    FArtProvider: TArtProvider;
    FTiledataProvider: TTiledataProvider;
    FAnimdataProvider: TAnimdataProvider;
    FTexmapProvider: TTexmapProvider;
    FHueProvider: THueProvider;
    FLightProvider: TLightProvider;
    FLandscape: TLandscape;
  public
    { Fields }
    property Art: TArtProvider read FArtProvider;
    property Hue: THueProvider read FHueProvider;
    property Landscape: TLandscape read FLandscape;
    property Tiledata: TTiledataProvider read FTiledataProvider;
    property Animdata: TAnimDataProvider read FAnimdataProvider;
    property Texmaps: TTexmapProvider read FTexmapProvider;
    property Lights: TLightProvider read FLightProvider;

    { Methods }
    function GetFile(AFileName: String): String;
    procedure InitLandscape(AWidth, AHeight: Word);
  public
    lbDlgErrorFilePathCaption: string;
    lbDlgErrorFilePathMsg: string;
  end;

var
  GameResourceManager: TGameResourceManager;
  ResMan: TGameResourceManager absolute GameResourceManager;

function InitGameResourceManager(ADataDir: String; Flags: Cardinal): Boolean;

implementation

uses
  UStatics, UfrmInitialize, Language;

//var
//  GameResourceInited : Boolean;//:= False;

function InitGameResourceManager(ADataDir: String; Flags: Cardinal): Boolean;
begin
  FreeAndNil(GameResourceManager);
  GameResourceManager := TGameResourceManager.Create(ADataDir, Flags, Result);
end;

{ TGameResourceManager }

constructor TGameResourceManager.Create(ADataDir: String; Flags: Cardinal; out Success: Boolean);
begin
  inherited Create;
  LanguageTranslate(nil, nil, self);
  FDataDir := IncludeTrailingPathDelimiter(ADataDir);

  // 0xF0   - FlagsData Version Type
  // 0x0000 - UnUsed
  // 0x01   - pre-alpha client ()
  // 0x02   - Reserved (alpha client?)
  // 0x04   - Reserved (use Verdata)
  // 0x08   - HS Client Format

  // Проверка путей
  if (not FileExists(GetFile('art.mul'))) or
     (not FileExists(GetFile('artidx.mul'))) or
     (not FileExists(GetFile('hues.mul'))) or
     (not FileExists(GetFile('tiledata.mul'))) or
     (not FileExists(GetFile('animdata.mul'))  and ((Flags and $01) = 0)) or
     (not FileExists(GetFile('texmaps.mul'))   and ((Flags and $01) = 0)) or
     (not FileExists(GetFile('texidx.mul'))    and ((Flags and $01) = 0)) or
     (not FileExists(GetFile('light.mul'))     and ((Flags and $01) = 0)) or
     (not FileExists(GetFile('lightidx.mul'))  and ((Flags and $01) = 0)) then
  begin
    MessageDlg(lbDlgErrorFilePathCaption, lbDlgErrorFilePathMsg, mtWarning, [mbOK], 0);
    Success := False;
    Destroy;  exit;
  end;

  UseStaticsOldFormat := (Flags and $01) <> 0;

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['art.mul, artidx.mul']));
  FArtProvider := TArtProvider.Create((Flags and $01) <> 0, GetFile('art.mul'), GetFile('artidx.mul'), True);

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['tiledata.mul']));
  FTiledataProvider := TTiledataProvider.Create((Flags and $08) = 0, GetFile('tiledata.mul'), True);

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['animdata.mul']));
  FAnimdataProvider := TAnimDataProvider.Create(GetFile('animdata.mul'), True);

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['texmaps.mul, texidx.mul']));
  FTexmapProvider := TTexmapProvider.Create(Boolean(Flags and $01), GetFile('texmaps.mul'), GetFile('texidx.mul'), True);

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['hues.mul']));
  FHueProvider := THueProvider.Create(GetFile('hues.mul'), True);

  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['light.mul, lightidx.mul']));
  FLightProvider := TLightProvider.Create(GetFile('light.mul'), GetFile('lightidx.mul'), True);

  Success := True;
end;

destructor TGameResourceManager.Destroy;
begin
  FreeAndNil(FArtProvider);
  FreeAndNil(FTiledataProvider);
  FreeAndNil(FAnimdataProvider);
  FreeAndNil(FTexmapProvider);
  FreeAndNil(FHueProvider);
  FreeAndNil(FLightProvider);
  FreeAndNil(FLandscape);
  inherited Destroy;
end;

function TGameResourceManager.GetFile(AFileName: String): String;
begin
  Result := FDataDir + AFileName;
end;

procedure TGameResourceManager.InitLandscape(AWidth, AHeight: Word);
begin
  frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['Landscape']));

  FreeAndNil(FLandscape);
  FLandscape := TLandscape.Create(AWidth, AHeight);
end;

finalization
  FreeAndNil(GameResourceManager);

end.
