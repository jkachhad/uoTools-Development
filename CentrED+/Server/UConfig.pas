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
 *      Portions Copyright 2008 Andreas Schneider
 *)
unit UConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, XMLRead, XMLWrite, md5, Keyboard, UAccount,
  UXmlHelper, UInterfaces, UEnums, URegions, LConvEncoding, Language;

type

  TInvalidConfigException = class(Exception);

  { TMapInfo }

  TMapInfo = class(TObject, ISerializable)
    constructor Create(AOwner: IInvalidate);
    constructor Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
    procedure Serialize(AElement: TDOMElement);
  protected
    FOwner: IInvalidate;
    FMapFile: string;
    FStaticsFile: string;
    FStaIdxFile: string;
    FWidth: Word;
    FHeight: Word;
    FFormatFlags: Cardinal;
    procedure SetHeight(const AValue: Word);
    procedure SetMapFile(const AValue: string);
    procedure SetStaIdxFile(const AValue: string);
    procedure SetStaticsFile(const AValue: string);
    procedure SetWidth(const AValue: Word);
    procedure SetFormatFlags(const AValue: Cardinal);
  public
    property MapFile: string read FMapFile write SetMapFile;
    property StaticsFile: string read FStaticsFile write SetStaticsFile;
    property StaIdxFile: string read FStaIdxFile write SetStaIdxFile;
    property Width: Word read FWidth write SetWidth;
    property Height: Word read FHeight write SetHeight;
    property FormatFlags: Cardinal read FFormatFlags write SetFormatFlags;
  end;

  { TConfig }

  TConfig = class(TObject, ISerializable, IInvalidate)
    constructor Create(AFilename: string);
    constructor Init(AFilename: string);
    destructor Destroy; override;
    procedure Serialize(AElement: TDOMElement);
  protected
    FFilename: string;
    FPort: Integer;
    FMap: TMapInfo;
    FTiledata: string;
    FRadarcol: string;
    FLanguage: string;
    FRegions: TRegionList;
    FAccounts: TAccountList;
    FChanged: Boolean;
    procedure SetPort(const AValue: Integer);
    procedure SetRadarcol(const AValue: string);
    procedure SetTiledata(const AValue: string);
  public
    property Port: Integer read FPort write SetPort;
    property Map: TMapInfo read FMap;
    property Tiledata: string read FTiledata write SetTiledata;
    property Radarcol: string read FRadarcol write SetRadarcol;
    property Regions: TRegionList read FRegions;
    property Accounts: TAccountList read FAccounts;
    property Language: string read FLanguage;
    procedure Flush;
    procedure Invalidate;
  end;
  
var
  AppDir: string;
  ConfigFile: string;
  Config: TConfig;
  tmp_i: Integer;
  
function TimeStamp: string;

implementation

const
  CONFIGVERSION = 5;
  
function QueryPassword: String;
var
  pwChar: char;
begin
  Result := '';

  InitKeyboard;
  try
    repeat
      pwChar := GetKeyEventChar(TranslateKeyEvent(GetKeyEvent));
      case pwChar of
        #8: Result := Copy(Result, 1, Length(Result) - 1);
        #13: break;
      else
        Result := Result + pwChar;
      end;
    until pwChar = #13;
  finally
    DoneKeyboard;
  end;
  writeln('');
end;

function TimeStamp: string;
begin
  //Result := '[' + DateTimeToStr(Now) + '] ';
  //Result := FormatDateTime('[yyyy.mm.dd hh:mm:ss] ', Now);
  Result := FormatDateTime('[hh:mm:ss] ', Now);
end;

{ TMapInfo }

constructor TMapInfo.Create(AOwner: IInvalidate);
begin
  inherited Create;
  FOwner := AOwner;
end;

constructor TMapInfo.Deserialize(AOwner: IInvalidate; AElement: TDOMElement);
begin
  Create(AOwner);
  FMapFile := TXmlHelper.ReadString(AElement, 'Map', 'map0.mul');
  FStaIdxFile := TXmlHelper.ReadString(AElement, 'StaIdx', 'staidx0.mul');
  FStaticsFile := TXmlHelper.ReadString(AElement, 'Statics', 'statics0.mul');
  FWidth := TXmlHelper.ReadInteger(AElement, 'Width', 768);
  FHeight := TXmlHelper.ReadInteger(AElement, 'Height', 512);
  FFormatFlags := $F0000000 + Cardinal(TXmlHelper.ReadInteger(AElement, 'Format', $0000) and $0000FFFF);
end;

procedure TMapInfo.Serialize(AElement: TDOMElement);
begin
  TXmlHelper.WriteString(AElement, 'Map',    FMapFile);
  TXmlHelper.WriteString(AElement, 'StaIdx', FStaIdxFile);
  TXmlHelper.WriteString(AElement, 'Statics',FStaticsFile);
  TXmlHelper.WriteInteger(AElement,'Width',  FWidth);
  TXmlHelper.WriteInteger(AElement,'Height', FHeight);
  TXmlHelper.WriteString(AElement, 'Format', Format('0x%.8x', [(FFormatFlags and $0000FFFF)]));
end;


procedure TMapInfo.SetMapFile(const AValue: string);
begin
  FMapFile := AValue;
  FOwner.Invalidate;
end;

procedure TMapInfo.SetStaIdxFile(const AValue: string);
begin
  FStaIdxFile := AValue;
  FOwner.Invalidate;
end;

procedure TMapInfo.SetStaticsFile(const AValue: string);
begin
  FStaticsFile := AValue;
  FOwner.Invalidate;
end;

procedure TMapInfo.SetWidth(const AValue: Word);
begin
  FWidth := AValue;
  FOwner.Invalidate;
end;

procedure TMapInfo.SetHeight(const AValue: Word);
begin
  FHeight := AValue;
  FOwner.Invalidate;
end;

procedure TMapInfo.SetFormatFlags(const AValue: Cardinal);
begin
  FFormatFlags := AValue;
  FOwner.Invalidate;
end;

{ TConfig }

constructor TConfig.Create(AFilename: string);
var
  xmlDoc: TXMLDocument;
  version: Integer;
  xmlElement: TDOMElement;
begin
  inherited Create;
  FFilename := AFilename;
  ReadXMLFile(xmlDoc, AFilename);
  version := 0;
  if not ((xmlDoc.DocumentElement.NodeName = 'CEDConfig') and
    TryStrToInt(xmlDoc.DocumentElement.AttribStrings['Version'], version) and
    (version = CONFIGVERSION)) then
    raise TInvalidConfigException.Create(Format('Version mismatch: %d <> %d', [version, CONFIGVERSION]));

  FPort := TXmlHelper.ReadInteger(xmlDoc.DocumentElement, 'Port', 2597);

  xmlElement := TDOMElement(xmlDoc.DocumentElement.FindNode('Map'));
  if not assigned(xmlElement) then
    raise TInvalidConfigException.Create('Map information not found');
  FMap := TMapInfo.Deserialize(Self, xmlElement);
  
  FTiledata := TXmlHelper.ReadString(xmlDoc.DocumentElement, 'Tiledata', 'tiledata.mul');
  FRadarcol := TXmlHelper.ReadString(xmlDoc.DocumentElement, 'Radarcol', 'radarcol.mul');
  FLanguage := TXmlHelper.ReadString(xmlDoc.DocumentElement, 'Language', '..\Language\English.ini');
  xmlElement := TDOMElement(xmlDoc.DocumentElement.FindNode('Regions'));

  if assigned(xmlElement) then
    FRegions := TRegionList.Deserialize(Self, xmlElement)
  else
    Fregions := TRegionList.Create(Self);

  xmlElement := TDOMElement(xmlDoc.DocumentElement.FindNode('Accounts'));
  if not assigned(xmlElement) then
    raise TInvalidConfigException.Create('Account information not found');
  FAccounts := TAccountList.Deserialize(Self, xmlElement);

  xmlDoc.Free;
  
  FChanged := False;
end;

constructor TConfig.Init(AFilename: string);
var
  stringValue, password: string;
  intValue: Integer;
begin
  inherited Create;
  FFilename := AFilename;
  FMap := TMapInfo.Create(Self);
  FAccounts := TAccountList.Create(Self);
  FRegions := TRegionList.Create(Self);

  Writeln('');
  Writeln('==============');
  FLanguage := '../Language/English.ini';
  Writeln(UTF8ToCP866('language file [' + FLanguage + ']'));
  Readln (stringValue);
  if (stringValue <> '')
  then FLanguage := stringValue;
  LanguageLoad(FLanguage);
  Writeln('');

  Writeln(GetText('iNetwork'));
  Writeln('==============');
  Write  (GetText('iSetPort') + UTF8ToCP866(' [2597]: '));
  Readln (stringValue);
  intValue := 0;
  if not TryStrToInt(stringValue, intValue) then intValue := 2597;
  FPort := intValue;
  Writeln('');

  Writeln(GetText('iDatPath'));
  Writeln('===============');
  Write  ('map      [map0.mul]: ');
  Readln (FMap.FMapFile);
  if FMap.MapFile = '' then FMap.MapFile := 'map0.mul';
  Write  ('staidx   [staidx0.mul]: ');
  Readln (FMap.FStaIdxFile);
  if FMap.StaIdxFile = '' then FMap.StaIdxFile := 'staidx0.mul';
  Write  ('statics  [statics0.mul]: ');
  Readln (FMap.FStaticsFile);
  if FMap.StaticsFile = '' then FMap.StaticsFile := 'statics0.mul';
  Write  ('tiledata [tiledata.mul]: ');
  Readln (FTiledata);
  if FTiledata = '' then FTiledata := 'tiledata.mul';
  Write  ('radarcol [radarcol.mul]: ');
  Readln (FRadarcol);
  if FRadarcol = '' then FRadarcol := 'radarcol.mul';
  Writeln('');

  Writeln(GetText('iMapDesc'));
  Writeln('===============');
  Write  (GetText('iMapWidt') + UTF8ToCP866(' [768]: '));
  Readln (stringValue);
  if not TryStrToInt(stringValue, intValue) then intValue := 768;
  FMap.Width := intValue;
  Write  (GetText('iMapHeig') + UTF8ToCP866(' [512]: '));
  Readln (stringValue);
  if not TryStrToInt(stringValue, intValue) then intValue := 512;
  FMap.Height := intValue;
  Write  (GetText('iDFormat') + UTF8ToCP866(' [0x0000]: '));
  Readln (stringValue);
  if not TryStrToInt(stringValue, intValue) then intValue := $0000;
  FMap.FormatFlags := $F0000000 + Cardinal(intValue);
  Writeln('');

  Writeln(GetText('iAccount'));
  Writeln('======================');
  repeat
    Write(GetText('iUserAcc') + UTF8ToCP866(' '));
    Readln(stringValue);
  until stringValue <> '';
  Write  (GetText('iUserPas') + UTF8ToCP866(' '));
  password := QueryPassword;
  FAccounts.Add(TAccount.Create(FAccounts, stringValue,
    MD5Print(MD5String(password)), alAdministrator, nil));
  
  FChanged := True;
end;

destructor TConfig.Destroy;
begin
  if Assigned(FMap) then FreeAndNil(FMap);
  if Assigned(FAccounts) then FreeAndNil(FAccounts);
  if Assigned(FRegions) then FreeAndNil(FRegions);
  inherited Destroy;
end;

procedure TConfig.Serialize(AElement: TDOMElement);
begin
  TXmlHelper.WriteString(AElement, 'Language', FLanguage);
  TXmlHelper.WriteInteger(AElement, 'Port', FPort);
  FMap.Serialize(TXmlHelper.AssureElement(AElement, 'Map'));
  TXmlHelper.WriteString(AElement, 'Tiledata', FTiledata);
  TXmlHelper.WriteString(AElement, 'Radarcol', FRadarcol);
  FAccounts.Serialize(TXmlHelper.AssureElement(AElement, 'Accounts'));
  FRegions.Serialize(TXmlHelper.AssureElement(AElement, 'Regions'));
end;

procedure TConfig.SetPort(const AValue: Integer);
begin
  FPort := AValue;
  Invalidate;
end;

procedure TConfig.SetRadarcol(const AValue: string);
begin
  FRadarcol := AValue;
  Invalidate;
end;

procedure TConfig.SetTiledata(const AValue: string);
begin
  FTiledata := AValue;
  Invalidate;
end;

procedure TConfig.Flush;
var
  xmlDoc: TXMLDocument;
begin
  if FChanged then
  begin
    xmlDoc := TXMLDocument.Create;
    xmlDoc.AppendChild(xmlDoc.CreateElement('CEDConfig'));
    xmlDoc.DocumentElement.AttribStrings['Version'] := IntToStr(CONFIGVERSION);
    Serialize(xmlDoc.DocumentElement);
    WriteXMLFile(xmlDoc, FFilename);
    xmlDoc.Free;
    FChanged := False;
  end;
end;

procedure TConfig.Invalidate;
begin
  FChanged := True;
end;

initialization
begin
  AppDir := ExtractFilePath(ParamStr(0));
  if AppDir[Length(AppDir)] <> PathDelim then
    AppDir := AppDir + PathDelim;

  Config := nil;
  ConfigFile := '';
  for tmp_i := 0 to ParamCount do begin
    if LowerCase(ExtractFileExt(ParamStr(tmp_i))) = '.xml'
      then ConfigFile := ExtractFilePath(ParamStr(0)) + ParamStr(1);
  end;
  if ConfigFile = ''
      then ConfigFile := ChangeFileExt(ParamStr(0), '.xml');
end;

end.

