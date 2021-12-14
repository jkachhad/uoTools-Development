(*
 * CDDL HEADER START
 *
 * gfgfgfg
 *)
unit Language;
{$mode objfpc}{$H+}

interface

procedure LanguageLoad(path: string);
function TranslateText(text: string) : string;
function TranslateTextA(text: string) : string;
function GetText(section : string; key: string) : string;
function GetText(key: string) : string;



implementation
uses SysUtils, Classes, IniFiles, LConvEncoding, UConfig;


var
  LangPath : string;
  LangFile : TIniFile;
  CodePage : Integer;
  WinIsoCP : Integer;
  LangAbbr : string;
  LangName : string;

procedure LanguageLoad(path: string);
begin
  if (Copy(path, 2, 1) = ':')
    then LangPath := path
    else LangPath := ExtractFilePath(ParamStr(0)) + path;

  if not FileExists(LangPath) then begin
    Writeln(TranslateText('Language files doesn''t exists: "' + LangPath + '"'));
    Halt;
  end;

  LangFile := TIniFile.Create(LangPath);
  LangAbbr := LangFile.ReadString(   'info', 'LangAbbr', '');
  WinIsoCP := LangFile.ReadInteger('  info', 'CodePage', 1250);
  CodePage := LangFile.ReadInteger('Server', 'CodePage',  850);
  LangName := LangFile.ReadString('info', 'Language', '');

  Writeln(TimeStamp, GetText('iLangUse') + ' ', TranslateText(LangName));
end;

function  LanguageGetName() : string;
begin
  Result := LangName;
end;

function TranslateText(text: string) : string;
begin
  case CodePage of
       437 : Result := UTF8ToCP437(text);
       850 : Result := UTF8ToCP850(text);
       866 : Result := UTF8ToCP866(text);
       874 : Result := UTF8ToCP874(text);
       932 : Result := UTF8ToCP932(text);
       936 : Result := UTF8ToCP936(text);
       949 : Result := UTF8ToCP949(text);
       950 : Result := UTF8ToCP950(text);
      1250 : Result := UTF8ToCP1250(text);
      1251 : Result := UTF8ToCP1251(text);
      1252 : Result := UTF8ToCP1252(text);
      1253 : Result := UTF8ToCP1253(text);
      1254 : Result := UTF8ToCP1254(text);
      1255 : Result := UTF8ToCP1255(text);
      1256 : Result := UTF8ToCP1256(text);
      1257 : Result := UTF8ToCP1257(text);
      1258 : Result := UTF8ToCP1258(text);
      else   Result := UTF8ToCP1250(text);
  end;
end;

function TranslateTextA(text: string) : string;
begin
  case WinIsoCP of
      437 : Result := TranslateText(CP437ToUTF8(text));
      850 : Result := TranslateText(CP850ToUTF8(text));
      866 : Result := TranslateText(CP866ToUTF8(text));
      874 : Result := TranslateText(CP874ToUTF8(text));
      932 : Result := TranslateText(CP932ToUTF8(text));
      936 : Result := TranslateText(CP936ToUTF8(text));
      949 : Result := TranslateText(CP949ToUTF8(text));
      950 : Result := TranslateText(CP950ToUTF8(text));
     1250 : Result := TranslateText(CP1250ToUTF8(text));
     1251 : Result := TranslateText(CP1251ToUTF8(text));
     1252 : Result := TranslateText(CP1252ToUTF8(text));
     1253 : Result := TranslateText(CP1253ToUTF8(text));
     1254 : Result := TranslateText(CP1254ToUTF8(text));
     1255 : Result := TranslateText(CP1255ToUTF8(text));
     1256 : Result := TranslateText(CP1256ToUTF8(text));
     1257 : Result := TranslateText(CP1257ToUTF8(text));
     1258 : Result := TranslateText(CP1258ToUTF8(text));
     else   Result := TranslateText(CP1250ToUTF8(text));
    end;
end;

function GetText(section : string; key: string) : string;
var text : string;
begin
  text := LangFile.ReadString(section, key, '');
  if (text <> '')
  then result := TranslateText(text)
  else result := Format('%%%%[%s]%s%%%%', [section, key]);
end;

function GetText(key: string) : string;
begin
  Result := GetText('Server', key);
end;


end.

