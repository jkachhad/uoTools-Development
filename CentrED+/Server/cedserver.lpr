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
 *      Portions Copyright 2015 Andreas Schneider
 *      Portions Copyright 2015 StaticZ
 *)
program cedserver;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  SysUtils, UConfig, UCEDServer, LConvEncoding, vinfo, Language;
  
{$I version.inc}
  
//{$IFDEF WINDOWS}{$R cedserver.rc}{$ENDIF}

{$R *.res}


begin
  Writeln('');
  Writeln(Format('======= CentrED+ Server [Version: %s  Build: %d] =======',
                 [VersionInfo.GetFileVersionString, VersionInfo.Build]));
  Writeln('Copyright: ', Original);
  Writeln('         : ', '"CentrED+" version (c) ', Copyright, ' (uoquint.ru)');
  //Writeln('         : ', '!!! pre-release (not stable version) !!!');
  //Writeln('Modified by StaticZ (uoquint.ru)');
  //Writeln('================================');

  {$IFDEF Windows}
  if FileExists(ConfigFile) then
    Config := TConfig.Create(ConfigFile)
  else
    Config := TConfig.Init(ConfigFile);
  LanguageLoad(Config.Language);
  {$ELSE}
  if ParamStr(1) = '--init' then
    Config := TConfig.Init(ConfigFile)
  else if FileExists(ConfigFile) then
    Config := TConfig.Create(ConfigFile)
  else begin
    Writeln(UTF8ToCP866('Файл конфигурации не был найден. Запустите програму с параметром --init чтобы создать новый файл конфигурации.'));
    Halt;
  end;
  LanguageLoad(Config.Language);
  {$ENDIF}

  Writeln(TimeStamp, GetText('xmLoaded') + ' "' + ExtractFileName(ConfigFile) + '"');
  Writeln(TimeStamp, GetText('dfStRead'));
  Randomize;
  CEDServerInstance := TCEDServer.Create;
  Writeln(TimeStamp, GetText('dfInited'));
  CEDServerInstance.Run;
  Write(TimeStamp, GetText('Quieting'));
  FreeAndNil(CEDServerInstance);
  Config.Flush;
  FreeAndNil(Config);
  Writeln(GetText('SucsDone'));
end.

