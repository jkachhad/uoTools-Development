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
unit UfrmLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Spin, EditBtn, Buttons, IniFiles, LConvEncoding, LazHelpHTML,
  Registry;

type

  { TfrmLogin }

  TfrmLogin = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    btnDefaultOptions: TSpeedButton;
    cbProfile: TComboBox;
    cbLanguage: TComboBox;
    edData: TDirectoryEdit;
    edHost: TEdit;
    edUsername: TEdit;
    edPassword: TEdit;
    gbBaner: TGroupBox;
    gbConnection: TGroupBox;
    gbData: TGroupBox;
    gbActions: TGroupBox;
    gbProfiles: TGroupBox;
    imgBaner: TImage;
    imgHost: TImage;
    imgUsername: TImage;
    imgPassword: TImage;
    lblCopyright: TLabel;
    lblHost: TLabel;
    lblPlusCopyright: TLabel;
    lblUsername: TLabel;
    lblPassword: TLabel;
    edPort: TSpinEdit;
    lblData: TLabel;
    btnSaveProfile: TSpeedButton;
    btnDeleteProfile: TSpeedButton;
    BanerAnim: TTimer;
    pLayout: TPanel;
    procedure BanerAnimTimer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnDefaultOptionsClick(Sender: TObject);
    procedure btnDeleteProfileClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnSaveProfileClick(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure cbProfileChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BanerClick(Sender: TObject);
    procedure BanerMouseEnter(Sender: TObject);
    procedure BanerMouseLeave(Sender: TObject);
    procedure BanerDrawImage(baner : array of Byte);
  protected
    FProfilePath: string;
  public
    { public declarations }
    SaveProfileCaption: string;
    SaveProfileDescription: string;
  end; 

var
  frmLogin: TfrmLogin;
  sprofile: string;
  LastTickCount: DWORD;

implementation

uses
  UdmNetwork, Logging, vinfo, Language;
  
{$I version.inc}

{ TfrmLogin }

procedure TfrmLogin.btnCancelClick(Sender: TObject);
begin
  Close;
end;



procedure TfrmLogin.btnDefaultOptionsClick(Sender: TObject);
begin
  if cbProfile.ItemIndex > -1 then
  begin
    DeleteFile(FProfilePath + UTF8ToCP1251(cbProfile.Text) + PathDelim + 'RadarMap.cache');
    DeleteFile(FProfilePath + UTF8ToCP1251(cbProfile.Text) + PathDelim + 'TilesEntry.cache');
    DeleteFile(FProfilePath + UTF8ToCP1251(cbProfile.Text) + PathDelim + 'Config.xml');
  end;
end;

procedure TfrmLogin.btnDeleteProfileClick(Sender: TObject);
begin
  if cbProfile.ItemIndex > -1 then
  begin
    DeleteFile(FProfilePath + UTF8ToCP1251(cbProfile.Text) + PathDelim + 'login.ini');
    RemoveDir(FProfilePath + UTF8ToCP1251(cbProfile.Text));
    cbProfile.Items.Delete(cbProfile.ItemIndex);
    sprofile := '';
  end;
end;

procedure TfrmLogin.btnOKClick(Sender: TObject);
var
  path: string;
  configDir: string;
  settings: TIniFile;
  ARegistry: TRegistry;
begin
  // Download settings
  configDir := GetAppConfigDir(False);
  (*ARegistry := TRegistry.Create();
  ARegistry.RootKey := HKEY_LOCAL_MACHINE;
  ARegistry.OpenKey('\SOFTWARE\Quintessence\UO CentrED+', False);
  if ARegistry.ReadBool('UseConfigDir')
    then configDir := GetAppConfigDir(False)
    else configDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
                      + '..' + PathDelim + 'LocalData' + PathDelim + 'UsersData' + PathDelim ;
  ARegistry.Free;*) //TODO cross platform?

  // Save settings
  settings := TIniFile.Create(configDir + 'LoginSettings.ini');
  settings.WriteString('Connection', 'Host', edHost.Text);
  settings.WriteInteger('Connection', 'Port', edPort.Value);
  settings.WriteString('Connection', 'Username', edUsername.Text);
  settings.WriteString('Data', 'Path', edData.Text);
  if (cbProfile.ItemIndex > -1) and (cbProfile.ItemIndex < cbProfile.Items.Count) then
     settings.WriteString('Profile', 'Last', cbProfile.Items[cbProfile.ItemIndex])
  else
     settings.WriteString('Profile', 'Last', '');
  if (cbLanguage.ItemIndex > -1) and (cbLanguage.ItemIndex < cbLanguage.Items.Count) then
     settings.WriteString('Profile', 'Lang', LanguageGetName)
  else
     settings.WriteString('Profile', 'Lang', '');
  settings.Free;
  {
  // Check tracks
  path := IncludeTrailingPathDelimiter(UTF8ToCP1251(edData.Text));
  if (not FileExists(path + 'art.mul')) or
     (not FileExists(path + 'artidx.mul')) or                 LangDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + PathDelim + 'Language' + PathDelim;

     (not FileExists(path + 'hues.mul')) or
     (not FileExists(path + 'tiledata.mul')) or
     (not FileExists(path + 'animdata.mul')) or
     (not FileExists(path + 'texmaps.mul')) or
     (not FileExists(path + 'texidx.mul')) or
     (not FileExists(path + 'light.mul')) or
     (not FileExists(path + 'lightidx.mul')) then
  begin
    MessageDlg('Неверный путь', 'Указанный вами путь, не является коректным,'
      + ' т.к. не содержит требуемые файлы.', mtWarning, [mbOK], 0);
    edData.SetFocus;
  end else }
    ModalResult := mrOK;
  Logger.Send([lcClient, lcInfo], 'Logging into server');
end;

procedure TfrmLogin.btnSaveProfileClick(Sender: TObject);
var
  profileName: string;
  profile: TIniFile;
begin
  profileName := cbProfile.Text;
  if InputQuery(SaveProfileCaption, SaveProfileDescription, profileName) then
  begin
    if not DirectoryExists(FProfilePath + UTF8ToCP1251(profileName))
      then ForceDirectories(FProfilePath + UTF8ToCP1251(profileName));
    profile := TIniFile.Create(FProfilePath + UTF8ToCP1251(profileName) + PathDelim + 'login.ini');
    profile.WriteString('Connection', 'Host', UTF8ToCP1251(edHost.Text));
    profile.WriteInteger('Connection', 'Port', edPort.Value);
    profile.WriteString('Connection', 'Username', UTF8ToCP1251(edUsername.Text));
    profile.WriteString('Data', 'Path', UTF8ToCP1251(edData.Text));
    profile.Free;
    cbProfile.ItemIndex := cbProfile.Items.IndexOf(profileName);
    if cbProfile.ItemIndex = -1 then
    begin
      cbProfile.Items.Add(profileName);
      cbProfile.ItemIndex := cbProfile.Items.Count - 1;
      cbProfileChange(nil);
    end;
  end;
end;

procedure TfrmLogin.cbLanguageChange(Sender: TObject);
begin
  LanguageSet(cbLanguage.ItemIndex);
  LanguageTranslate(Self);
  cbLanguage.Hint := LanguageGetName();
  Self.Repaint;
end;

procedure TfrmLogin.cbProfileChange(Sender: TObject);
var
  profile: TIniFile;
begin
  if cbProfile.ItemIndex > -1 then
  begin
    btnDefaultOptions.Enabled := true;
    btnDeleteProfile.Enabled  := true;
    sprofile := cbProfile.Text;
    profile := TIniFile.Create(FProfilePath + UTF8ToCP1251(cbProfile.Text) + PathDelim + 'login.ini');
    edHost.Text := CP1251ToUTF8(profile.ReadString('Connection', 'Host', ''));
    edPort.Value := profile.ReadInteger('Connection', 'Port', 2597);
    edUsername.Text := CP1251ToUTF8(profile.ReadString('Connection', 'Username', ''));
    edPassword.Text := '';
    edData.Text := CP1251ToUTF8(profile.ReadString('Data', 'Path', ''));
    if Sender <> nil then
       edPassword.SetFocus;
    profile.Free;
  end else begin
    btnDefaultOptions.Enabled := false;
    btnDeleteProfile.Enabled := false;
    sprofile := '';
  end;
end;

procedure TfrmLogin.FormActivate(Sender: TObject);
begin
  //TODO ? GlassForm(frmLogin);
end;

procedure TfrmLogin.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ModalResult <> mrOK then
    dmNetwork.CheckClose(Self);
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
var
  searchRec: TSearchRec;
  iniSettings: TIniFile;
  lastProfile: string;
  lastLanguage: string;
  nodeindex: integer;
  langDirectory: string;
  langFileName : string;
  ARegistry: TRegistry;
  configDir: string;
begin
  Width := 494;
  Height := 266;

  Caption := Format('UO CentrED+ v%s build: %d',
  //Caption := Format('UO CentrED+ v%s build: %d  !!! pre-release (not stable version) !!! ',
            [VersionInfo.GetProductVersionString, VersionInfo.Build]);
  lblCopyright.Caption := Format('%s || "UO CentrED+" ver %s (c) %s',
    [Original, VersionInfo.GetFileVersionString, Copyright]);
  BanerMouseLeave(Sender);
  edData.DialogTitle:=lblData.Caption;

  // Download settings
  configDir := GetAppConfigDir(False);
  (*ARegistry := TRegistry.Create();
  ARegistry.RootKey := HKEY_LOCAL_MACHINE;
  ARegistry.OpenKey('\SOFTWARE\Quintessence\UO CentrED+', False);
  if ARegistry.ReadBool('UseConfigDir')
    then configDir := GetAppConfigDir(False)
    else configDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
                      + '..' + PathDelim + 'LocalData' + PathDelim + 'UsersData' + PathDelim ;
  ARegistry.Free;*) //TODO Cross platform?

  sprofile := '';
  iniSettings := TIniFile.Create(configDir + 'LoginSettings.ini');
  lastProfile := iniSettings.ReadString('Profile', 'Last', '');
  lastLanguage:= iniSettings.ReadString('Profile', 'Lang', '');

  FProfilePath := configDir + 'Profiles' + PathDelim;
  ForceDirectories(FProfilePath);
  if FindFirst(FProfilePath + '*', faDirectory, searchRec) = 0 then
  begin
    repeat
      if FileExists(FProfilePath + PathDelim + searchRec.Name + PathDelim + 'login.ini') then
      begin
        nodeindex := cbProfile.Items.Add(CP1251ToUTF8(searchRec.Name));
        if (cbProfile.Items[nodeindex] <> '') and (cbProfile.Items[nodeindex] = lastProfile) then
        begin
          cbProfile.ItemIndex := nodeindex;
          cbProfileChange(nil);
        end;
      end;
    until FindNext(searchRec) <> 0;
  end;
  FindClose(searchRec);

  if (cbProfile.ItemIndex < 0) or (lastprofile = '') then
  begin
    edHost.Text := iniSettings.ReadString('Connection', 'Host', 'localhost');
    edPort.Value := iniSettings.ReadInteger('Connection', 'Port', 2597);
    edUsername.Text := iniSettings.ReadString('Connection', 'Username', '');
    edPassword.Text := '';
    edData.Text := iniSettings.ReadString('Data', 'Path', '');
  end;

  iniSettings.Free;

  // Download locations
  LangDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))
                   + PathDelim + '..' + PathDelim +'Language' + PathDelim;
  LanguageLoad(Self, lastLanguage, LangDirectory);
end;

procedure TfrmLogin.FormShow(Sender: TObject);
begin
  edPassword.SetFocus;
end;

procedure TfrmLogin.BanerClick(Sender: TObject);
begin
  // Open the site in the default browser
  //TODO ShellExecute(Handle, 'open', PChar('http://dev.uoquint.ru'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmLogin.BanerDrawImage(baner : array of Byte);
var stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.Write(baner[0],SizeOf(baner));
  stream.Position:=0;
  imgBaner.Picture.Bitmap.LoadFromStream(stream);
  imgBaner.Update;
  stream.Free;
end;

//function GetTickCount:DWORD; external 'kernel32' name 'GetTickCount';
procedure TfrmLogin.BanerAnimTimer(Sender: TObject);
var NowsTickCount : DWORD;
begin { GetTickCount timer does not work!!!
  NowsTickCount := GetTickCount;
  if (NowsTickCount - LastTickCount) < 10000 then exit;
  LastTickCount := NowsTickCount;

  if BanerAnim.Tag = 1 then begin
    BanerAnim.Tag := 2; BanerDrawImage(baner_u);
  end;
  if BanerAnim.Tag = 2 then begin
    BanerAnim.Tag := 1; BanerDrawImage(baner_h);
  end; }
end;

procedure TfrmLogin.BanerMouseEnter(Sender: TObject);
var stream: TMemoryStream;
begin
  lblPlusCopyright.Font.Color:= $FF0000;
  lblPlusCopyright.Font.Size := 10;
  BanerDrawImage(baner_h);
end;

procedure TfrmLogin.BanerMouseLeave(Sender: TObject);
var stream: TMemoryStream;
begin
  lblPlusCopyright.Font.Color:= $000000;
  lblPlusCopyright.Font.Size := 8;
  BanerDrawImage(baner_u);
end;

initialization
  {$I UfrmLogin.lrs}

end.

