(*
 * CDDL HEADER START
 *
 * gfgfgfg
 *)
unit Language;
{$mode objfpc}{$H+}

interface
uses IniFiles, UfrmLogin, UdmNetwork, UfrmInitialize, UfrmMain, UfrmDrawSettings,
  UfrmConfirmation, UfrmMoveSettings, UfrmElevateSettings, UfrmHueSettings,
  UfrmBoundaries, UfrmVirtualLayer, UfrmFilter, UfrmLightlevel, UfrmRadar,
  UfrmAccountControl, UfrmEditAccount, UfrmRegionControl, UfrmLargeScaleCommand,
  UfrmAbout, UGameResources, UfrmFillSettings, UfrmSelectionSettings,
  UfrmSurfElevateSettings, UfrmSurfStretchSettings, UfrmSurfSmoothSettings;

procedure LanguageLoad(form: TfrmLogin; lang: string; path: string);
procedure LanguageSet(index: Integer);
function  LanguageGetName() : string;
procedure ReloadLanguageTranslation();
procedure LanguageTranslate(form: TfrmLogin);
procedure LanguageTranslate(form: TfrmInitialize; unet: TdmNetwork; resm: TGameResourceManager);
procedure LanguageTranslate(form: TfrmMain);


procedure LanguageTranslate(form: TfrmConfirmation);
procedure LanguageTranslate(form: TfrmSelectionSettings);
procedure LanguageTranslate(form: TfrmMoveSettings);
procedure LanguageTranslate(form: TfrmElevateSettings);
procedure LanguageTranslate(form: TfrmSurfElevateSettings);
procedure LanguageTranslate(form: TfrmSurfStretchSettings);
procedure LanguageTranslate(form: TfrmSurfSmoothSettings);
procedure LanguageTranslate(form: TfrmDrawSettings);
procedure LanguageTranslate(form: TfrmHueSettings);
procedure LanguageTranslate(form: TfrmFillSettings);
procedure LanguageTranslate(form: TfrmVirtualLayer);
procedure LanguageTranslate(form: TfrmBoundaries);
procedure LanguageTranslate(form: TfrmFilter);
procedure LanguageTranslate(form: TfrmLightlevel);
procedure LanguageTranslate(form: TfrmRadarMap);
procedure LanguageTranslate(form: TfrmAccountControl);
procedure LanguageTranslate(form: TfrmEditAccount);
procedure LanguageTranslate(form: TfrmRegionControl);
procedure LanguageTranslate(form: TfrmLargeScaleCommand);
procedure LanguageTranslate(form: TfrmAbout);
function GetParseErText(key: string) : string;


implementation
uses SysUtils, Classes, LConvEncoding, Menus;


var
  LangPath : string;
  LangFile : TIniFile;
  LangIndex: Integer;
  LangFlags: array of Integer;
  LangAbbrs: array of string;
  Languages: array of string;
  LangFiles: array of string;
  CodePages: array of Integer;

procedure LanguageLoad(form: TfrmLogin; lang: string; path: string);
var
  langSize : Integer;
  nodeIndex: Integer;
  tempIndex: Integer;
  searchRec: TSearchRec;
  flags: array of Integer;
  abbrs: array of string;
  langs: array of string;
  files: array of string;
  pages: array of Integer;
begin
  LangPath := path;
  LangIndex:= -1;
  LangSize := -1;
  if FindFirst(LangPath+'*.ini', faAnyFile, searchRec) = 0 then
  begin
    repeat
        inc(LangSize);
        SetLength(flags, LangSize+1);
        SetLength(abbrs, LangSize+1);
        SetLength(langs, LangSize+1);
        SetLength(files, LangSize+1);
        SetLength(pages, LangSize+1);

        files[langSize] := CP1251ToUTF8(searchRec.Name);
        LangFile := TIniFile.Create(LangPath + files[langSize]);
        flags[langSize] := LangFile.ReadInteger('info', 'LangFlag', -1);
        abbrs[langSize] := LangFile.ReadString( 'info', 'LangAbbr', '');
        langs[langSize] := LangFile.ReadString( 'info', 'Language', '');
        pages[langSize] := LangFile.ReadInteger('info', 'CodePage', 1250);
        LangFile.Free;
        LangFile:=nil;

        if ((flags[langSize] < 100) or (flags[langSize] > 107))
           then flags[langSize] := 99;

        if ((LowerCase(abbrs[langSize]) = 'rus') or (LowerCase(langs[langSize]) = 'русский'))
           then form.cbLanguage.Items.Insert(0, files[langSize])
        else if ((LowerCase(abbrs[langSize]) = 'eng') or (LowerCase(langs[langSize]) = 'english'))
        then if form.cbLanguage.Items.Count > 1
          then form.cbLanguage.Items.Insert(1, files[langSize])
          else form.cbLanguage.Items.Insert(0, files[langSize])
        else form.cbLanguage.Items.Add(files[langSize]);

    until FindNext(searchRec) <> 0;
  end;
  FindClose(searchRec);

  SetLength(LangFlags, LangSize+1);
  SetLength(LangAbbrs, LangSize+1);
  SetLength(Languages, LangSize+1);
  SetLength(CodePages, LangSize+1);
  SetLength(LangFiles, LangSize+1);
  for nodeIndex := 0 to langSize do
  for tempIndex := 0 to langSize do
  if form.cbLanguage.Items[nodeIndex] = files[tempIndex] then
  begin
    LangFlags[nodeIndex] := flags[tempIndex];
    LangAbbrs[nodeIndex] := abbrs[tempIndex];
    Languages[nodeIndex] := langs[tempIndex];
    CodePages[nodeIndex] := pages[tempIndex];
    LangFiles[nodeIndex] := files[tempIndex];
    form.cbLanguage.Items[nodeIndex] := UpCase(LangAbbrs[nodeIndex]);
    if Languages[nodeIndex] = lang then
    begin
      LangIndex := nodeIndex;
      form.cbLanguage.ItemIndex := nodeIndex;
      form.cbLanguageChange(nil);
    end;

  end;

  if LangIndex < 0 then begin
    LangIndex := 0;
    form.cbLanguage.ItemIndex := 0;
    form.cbLanguageChange(nil);
  end;
  SetLength(flags, 0);
  SetLength(abbrs, 0);
  SetLength(langs, 0);
  SetLength(files, 0);
  SetLength(pages, 0);
end;

procedure LanguageSet(index: Integer);
begin
  LangIndex := index;
  if (LangFile <> nil)
    then LangFile.Free;
  LangFile  := TIniFile.Create(LangPath + LangFiles[LangIndex]);
end;

function  LanguageGetName() : string;
begin
  Result := Languages[LangIndex];
end;

function GetText(section : string; key: string) : string;
var text : string;
begin
  text := LangFile.ReadString(section, key, '');
  if (text <> '')
  then result := text
  else result := Format('%%%%[%s]%s%%%%', [section, key]);
end;

procedure ReloadLanguageTranslation();
begin
  //LangFile.Free;
  //LanguageSet(LangIndex);
//  LanguageTranslate(frmLogin);
  LanguageTranslate(frmInitialize, dmNetwork, nil);
  LanguageTranslate(frmMain);
  LanguageTranslate(frmConfirmation);
  LanguageTranslate(frmSelectionSettings);
  LanguageTranslate(frmMoveSettings);
  LanguageTranslate(frmElevateSettings);
  LanguageTranslate(frmSurfElevateSettings);
  LanguageTranslate(frmSurfStretchSettings);
  LanguageTranslate(frmSurfSmoothSettings);
  LanguageTranslate(frmDrawSettings);
  LanguageTranslate(frmHueSettings);
  LanguageTranslate(frmFillSettings);
  LanguageTranslate(frmVirtualLayer);
  LanguageTranslate(frmBoundaries);
  LanguageTranslate(frmFilter);
  LanguageTranslate(frmLightlevel);
  LanguageTranslate(frmRadarMap);
  LanguageTranslate(frmAccountControl);
  LanguageTranslate(frmEditAccount);
  LanguageTranslate(frmRegionControl);
  LanguageTranslate(frmLargeScaleCommand);
  LanguageTranslate(frmAbout);
end;

// ----------------------------------------------------------------------------=

procedure LanguageTranslate(form: TfrmLogin);
begin
  form.gbConnection.Caption   := GetText('Login', 'ConnectionSettings');
  form.lblHost.Caption        := GetText('Login', 'Server');
  form.lblUsername.Caption    := GetText('Login', 'Account');
  form.lblPassword.Caption    := GetText('Login', 'Password');

  form.gbData.Caption         := GetText('Login', 'MulFiles');
  form.lblData.Caption        := GetText('Login', 'MulFilesDesc');
  form.edData.DialogTitle     := form.lblData.Caption;

  form.gbBaner.Caption        := GetText('Login', 'Project');
  form.imgBaner.Hint          := GetText('Login', 'WebSite');

  form.btnOK.Caption          := GetText('Login', 'Connect');
  form.btnCancel.Caption      := GetText('Login', 'Exit');

  form.gbProfiles.Caption     := GetText('Login', 'Profile');
  form.btnSaveProfile.Hint    := GetText('Login', 'SaveProfile');
  form.btnDefaultOptions.Hint := GetText('Login', 'ClearProfile');
  form.btnDeleteProfile.Hint  := GetText('Login', 'DeleteProfile');
  form.SaveProfileCaption     := GetText('Login', 'SaveProfileCaption');
  form.SaveProfileDescription := GetText('Login', 'SaveProfileDescription');
end;

procedure LanguageTranslate(form: TfrmInitialize; unet: TdmNetwork; resm: TGameResourceManager);
begin
  if form <> nil then begin
    form.SplashConnection       := GetText('Splash', 'Connection');
    form.SplashAuthorization    := GetText('Splash', 'Authorization');
    form.SplashInicialization   := GetText('Splash', 'Inicialization');
    form.SplashLoading          := GetText('Splash', 'Loading');
    form.SplashUpdates          := GetText('Splash', 'Updates');
    form.SplashSuspend          := GetText('Splash', 'Suspend');
    form.SplashUpdatingMiniMap  := GetText('Splash', 'UpdatingMiniMap');
  end;
  if unet <> nil then begin
    unet.ErrorCaption           := GetText('Splash', 'ErrorCaption');
    unet.WrongServer            := GetText('Splash', 'WrongServer');
    unet.WrongAccount           := GetText('Splash', 'WrongAccount');
    unet.WrongPassword          := GetText('Splash', 'WrongPassword');
    unet.NoAccess               := GetText('Splash', 'NoAccess');
    unet.AlreadyLogined         := GetText('Splash', 'AlreadyLogined');
    unet.TCPErrorCaption        := GetText('Splash', 'TCPErrorCaption');
    unet.UnsuportedVersion      := GetText('Splash', 'UnsuportedVersion');
  end;
  if resm <> nil then begin
    resm.lbDlgErrorFilePathCaption := GetText('Splash', 'DlgErrorFilePathCaption');
    resm.lbDlgErrorFilePathMsg     := GetText('Splash', 'DlgErrorFilePathMsg');
  end;
end;

procedure LanguageTranslate(form: TfrmMain);
var
  index: Integer;
  entry: TMenuItem;
begin
  form.lbFormTitleProfile            := GetText('Login', 'Account');
  form.lbFormTitleAccount            := GetText('Login', 'Profile');

  // ----------
  form.mnuCentrED.Caption            := GetText('MainMenu', 'File');
  form.mnuCentrED.Hint               := GetText('MainMenu', 'FileHint');
  form.mnuMakeScreenShot.Caption     := GetText('MainMenu', 'FileScreenShoot');
  form.mnuMakeScreenShot.Hint        := GetText('MainMenu', 'FileScreenShootHint');
  form.mnuReloadGroups.Caption       := GetText('MainMenu', 'FileReloadGroups');
  form.mnuReloadGroups.Hint          := GetText('MainMenu', 'FileReloadGroupsHint');
  form.mnuSetLanguage.Caption        := GetText('MainMenu', 'FileSetLanguage');
  form.mnuSetLanguage.Hint           := GetText('MainMenu', 'FileSetLanguageHint');

  form.mnuSetLanguage.ImageIndex     := LangFlags[LangIndex];
  form.mnuSetLanguage.Tag            := PtrInt(LangIndex);
  form.mnuSetLanguage.Clear;
  for index:= 0 to length(Languages)-1 do begin
    entry:= TMenuItem.Create(form);
    entry.Caption    := Languages[index];
    entry.ImageIndex := LangFlags[index];
    entry.Tag        := PtrInt(index);
    entry.OnClick    := @form.mnuSetLanguageClick;
    form.mnuSetLanguage.Add(entry);
  end;

  form.mnuDisconnect.Caption         := GetText('MainMenu', 'FileDisconnetct');
  form.mnuDisconnect.Hint            := GetText('MainMenu', 'FileDisconnetctHint');
  form.mnuExit.Caption               := GetText('MainMenu', 'FileExit');
  form.mnuExit.Hint                  := GetText('MainMenu', 'FileExitHint');

  form.mnuAdministration.Caption     := GetText('MainMenu', 'Admin');
  form.mnuAdministration.Hint        := GetText('MainMenu', 'AdminHint');
  form.mnuFlush.Caption              := GetText('MainMenu', 'AdminFlush');
  form.mnuFlush.Hint                 := GetText('MainMenu', 'AdminFlushHint');
  form.mnuShutdown.Caption           := GetText('MainMenu', 'AdminShutDown');
  form.mnuShutdown.Hint              := GetText('MainMenu', 'AdminShutDownHint');
  form.mnuAccountControl.Caption     := GetText('MainMenu', 'AdminAccounts');
  form.mnuAccountControl.Hint        := GetText('MainMenu', 'AdminAccountsHint');
  form.mnuRegionControl.Caption      := GetText('MainMenu', 'AdminRegions');
  form.mnuRegionControl.Hint         := GetText('MainMenu', 'AdminRegionsHint');
  form.mnuLargeScaleCommands.Caption := GetText('MainMenu', 'AdminCommands');
  form.mnuLargeScaleCommands.Hint    := GetText('MainMenu', 'AdminCommandsHint');

  form.mnuSettings.Caption           := GetText('MainMenu', 'Settings');
  form.mnuSettings.Hint              := GetText('MainMenu', 'SettingsHint');
  form.mnuWindowedMode.Caption       := GetText('MainMenu', 'SettingsWindowed');
  form.mnuWindowedMode.Hint          := GetText('MainMenu', 'SettingsWindowedHint');

  form.mnuTileList.Caption           := GetText('MainMenu', 'SettingsMainTileList');
  form.mnuTileList.Hint              := GetText('MainMenu', 'SettingsMainTileListHint');
  form.mnuTileListTable.Caption      := GetText('MainMenu', 'SettingsTileListTable');
  form.mnuTileListTable.Hint         := GetText('MainMenu', 'SettingsTileListTableHint');
  form.mnuTileListSmall.Caption      := GetText('MainMenu', 'SettingsTileListSmall');
  form.mnuTileListSmall.Hint         := GetText('MainMenu', 'SettingsTileListSmallHint');
  form.mnuTileListMidle.Caption      := GetText('MainMenu', 'SettingsTileListMidle');
  form.mnuTileListMidle.Hint         := GetText('MainMenu', 'SettingsTileListMidleHint');
  form.mnuTileListLarge.Caption      := GetText('MainMenu', 'SettingsTileListLarge');
  form.mnuTileListLarge.Hint         := GetText('MainMenu', 'SettingsTileListLargeHint');
  form.mnuTileListStretch.Caption    := GetText('MainMenu', 'SettingsTileListStretch');
  form.mnuTileListStretch.Hint       := GetText('MainMenu', 'SettingsTileListStretchHint');
  form.mnuTileListClip.Caption       := GetText('MainMenu', 'SettingsTileListClip');
  form.mnuTileListClip.Hint          := GetText('MainMenu', 'SettingsTileListClipHint');
  form.mnuTileListCentre.Caption     := GetText('MainMenu', 'SettingsTileListCentre');
  form.mnuTileListCentre.Hint        := GetText('MainMenu', 'SettingsTileListCentreHint');
  form.mnuTileListDrawInfo.Caption   := GetText('MainMenu', 'SettingsTileListDrawInfo');
  form.mnuTileListDrawInfo.Hint      := GetText('MainMenu', 'SettingsTileListDrawInfoHint');

  form.mnuMiscTileList.Caption       := GetText('MainMenu', 'SettingsMiscTileList');
  form.mnuMiscTileList.Hint          := GetText('MainMenu', 'SettingsMiscTileListHint');
  form.mnuMiscTileListTable.Caption  := GetText('MainMenu', 'SettingsTileListTable');
  form.mnuMiscTileListTable.Hint     := GetText('MainMenu', 'SettingsTileListTableHint');
  form.mnuMiscTileListSmall.Caption  := GetText('MainMenu', 'SettingsTileListSmall');
  form.mnuMiscTileListSmall.Hint     := GetText('MainMenu', 'SettingsTileListSmallHint');
  form.mnuMiscTileListMidle.Caption  := GetText('MainMenu', 'SettingsTileListMidle');
  form.mnuMiscTileListMidle.Hint     := GetText('MainMenu', 'SettingsTileListMidleHint');
  form.mnuMiscTileListLarge.Caption  := GetText('MainMenu', 'SettingsTileListLarge');
  form.mnuMiscTileListLarge.Hint     := GetText('MainMenu', 'SettingsTileListLargeHint');
  form.mnuMiscTileListStretch.Caption:= GetText('MainMenu', 'SettingsTileListStretch');
  form.mnuMiscTileListStretch.Hint   := GetText('MainMenu', 'SettingsTileListStretchHint');
  form.mnuMiscTileListClip.Caption   := GetText('MainMenu', 'SettingsTileListClip');
  form.mnuMiscTileListClip.Hint      := GetText('MainMenu', 'SettingsTileListClipHint');
  form.mnuMiscTileListCentre.Caption := GetText('MainMenu', 'SettingsTileListCentre');
  form.mnuMiscTileListCentre.Hint    := GetText('MainMenu', 'SettingsTileListCentreHint');
  form.mnuMiscTileListDrawInfo.Caption:=GetText('MainMenu', 'SettingsTileListDrawInfo');
  form.mnuMiscTileListDrawInfo.Hint  := GetText('MainMenu', 'SettingsTileListDrawInfoHint');

  form.mnuAutoHideGroupList.Caption  := GetText('MainMenu', 'SettingsHideGroups');
  form.mnuAutoHideGroupList.Hint     := GetText('MainMenu', 'SettingsHideGroupsHint');
  form.mnuAutoHideRandomList.Caption := GetText('MainMenu', 'SettingsHideRandom');
  form.mnuAutoHideRandomList.Hint    := GetText('MainMenu', 'SettingsHideRandomHint');
  form.mnuAutoShowFilterWindow.Caption:=GetText('MainMenu', 'SettingsAutoShowFilter');
  form.mnuAutoShowFilterWindow.Hint  := GetText('MainMenu', 'SettingsAutoShowFilterHint');
  form.mnuShowAnimations.Caption     := GetText('MainMenu', 'SettingsPlayAnimation');
  form.mnuShowAnimations.Hint        := GetText('MainMenu', 'SettingsPlayAnimationHint');
  form.mnuSecurityQuestion.Caption   := GetText('MainMenu', 'SettingsQuestion');
  form.mnuSecurityQuestion.Hint      := GetText('MainMenu', 'SettingsQuestionHint');
  form.mnuWhiteBackground.Caption    := GetText('MainMenu', 'SettingsWhiteBack');
  form.mnuWhiteBackground.Hint       := GetText('MainMenu', 'SettingsWhiteBackHint');

  form.mnuHelp.Caption               := GetText('MainMenu', 'Help');
  form.mnuHelp.Hint                  := GetText('MainMenu', 'HelpHint');

  form.mnuDocs.Caption               := GetText('MainMenu', 'HelpDocs');
  form.mnuDocs.Hint                  := GetText('MainMenu', 'HelpDocsHint');
  form.mnuSupport.Caption            := GetText('MainMenu', 'HelpSupport');
  form.mnuSupport.Hint               := GetText('MainMenu', 'HelpSupportHint');
  form.mnuEngCom.Caption             := GetText('MainMenu', 'HelpSupportEng');
  form.mnuEngCom.Hint                := GetText('MainMenu', 'HelpSupportEngHint');
  form.mnuEng2Com.Caption            := GetText('MainMenu', 'HelpSupportEng2');
  form.mnuEng2Com.Hint               := GetText('MainMenu', 'HelpSupportEng2Hint');
  form.mnuRusCom.Caption             := GetText('MainMenu', 'HelpSupportRus');
  form.mnuRusCom.Hint                := GetText('MainMenu', 'HelpSupportRusHint');

  form.mnuAbout.Caption              := GetText('MainMenu', 'HelpAbout');
  form.mnuAbout.Hint                 := GetText('MainMenu', 'HelpAboutHint');

  // ----------
  form.lbFormTitleAccount            := GetText('MainForm', 'FormTitleAccount');
  form.lbFormTitleProfile            := GetText('MainForm', 'FormTitleProfile');
  form.lblChatHeaderCaption.Caption  := GetText('MainForm', 'BottomChatHeader');
  form.lblTileInfoWLabel.Caption     := GetText('MainForm', 'BottomCursorWidth');
  form.lblTileInfoHLabel.Caption     := GetText('MainForm', 'BottomCursorHeight');
  form.lbBottomCursorVLayer1         := GetText('MainForm', 'BottomCursorVLayer1');
  form.lbBottomCursorVLayer2         := GetText('MainForm', 'BottomCursorVLayer2');
  form.lbBottomCursorItemId          := GetText('MainForm', 'BottomCursorItemId');
  form.lbBottomCursorLandId          := GetText('MainForm', 'BottomCursorLandId');
  form.lbBottomCursorPosX            := GetText('MainForm', 'BottomCursorPosX');
  form.lbBottomCursorPosY            := GetText('MainForm', 'BottomCursorPosY');
  form.lbBottomCursorPosZ            := GetText('MainForm', 'BottomCursorPosZ');
  form.lbBottomCursorItemHue         := GetText('MainForm', 'BottomCursorItemHue');
  form.vstChat.Header.Columns[0].Text:= GetText('MainForm', 'BottomChatTime');
  form.vstChat.Header.Columns[1].Text:= GetText('MainForm', 'BottomChatSender');
  form.vstChat.Header.Columns[2].Text:= GetText('MainForm', 'BottomChatMessage');

  form.mnuGrabTileID.Caption         := GetText('MainForm', 'GrabTileId');
  form.mnuGrabTileID.Hint            := GetText('MainForm', 'GrabTileIdHint');
  form.mnuGrabHue.Caption            := GetText('MainForm', 'GrabTileHue');
  form.mnuGrabHue.Hint               := GetText('MainForm', 'GrabTileHueHint');
  form.mnuGrabFilterTileID.Caption   := GetText('MainForm', 'GrabTileAddIdToFilter');
  form.mnuGrabFilterTileID.Hint      := GetText('MainForm', 'GrabTileAddIdToFilterHint');
  form.mnuGrabFilterHue.Caption      := GetText('MainForm', 'GrabTileAddHueToFilter');
  form.mnuGrabFilterHue.Hint         := GetText('MainForm', 'GrabTileAddHueToFilterHint');
  form.mnuGrabVirtualLayerZ.Caption  := GetText('MainForm', 'GrabTileSetVLayer');
  form.mnuGrabVirtualLayerZ.Hint     := GetText('MainForm', 'GrabTileSetVLayerHint');
  form.mnuGrabBoundaries.Caption     := GetText('MainForm', 'GrabBoundaries');
  form.mnuGrabBoundaries.Hint        := GetText('MainForm', 'GrabBoundariesHint');
  form.mnuGrabBoundMinZ.Caption      := GetText('MainForm', 'GrabBoundMinZ');
  form.mnuGrabBoundMinZ.Hint         := GetText('MainForm', 'GrabBoundMinZHint');
  form.mnuGrabBoundMaxZ.Caption      := GetText('MainForm', 'GrabBoundMaxZ');
  form.mnuGrabBoundMaxZ.Hint         := GetText('MainForm', 'GrabBoundMaxZHint');
  form.mnuGrabBoundMinX.Caption      := GetText('MainForm', 'GrabBoundMinX');
  form.mnuGrabBoundMinX.Hint         := GetText('MainForm', 'GrabBoundMinXHint');
  form.mnuGrabBoundMaxX.Caption      := GetText('MainForm', 'GrabBoundMaxX');
  form.mnuGrabBoundMaxX.Hint         := GetText('MainForm', 'GrabBoundMaxXHint');
  form.mnuGrabBoundMinY.Caption      := GetText('MainForm', 'GrabBoundMinY');
  form.mnuGrabBoundMinY.Hint         := GetText('MainForm', 'GrabBoundMinYHint');
  form.mnuGrabBoundMaxY.Caption      := GetText('MainForm', 'GrabBoundMaxY');
  form.mnuGrabBoundMaxY.Hint         := GetText('MainForm', 'GrabBoundMaxYHint');

  form.mnuSelect.Caption             := GetText('MainForm', 'ToolsSelect');
  form.mnuSelect.Hint                := GetText('MainForm', 'ToolsSelectHint');
  form.mnuSelection.Caption          := GetText('MainForm', 'ToolsSelection');
  form.mnuSelection.Hint             := GetText('MainForm', 'ToolsSelectionHint');
  form.mnuMove.Caption               := GetText('MainForm', 'ToolsMove');
  form.mnuMove.Hint                  := GetText('MainForm', 'ToolsMoveHint');
  form.mnuElevate.Caption            := GetText('MainForm', 'ToolsElevate');
  form.mnuElevate.Hint               := GetText('MainForm', 'ToolsElevateHint');
  form.mnuSurfElevate.Caption        := GetText('MainForm', 'ToolsSurfElevate');
  form.mnuSurfElevate.Hint           := GetText('MainForm', 'ToolsSurfElevateHint');
  form.mnuSurfStretch.Caption        := GetText('MainForm', 'ToolsSurfStretch');
  form.mnuSurfStretch.Hint           := GetText('MainForm', 'ToolsSurfStretchHint');
  form.mnuSurfSmooth.Caption         := GetText('MainForm', 'ToolsSurfSmooth');
  form.mnuSurfSmooth.Hint            := GetText('MainForm', 'ToolsSurfSmoothHint');
  form.mnuDraw.Caption               := GetText('MainForm', 'ToolsDraw');
  form.mnuDraw.Hint                  := GetText('MainForm', 'ToolsDrawHint');
  form.mnuSetHue.Caption             := GetText('MainForm', 'ToolsSetHue');
  form.mnuSetHue.Hint                := GetText('MainForm', 'ToolsSetHueHint');
  form.mnuFill.Caption               := GetText('MainForm', 'ToolsFill');
  form.mnuFill.Hint                  := GetText('MainForm', 'ToolsSetFill');
  form.mnuDelete.Caption             := GetText('MainForm', 'ToolsDelete');
  form.mnuDelete.Hint                := GetText('MainForm', 'ToolsDeleteHint');
  form.mnuVirtualLayer.Caption       := GetText('MainForm', 'ToolsVLayer');
  form.mnuVirtualLayer.Hint          := GetText('MainForm', 'ToolsVLayerHint');
  form.mnuBoundaries.Caption         := GetText('MainForm', 'ToolsBoundaries');
  form.mnuBoundaries.Hint            := GetText('MainForm', 'ToolsBoundariesHint');


  form.lbDlgWindowedModeSwitchCaption:= GetText('MainForm', 'DlgWindowedModeSwitchCaption');
  form.lbDlgWindowedModeSwitch       := GetText('MainForm', 'DlgWindowedModeSwitch');
  form.lbScreenShotMsg               := GetText('MainForm', 'ScreenShotMsg');
  form.lbUserLoginedMsg              := GetText('MainForm', 'UserLoginedMsg');
  form.lbUserLogoutedMsg             := GetText('MainForm', 'UserLogoutedMsg');
  form.lbDlgGetDcErrCaption          := GetText('MainForm', 'DlgGetDcErrCaption');
  form.lbDlgGetDcErr                 := GetText('MainForm', 'DlgGetDcErr');
  form.lbDlgFreeDcErrCaption         := GetText('MainForm', 'DlgFreeDcErrCaption');
  form.lbDlgFreeDcErr                := GetText('MainForm', 'DlgFreeDcErr');
  form.lbDlgCnangedAccessCaption     := GetText('MainForm', 'DlgCnangedAccessCaption');
  form.lbDlgCnangedAccess            := GetText('MainForm', 'DlgCnangedAccess');
  form.lbDlgBlockedAccessCaption     := GetText('MainForm', 'DlgBlockedAccessCaption');
  form.lbDlgBlockedAccess            := GetText('MainForm', 'DlgBlockedAccess');

  // ----------
  form.tbDisconnect.Hint             := GetText('ToolBar', 'DisconnectHint');
  form.tbSelect.Hint                 := GetText('ToolBar', 'SelectHint');
  form.tbSelection.Hint              := GetText('ToolBar', 'SelectionHint');
  form.tbMoveTile.Hint               := GetText('ToolBar', 'MoveTileHint');
  form.tbElevateTile.Hint            := GetText('ToolBar', 'ElevateTileHint');
  form.tbSurfElevate.Hint            := GetText('ToolBar', 'SurfElevateHint');
  form.tbSurfStretch.Hint            := GetText('ToolBar', 'SurfStretchHint');
  form.tbSurfSmooth.Hint             := GetText('ToolBar', 'SurfSmoothHint');
  form.tbDrawTile.Hint               := GetText('ToolBar', 'DrawTileHint');
  form.tbSetHue.Hint                 := GetText('ToolBar', 'SetHueHint');
  form.tbFill.Hint                   := GetText('ToolBar', 'FillHint');
  form.tbDeleteTile.Hint             := GetText('ToolBar', 'DeleteTileHint');
  form.tbUndo.Hint                   := GetText('ToolBar', 'UndoHint') + ' (0).';
  form.lbToolbarUndo                 := GetText('ToolBar', 'UndoHint');
  form.tbRedo.Hint                   := GetText('ToolBar', 'RedoHint') + ' (0).';
  // form.lbToolbarRedo -- ????????
  form.tbVirtualLayer.Hint           := GetText('ToolBar', 'VirtualLayerHint');
  form.tbBoundaries.Hint             := GetText('ToolBar', 'BoundariesHint');
  form.tbFilter.Hint                 := GetText('ToolBar', 'FilterHint');
  form.tbTerrain.Hint                := GetText('ToolBar', 'TerrainHint');
  form.tbStatics.Hint                := GetText('ToolBar', 'StaticsHint');
  form.tbNoDraw.Hint                 := GetText('ToolBar', 'NoDrawHint');
  form.tbWalkable.Hint               := GetText('ToolBar', 'WalkableHint');
  form.tbFlat.Hint                   := GetText('ToolBar', 'FlatHint');
  form.tbLightlevel.Hint             := GetText('ToolBar', 'LightlevelHint');;
  form.tbZoom.Hint                   := GetText('ToolBar', 'ZoomHint');
  form.tbRadarMap.Hint               := GetText('ToolBar', 'RadarMapHint');

  form.mnuShowGrid.Caption           := GetText('ToolBar', 'ShowGrid');
  form.mnuShowGrid.Hint              := GetText('ToolBar', 'ShowGridHint');
  form.mnuShowBlocks.Caption         := GetText('ToolBar', 'ShowBlocks');
  form.mnuShowBlocks.Hint            := GetText('ToolBar', 'ShowBlocksHint');

  form.mnuShowWalls.Caption          := GetText('ToolBar', 'ShowWalls');
  form.mnuShowWalls.Hint             := GetText('ToolBar', 'ShowWallsHint');
  form.mnuShowBridges.Caption        := GetText('ToolBar', 'ShowBridges');
  form.mnuShowBridges.Hint           := GetText('ToolBar', 'ShowBridgesHint');
  form.mnuShowSurfaces.Caption       := GetText('ToolBar', 'ShowSurfaces');
  form.mnuShowSurfaces.Hint          := GetText('ToolBar', 'ShowSurfacesHint');
  form.mnuShowRoofs.Caption          := GetText('ToolBar', 'ShowRoofs');
  form.mnuShowRoofs.Hint             := GetText('ToolBar', 'ShowRoofsHint');
  form.mnuShowFoliage.Caption        := GetText('ToolBar', 'ShowFoliage');
  form.mnuShowFoliage.Hint           := GetText('ToolBar', 'ShowFoliageHint');
  form.mnuShowWater.Caption          := GetText('ToolBar', 'ShowWater');
  form.mnuShowWater.Hint             := GetText('ToolBar', 'ShowWaterHint');

  form.mnuShowNoDrawTiles.Caption    := GetText('ToolBar', 'ShowNoDrawTiles');
  form.mnuShowNoDrawTiles.Hint       := GetText('ToolBar', 'ShowNoDrawTilesHint');
  form.mnuShowLightSource.Caption    := GetText('ToolBar', 'ShowLightSource');
  form.mnuShowLightSource.Hint       := GetText('ToolBar', 'ShowLightSourceHint');

  form.mnuFlatShowHeight.Caption     := GetText('ToolBar', 'FlatShowHeight');
  form.mnuFlatShowHeight.Hint        := GetText('ToolBar', 'FlatShowHeightHint');

  form.mnuZoom025.Caption            := GetText('ToolBar', 'Zoom025');
  form.mnuZoom025.Hint               := GetText('ToolBar', 'Zoom025Hint');
  form.mnuZoom033.Caption            := GetText('ToolBar', 'Zoom033');
  form.mnuZoom033.Hint               := GetText('ToolBar', 'Zoom033Hint');
  form.mnuZoom050.Caption            := GetText('ToolBar', 'Zoom050');
  form.mnuZoom050.Hint               := GetText('ToolBar', 'Zoom050Hint');
  form.mnuZoom075.Caption            := GetText('ToolBar', 'Zoom075');
  form.mnuZoom075.Hint               := GetText('ToolBar', 'Zoom075Hint');
  form.mnuZoom100.Caption            := GetText('ToolBar', 'Zoom100');
  form.mnuZoom100.Hint               := GetText('ToolBar', 'Zoom100Hint');
  form.mnuZoom150.Caption            := GetText('ToolBar', 'Zoom150');
  form.mnuZoom150.Hint               := GetText('ToolBar', 'Zoom150Hint');
  form.mnuZoom200.Caption            := GetText('ToolBar', 'Zoom200');
  form.mnuZoom200.Hint               := GetText('ToolBar', 'Zoom200Hint');
  form.mnuZoom300.Caption            := GetText('ToolBar', 'Zoom300');
  form.mnuZoom300.Hint               := GetText('ToolBar', 'Zoom300Hint');
  form.mnuZoom400.Caption            := GetText('ToolBar', 'Zoom400');
  form.mnuZoom400.Hint               := GetText('ToolBar', 'Zoom400Hint');

  // ----------
  form.tsTiles.Caption               := GetText('TabTiles', 'TabPage');

  form.cbTerrain.Caption             := GetText('TabTiles', 'Lands');
  form.cbTerrain.Hint                := GetText('TabTiles', 'LandsHint');
  form.cbStatics.Caption             := GetText('TabTiles', 'Items');
  form.cbStatics.Hint                := GetText('TabTiles', 'ItemsHint');
  form.lblFilter.Caption             := GetText('TabTiles', 'edFilterLabel');
  form.edFilter.Hint                 := GetText('TabTiles', 'edFilterHint');

  form.vdtTiles.Header.Columns[0].Text  := GetText('TabTiles', 'TilesId');
  form.vdtTiles.Header.Columns[1].Text  := GetText('TabTiles', 'TilesImage');
  form.vdtTiles.Header.Columns[2].Text  := GetText('TabTiles', 'TilesName');

  form.mnuAddToRandom.Caption        := GetText('TabTiles', 'TilesAddToRandom');
  form.mnuAddToRandom.Hint           := GetText('TabTiles', 'TilesAddToRandomHint');

  form.gbRandom.Caption              := GetText('TabTiles', 'RandomBlock');
  form.vdtRandom.Header.Columns[0].Text := GetText('TabTiles', 'RandomId');
  form.vdtRandom.Header.Columns[1].Text := GetText('TabTiles', 'RandomImage');
  form.vdtRandom.Header.Columns[2].Text := GetText('TabTiles', 'RandomName');
  form.btnClearRandom.Hint           := GetText('TabTiles', 'RandomClearHint');
  form.btnDeleteRandom.Hint          := GetText('TabTiles', 'RandomRemoveHint');
  form.btnAddRandom.Hint             := GetText('TabTiles', 'RandomAddHint');
  form.btnRandomPresetSave.Hint      := GetText('TabTiles', 'RandomSaveHint');
  form.btnRandomPresetDelete.Hint    := GetText('TabTiles', 'RandomDeleteHint');

  form.lbDlgSaveRandPrsCaption       := GetText('TabTiles', 'DlgSaveRandPrsCaption');
  form.lbDlgSaveRandPrs              := GetText('TabTiles', 'DlgSaveRandPrs');
  form.lbDlgSearchIdErrCaption       := GetText('TabTiles', 'DlgSearchIdErrCaption');
  form.lbDlgSearchIdErr              := GetText('TabTiles', 'DlgSearchIdErr');
  form.lbDlgNotFoundErrCaption       := GetText('TabTiles', 'DlgNotFoundErrCaption');
  form.lbDlgNotFoundErr              := GetText('TabTiles', 'DlgNotFoundErr');

  // ----------
  form.tsObjects.Caption             := GetText('TabObjects', 'TabPage');

  // ----------
  form.tsNavigation.Caption          := GetText('TabNavigation', 'TabPage');

  form.vstClients.Header.Columns[1].Text   := GetText('TabNavigation', 'ClientsAccounts');
  form.vstClients.Header.Columns[2].Text   := GetText('TabNavigation', 'ClientsUptime');

  form.mnuGoToClient.Caption         := GetText('TabNavigation', 'ClientsGoTo');
  form.mnuGoToClient.Hint            := GetText('TabNavigation', 'ClientsGoToHint');

  form.vstLocations.Header.Columns[0].Text := GetText('TabNavigation', 'LocationsPosition');
  form.vstLocations.Header.Columns[1].Text := GetText('TabNavigation', 'LocationsName');
  form.btnClearLocations.Hint        := GetText('TabNavigation', 'LocationsClearHint');
  form.btnDeleteLocation.Hint        := GetText('TabNavigation', 'LocationsRemoveHint');
  form.btnAddLocation.Hint           := GetText('TabNavigation', 'LocationsAddHint');

  form.gbGoTo.Caption                := GetText('TabNavigation', 'GoToBlock');
  form.lblX.Caption                  := GetText('TabNavigation', 'GoToPosX');
  form.lblY.Caption                  := GetText('TabNavigation', 'GoToPosY');
  form.btnGoTo.Caption               := GetText('TabNavigation', 'GoToButton');
  form.btnGoTo.Hint                  := GetText('TabNavigation', 'GoToButtonHint');

  form.lbDlgDelConfCaption           := GetText('TabNavigation', 'DlgDelConfCaption');
  form.lbDlgDelConf                  := GetText('TabNavigation', 'DlgDelConf');
  form.lbDlgNewQuerryCaption         := GetText('TabNavigation', 'DlgNewQuerryCaption');
  form.lbDlgNewQuerry                := GetText('TabNavigation', 'DlgNewQuerry');
end;

procedure LanguageTranslate(form: TfrmConfirmation);
begin
  form.Caption                       := GetText('ConfForm', 'FormTitle');
  form.btnYes.Caption                := GetText('ConfForm', 'Yes');
  form.btnNo.Caption                 := GetText('ConfForm', 'No');
end;

procedure LanguageTranslate(form: TfrmSelectionSettings);
begin
  form.Caption                       := GetText('SelnForm', 'FormTitle');
  // TODO
end;

procedure LanguageTranslate(form: TfrmMoveSettings);
begin
  form.Caption                       := GetText('MoveForm', 'FormTitle');
  form.seOffset.Hint                 := GetText('MoveForm', 'OffsetHint');
  form.cbAsk.Caption                 := GetText('MoveForm', 'Ask');
  form.cbAsk.Hint                    := GetText('MoveForm', 'AskHint');
  form.btnCancel.Caption             := GetText('MoveForm', 'Cancel');
  form.gbMovment.Caption             := GetText('MoveForm', 'Movment');
  form.cbLand.Caption                := GetText('MoveForm', 'Land');
  form.cbLand.Hint                   := GetText('MoveForm', 'LandHint');
  form.cbItem.Caption                := GetText('MoveForm', 'Item');
  form.cbItem.Hint                   := GetText('MoveForm', 'ItemHint');
end;

procedure LanguageTranslate(form: TfrmElevateSettings);
begin
  form.Caption                       := GetText('ElevForm', 'FormTitle');
  form.rbRaise.Caption               := GetText('ElevForm', 'Raise');
  form.rbRaise.Hint                  := GetText('ElevForm', 'RaiseHint');
  form.rbLower.Caption               := GetText('ElevForm', 'Lower');
  form.rbLower.Hint                  := GetText('ElevForm', 'LowerHint');
  form.rbSet.Caption                 := GetText('ElevForm', 'Set');
  form.rbSet.Hint                    := GetText('ElevForm', 'SetHint');
  form.cbRandomHeight.Caption        := GetText('ElevForm', 'AddRandom');
  form.cbRandomHeight.Hint           := GetText('ElevForm', 'AddRandomHint');
end;

procedure LanguageTranslate(form: TfrmSurfElevateSettings);
begin
  form.Caption                       := GetText('SElvForm', 'FormTitle');
  // TODO
end;

procedure LanguageTranslate(form: TfrmSurfStretchSettings);
begin
  form.Caption                       := GetText('SStrForm', 'FormTitle');
  // TODO
end;

procedure LanguageTranslate(form: TfrmSurfSmoothSettings);
begin
  form.Caption                       := GetText('SSmtForm', 'FormTitle');
  // TODO
end;

procedure LanguageTranslate(form: TfrmDrawSettings);
begin
  form.Caption                       := GetText('DrawForm', 'FormTitle');
  form.rbTileList.Caption            := GetText('DrawForm', 'UseTiles');
  form.rbTileList.Hint               := GetText('DrawForm', 'UseTilesHint');
  form.rbRandom.Caption              := GetText('DrawForm', 'UseRandom');
  form.rbRandom.Hint                 := GetText('DrawForm', 'UseRandomHint');
  form.cbProbability.Caption         := GetText('DrawForm', 'Probability');
  form.cbProbability.Hint            := GetText('DrawForm', 'ProbabilityHint');
  form.cbUseFreeTilesOnly.Caption    := GetText('DrawForm', 'FreeTiles');
  form.cbUseFreeTilesOnly.Hint       := GetText('DrawForm', 'FreeTilesHint');
  form.cbUseSurfaceAltitude.Caption  := GetText('DrawForm', 'SurfaceAlt');
  form.cbUseSurfaceAltitude.Hint     := GetText('DrawForm', 'SurfaceAltHint');
  form.cbForceAltitude.Caption       := GetText('DrawForm', 'ForceAlt');
  form.cbForceAltitude.Hint          := GetText('DrawForm', 'ForceAltHint');
  form.cbRandomHeight.Caption        := GetText('DrawForm', 'AddRandomAlt');
  form.cbRandomHeight.Hint           := GetText('DrawForm', 'AddRandomAltHint');
  form.gbHue.Caption                 := GetText('DrawForm', 'ItemColor');
end;

procedure LanguageTranslate(form: TfrmHueSettings);
begin
  form.Caption                       := GetText('HuesForm', 'FormTitle');
  form.cbRandom.Caption              := GetText('HuesForm', 'Random');
  form.cbRandom.Hint                 := GetText('HuesForm', 'RandomHint');
  form.lblHue.Caption                := GetText('HuesForm', 'Color');
  form.gbPreview.Caption             := GetText('HuesForm', 'PreviewBlock');
  form.ShowAll.Caption               := GetText('HuesForm', 'ShowAll');
  form.ShowAll.Hint                  := GetText('HuesForm', 'ShowAllHint');
  form.lblTileId.Caption             := GetText('HuesForm', 'Tile');
  form.gbLastUsed.Caption            := GetText('HuesForm', 'LastUsedBlock');
  form.gbRandom.Caption              := GetText('HuesForm', 'RandomBlock');
  form.btnAddRandom.Hint             := GetText('HuesForm', 'AddPresetHint');
  form.btnDeleteRandom.Hint          := GetText('HuesForm', 'RemovePresetHint');
  form.btnClearRandom.Hint           := GetText('HuesForm', 'ClearPresetHint');
  form.btnRandomPresetSave.Hint      := GetText('HuesForm', 'SavePresetHint');
  form.btnRandomPresetDelete.Hint    := GetText('HuesForm', 'DeletePresetHint');

  form.lbDlgBadColrCaption           := GetText('HuesForm', 'DlgBadColrCaption');
  form.lbDlgBadColr                  := GetText('HuesForm', 'DlgBadColr');
  form.lbDlgBadTileCaption           := GetText('HuesForm', 'DlgBadTileCaption');
  form.lbDlgBadTile                  := GetText('HuesForm', 'DlgBadTile');
  form.lbDlgSavePrsCaption           := GetText('HuesForm', 'DlgSavePrsCaption');
  form.lbDlgSavePrs                  := GetText('HuesForm', 'DlgSavePrs');
  form.lbNoHuesName                  := GetText('HuesForm', 'NoHuesName');
end;

procedure LanguageTranslate(form: TfrmFillSettings);
begin
  form.Caption                       := GetText('FillForm', 'FormTitle');
  form.lblFillType.Caption           := GetText('FillForm', 'FillType');
  form.cbFillReview.Caption          := GetText('FillForm', 'FillReview');
  form.cbFillConfirm.Caption         := GetText('FillForm', 'FillConfirm');
  form.gbHue.Caption                 := GetText('FillForm', 'ItemColor');
  // TODO
end;

procedure LanguageTranslate(form: TfrmVirtualLayer);
begin
  form.Caption                       := GetText('VLayForm', 'FormTitle');
  form.cbShowLayer.Caption           := GetText('VLayForm', 'ShowLayer');
  form.cbShowLayer.Hint              := GetText('VLayForm', 'ShowLayerHint');
  form.cbShowBlocks.Caption          := GetText('VLayForm', 'ShowBlocks');
  form.cbShowBlocks.Hint             := GetText('VLayForm', 'ShowBlocksHint');
end;

procedure LanguageTranslate(form: TfrmBoundaries);
begin
  form.Caption                       := GetText('BounForm', 'FormTitle');
  form.GroupBox1.Caption             := GetText('BounForm', 'AltBlock');;
  form.lblMinZ.Caption               := GetText('BounForm', 'MinAlt');
  form.lblMinZ.Hint                  := GetText('BounForm', 'MinAltHint');
  form.lblMaxZ.Caption               := GetText('BounForm', 'MaxAlt');
  form.lblMaxZ.Hint                  := GetText('BounForm', 'MaxAltHint');
  form.GroupBox2.Caption             := GetText('BounForm', 'XYBlock');
  form.lblAxeX.Caption               := GetText('BounForm', 'AxeXName');
  form.seMinX.Hint                   := GetText('BounForm', 'MinXHint');
  form.seMaxX.Hint                   := GetText('BounForm', 'MaxXHint');
  form.sbClearXbnd.Hint              := GetText('BounForm', 'XClearHint');
  form.lblAxeY.Caption               := GetText('BounForm', 'AxeYName');
  form.seMinY.Hint                   := GetText('BounForm', 'MinYHint');
  form.seMaxY.Hint                   := GetText('BounForm', 'MaxYHint');
  form.sbClearYbnd.Hint              := GetText('BounForm', 'YClearHint');
end;

procedure LanguageTranslate(form: TfrmFilter);
begin
  form.Caption                       := GetText('FiltForm', 'FormTitle');
  form.rgFilterType.Caption          := GetText('FiltForm', 'FilterBlock');
  form.rgFilterType.Items[0]         := GetText('FiltForm', 'FilterType1');
  form.rgFilterType.Items[1]         := GetText('FiltForm', 'FilterType2');
  form.GroupBox1.Caption             := GetText('FiltForm', 'TileBlock');
  form.cbTileFilter.Caption          := GetText('FiltForm', 'TileUse');
  form.cbTileFilter.Hint             := GetText('FiltForm', 'TileUseHint');
  form.Label1.Caption                := GetText('FiltForm', 'TileHelp');
  form.vdtFilter.Header.Columns[0].Text := GetText('FiltForm', 'TileId');
  form.vdtFilter.Header.Columns[1].Text := GetText('FiltForm', 'TileImage');
  form.vdtFilter.Header.Columns[2].Text := GetText('FiltForm', 'TileName');
  form.btnDelete.Hint                := GetText('FiltForm', 'TileRemoveHint');
  form.btnClear.Hint                 := GetText('FiltForm', 'TileClearHint');
  form.GroupBox2.Caption             := GetText('FiltForm', 'ColorBlock');
  form.cbHueFilter.Caption           := GetText('FiltForm', 'ColorUse');
  form.cbHueFilter.Hint              := GetText('FiltForm', 'ColorUseHint');
  form.vdtHues.Header.Columns[1].Text   := GetText('FiltForm', 'ColorHue');
  form.vdtHues.Header.Columns[2].Text   := GetText('FiltForm', 'ColorName');

  form.mnuCheckHues.Caption          := GetText('FiltForm', 'ColorCheck');
  form.mnuCheckHues.Hint             := GetText('FiltForm', 'ColorCheckHint');
  form.mnuUncheckHues.Caption        := GetText('FiltForm', 'ColorUncheck');
  form.mnuUncheckHues.Hint           := GetText('FiltForm', 'ColorUncheckHint');
end;

procedure LanguageTranslate(form: TfrmLightlevel);
begin
  form.Caption            := GetText('LighForm', 'FormTitle');
end;

procedure LanguageTranslate(form: TfrmRadarMap);
begin
  form.Caption                       := GetText('RMapForm', 'FormTitle');
  form.cbStayOnTop.Caption           := GetText('RMapForm', 'StayOnTop');
  form.cbStayOnTop.Hint              := GetText('RMapForm', 'StayOnTopHint');
end;

procedure LanguageTranslate(form: TfrmAccountControl);
begin
  form.Caption                       := GetText('MAccForm', 'FormTitle');
  form.tbRefresh.Hint                := GetText('MAccForm', 'RefreshHint');
  form.tbAddUser.Hint                := GetText('MAccForm', 'AddUserHint');
  form.tbEditUser.Hint               := GetText('MAccForm', 'EditUserHint');
  form.tbDeleteUser.Hint             := GetText('MAccForm', 'DeleteUserHint');
  form.vstAccounts.Header.Columns[1].Text := GetText('MAccForm', 'UserName');
  form.vstAccounts.Header.Columns[2].Text := GetText('MAccForm', 'Access');
  form.vstAccounts.Header.Columns[3].Text := GetText('MAccForm', 'Regions');

  form.lbDlgDelConfCaption           := GetText('MAccForm', 'DlgDelConfCaption');
  form.lbDlgDelConf                  := GetText('MAccForm', 'DlgDelConf');
  form.lbDlgAddNotiCaption           := GetText('MAccForm', 'DlgAddNotiCaption');
  form.lbDlgAddNoti                  := GetText('MAccForm', 'DlgAddNoti');
  form.lbDlgModNotiCaption           := GetText('MAccForm', 'DlgModNotiCaption');
  form.lbDlgModNoti                  := GetText('MAccForm', 'DlgModNoti');
  form.lbDlgInvlErrCaption           := GetText('MAccForm', 'DlgInvlErrCaption');
  form.lbDlgInvlErr                  := GetText('MAccForm', 'DlgInvlErr');
  form.lbDlgDelNotiCaption           := GetText('MAccForm', 'DlgDelNotiCaption');
  form.lbDlgDelNoti                  := GetText('MAccForm', 'DlgDelNoti');
  form.lbDlgDelfErrCaption           := GetText('MAccForm', 'DlgDelfErrCaption');
  form.lbDlgDelfErr                  := GetText('MAccForm', 'DlgDelfErr');
end;

procedure LanguageTranslate(form: TfrmEditAccount);
begin
  form.Caption                       := GetText('EAccForm', 'FormTitle');
  form.tsGeneral.Caption             := GetText('EAccForm', 'TabGeneral');
  form.tsRegions.Caption             := GetText('EAccForm', 'TabRegions');
  form.lblUsername.Caption           := GetText('EAccForm', 'Account');
  form.lblPassword.Caption           := GetText('EAccForm', 'Password');
  form.lblAccessLevel.Caption        := GetText('EAccForm', 'Access');
  form.lblPasswordHint.Caption       := GetText('EAccForm', 'PswHelp');
  form.Label1.Caption                := GetText('EAccForm', 'AllowRegions');
  form.btnOK.Caption                 := GetText('EAccForm', 'Apply');
  form.btnCancel.Caption             := GetText('EAccForm', 'Cancel');

  form.cbAccessLevel.Items[0]        := GetText('EAccForm', 'AccessNone');
  form.cbAccessLevel.Items[1]        := GetText('EAccForm', 'AccessViewer');
  form.cbAccessLevel.Items[2]        := GetText('EAccForm', 'AccessNormal');
  form.cbAccessLevel.Items[3]        := GetText('EAccForm', 'AccessDevelop');
  form.cbAccessLevel.Items[4]        := GetText('EAccForm', 'AccessAdmin');
end;

procedure LanguageTranslate(form: TfrmRegionControl);
begin
  form.Caption                       := GetText('MRegForm', 'FormTitle');
  form.vstRegions.Header.Columns[0].Text := GetText('MRegForm', 'Regions');
  form.mnuAddRegion.Caption          := GetText('MRegForm', 'AddRegion');
  form.mnuAddRegion.Hint             := GetText('MRegForm', 'AddRegionHint');
  form.btnAddRegion.Hint             := GetText('MRegForm', 'AddRegionHint');
  form.mnuDeleteRegion.Caption       := GetText('MRegForm', 'DeleteRegion');
  form.mnuDeleteRegion.Hint          := GetText('MRegForm', 'DeleteRegionHint');
  form.btnDeleteRegion.Hint          := GetText('MRegForm', 'DeleteRegionHint');
  form.Label1.Caption                := GetText('MRegForm', 'Area');
  form.btnAddArea.Hint               := GetText('MRegForm', 'AddAreaHint');
  form.btnDeleteArea.Hint            := GetText('MRegForm', 'DeleteAreaHint');
  form.btnClearArea.Hint             := GetText('MRegForm', 'ClearAreaHint');
  form.lblX.Caption                  := GetText('MRegForm', 'PosX');
  form.lblY.Caption                  := GetText('MRegForm', 'PosY');
  form.btnGrab1.Hint                 := GetText('MRegForm', 'PosGrab1Hint');
  form.btnGrab2.Hint                 := GetText('MRegForm', 'PosGrab2Hint');
  form.btnSave.Caption               := GetText('MRegForm', 'Save');
  form.btnClose.Caption              := GetText('MRegForm', 'Close');

  form.lbDlgUnsaveCaption            := GetText('MRegForm', 'DlgUnsaveCaption');
  form.lbDlgUnsave                   := GetText('MRegForm', 'DlgUnsave');
  form.lbDlgDelConfCaption           := GetText('MRegForm', 'DlgDelConfCaption');
  form.lbDlgDelConf                  := GetText('MRegForm', 'DlgDelConf');
  form.lbDlgNewExistsCaption         := GetText('MRegForm', 'DlgNewExistsCaption');
  form.lbDlgNewExists                := GetText('MRegForm', 'DlgNewExists');
  form.lbNewInputQueryCaption        := GetText('MRegForm', 'NewInputQueryCaption');
  form.lbNewInputQuery               := GetText('MRegForm', 'NewInputQuery');
end;

procedure LanguageTranslate(form: TfrmLargeScaleCommand);
begin
  form.Caption                       := GetText('GCmdForm', 'FormTitle');
  form.vstActions.Header.Columns[0].Text := GetText('GCmdForm', 'Actions');
  form.Label1.Caption                := GetText('GCmdForm', 'Ares');
  form.btnAddArea.Hint               := GetText('GCmdForm', 'AddAreaHint');
  form.btnDeleteArea.Hint            := GetText('GCmdForm', 'DeleteAreaHint');
  form.btnClearArea.Hint             := GetText('GCmdForm', 'ClearAreaHint');
  form.lblY.Caption                  := GetText('GCmdForm', 'PosX');
  form.lblX.Caption                  := GetText('GCmdForm', 'PosY');
  form.btnGrab1.Hint                 := GetText('GCmdForm', 'PosGrab1Hint');
  form.btnGrab2.Hint                 := GetText('GCmdForm', 'PosGrab2Hint');
  form.btnExecute.Caption            := GetText('GCmdForm', 'Execute');
  form.btnClose.Caption              := GetText('GCmdForm', 'Close');

  // ----------
  form.pgArea.Caption                := GetText('GCmdForm', 'PageSelectArea');
  form.mnuSelectTopLeft.Caption      := GetText('GCmdForm', 'psaSelectTL');
  form.mnuSelectTopLeft.Hint         := GetText('GCmdForm', 'psaSelectTLHint');
  form.mnuSelectTopRight.Caption     := GetText('GCmdForm', 'psaSelectTR');
  form.mnuSelectTopRight.Hint        := GetText('GCmdForm', 'psaSelectTRHint');
  form.mnuSelectBottomLeft.Caption   := GetText('GCmdForm', 'psaSelectBL');
  form.mnuSelectBottomLeft.Hint      := GetText('GCmdForm', 'psaSelectBLHint');
  form.mnuSelectBottomRight.Caption  := GetText('GCmdForm', 'psaSelectBR');
  form.mnuSelectBottomRight.Hint     := GetText('GCmdForm', 'psaSelectBRHint');

  // ----------
  form.pgCopyMove.Caption            := GetText('GCmdForm', 'PageCopyMove');
  form.rgCMAction.Caption            := GetText('GCmdForm', 'pcmActionBloack');
  form.rgCMAction.Items[0]           := GetText('GCmdForm', 'pcmAction1');
  form.rgCMAction.Items[1]           := GetText('GCmdForm', 'pcmAction2');
  form.gbCMOffset.Caption            := GetText('GCmdForm', 'pcmOffsetBlock');
  form.Label9.Caption                := GetText('GCmdForm', 'pcmOffsetPosX');
  form.Label10.Caption               := GetText('GCmdForm', 'pcmOffsetPosY');
  form.btnGrabOffset.Hint            := GetText('GCmdForm', 'pcmOffsetGrabHint');
  form.cbCMEraseTarget.Caption       := GetText('GCmdForm', 'pcmEraseTarget');
  form.cbCMEraseTarget.Hint          := GetText('GCmdForm', 'pcmEraseTargetHint');

  // ----------
  form.pgModifyAltitude.Caption      := GetText('GCmdForm', 'PageModifyAlt');
  form.rbSetTerrainAltitude.Caption  := GetText('GCmdForm', 'pmaSetLandAlt');
  form.rbSetTerrainAltitude.Hint     := GetText('GCmdForm', 'pmaSetLandAltHint');
  form.Label2.Caption                := GetText('GCmdForm', 'pmaSetLandAltHelp');
  form.rbRelativeAltitudeChange.Caption             := GetText('GCmdForm', 'pmaChangeRelativeAlt');
  form.rbRelativeAltitudeChange.Hint                := GetText('GCmdForm', 'pmaChangeRelativeAltHint');
  form.Label3.Caption                := GetText('GCmdForm', 'pmaSetAltFromTo');
  form.Label4.Caption                := GetText('GCmdForm', 'pmaTo');

  // ----------
  form.pgDrawTerrain.Caption         := GetText('GCmdForm', 'PageDrawTerrain');
  form.gbDrawTerrainTiles.Caption    := GetText('GCmdForm', 'pdtTileBlock');
  form.lblDrawTerrainTilesDesc.Caption              := GetText('GCmdForm', 'pdtTileHelp');
  form.vdtTerrainTiles.Header.Columns[0].Text       := GetText('GCmdForm', 'pdtId');
  form.vdtTerrainTiles.Header.Columns[1].Text       := GetText('GCmdForm', 'pdtImage');
  form.vdtTerrainTiles.Header.Columns[2].Text       := GetText('GCmdForm', 'pdtName');
  form.btnDeleteTerrain.Hint         := GetText('GCmdForm', 'pdtDeleteHint');
  form.btnClearTerrain.Hint          := GetText('GCmdForm', 'pdtClearHint');

  // ----------
  form.pgDeleteStatics.Caption       := GetText('GCmdForm', 'PageDeleteItems');
  form.gbDeleteStaticsTiles.Caption  := GetText('GCmdForm', 'pdiTileBlock');
  form.lblDeleteStaticsTilesDesc.Caption            := GetText('GCmdForm', 'pdiTileHelp');
  form.vdtDeleteStaticsTiles.Header.Columns[0].Text := GetText('GCmdForm', 'pdiId');
  form.vdtDeleteStaticsTiles.Header.Columns[1].Text := GetText('GCmdForm', 'pdiImage');
  form.vdtDeleteStaticsTiles.Header.Columns[2].Text := GetText('GCmdForm', 'pdiName');
  form.btnDeleteDStaticsTiles.Hint   := GetText('GCmdForm', 'pdiDeleteHint');
  form.btnClearDStaticsTiles.Hint    := GetText('GCmdForm', 'pdiClearHint');
  form.GroupBox1.Caption             := GetText('GCmdForm', 'pdiAltBoundBlock');
  form.Label7.Caption                := GetText('GCmdForm', 'pdiAltBoundHelp');
  form.Label8.Caption                := GetText('GCmdForm', 'pdiAltBoundTo');

  // ----------
  form.pgInsertStatics.Caption       := GetText('GCmdForm', 'PageInsertItems');
  form.gbInserStaticsTiles.Caption   := GetText('GCmdForm', 'piiTileBlock');
  form.lblInsertStaticsTiles.Caption := GetText('GCmdForm', 'piiTileHelp');
  form.vdtInsertStaticsTiles.Header.Columns[0].Text := GetText('GCmdForm', 'piiId');
  form.vdtInsertStaticsTiles.Header.Columns[1].Text := GetText('GCmdForm', 'piiImage');
  form.vdtInsertStaticsTiles.Header.Columns[2].Text := GetText('GCmdForm', 'piiName');
  form.btnDeleteIStaticsTiles.Hint   := GetText('GCmdForm', 'piiDeleteHint');
  form.btnClearIStaticsTiles.Hint    := GetText('GCmdForm', 'piiClearHint');
  form.gbStaticsProbability.Caption  := GetText('GCmdForm', 'piiProbBlock');
  form.Label5.Caption                := GetText('GCmdForm', 'piiProbHelp');
  form.Label6.Caption                := GetText('GCmdForm', 'piiProbPercent');
  form.gbStaticsPlacement.Caption    := GetText('GCmdForm', 'piiAltPlaceBlock');
  form.rbPlaceStaticsOnTerrain.Caption              := GetText('GCmdForm', 'piiAltPlace1');
  form.rbPlaceStaticsOnTop.Caption   := GetText('GCmdForm', 'piiAltPlace2');
  form.rbPlaceStaticsOnZ.Caption     := GetText('GCmdForm', 'piiAltPlace3');

  // ----------
  // Обновляем вписок для перезагрузки текста
  form.RenameNode(-1, form.pgArea.Caption);
  form.RenameNode( 0, form.pgCopyMove.Caption);
  form.RenameNode( 1, form.pgModifyAltitude.Caption);
  form.RenameNode( 2, form.pgDrawTerrain.Caption);
  form.RenameNode( 3, form.pgDeleteStatics.Caption);
  form.RenameNode( 4, form.pgInsertStatics.Caption);

end;
{
procedure LanguageTranslate(form: TfrmMain);
begin
  form.Caption            := GetText('MainForm', '');
  form.Hint               := GetText('MainForm', 'Hint');
end;

procedure LanguageTranslate(form: TfrmMain);
begin
  form.Caption            := GetText('MainForm', '');
  form.Hint               := GetText('MainForm', 'Hint');
end;
 }

procedure LanguageTranslate(form: TfrmAbout);
begin
  form.Caption                       := GetText('AbouForm', 'FormTitle');
  form.lbVersion                     := GetText('AbouForm', 'Version');
  form.lbBuild                       := GetText('AbouForm', 'Build');
end;

function GetParseErText(key: string) : string;
begin
  Result := GetText('ParseErr', key);
end;

end.

