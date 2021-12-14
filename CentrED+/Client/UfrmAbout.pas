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
 *      Portions Copyright 2007 Andreas Schneider
 *)
unit UfrmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TButton;
    imgLogo: TImage;
    imgBaner: TImage;
    Label3: TLabel;
    Label8: TLabel;
    lblCopyright: TLabel;
    lblVersion: TLabel;
    lblWebLink: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgBanerClick(Sender: TObject);
    procedure imgBanerMouseEnter(Sender: TObject);
    procedure imgBanerMouseLeave(Sender: TObject);
    procedure lblWebLinkClick(Sender: TObject);
    procedure lblWebLinkMouseEnter(Sender: TObject);
    procedure lblWebLinkMouseLeave(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
  private
    { private declarations }
    pSound: Pointer;
  public
    { public declarations }
    lbVersion: string;
    lbBuild: string;
  end; 

var
  frmAbout: TfrmAbout;

implementation

uses vinfo, Language, UResourceManager;
{$I version.inc}


{ TfrmAbout }

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAbout.FormCreate(Sender: TObject);
begin
  pSound:= nil;
  LanguageTranslate(Self);
  lblVersion.Caption := Format('%s %s%s  %s %d',
  [lbVersion, VersionInfo.GetFileVersionString, #13#10, lbBuild, VersionInfo.Build]);
  imgLogo.Picture.Bitmap.LoadFromStream(ResourceManager.GetResource('Overlay/About.bmp'));
  //lblVersion.Caption := Format('Version %s', [ProductVersion]);
  //lblCopyright.Caption := Format('Copyright %s', [Copyright]);
end;

procedure TfrmAbout.FormShow(Sender: TObject);
var
  stream: TStream;
begin
  if (pSound = nil) then begin
    stream := ResourceManager.GetResource('Sounds/MusicInf.wav');
    GetMem(pSound, stream.Size);
    stream.Read(pSound^, stream.Size);
  end;
  //TODO: sndPlaySound(pSound, SND_MEMORY or SND_ASYNC or SND_LOOP);
end;

procedure TfrmAbout.FormHide(Sender: TObject);
begin
  if pSound = nil then Exit;
  //TODO: sndPlaySound(nil, SND_ASYNC or SND_LOOP);
  FreeMem(pSound);
  pSound := nil;
end;

procedure TfrmAbout.imgBanerClick(Sender: TObject);
begin
  //Open the site in the default browser
  //TODO: ShellExecute(Handle, 'open', PChar('http://uoquint.ru'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmAbout.imgBanerMouseEnter(Sender: TObject);
var stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.Write(baner_h[0],SizeOf(baner_h));
  stream.Position:=0;
  imgBaner.Picture.Bitmap.LoadFromStream(stream);
  stream.Free;
end;

procedure TfrmAbout.imgBanerMouseLeave(Sender: TObject);
var stream: TMemoryStream;
begin
  stream := TMemoryStream.Create;
  stream.Write(baner_u[0],SizeOf(baner_u));
  stream.Position:=0;
  imgBaner.Picture.Bitmap.LoadFromStream(stream);
  stream.Free;
//  sndPlaySound
end;

procedure TfrmAbout.lblWebLinkClick(Sender: TObject);
begin
  //Open the site in the default browser
  //ShellExecute(Handle, 'open', PChar('http://dev.uoquint.ru/projects/centred'), nil, nil, 1 {SW_SHOWNORMAL});
end;

procedure TfrmAbout.lblWebLinkMouseEnter(Sender: TObject);
begin
  lblWebLink.Font.Color := TColor($00EAAE86);
end;

procedure TfrmAbout.lblWebLinkMouseLeave(Sender: TObject);
begin
  lblWebLink.Font.Color := TColor($00E18A51);
end;

procedure TfrmAbout.Panel2Click(Sender: TObject);
begin

end;

initialization
  {$I UfrmAbout.lrs}

end.

