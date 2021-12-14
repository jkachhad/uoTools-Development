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
unit UfrmInitialize;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, LCLIntf, LCLType, WSForms;

type

  { TfrmInitialize }

  TfrmInitialize = class(TForm)
    imgSplah: TImage;
    lblStatus: TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  protected
    FActiveWindow: HWND;
    FModal: Boolean;
  public
    procedure SetModal;
    procedure UnsetModal;
    procedure SetStatusLabel(message: string);
  public // Локализация
    SplashConnection: string;
    SplashAuthorization: string;
    SplashInicialization: string;
    SplashLoading: string;
    SplashUpdates: string;
    SplashSuspend: string;
    SplashUpdatingMiniMap: string;
  end; 

var
  frmInitialize: TfrmInitialize;

implementation

uses UResourceManager;

{ TfrmInitialize }

procedure TfrmInitialize.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caNone;
end;

procedure TfrmInitialize.FormCreate(Sender: TObject);
begin
  FModal := False;
  imgSplah.Picture.Bitmap.LoadFromStream(ResourceManager.GetResource('Overlay/Splash.bmp'));
end;

procedure TfrmInitialize.SetModal;
begin
  if FModal then Exit;
  FActiveWindow := GetActiveWindow;
  TWSCustomFormClass(WidgetSetClass).ShowModal(Self);
  {FormStyle := fsStayOnTop;
  Screen.MoveFormToFocusFront(Self);
  Screen.MoveFormToZFront(Self);}
  FModal := True;
end;

procedure TfrmInitialize.UnsetModal;
begin
  if not FModal then Exit;
  TWSCustomFormClass(WidgetSetClass).CloseModal(Self);
  if FActiveWindow <> 0 then SetActiveWindow(FActiveWindow);
  FActiveWindow := 0;
  //FormStyle := fsNormal;
  FModal := False;
end;

procedure TfrmInitialize.SetStatusLabel(message: string);
begin
  frmInitialize.lblStatus.Caption := message;
  frmInitialize.Update;
  frmInitialize.Repaint;
  frmInitialize.lblStatus.Repaint;
  Application.ProcessMessages;
end;

initialization
  {$I UfrmInitialize.lrs}

end.

