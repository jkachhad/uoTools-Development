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
unit UfrmMoveSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Spin, ExtCtrls, Math, UfrmToolWindow;

type

  { TfrmMoveSettings }

  TfrmMoveSettings = class(TfrmToolWindow)
    btnCancel: TButton;
    cbAsk: TCheckBox;
    cbLand: TCheckBox;
    cbItem: TCheckBox;
    gbDirection: TGroupBox;
    btnTopLeft: TSpeedButton;
    btnTop: TSpeedButton;
    btnTopRight: TSpeedButton;
    btnRight: TSpeedButton;
    btnBottomRight: TSpeedButton;
    btnBottom: TSpeedButton;
    btnBottomLeft: TSpeedButton;
    btnLeft: TSpeedButton;
    gbMovment: TGroupBox;
    seOffset: TSpinEdit;
    procedure btnTopLeftClick(Sender: TObject);
    procedure cbItemChange(Sender: TObject);
    procedure cbLandChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject); override;
    procedure FormShow(Sender: TObject); override;
  protected
    function CanClose: Boolean; override;
  public
    function GetOffsetX: Integer;
    function GetOffsetY: Integer;
  end; 

var
  frmMoveSettings: TfrmMoveSettings;

implementation

uses
  UdmNetwork, UfrmMain, UEnums, Language;

{ TfrmMoveSettings }

procedure TfrmMoveSettings.FormDeactivate(Sender: TObject);
begin
  if not (fsModal in FormState) then
    inherited FormDeactivate(Sender);
end;

procedure TfrmMoveSettings.FormShow(Sender: TObject);
begin
  btnCancel.Visible := (fsModal in FormState);
  if dmNetwork.AccessLevel = alDeveloper then
    seOffset.MaxValue := Max(frmMain.Landscape.CellWidth, frmMain.Landscape.CellHeight);

  inherited FormShow(Sender);
end;

function TfrmMoveSettings.CanClose: Boolean;
begin
  Result := Visible and (not (fsModal in FormState)) and
    inherited CanClose;
end;

function TfrmMoveSettings.GetOffsetX: Integer;
begin
  if btnTopLeft.Down then
    Result := -seOffset.Value
  else if btnTop.Down then
    Result := -seOffset.Value
  else if btnTopRight.Down then
    Result := 0
  else if btnRight.Down then
    Result := seOffset.Value
  else if btnBottomRight.Down then
    Result := seOffset.Value
  else if btnBottom.Down then
    Result := seOffset.Value
  else if btnBottomLeft.Down then
    Result := 0
  else if btnLeft.Down then
    Result := -seOffset.Value
  else
    Result := 0;
end;

function TfrmMoveSettings.GetOffsetY: Integer;
begin
  if btnTopLeft.Down then
    Result := 0
  else if btnTop.Down then
    Result := -seOffset.Value
  else if btnTopRight.Down then
    Result := -seOffset.Value
  else if btnRight.Down then
    Result := -seOffset.Value
  else if btnBottomRight.Down then
    Result := 0
  else if btnBottom.Down then
    Result := seOffset.Value
  else if btnBottomLeft.Down then
    Result := seOffset.Value
  else if btnLeft.Down then
    Result := seOffset.Value
  else
    Result := 0;
end;

procedure TfrmMoveSettings.btnTopLeftClick(Sender: TObject);
begin
  ModalResult := mrYes;
end;

procedure TfrmMoveSettings.cbItemChange(Sender: TObject);
begin
  if not cbItem.Checked
    then cbLand.Checked := True;
end;

procedure TfrmMoveSettings.cbLandChange(Sender: TObject);
begin
  if not cbLand.Checked
    then cbItem.Checked := True;
end;

procedure TfrmMoveSettings.FormCreate(Sender: TObject);
begin
  LanguageTranslate(Self);
end;


initialization
  {$I UfrmMoveSettings.lrs}

end.

