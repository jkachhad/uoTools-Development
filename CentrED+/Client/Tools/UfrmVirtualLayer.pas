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
unit UfrmVirtualLayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ComCtrls, ExtCtrls, UfrmToolWindow;

type

  { TfrmVirtualLayer }

  TfrmVirtualLayer = class(TfrmToolWindow)
    btnZi20: TButton;
    btnZi05: TButton;
    btnZd20: TButton;
    btnZd05: TButton;
    btnZd03: TButton;
    btnZi03: TButton;
    cbShowLayer: TCheckBox;
    cbShowBlocks: TCheckBox;
    seZ: TSpinEdit;
    tbZ: TTrackBar;
    procedure btnZ_Click(Sender: TObject);
    procedure cbShowLayerChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure seZChange(Sender: TObject);
    procedure tbZChange(Sender: TObject);
  public
    { public declarations }
  end; 

var
  frmVirtualLayer: TfrmVirtualLayer;

implementation

uses
  UfrmMain, Language;

{ TfrmVirtualLayer }

procedure TfrmVirtualLayer.seZChange(Sender: TObject);
begin
  tbZ.Position := seZ.Value;
  frmMain.InvalidateScreenBuffer;
end;

procedure TfrmVirtualLayer.cbShowLayerChange(Sender: TObject);
begin
  frmMain.InvalidateScreenBuffer;
end;

procedure TfrmVirtualLayer.btnZ_Click(Sender: TObject);
var
  value: integer;
begin
  value := seZ.Value;
  if (Sender = btnZd20) then dec(value, 20) else
  if (Sender = btnZd05) then dec(value, 05) else
  if (Sender = btnZd03) then dec(value, 03) else
  if (Sender = btnZi03) then inc(value, 03) else
  if (Sender = btnZi05) then inc(value, 05) else
  if (Sender = btnZi20) then inc(value, 20) else
     exit;
  if (value < -128) or (value > 127) then
     exit;
  seZ.Value := value;
  seZChange(Sender);
end;

procedure TfrmVirtualLayer.FormCreate(Sender: TObject);
begin
  LanguageTranslate(Self);
end;

procedure TfrmVirtualLayer.tbZChange(Sender: TObject);
begin
  seZ.Value := tbZ.Position;
  frmMain.InvalidateScreenBuffer;
end;

initialization
  {$I UfrmVirtualLayer.lrs}

end.

