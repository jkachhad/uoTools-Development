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
unit UfrmBoundaries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, Spin, ExtCtrls, Buttons, UfrmToolWindow;

type

  { TfrmBoundaries }

  TfrmBoundaries = class(TfrmToolWindow)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lblAxeX1: TLabel;
    lblAxeY1: TLabel;
    lblMaxZ: TLabel;
    lblMinZ: TLabel;
    lblAxeX: TLabel;
    lblAxeY: TLabel;
    seMaxZ: TSpinEdit;
    seMinZ: TSpinEdit;
    seMaxX: TSpinEdit;
    seMaxY: TSpinEdit;
    seMinY: TSpinEdit;
    seMinX: TSpinEdit;
    sbClearXbnd: TSpeedButton;
    sbClearYbnd: TSpeedButton;
    tbMinZ: TTrackBar;
    tbMaxZ: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure sbClearXbndClick(Sender: TObject);
    procedure sbClearYbndClick(Sender: TObject);
    procedure seMaxXChange(Sender: TObject);
    procedure seMaxYChange(Sender: TObject);
    procedure seMaxZChange(Sender: TObject);
    procedure seMinXChange(Sender: TObject);
    procedure seMinYChange(Sender: TObject);
    procedure seMinZChange(Sender: TObject);
    procedure tbMaxZChange(Sender: TObject);
    procedure tbMinZChange(Sender: TObject);
  public
    { public declarations }
  end; 

var
  frmBoundaries: TfrmBoundaries;

implementation

uses
  UfrmMain, Language;

{ TfrmBoundaries }

procedure TfrmBoundaries.FormCreate(Sender: TObject);
begin
  LanguageTranslate(Self);
  seMaxX.MaxValue := 8*frmMain.Landscape.Width-1;
  seMinX.MaxValue := seMaxX.MaxValue;  seMaxX.Value := seMaxX.MaxValue;
  seMaxY.MaxValue := 8*frmMain.Landscape.Height-1;
  seMinY.MaxValue := seMaxY.MaxValue;  seMaxY.Value := seMaxY.MaxValue;
end;

procedure TfrmBoundaries.sbClearXbndClick(Sender: TObject);
begin
  seMinX.Value := seMinX.MinValue;
  seMaxX.Value := seMaxX.MaxValue;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.sbClearYbndClick(Sender: TObject);
begin
  seMinY.Value := seMinY.MinValue;
  seMaxY.Value := seMaxY.MaxValue;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMinXChange(Sender: TObject);
begin
  if (seMaxX.Value <= seMinX.Value) then if (seMinX.Value < seMaxX.MaxValue)
    then seMaxX.Value := seMinX.Value+1
    else seMaxX.Value := seMaxX.MaxValue;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMaxXChange(Sender: TObject);
begin
  if (seMinX.Value >= seMaxX.Value) then if (seMaxX.Value > seMinX.MinValue)
    then seMinX.Value := seMaxX.Value-1
    else seMinX.Value := seMinX.MinValue;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMinYChange(Sender: TObject);
begin
  if (seMaxY.Value <= seMinY.Value) then if (seMinY.Value < seMaxY.MaxValue)
    then seMaxY.Value := seMinY.Value+1
    else seMaxY.Value := seMaxY.MaxValue;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMaxYChange(Sender: TObject);
begin
  if (seMinY.Value >= seMaxY.Value) then if (seMaxY.Value > seMinY.MinValue)
    then seMinY.Value := seMaxY.Value-1
    else seMinY.Value := seMinY.MinValue;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMinZChange(Sender: TObject);
begin
  if (seMaxZ.Value <= seMinZ.Value) then if (seMinZ.Value < seMaxZ.MaxValue)
    then seMaxZ.Value := seMinZ.Value+1
    else seMaxZ.Value := seMaxZ.MaxValue;
  tbMinZ.Position := seMinZ.Value;
  tbMaxZ.Position := seMaxZ.Value;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.seMaxZChange(Sender: TObject);
begin
  if (seMinZ.Value >= seMaxZ.Value) then if (seMaxZ.Value > seMinZ.MinValue)
    then seMinZ.Value := seMaxZ.Value-1
    else seMinZ.Value := seMinZ.MinValue;
  tbMaxZ.Position := seMaxZ.Value;
  tbMinZ.Position := seMinZ.Value;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.tbMinZChange(Sender: TObject);
begin
  if (tbMaxZ.Position <= tbMinZ.Position) then if (tbMinZ.Position < tbMaxZ.Max)
    then tbMaxZ.Position := tbMinZ.Position+1
    else tbMaxZ.Position := tbMaxZ.Max;
  seMinZ.Value := tbMinZ.Position;
  seMaxZ.Value := tbMaxZ.Position;
  frmMain.InvalidateFilter;
end;

procedure TfrmBoundaries.tbMaxZChange(Sender: TObject);
begin
  if (tbMinZ.Position >= tbMaxZ.Position) then if (tbMaxZ.Position > tbMinZ.Min)
    then tbMinZ.Position := tbMaxZ.Position-1
    else tbMinZ.Position := tbMinZ.Min;
  seMaxZ.Value := tbMaxZ.Position;
  seMinZ.Value := tbMinZ.Position;
  frmMain.InvalidateFilter;
end;

initialization
  {$I UfrmBoundaries.lrs}

end.

