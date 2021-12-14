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
unit UfrmEditAccount;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  UEnums, ComCtrls, ExtCtrls, CheckLst, UfrmRegionControl, VirtualTrees;

function GetAccessLevel(AAccessLevel: TAccessLevel): string;

type

  { TfrmEditAccount }
  TfrmEditAccount = class(TForm)
    btnCancel: TButton;
    btnOK: TButton;
    cbAccessLevel: TComboBox;
    cbRegions: TCheckListBox;
    edPassword: TEdit;
    edUsername: TEdit;
    Label1: TLabel;
    lblAccessLevel: TLabel;
    lblPassword: TLabel;
    lblPasswordHint: TLabel;
    lblUsername: TLabel;
    PageControl1: TPageControl;
    Panel1: TPanel;
    tsGeneral: TTabSheet;
    tsRegions: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    //constructor Create();// override;
  public
    function GetAccessLevel: TAccessLevel;
    function GetRegions: TStrings;
    procedure SetAccessLevel(AAccessLevel: TAccessLevel);
    procedure SetRegions(ARegions: TStrings);
  protected
    procedure RegionModified(ARegion: TRegionInfo);
    procedure RegionDeleted(ARegionName: string);
    procedure RegionList;
  end; 

var
  frmEditAccount: TfrmEditAccount;

implementation

uses Language;

{ TfrmEditAccount }

function GetAccessLevel(AAccessLevel: TAccessLevel): string;
var
  index: Integer;
  form: TfrmEditAccount;
begin
  if frmEditAccount = nil
    then form := TfrmEditAccount.Create(nil)
    else form := frmEditAccount;
  case AAccessLevel of
    alNone:          index := 0;
    alView:          index := 1;
    alNormal:        index := 2;
    alDeveloper:     index := 3;
    alAdministrator: index := 4;
  end;
  Result := form.cbAccessLevel.Items[index];
  if frmEditAccount = nil
    then form.Destroy;
end;

//procedure TfrmEditAccount.Create;// override;
//begin
//  LanguageTranslate(Self);
//end;

procedure TfrmEditAccount.FormCreate(Sender: TObject);
begin
  LanguageTranslate(Self);
  if frmRegionControl <> nil then begin
    frmRegionControl.OnRegionModified := @RegionModified;
    frmRegionControl.OnRegionDeleted := @RegionDeleted;
    frmRegionControl.OnRegionList := @RegionList;
  end;
end;

procedure TfrmEditAccount.FormDestroy(Sender: TObject);
begin
  if frmRegionControl <> nil then begin
    frmRegionControl.OnRegionModified := nil;
    frmRegionControl.OnRegionDeleted := nil;
    frmRegionControl.OnRegionList := nil;
  end;
end;

procedure TfrmEditAccount.FormShow(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
end;

function TfrmEditAccount.GetAccessLevel: TAccessLevel;
begin
  case cbAccessLevel.ItemIndex of
    0: Result := alNone;
    1: Result := alView;
    2: Result := alNormal;
    3: Result := alDeveloper;
    4: Result := alAdministrator;
  end;
end;

function TfrmEditAccount.GetRegions: TStrings;
var
  regions: TStringList;
  i: Integer;
begin
  regions := TStringList.Create;
  for i := 0 to cbRegions.Items.Count - 1 do
  begin
    if cbRegions.Checked[i] then
      regions.Add(cbRegions.Items[i]);
  end;
  Result := regions;
end;

procedure TfrmEditAccount.SetAccessLevel(AAccessLevel: TAccessLevel);
begin
  case AAccessLevel of
    alNone: cbAccessLevel.ItemIndex := 0;
    alView: cbAccessLevel.ItemIndex := 1;
    alNormal: cbAccessLevel.ItemIndex := 2;
    alDeveloper: cbAccessLevel.ItemIndex := 3;
    alAdministrator: cbAccessLevel.ItemIndex := 4;
  end;
end;

procedure TfrmEditAccount.SetRegions(ARegions: TStrings);
var
  i: Integer;
begin
  for i := 0 to cbRegions.Items.Count - 1 do
    cbRegions.Checked[i] := (ARegions <> nil) and
      (ARegions.IndexOf(cbRegions.Items.Strings[i]) > -1);
end;

procedure TfrmEditAccount.RegionModified(ARegion: TRegionInfo);
begin
  if cbRegions.Items.IndexOf(ARegion.Name) = -1 then
    cbRegions.Items.Add(ARegion.Name);
end;

procedure TfrmEditAccount.RegionDeleted(ARegionName: string);
var
  index: Integer;
begin
  index := cbRegions.Items.IndexOf(ARegionName);
  if index > -1 then
    cbRegions.Items.Delete(index);
end;

procedure TfrmEditAccount.RegionList;
var
  regionNode: PVirtualNode;
  regionInfo: PRegionInfo;
begin
  cbRegions.Items.BeginUpdate;
  cbRegions.Items.Clear;
  regionNode := frmRegionControl.vstRegions.GetFirst;
  while regionNode <> nil do
  begin
    regionInfo := frmRegionControl.vstRegions.GetNodeData(regionNode);
    cbRegions.Items.Add(regionInfo^.Name);
    regionNode := frmRegionControl.vstRegions.GetNext(regionNode);
  end;
  cbRegions.Items.EndUpdate;
end;

initialization
  {$I UfrmEditAccount.lrs}

end.

