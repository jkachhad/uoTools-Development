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
unit UfrmFilter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, VirtualTrees, VirtualList, LCLIntf, LCLType, LMessages, Buttons,
  UPlatformTypes, UStatics, Menus, Logging;

type

  { TfrmFilter }

  TfrmFilter = class(TForm)
    btnClear: TSpeedButton;
    btnDelete: TSpeedButton;
    btnRandomPresetDelete: TSpeedButton;
    btnRandomPresetSave: TSpeedButton;
    cbRandomPreset: TComboBox;
    cbTileFilter: TCheckBox;
    cbHueFilter: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    mnuUncheckHues: TMenuItem;
    mnuCheckHues: TMenuItem;
    pnlRandomPreset: TPanel;
    pmHues: TPopupMenu;
    rgFilterType: TRadioGroup;
    Splitter1: TSplitter;
    tFormClose: TTimer;
    vdtFilter: TVirtualList;
    vdtHues: TVirtualDrawTree;
    procedure btnClearClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure cbHueFilterChange(Sender: TObject);
    procedure cbTileFilterChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure GroupBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure mnuUncheckHuesClick(Sender: TObject);
    procedure mnuCheckHuesClick(Sender: TObject);
    procedure rgFilterTypeClick(Sender: TObject);
    procedure tFormCloseTimer(Sender: TObject);
    procedure vdtFilterDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      Pt: TPoint; var Effect: Integer; Mode: TDropMode);
    procedure vdtFilterDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; Pt: TPoint; Mode: TDropMode;
      var Effect: Integer; var Accept: Boolean);
    procedure vdtFilterDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtHuesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vdtHuesDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
  private
    FLastRMouseDown: DWORD;
  protected
    FLocked: Boolean;
    FCheckedHues: TBits;
    procedure MouseLeave(var msg: TLMessage); message CM_MouseLeave;
  public
    property Locked: Boolean read FLocked write FLocked;
    function Filter(AStatic: TStaticItem): Boolean;
    procedure JumpToHue(AHueID: Word);
    procedure AddTile(ATileID: LongWord);
    procedure AddHue(AHueID: Word);
  end; 

var
  frmFilter: TfrmFilter;

implementation

uses
  UfrmMain, UGameResources, UHue, UGraphicHelper, UGUIPlatformUtils, Language;
  
type
  PTileInfo = ^TTileInfo;
  TTileInfo = record
    ID: LongWord;
    ptr: Pointer;
  end;
  PHueInfo = ^THueInfo;
  THueInfo = record
    ID: LongWord;
    Hue: THue;
  end;

{ TfrmFilter }

procedure TfrmFilter.FormShow(Sender: TObject);
var
  wspos : TPoint;
  wrect : TRect;
begin
  SetWindowParent(Handle, frmMain.Handle);
  GetWindowRect(frmFilter.Handle, wrect);
  wspos := frmMain.oglGameWindow.ClientToScreen(Classes.Point(0, 0));
  Left  := wspos.X - 1;
  Top   := wspos.Y - 1;
  Height:= frmMain.oglGameWindow.ClientHeight - (wrect.Bottom - wrect.Top - ClientHeight);
end;

procedure TfrmFilter.GroupBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TfrmFilter.mnuUncheckHuesClick(Sender: TObject);
begin
  vdtHues.ClearChecked;
end;

procedure TfrmFilter.mnuCheckHuesClick(Sender: TObject);
var
  node: PVirtualNode;
begin
  node := vdtHues.GetFirst;
  while node <> nil do
  begin
    vdtHues.CheckState[node] := csCheckedNormal;
    node := vdtHues.GetNext(node);
  end;
end;

procedure TfrmFilter.rgFilterTypeClick(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmFilter.vdtFilterDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; Pt: TPoint; var Effect: Integer; Mode: TDropMode);
var
  sourceTree: TVirtualList;
  selected: PVirtualItem;
  node: PVirtualNode;
  sourceTileInfo, targetTileInfo: PTileInfo;
begin
  sourceTree := Source as TVirtualList;
  if (sourceTree <> Sender) and (sourceTree <> nil) then
  begin
    Sender.BeginUpdate;
    selected := sourceTree.GetFirstSelected;
    while selected <> nil do
    begin
      sourceTileInfo := sourceTree.GetNodeData(selected);
      if (sourceTileInfo^.ID > $3FFF) and (sourceTileInfo^.ID < $0F000000) then
      begin
        //node := Sender.AddChild(nil);
        //targetTileInfo := Sender.GetNodeData(node);
        //targetTileInfo^.ID := sourceTileInfo^.ID;
        Logger.Send([lcClient, lcDebug], 'TfrmFilter.vdtFilterDragDrop TileID', Format('0x%.8x', [sourceTileInfo^.ID]));
        AddTile(sourceTileInfo^.ID);

        cbTileFilter.Checked := True;
        frmMain.InvalidateFilter;
      end;
      selected := sourceTree.GetNextSelected(selected);
    end;
    Sender.EndUpdate;
  end;
end;

procedure TfrmFilter.vdtFilterDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; Pt: TPoint;
  Mode: TDropMode; var Effect: Integer; var Accept: Boolean);
begin
  if (Source <> Sender) and (Source is TVirtualDrawTree) then
  begin
    Accept := True;
  end;
end;

procedure TfrmFilter.vdtFilterDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
begin
  frmMain.vdtTilesDrawNode(Sender, PaintInfo);
end;

procedure TfrmFilter.vdtHuesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  hueInfo: PHueInfo;
begin
  hueInfo := Sender.GetNodeData(Node);
  FCheckedHues.Bits[hueInfo^.ID] := (Sender.CheckState[node] = csCheckedNormal);
  cbHueFilter.Checked := True;
  frmMain.InvalidateFilter;
end;

procedure TfrmFilter.vdtHuesDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var
  hueInfo: PHueInfo;
  hueColor: TColor;
  i: Integer;
  textStyle: TTextStyle;
begin
  hueInfo := Sender.GetNodeData(PaintInfo.Node);
  textStyle := PaintInfo.Canvas.TextStyle;
  textStyle.Alignment := taLeftJustify;
  textStyle.Layout := tlCenter;
  textStyle.Wordbreak := True;
  case PaintInfo.Column of
    1:
      begin
        for i := 0 to 31 do
        begin
          hueColor := ARGB2RGB(hueInfo^.Hue.ColorTable[i]);
          PaintInfo.Canvas.Pen.Color := hueColor;
          PaintInfo.Canvas.MoveTo(PaintInfo.CellRect.Left + 2 + i, PaintInfo.CellRect.Top + 1);
          PaintInfo.Canvas.LineTo(PaintInfo.CellRect.Left + 2 + i, PaintInfo.CellRect.Bottom - 1);
        end;
      end;
    2:
      begin
        PaintInfo.Canvas.TextRect(PaintInfo.CellRect, PaintInfo.CellRect.Left, PaintInfo.CellRect.Top, Format('$%x (%s)', [hueInfo^.ID, hueInfo^.Hue.Name]), textStyle);
      end;
  end;
end;

procedure TfrmFilter.MouseLeave(var msg: TLMessage);
begin
  {if Active and (not PtInRect(ClientRect, ScreenToClient(Mouse.CursorPos))) then
    Close;}
end;

function TfrmFilter.Filter(AStatic: TStaticItem): Boolean;
var
  found: Boolean;
  tileInfo: PTileInfo;
  node: PVirtualItem;
  id: LongWord;
begin
  if cbTileFilter.Checked then
  begin
    id := AStatic.TileID + $4000;

    found := False;
    node := vdtFilter.GetFirst;
    while (node <> nil) and (not found) do
    begin
      tileInfo := vdtFilter.GetNodeData(node);
      if tileInfo^.ID = id then
        found := True
      else
        node := vdtFilter.GetNext(node);
    end;

    Result := ((rgFilterType.ItemIndex = 0) and (not found)) or
              ((rgFilterType.ItemIndex = 1) and found);
  end else
    Result := True;
    
  if cbHueFilter.Checked then
  begin
    Result := Result and (
                ((rgFilterType.ItemIndex = 0) and (not FCheckedHues.Bits[AStatic.Hue])) or
                ((rgFilterType.ItemIndex = 1) and (FCheckedHues.Bits[AStatic.Hue]))
              );
  end;
end;

procedure TfrmFilter.JumpToHue(AHueID: Word);
var
  hueInfo: PHueInfo;
  node: PVirtualNode;
begin
  node := vdtHues.GetFirst;
  while node <> nil do
  begin
    hueInfo := vdtHues.GetNodeData(node);
    if hueInfo^.ID = AHueID then
    begin
      vdtHues.ClearSelection;
      vdtHues.Selected[node] := True;
      vdtHues.FocusedNode := node;
      node := nil;
    end else
      node := vdtHues.GetNext(node);
  end;
end;

procedure TfrmFilter.AddTile(ATileID: LongWord);
var
  selected, node: PVirtualItem;
  sourceTileInfo, targetTileInfo: PTileInfo;
  exists: Boolean;
begin
    if (ATileID > $3FFF) and (ATileID < $0F000000) then
    begin
      exists := False;
      vdtFilter.BeginUpdate;

      selected := vdtFilter.GetFirst();
      while selected <> nil do
      begin
        sourceTileInfo := vdtFilter.GetNodeData(selected);
        if sourceTileInfo^.ID = ATileID then
        begin
          exists := True;
          break;
        end;
        selected := vdtFilter.GetNext(selected);
      end;
      if not exists then
      begin
        node := vdtFilter.AddItem(nil);
        targetTileInfo := vdtFilter.GetNodeData(node);
        targetTileInfo^.ID := ATileID;
      end;

      vdtFilter.EndUpdate;
    end;
end;

procedure TfrmFilter.AddHue(AHueID: Word);
var
  hueInfo: PHueInfo;
  node: PVirtualNode;
begin
  node := vdtHues.GetFirst;
  while node <> nil do
  begin
    hueInfo := vdtHues.GetNodeData(node);
    if hueInfo^.ID = AHueID then
    begin
      //FCheckedHues.Bits[AHueID] := True;
      vdtHues.CheckState[node] := csCheckedNormal;
      vdtHues.ClearSelection;
      vdtHues.Selected[node] := True;
      vdtHues.FocusedNode := node;
      node := nil;
    end else
      node := vdtHues.GetNext(node);
  end;
end;

procedure TfrmFilter.FormCreate(Sender: TObject);
var
  i: Integer;
  hueInfo: PHueInfo;
  node: PVirtualNode;
begin
  vdtFilter := TVirtualList.Create(vdtFilter);
  LanguageTranslate(Self);

  FLocked := False;
  vdtFilter.NodeDataSize := SizeOf(TTileInfo);
  vdtHues.NodeDataSize := SizeOf(THueInfo);
  
  vdtHues.BeginUpdate;
  vdtHues.Clear;
  for i := 0 to ResMan.Hue.Count - 1 do
  begin
    node := vdtHues.AddChild(nil);
    hueInfo := vdtHues.GetNodeData(node);
    hueInfo^.ID := i + 1;
    hueInfo^.Hue := ResMan.Hue.Hues[i];
    vdtHues.CheckType[node] := ctCheckBox;
  end;
  vdtHues.EndUpdate;
  FCheckedHues := TBits.Create(ResMan.Hue.Count + 1);
  //FCheckedHues.Bits[0] := True;
end;

procedure TfrmFilter.FormDestroy(Sender: TObject);
begin
  if FCheckedHues <> nil then FreeAndNil(FCheckedHues);
end;

procedure TfrmFilter.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_SPACE) and Visible then begin
    frmFilter.Locked := True;
    frmFilter.Hide;
    frmFilter.Locked := False;
    // Говно код для задержки чтобы дать время обработать события что возвращают фокус
    tFormClose.Interval := 10;
    tFormClose.Tag      := PtrInt(False);
    tFormClose.Enabled  := True;
  end;
end;

procedure TfrmFilter.tFormCloseTimer(Sender: TObject);
begin
  if (Boolean(tFormClose.Tag)) then begin
    frmFilter.Locked := True;
    frmFilter.Hide;
    frmFilter.Locked := False;
  end;
  frmMain.SetFocus;
  tFormClose.Enabled := False;
end;

procedure TfrmFilter.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  If Button = mbRight then
    FLastRMouseDown := GetTickCount;
end;

procedure TfrmFilter.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Visible and not frmMain.mnuAutoShowFilterWindow.Checked and (GetTickCount - FLastRMouseDown < 1000)) then
  begin
    frmFilter.Locked := True;
    frmFilter.Hide;
    frmFilter.Locked := False;
    frmMain.SetFocus;
  end;
end;

procedure TfrmFilter.btnDeleteClick(Sender: TObject);
begin
  vdtFilter.BeginUpdate;
  vdtFilter.DeleteSelectedNodes;
  vdtFilter.EndUpdate;
  frmMain.InvalidateFilter;
end;

procedure TfrmFilter.cbHueFilterChange(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmFilter.cbTileFilterChange(Sender: TObject);
begin
  frmMain.InvalidateFilter;
end;

procedure TfrmFilter.btnClearClick(Sender: TObject);
begin
  vdtFilter.Clear;
  frmMain.InvalidateFilter;
end;

initialization
  {$I UfrmFilter.lrs}

end.

