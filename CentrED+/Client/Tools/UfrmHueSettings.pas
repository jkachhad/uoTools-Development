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
unit UfrmHueSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, UfrmToolWindow, XMLRead, XMLWrite, DOM, VirtualTrees,
  VirtualList, Math, UHue, UArt, UTiledata, UfrmMain, ImagingComponents,
  LConvEncoding, ComCtrls, Logging, LCLIntf, LCLType, ULandscape;

type

  { TfrmHueSettings }

  TfrmHueSettings = class(TfrmToolWindow)
    btnAddRandom: TSpeedButton;
    btnClearRandom: TSpeedButton;
    btnDeleteRandom: TSpeedButton;
    btnRandomPresetDelete: TSpeedButton;
    btnRandomPresetSave: TSpeedButton;
    gbLastUsed: TGroupBox;
    lbLastUsed: TListBox;
    ShowAll: TButton;
    cbRandom: TCheckBox;
    cbRandomPreset: TComboBox;
    edHue: TEdit;
    edTileId: TEdit;
    gbRandom: TGroupBox;
    gbPreview: TGroupBox;
    ImagePreviewItem: TImage;
    lblHue: TLabel;
    lbHue: TListBox;
    lblTileId: TLabel;
    lbRandom: TListBox;
    vdtHuePreview: TVirtualDrawTree;
    procedure btnAddRandomClick(Sender: TObject);
    procedure btnClearRandomClick(Sender: TObject);
    procedure btnDeleteRandomClick(Sender: TObject);
    procedure btnRandomPresetDeleteClick(Sender: TObject);
    procedure btnRandomPresetSaveClick(Sender: TObject);
    procedure cbRandomChange(Sender: TObject);
    procedure cbRandomPresetChange(Sender: TObject);
    procedure edHueEditingDone(Sender: TObject);
    procedure edTileIdEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbHueDblClick(Sender: TObject);
    procedure lbHueDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
      State: TOwnerDrawState);
    procedure lbHueSelectionChange(Sender: TObject; User: boolean);
    procedure lbLastUsedDblClick(Sender: TObject);
    procedure lbRandomDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbRandomDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvHueCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ShowAllClick(Sender: TObject);
    procedure UpdateItemPreview(ID : Integer; Hue: Integer);
    procedure vdtHuePreviewClick(Sender: TObject);
    procedure vdtHuePreviewDrawNode(Sender: TBaseVirtualTree;
      const PaintInfo: TVTPaintInfo);
    procedure vdtHuePreviewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    FConfigDir: String;
    FRandomHuePresetsFile: String;
    FRandomHuePresetsDoc: TXMLDocument;
    LastSelectedNode: PVirtualNode;
    LastSelectedColm: Integer;
    LastFormPosition: TPoint;
    function FindRandomPreset(AName: String): TDOMElement;
    procedure LoadRandomPresets;
    procedure SaveRandomPresets;
    procedure HuePreviewRebuild(ItemID, ItemWidth, ItemHeight: Integer);
    procedure HuePreviewFree();
  public
    function GetHue: Word;
    procedure AddLastUsed(hue: Word);
  public
    class procedure DrawHue(AHue: THue; ACanvas: TCanvas; ARect: TRect;
      ACaption: string; lineWidth: Integer = 2);
  public
    lbDlgBadColrCaption: string;
    lbDlgBadColr: string;
    lbDlgBadTileCaption: string;
    lbDlgBadTile: string;
    lbDlgSavePrsCaption: string;
    lbDlgSavePrs: string;
    lbNoHuesName: string;
  end; 

const
  vdtHuesPreviewColumnsMax = 1920 div 44;
  vdtHuesLastUsedLength = 14;

var
  frmHueSettings: TfrmHueSettings;

implementation

uses
  UGameResources, UGraphicHelper, UfrmLogin, Language;

type
  PHuesInfo = ^THuesInfo;
  THuesInfo = record
    ID : array[0..vdtHuesPreviewColumnsMax] of Integer;
    Art: array[0..vdtHuesPreviewColumnsMax] of TArt;
  end;

  PTileInfo = ^TTileInfo;
  TTileInfo = record
    ID: LongWord;
    ptr: Pointer;
  end;

{ TfrmHueSettings }

procedure TfrmHueSettings.edHueEditingDone(Sender: TObject);
var
  hueID: Integer;
begin
  if (not TryStrToInt(edHue.Text, hueID)) or (hueID >= lbHue.Items.Count) then
  begin
    edHue.Text := Format('$%x', [lbHue.ItemIndex]);
    MessageDlg(lbDlgBadColrCaption, lbDlgBadColr, mtWarning, [mbOK], 0);
  end else
    lbHue.ItemIndex := hueID;
end;

procedure TfrmHueSettings.edTileIdEditingDone(Sender: TObject);
var
  tileID: Integer;
begin
  tileID := 0;
  if (not TryStrToInt(edTileId.Text, tileID)) or (tileID >= ResMan.Landscape.MaxStaticID) or (tileID <= 0) then
  begin
    edTileId.Text := Format('$%x', [edTileId.Tag - $4000]);
    MessageDlg(lbDlgBadTileCaption, lbDlgBadTile, mtWarning, [mbOK], 0);
  end else
  begin
    edTileId.Tag := $4000 + tileID;
    UpdateItemPreview(edTileId.Tag, lbHue.ItemIndex);
  end;
end;

procedure TfrmHueSettings.AddLastUsed(hue: Word);
var
  index: Integer;
begin
  lbLastUsed.Items.BeginUpdate;
  for index := 0 to lbLastUsed.Items.Count - 1 do
    if PtrInt(lbLastUsed.Items.Objects[index]) = hue then begin
      lbLastUsed.Items.Delete(index);
      break;
    end;
  lbLastUsed.Items.InsertObject(0, lbHue.Items.Strings[hue], TObject(PtrInt(hue)));
  while lbLastUsed.Items.Count > vdtHuesLastUsedLength do begin
    lbLastUsed.Items.Delete(lbLastUsed.Items.Count - 1);
  end;
  lbLastUsed.Items.EndUpdate;
end;

procedure TfrmHueSettings.lbLastUsedDblClick(Sender: TObject);
var
  hue: Integer;
begin
  if (lbLastUsed.ItemIndex < 0) or (lbLastUsed.ItemIndex >= lbLastUsed.Items.Count)
      then exit;
  hue := PtrInt(lbLastUsed.Items.Objects[lbLastUsed.ItemIndex]);
  edHue.Text := Format('$%x', [hue]);
  edHueEditingDone(Sender);
end;

procedure TfrmHueSettings.btnDeleteRandomClick(Sender: TObject);
var
  i: Integer;
begin
  lbRandom.Items.BeginUpdate;
  for i := lbRandom.Items.Count - 1 downto 0 do
    if lbRandom.Selected[i] then
      lbRandom.Items.Delete(i);
  lbRandom.Items.EndUpdate;
end;

procedure TfrmHueSettings.btnRandomPresetDeleteClick(Sender: TObject);
var
  preset: TDOMElement;
begin
  if cbRandomPreset.ItemIndex > -1 then
  begin
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    FRandomHuePresetsDoc.DocumentElement.RemoveChild(preset);
    cbRandomPreset.Items.Delete(cbRandomPreset.ItemIndex);
    cbRandomPreset.ItemIndex := -1;
  end;
end;

procedure TfrmHueSettings.btnRandomPresetSaveClick(Sender: TObject);
var
  presetName: string;
  i: Integer;
  preset, hue: TDOMElement;
  children: TDOMNodeList;
begin
  presetName := cbRandomPreset.Text;
  if InputQuery(lbDlgSavePrsCaption, lbDlgSavePrs, presetName) then
  begin
    preset := FindRandomPreset(presetName);
    if preset = nil then
    begin
      preset := FRandomHuePresetsDoc.CreateElement('Preset');
      preset.AttribStrings['Name'] := UTF8ToCP1251(presetName);
      FRandomHuePresetsDoc.DocumentElement.AppendChild(preset);
      cbRandomPreset.Items.AddObject(presetName, preset);
    end else
    begin
      children := preset.GetChildNodes;
      for i := children.Count - 1 downto 0 do
        preset.RemoveChild(children[i]);
    end;

    for i := 0 to lbRandom.Items.Count - 1 do
    begin
      hue := FRandomHuePresetsDoc.CreateElement('Hue');
      hue.AttribStrings['ID'] := IntToStr(PtrInt(lbRandom.Items.Objects[i]));
      preset.AppendChild(hue);
    end;

    cbRandomPreset.ItemIndex := cbRandomPreset.Items.IndexOfObject(preset);

    SaveRandomPresets;
  end;
end;

procedure TfrmHueSettings.cbRandomChange(Sender: TObject);
begin
  lbHue.MultiSelect := cbRandom.Checked;
  gbRandom.Visible := cbRandom.Checked;
  gbLastUsed.Visible := not cbRandom.Checked;
end;

procedure TfrmHueSettings.cbRandomPresetChange(Sender: TObject);
var
  preset, hue: TDOMElement;
  id: PtrInt;
begin
  lbRandom.Clear;
  if cbRandomPreset.ItemIndex > -1 then
  begin
    preset := TDOMElement(cbRandomPreset.Items.Objects[cbRandomPreset.ItemIndex]);
    hue := TDOMElement(preset.FirstChild);

    while hue <> nil do
    begin
      if hue.NodeName = 'Hue' then
      begin
        id := StrToInt(hue.AttribStrings['ID']);
        lbRandom.Items.AddObject(lbHue.Items.Strings[id], TObject(id));
      end;
      hue := TDOMElement(hue.NextSibling);
    end;
  end;
end;

procedure TfrmHueSettings.btnClearRandomClick(Sender: TObject);
begin
  lbRandom.Items.Clear;
end;

procedure TfrmHueSettings.btnAddRandomClick(Sender: TObject);
var
  i: PtrInt;
begin
  lbRandom.Items.BeginUpdate;
  for i := 0 to lbHue.Count - 1 do
    if lbHue.Selected[i] then
      lbRandom.Items.AddObject(lbHue.Items.Strings[i], TObject(i));
  lbRandom.Items.EndUpdate;
end;

procedure TfrmHueSettings.FormCreate(Sender: TObject);
var
  i: Integer;
  hue: THue;
begin
  LanguageTranslate(Self);

  lbHue.Clear;
  lbHue.Items.Add(lbNoHuesName);
  for i := 1 to ResMan.Hue.Count do
  begin
    hue := ResMan.Hue.Hues[i-1];
    lbHue.Items.AddObject(Format('$%x (%s)', [i, hue.Name]), hue);
  end;
  lbHue.ItemIndex := 0;

  if (sprofile <> '') then
    if (frmMain.ProfileDir <> '')
      then FConfigDir := frmMain.ProfileDir
      else FConfigDir := frmMain.ConfigDir;
  FRandomHuePresetsFile := FConfigDir + 'RandomHuePresets.xml';
  ForceDirectories(FConfigDir);

  Logger.Send([lcClient, lcInfo], '  FConfigDir', FRandomHuePresetsFile);
  LoadRandomPresets;

  vdtHuePreview.Align := alClient;
  vdtHuePreview.NodeDataSize := SizeOf(THuesInfo);

  gbLastUsed.Height := gbRandom.Height;
  cbRandomChange(Sender);
  //ImagePreviewItem.Height := gbPreview.Height - ImagePreviewItem.Top - 4;
end;

procedure TfrmHueSettings.FormShow(Sender: TObject);
var
  item: PVirtualItem;
  tileInfo: PTileInfo;
  selectedID: LongWord;
begin
  item := frmMain.vdtTiles.GetFirstSelected;
  if item <> nil then
  begin
    tileInfo := frmMain.vdtTiles.GetNodeData(item);
    selectedID := tileInfo^.ID;
  end;
  if (selectedID < $4000) or (selectedID >= ResMan.Tiledata.StaticCount + $4000)
    then selectedID := $4001;

  edTileId.Tag := selectedID;
  edTileId.Text := Format('$%x', [selectedID - $4000]);
  UpdateItemPreview(edTileId.Tag, lbHue.ItemIndex);

  vdtHuePreview.Visible := False;
  (frmHueSettings as TfrmToolWindow).FormShow(Sender);
  gbPreview.Visible := True;
  frmHueSettings.Width := 516;
  frmHueSettings.Height:= 534;
  //lbHue.Visible:= false;
end;


procedure TfrmHueSettings.FormHide(Sender: TObject);
begin
  HuePreviewFree();
end;

procedure TfrmHueSettings.FormClose(Sender: TObject);
begin
  HuePreviewFree();
end;

procedure TfrmHueSettings.UpdateItemPreview(ID : Integer; Hue: Integer);
var
  tileData: TTileData;
  artHue: THue;
  artEntry: TArt;
  artPartial: Boolean;
  destRect: TRect;
  destColor: Word;
begin
  if ResMan.Art.Exists(ID) then
  begin
    if Hue > 0 then
      artHue := ResMan.Hue.Hues[Hue - 1]
    else
      artHue := nil;
    tileData := TTileData(ResMan.Tiledata.Block[ID]);
    artPartial := tdfPartialHue in tileData.Flags;

    //destColor := ARGB2RGB(ImagePreviewItem.Canvas.Pixels[destRect.Left, destRect.Top]);
    destColor := EncodeUOColor($00F0F0F0);
    artEntry := ResMan.Art.GetArt(ID, destColor, artHue, artPartial);

    destRect.Bottom := Min(artEntry.Graphic.Height, ImagePreviewItem.Canvas.Height);
    destRect.Right  := Min(artEntry.Graphic.Width,  ImagePreviewItem.Canvas.Width);
    destRect.Top    := ((ImagePreviewItem.Canvas.Height - destRect.Bottom) div 2);
    destRect.Left   := ((ImagePreviewItem.Canvas.Width  - destRect.Right)  div 2);

    destRect.Bottom := ((ImagePreviewItem.Canvas.Height + destRect.Bottom) div 2);
    destRect.Right  := ((ImagePreviewItem.Canvas.Width  + destRect.Right)  div 2);

    ImagePreviewItem.Canvas.Brush.Color := $00F5F5F5;//DecodeUOColor(destColor);
    ImagePreviewItem.Canvas.Clear;
    DisplayImage(ImagePreviewItem.Canvas, destRect, artEntry.Graphic);
    ImagePreviewItem.Update;

    artEntry.Free;
  end;
end;


procedure TfrmHueSettings.lbHueDrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  hue: THue;
begin
  if ((Control = lbRandom) or (Control = lbLastUsed)) then begin
    Index := PtrInt((Control as TListBox).Items.Objects[Index]);
    if Index > 0
      then hue := ResMan.Hue.Hues[Index-1]
      else hue := nil;
    DrawHue(hue, (Control as TListBox).Canvas, ARect, Format('$%x', [Index]), 3);
  end else if Control = lbLastUsed then begin
    Index := PtrInt(lbLastUsed.Items.Objects[Index]);
    if Index > 0
      then hue := ResMan.Hue.Hues[Index-1]
      else hue := nil;
    DrawHue(hue, lbLastUsed.Canvas, ARect, Format('$%x', [Index]), 3);
  end else begin // Control = lbHue
    if Index > 0
      then hue := ResMan.Hue.Hues[Index-1]
      else hue := nil;
    DrawHue(hue, lbHue.Canvas, ARect, lbHue.Items.Strings[Index], 4);
  end
end;

procedure TfrmHueSettings.lbHueDblClick(Sender: TObject);
begin
  lbHueSelectionChange(Sender, True);
end;

procedure TfrmHueSettings.lbHueSelectionChange(Sender: TObject; User: boolean);
begin
  edHue.Text := Format('$%x', [lbHue.ItemIndex]);

  //if (ImagePreviewItem <> nil) and (ImagePreviewItem.Canvas <> nil) then
  UpdateItemPreview(edTileId.Tag, lbHue.ItemIndex);
  AddLastUsed(lbHue.ItemIndex);
end;

procedure TfrmHueSettings.lbRandomDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Source = lbHue then
    btnAddRandomClick(Sender);
end;

procedure TfrmHueSettings.lbRandomDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  if Source = lbHue then Accept := True;
end;

procedure TfrmHueSettings.lvHueCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
begin

end;

function TfrmHueSettings.FindRandomPreset(AName: String): TDOMElement;
begin
  Result := TDOMElement(FRandomHuePresetsDoc.DocumentElement.FirstChild);
  while Result <> nil do
  begin
    if SameText(Result.AttribStrings['Name'], AName) then
      Break;

    Result := TDOMElement(Result.NextSibling);
  end;
end;

procedure TfrmHueSettings.LoadRandomPresets;
var
  presetElement, hueElement: TDOMElement;
begin
  FreeAndNil(FRandomHuePresetsDoc);
  cbRandomPreset.Items.Clear;
  if FileExists(FRandomHuePresetsFile) then
  begin
    ReadXMLFile(FRandomHuePresetsDoc, FRandomHuePresetsFile);
    presetElement := TDOMElement(FRandomHuePresetsDoc.DocumentElement.FirstChild);
    while presetElement <> nil do
    begin
      if presetElement.NodeName = 'Preset' then
        cbRandomPreset.Items.AddObject(
              CP1251ToUTF8(presetElement.AttribStrings['Name']), presetElement);
      presetElement := TDOMElement(presetElement.NextSibling);
    end;
  end else
  begin
    FRandomHuePresetsDoc := TXMLDocument.Create;
    FRandomHuePresetsDoc.AppendChild(FRandomHuePresetsDoc.CreateElement('RandomHuePresets'));
  end;
end;

procedure TfrmHueSettings.SaveRandomPresets;
begin
  WriteXMLFile(FRandomHuePresetsDoc, FRandomHuePresetsFile);
end;

function TfrmHueSettings.GetHue: Word;
begin
  if cbRandom.Checked and (lbRandom.Items.Count > 0) then
    Result := PtrInt(lbRandom.Items.Objects[Random(lbRandom.Items.Count)])
  else
    Result := lbHue.ItemIndex;
end;

class procedure TfrmHueSettings.DrawHue(AHue: THue; ACanvas: TCanvas; ARect: TRect;
  ACaption: string; lineWidth: Integer = 2);
var
  hueColor: TColor;
  i, w, leftCPos: Integer;
begin
  ACanvas.Pen.Color := clWhite;
  ACanvas.Rectangle(ARect);
  if AHue <> nil then
    for i := 0 to 31 do
    begin
      hueColor := ARGB2RGB(AHue.ColorTable[i]);
      ACanvas.Pen.Color := hueColor;
      leftCPos := ARect.Left + 2 + lineWidth*i;
      for w := 0 to lineWidth do begin
        ACanvas.MoveTo(leftCPos + w, ARect.Top + 1);
        ACanvas.LineTo(leftCPos + w, ARect.Bottom - 1);
      end;
    end;
  ACanvas.TextOut(ARect.Left + lineWidth*36, ARect.Top+1, ACaption);
end;

// vdtHuePreview

procedure TfrmHueSettings.ShowAllClick(Sender: TObject);
var
  artEntry: TArt;
  point: TPoint;
begin
  if ResMan.Art.Exists(edTileId.Tag) then begin
    LastFormPosition.y := Top;
    LastFormPosition.x := Left;

    artEntry := ResMan.Art.GetArt(edTileId.Tag, 0, nil, false);
    HuePreviewRebuild(edTileId.Tag, artEntry.Graphic.Width, artEntry.Graphic.Height);

    cbRandom.Checked := False;
    vdtHuePreview.Visible := True;
    vdtHuePreview.SetFocus;
  end;
end;

procedure TfrmHueSettings.HuePreviewRebuild(ItemID, ItemWidth, ItemHeight: Integer);
var
  item: PVirtualNode;
  info: PHuesInfo;
  cols: Integer;
  h,cn: Integer;
  ScrollBarWidth: Integer;
  wspos: TPoint;
  wrect: TRect;
begin
  HuePreviewFree;

  GetWindowRect(frmHueSettings.Handle, wrect);
  wspos.X := 0; wspos.Y := 0;
  wspos := frmMain.oglGameWindow.ClientToScreen(wspos);
  Left  := wspos.X - 20;
  Top   := wspos.Y - 1;
  Width := frmMain.oglGameWindow.ClientWidth  - (wrect.Right  - wrect.Left - ClientWidth);
  Height:= frmMain.oglGameWindow.ClientHeight - (wrect.Bottom - wrect.Top  - ClientHeight);


  if vdtHuePreview.Tag = edTileId.Tag then exit;

  // We determine the number of columns
  cols:= ((ClientWidth - GetSystemMetrics(SM_CXVSCROLL))// - 16)
          div (ItemWidth + 2)) - 1;
  if (cols > vdtHuesPreviewColumnsMax)
  then cols:=vdtHuesPreviewColumnsMax;
  Logger.Send([lcClient, lcInfo], 'TfrmHueSettings.HuePreviewRebuild(): Number of columns', cols+1);

  // Create Column
  vdtHuePreview.BeginUpdate;
  vdtHuePreview.Header.Columns.Clear;
  vdtHuePreview.Header.Columns.DefaultWidth := ItemWidth + 2;
  for cn := 0 to cols do
    vdtHuePreview.Header.Columns.Add;

  // Create a cell
  vdtHuePreview.Clear;

  vdtHuePreview.Tag := ItemID;
  vdtHuePreview.DefaultNodeHeight := ItemHeight + 2;
  h := 0;
  while h < ResMan.Hue.Count do begin
    item := vdtHuePreview.AddChild(nil);
    info := vdtHuePreview.GetNodeData(item);
    for cn := cols to vdtHuesPreviewColumnsMax do
      info^.Art[cn] := nil;
    for cn := 0 to cols do begin
      info^.Art[cn] := nil;
      info^.ID[cn]  := h;
      h := h + 1;
      if h >= ResMan.Hue.Count
      then break;
    end;
  end;
  vdtHuePreview.EndUpdate;
end;

procedure TfrmHueSettings.HuePreviewFree();
var
  colm: Integer;
  node: PVirtualNode;
  info: PHuesInfo;
begin
  vdtHuePreview.Tag := -1;
  node := vdtHuePreview.GetFirst();
  while node <> nil do begin
    info := vdtHuePreview.GetNodeData(node);
    for colm := 0 to vdtHuesPreviewColumnsMax do begin
      if info^.Art[colm] <> nil
      then info^.Art[colm].Free;
      info^.Art[colm] := nil;
    end;
    node := vdtHuePreview.GetNext(node);
  end;
end;

procedure TfrmHueSettings.vdtHuePreviewDrawNode(Sender: TBaseVirtualTree;
  const PaintInfo: TVTPaintInfo);
var
  //tileInfo : PTileInfo;
  destColor : Word;
  tileData  : TTileData;
  artPartial: Boolean;
  artEntry  : TArt;

  info: PHuesInfo;
  id  : Integer;
  hue : THue;
  mpos: TPoint;
  rect: TRect;
begin
  id  := TVirtualDrawTree(Sender).Tag;
  info:= Sender.GetNodeData(PaintInfo.Node);
  if info^.ID[PaintInfo.Column] <= 0 then begin
    hue := nil;
    if (PaintInfo.Node <> Sender.GetFirst()) then begin
      PaintInfo.Canvas.Brush.Color := vdtHuePreview.Color;
      exit; // Do not draw a cell without color if they are not in the first row
    end;
  end else hue := ResMan.Hue.Hues[info^.ID[PaintInfo.Column] - 1];

  if ResMan.Art.Exists(id) then begin
    // Determine the illumination color (ie, recovered the tile)
    {mpos := vdtHuePreview.ScreenToClient(Mouse.CursorPos);
    if  (vdtHuePreview.GetNodeAt(mpos.x, mpos.y) = PaintInfo.Node)
    and ((mpos.x div vdtHuePreview.Header.Columns.DefaultWidth) = PaintInfo.Column)
    then destColor := EncodeUOColor(vdtHuePreview.Colors.FocusedSelectionColor)
    else }destColor := EncodeUOColor(vdtHuePreview.Color);

    if info^.Art[PaintInfo.Column] = nil then begin
      tileData   := TTileData(ResMan.Tiledata.Block[ID]);
      artPartial := tdfPartialHue in tileData.Flags;
      artEntry   := ResMan.Art.GetArt(ID, destColor, hue, artPartial);
      info^.Art[PaintInfo.Column] := artEntry;
    end else artEntry := info^.Art[PaintInfo.Column];

    rect.Left  := PaintInfo.CellRect.Left   + 1;
    rect.Top   := PaintInfo.CellRect.Top    + 1;
    rect.Right := PaintInfo.CellRect.Right  - 1;
    rect.Bottom:= PaintInfo.CellRect.Bottom - 1;

    DisplayImage(PaintInfo.Canvas, rect, artEntry.Graphic);
    //artEntry.Free;
  end;

end;

procedure TfrmHueSettings.vdtHuePreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  mpos: TPoint;
  colm: Integer;
  node: PVirtualNode;
begin
  {mpos := vdtHuePreview.ScreenToClient(Mouse.CursorPos);
  colm := mpos.x div vdtHuePreview.Header.Columns.DefaultWidth;
  node := vdtHuePreview.GetNodeAt(mpos.x, mpos.y);
  if (colm <> LastSelectedColm) and (node <> LastSelectedNode) then begin
    vdtHuePreview.RepaintNode(node);
    LastSelectedColm := colm;
    LastSelectedNode := node;
    vdtHuePreview.RepaintNode(node);
  end;}
end;

procedure TfrmHueSettings.vdtHuePreviewClick(Sender: TObject);
var
  mpos: TPoint;
  info: PHuesInfo;
  hues: Integer;
begin
  mpos := vdtHuePreview.ScreenToClient(Mouse.CursorPos);
  info := vdtHuePreview.GetNodeData( vdtHuePreview.GetNodeAt(mpos.x, mpos.y) );
  hues := info^.ID[ mpos.x div vdtHuePreview.Header.Columns.DefaultWidth ];
  if hues < ResMan.Hue.Count then begin
    Logger.Send([lcClient, lcInfo], 'mpos.x="%d"; mpos.y="%d"; hues="%d"', [mpos.x, mpos.y, hues]);

    edHue.Text := Format('$%x', [hues]);
    edHueEditingDone(Sender);

    FormShow(Sender); // Close the window and return back
    Top  := LastFormPosition.y;
    Left := LastFormPosition.x;
    mpos.x := ImagePreviewItem.Width  div 2;
    mpos.y := ImagePreviewItem.Height div 2;
    Mouse.CursorPos := ImagePreviewItem.ClientToScreen(mpos);
  end;
end;


initialization
  {$I UfrmHueSettings.lrs}

end.

