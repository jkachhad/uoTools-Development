unit VirtualList;

{$mode delphi}{$H+}
interface

uses
  Forms, Controls, StdCtrls, Graphics, Classes, SysUtils, VirtualTrees,
  Logging, LMessages, LCLIntf, Math;

type
  {$Z4} INPUTTYPE = (INPUT_MOUSE = $00, INPUT_KEYBOARD = $01, INPUT_HARDWARE = $02);
  {$Z4} KEYEVENTF = (KEYEVENTF_EXTENDEDKEY = $01, KEYEVENTF_KEYUP = $02, KEYEVENTF_SCANCODE = $04, KEYEVENTF_UNICODE = $08);
  {TKEYINPUT = record
    itype: INPUTTYPE;
    // tagKEYBDINPUT
    wVk: WORD;
    wScan: WORD;
    dwFlags: KEYEVENTF;
    time: DWORD;
    dwExtraInfo: ULONG_PTR;
  end;} //TODO

  PVirtualItem = ^TVirtualItem;
  TVirtualItem = record
    NextItem: PVirtualItem;
    Node: PVirtualNode;
    Column: Word;
    Selected: Boolean;
  end;

  { TVirtualList }

  TVirtualList = class(TVirtualDrawTree)

  private
    HintCanvas: TCanvas;
    TileColumn: Word;
    FirstItem: PVirtualItem;
    LastItem: PVirtualItem;
    LastSelected: PVirtualItem;
    ClearAll: Boolean;
    FSelectionCount: DWord;
    FTilesCount: DWord;

    function GetSelected(Item: PVirtualItem): Boolean;
    procedure SetSelected(Item: PVirtualItem; Value: Boolean);
    function GetFocusedNode(): PVirtualItem;
    procedure SetFocusedNode(Item: PVirtualItem);

  public
    constructor Create(AOwner: TComponent); override;
    procedure UpdateHintCanvas(newCanvas: TCanvas);
    procedure DoGetHintSize(Node: PVirtualNode; Column: TColumnIndex; var R: TRect); override;

    procedure UpdateTileColumn(count: Word; Forse: Boolean = False);
    //function AddChild(Parent: PVirtualNode; UserData: Pointer = nil): PVirtualNode; override;
    function AddChild(ParentItem: PVirtualNode; UserData: Pointer = nil): PVirtualNode; override;
    function AddItem(ParentItem: PVirtualItem; UserData: Pointer = nil): PVirtualItem;
    function GetFirst(ConsiderChildrenAbove: Boolean = False): PVirtualItem;
    function GetNext(Item: PVirtualItem; ConsiderChildrenAbove: Boolean = False): PVirtualItem;
    function GetLast(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualItem;
    function GetItemAt(Node: PVirtualNode; Column: TColumnIndex): PVirtualItem;
    function GetNodeData(Item: PVirtualItem): Pointer;
    procedure Clear; override;

    function GetFirstSelected(ConsiderChildrenAbove: Boolean = False): PVirtualItem;
    function GetNextSelected(Item: PVirtualItem; ConsiderChildrenAbove: Boolean = False): PVirtualItem;
    property Selected[Item: PVirtualItem]: Boolean read GetSelected write SetSelected;
    procedure ClearSelection;
    procedure DeleteSelectedNodes; override;

    property FocusedNode: PVirtualItem read GetFocusedNode write SetFocusedNode;

    procedure DoPaintNode(var PaintInfo: TVTPaintInfo); override;
    procedure PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: Integer); override;
    procedure DoPaintDropMark(Canvas: TCanvas; Node: PVirtualNode; const R: TRect); override;

    procedure HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo); override;
    procedure HandleMouseDown(var Message: TLMMouse; var HitInfo: THitInfo); override;
    procedure HandleMouseUp(Keys: PtrUInt; const HitInfo: THitInfo); override;

    property SelectedCount: Dword read FSelectionCount;
    property TilesCount: Dword read FTilesCount;
  end;

//function SendInput(nInputs:UINT; pInputs:POINTER; cbSize:INTEGER):UINT; stdcall; external 'User32.dll' name 'SendInput';

Implementation

//----------------------------------------------------------------------------------------------------------------------

constructor TVirtualList.Create(AOwner: TComponent);
var
  Pvdt: TVirtualDrawTree;
  column: TVirtualTreeColumn;
  c: Integer;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.Create  START');
  if not (AOwner is TVirtualDrawTree) then begin
     Logger.Send([lcClient, lcDebug], 'TVirtualTree.Create(AOwner: TVirtualDrawTree) must get argument TVirtualDrawTree');
     Assert(not (AOwner is TVirtualDrawTree), 'TVirtualTree.Create(AOwner: TVirtualDrawTree) must get argument TVirtualDrawTree');
     Abort;
     Halt;
  end;
  inherited Create(AOwner.Owner);
  Pvdt := TVirtualDrawTree(AOwner);
  Self.Parent := Pvdt.Parent;

  FSelectionCount := 0;
  FTilesCount:= 0;
  TileColumn := 1;
  ClearAll := True;

  // Копирование свойств
  Self.AnchorSideTop.Control := Pvdt.AnchorSideTop.Control;
  Self.AnchorSideTop.Side := Pvdt.AnchorSideTop.Side;
  Self.AnchorSideLeft.Control := Pvdt.AnchorSideLeft.Control;
  Self.AnchorSideLeft.Side := Pvdt.AnchorSideLeft.Side;
  Self.AnchorSideRight.Control := Pvdt.AnchorSideRight.Control;
  Self.AnchorSideRight.Side := Pvdt.AnchorSideRight.Side;
  Self.AnchorSideBottom.Control := Pvdt.AnchorSideBottom.Control;
  Self.AnchorSideBottom.Side := Pvdt.AnchorSideBottom.Side;

  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.Create  0');

  Self.Left := Pvdt.Left;
  Self.Height := Pvdt.Height;
  Self.Hint := Pvdt.Hint;
  Self.Top := Pvdt.Top;
  Self.Width := Pvdt.Width;
  Self.Anchors := Pvdt.Anchors;
  Self.BorderSpacing.Top := Pvdt.BorderSpacing.Top;
  Self.BorderSpacing.Left := Pvdt.BorderSpacing.Left;
  Self.BorderSpacing.Right := Pvdt.BorderSpacing.Right;
  Self.BorderSpacing.Bottom := Pvdt.BorderSpacing.Bottom;
  Self.BiDiMode := Pvdt.BiDiMode;
  Self.Tag := Pvdt.Tag;
  Self.Color := Pvdt.Color;
  Self.Colors.DropMarkColor := Pvdt.Colors.DropMarkColor;
  Self.Colors.DropTargetColor := Pvdt.Colors.DropTargetColor;
  Self.Colors.DropTargetBorderColor := Pvdt.Colors.DropTargetBorderColor;
  Self.Colors.BorderColor := Pvdt.Colors.BorderColor;
  Self.Colors.GridLineColor := Pvdt.Colors.GridLineColor;
  Self.Colors.TreeLineColor := Pvdt.Colors.TreeLineColor;
  Self.Colors.FocusedSelectionColor := Pvdt.Colors.FocusedSelectionColor;
  Self.Colors.FocusedSelectionBorderColor := Pvdt.Colors.FocusedSelectionBorderColor;
  Self.Colors.SelectionRectangleBlendColor := Pvdt.Colors.SelectionRectangleBlendColor;
  Self.Colors.UnfocusedSelectionColor := Pvdt.Colors.UnfocusedSelectionColor;
  Self.Colors.UnfocusedSelectionBorderColor := Pvdt.Colors.UnfocusedSelectionBorderColor;
  Self.Constraints.MinHeight := Pvdt.Constraints.MinHeight;
  Self.Constraints.MinWidth := Pvdt.Constraints.MinWidth;
  Self.Constraints.MaxHeight := Pvdt.Constraints.MaxHeight;
  Self.Constraints.MaxWidth := Pvdt.Constraints.MaxWidth;
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.Create  0___');
//  Self.DefaultNodeHeight := Pvdt.DefaultNodeHeight;
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.Create  0&&&');
  Self.DragKind := PVdt.DragKind;
  Self.DragMode := Pvdt.DragMode;
  Self.DragOperations := Pvdt.DragOperations;
  Self.DragType := Pvdt.DragType;
  Self.DrawSelectionMode := Pvdt.DrawSelectionMode;
  Self.Font.Height := Pvdt.Font.Height;
  Self.Font.Name := Pvdt.Font.Name;
  Self.Font.Color := Pvdt.Font.Color;
  Self.Font.Style := Pvdt.Font.Style;
  Self.Font.Underline := Pvdt.Font.Underline;
  Self.Font.Orientation := Pvdt.Font.Orientation;
  Self.Font.Size := Pvdt.Font.Size;
  Self.Font.Pitch := Pvdt.Font.Pitch;
  Self.Font.Quality := Pvdt.Font.Quality;

  Self.Header.AutoSizeIndex := Pvdt.Header.AutoSizeIndex;
  Self.Header.DefaultHeight := Pvdt.Header.DefaultHeight;
  Self.Header.MainColumn := Pvdt.Header.MainColumn;
  Self.Header.Options := Pvdt.Header.Options;
  Self.Header.ParentFont := Pvdt.Header.ParentFont;
  Self.Header.Style := Pvdt.Header.Style;
  Self.HintMode := Pvdt.HintMode;
  Self.ParentFont := Pvdt.ParentFont;
  Self.ParentShowHint := Pvdt.ParentShowHint;
  Self.PopupMenu := Pvdt.PopupMenu;
  Self.ScrollBarOptions.AlwaysVisible := Pvdt.ScrollBarOptions.AlwaysVisible;
  Self.ScrollBarOptions.ScrollBars := Pvdt.ScrollBarOptions.ScrollBars;
  Self.ShowHint := Pvdt.ShowHint;
  Self.TabOrder := Pvdt.TabOrder;
  Self.TreeOptions.AutoOptions := Pvdt.TreeOptions.AutoOptions;
  Self.TreeOptions.MiscOptions := Pvdt.TreeOptions.MiscOptions;
  Self.TreeOptions.PaintOptions := Pvdt.TreeOptions.PaintOptions;
  Self.TreeOptions.SelectionOptions := Pvdt.TreeOptions.SelectionOptions;

  // Копирование событий
  Self.OnChange := Pvdt.OnChange;
  Self.OnClick := Pvdt.OnClick;
  Self.OnDrawHint := Pvdt.OnDrawHint;
  Self.OnDrawNode := Pvdt.OnDrawNode;
  Self.OnEnter := Pvdt.OnEnter;
  Self.OnGetHintSize := Pvdt.OnGetHintSize;
  Self.OnKeyDown := Pvdt.OnKeyDown;
  Self.OnKeyPress := Pvdt.OnKeyPress;
  Self.OnMouseDown := Pvdt.OnMouseDown;
  Self.OnMouseMove := Pvdt.OnMouseMove;
  Self.OnScroll := Pvdt.OnScroll;
  Self.OnDragAllowed := Pvdt.OnDragAllowed;
  Self.OnDragDrop := Pvdt.OnDragDrop;
  Self.OnDragOver := Pvdt.OnDragOver;


  // Копирование колонок
  for c := 0 to Pvdt.Header.Columns.Count-1 do begin
    column := Self.Header.Columns.Add;
    column.Options := Pvdt.Header.Columns[c].Options;
    column.Position := Pvdt.Header.Columns[c].Position;
    column.MaxWidth := Pvdt.Header.Columns[c].MaxWidth;
    column.MinWidth := Pvdt.Header.Columns[c].MinWidth;
    column.Width := Pvdt.Header.Columns[c].Width;
    column.Spacing := Pvdt.Header.Columns[c].Spacing;
    column.Margin := Pvdt.Header.Columns[c].Margin;
    column.Style := Pvdt.Header.Columns[c].Style;
    column.Text := Pvdt.Header.Columns[c].Text;
  end;

  Pvdt.Destroy;
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.Create  DONE');
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualList.UpdateHintCanvas(newCanvas: TCanvas);
begin
  // Для перерисовки тултипа нужна его канва, достать ее можно только при получении
  // сообщения CM_HINTSHOW (см CMHintShow), но так как все нужные свойства закрыты
  // единственным способом ее получения является обработчик события OnDrawHint
  Self.HintCanvas := newCanvas;
end;

procedure TVirtualList.DoGetHintSize(Node: PVirtualNode; Column: TColumnIndex; var R: TRect);
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.DoGetHintSize %d', [Column]);
  inherited DoGetHintSize(Node, Column, R);
  if (Self.HintCanvas <> nil) then begin
     //Self.HintCanvas.Brush.Color := clRed;
     Self.HintCanvas.Brush.Style := bsSolid;
     Self.HintCanvas.FillRect(0,0,Self.HintCanvas.Width, Self.HintCanvas.Height);
     inherited DoDrawHint(Self.HintCanvas, Node, R, Column);
  end;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualList.UpdateTileColumn(count: Word; Forse: Boolean = False);
var
  data, RawData, NodeDat: PByte;
  n, c: DWord;
  node: PVirtualNode;
  item: PVirtualItem;
begin
  if (not Forse and ((Self.TileColumn = count) or (Self.Header.Columns.Count <= count)))
     then Exit;

  getmem(RawData, NodeDataSize * RootNodeCount + NodeDataSize div Self.TileColumn * count);
  data := RawData;
  node := inherited GetFirst(False);
  while node <> nil do begin
    Move(inherited GetNodeData(node)^, data^, NodeDataSize);
    inc(data, NodeDataSize);
    node := inherited GetNext(node, False);
  end;

  SetRoundMode(rmUp);
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.UpdateTileColumn %d %d %d', [Self.RootNodeCount, Self.TileColumn, count]);
  //nodes := Round(Self.RootNodeCount * Self.TileColumn / count);
  SetRoundMode(rmNearest);
  Self.ClearAll := False;
  inherited Clear;
  Self.NodeDataSize := Self.NodeDataSize div Self.TileColumn * count;
  Self.ClearAll := True;
  Self.TileColumn := count;

  item := Self.FirstItem;
  data := RawData;
  //if (item <> nil) then
  n:=0;
  while item <> nil do begin
    if (item^.NextItem = nil)
      then Break;
    node := inherited AddChild(nil);
    NodeDat := inherited GetNodeData(node);
    Move(data^, NodeDat^, Self.NodeDataSize);
    inc(data, Self.NodeDataSize);
    for c:=0 to Self.TileColumn - 1 do begin
      if (item^.NextItem = nil)
         then Break;
      item^.Node := node;
      item^.Column := c;
      item := item^.NextItem;
    end;
    inc(n, +1);
  end;
  freemem(RawData);
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.UpdateTileColumn %s', ['Done']);
end;

function TVirtualList.AddChild(ParentItem: PVirtualNode; UserData: Pointer = nil): PVirtualNode;
begin
  Result := PVirtualNode(Self.AddItem(PVirtualItem(Parent), UserData));
end;

function TVirtualList.AddItem(ParentItem: PVirtualItem; UserData: Pointer = nil): PVirtualItem;
var
  item: PVirtualItem;
begin
//  Logger.Send([lcClient, lcDebug], 'TVirtualTree.AddChild %s', ['Start']);
  getmem(item, SizeOf(TVirtualItem));
  item^.NextItem:=nil;
  item^.Selected:=False;
  if ((Self.LastItem = nil) or (Self.LastItem^.Column = Self.TileColumn - 1))
     then begin
        item^.Node := inherited AddChild(nil);
        item^.Column := 0;
        if (Self.FirstItem = nil)
          then Self.FirstItem := item;
     end else begin
       item^.Node := Self.LastItem^.Node;
       item^.Column := Self.LastItem^.Column + 1;
     end;
  if (Self.LastItem <> nil)
    then Self.LastItem^.NextItem := item;
  Self.LastItem := item;
  Result := item;
  inc(FTilesCount, +1);
//  Logger.Send([lcClient, lcDebug], 'TVirtualTree.AddChild %s', ['Done']);
end;

function TVirtualList.GetFirst(ConsiderChildrenAbove: Boolean = False): PVirtualItem;
begin
  Result := Self.FirstItem;
end;

function TVirtualList.GetNext(Item: PVirtualItem; ConsiderChildrenAbove: Boolean = False): PVirtualItem;
begin
  Result := Item^.NextItem;
end;

function TVirtualList.GetLast(Node: PVirtualNode = nil; ConsiderChildrenAbove: Boolean = False): PVirtualItem;
begin
  Result := Self.LastItem;
end;

function TVirtualList.GetItemAt(Node: PVirtualNode; Column: TColumnIndex): PVirtualItem;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.GetItemAt %s', ['Start']);
  if (Column < 0) or (Column >= Self.Header.Columns.Count) then  begin
    Result := nil;
    Exit;
  end;

  Result := Self.FirstItem;
  while (Result <> nil) and ((Result^.Node <> Node) or (Result^.Column <> Word(Self.Header.Columns[Column].Tag)))
    do Result := Result^.NextItem;
end;

function TVirtualList.GetNodeData(Item: PVirtualItem): Pointer;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.GetNodeData %s', ['Start']);
  Result := inherited GetNodeData(Item^.Node) + (Item^.Column * NodeDataSize div Self.TileColumn);
end;

procedure TVirtualList.Clear;
var
  item: PVirtualItem;
  next: PVirtualItem;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.Clear %s', ['Called']);
  // Злоябучий паскаль автоматически вызывает чистку при изменении NodeDataSize, что не всегда нужно...
  if (Self.ClearAll) and (Self.FirstItem <> nil) then begin
    next := Self.FirstItem;
    while (next <> nil) do begin
      item := next;
      next := next^.NextItem;
      freemem(item);
    end;
    Self.FirstItem:=nil;
    Self.LastItem:=nil;
  end;
  inherited;
  FTilesCount := 0;
  FSelectionCount := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualList.GetFirstSelected(ConsiderChildrenAbove: Boolean = False): PVirtualItem;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.GetFirstSelected %s', ['']);
  Result := Self.FirstItem;
  while ((Result <> nil) and (not Result^.Selected)) do begin
    Result := Result^.NextItem;
  end;
end;

function TVirtualList.GetNextSelected(Item: PVirtualItem; ConsiderChildrenAbove: Boolean = False): PVirtualItem;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.GetNextSelected %s', ['']);
  Result := Item^.NextItem;
  while ((Result <> nil) and (not Result^.Selected)) do begin
    Result := Result^.NextItem;
  end;
end;

function TVirtualList.GetSelected(Item: PVirtualItem): Boolean;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.GetSelected %s', ['']);
  Result := Item^.Selected;
end;

procedure TVirtualList.SetSelected(Item: PVirtualItem; Value: Boolean);
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.SetSelected %s', ['']);
  if (Item^.Selected = Value)
     then Exit;
  Item^.Selected := Value;
  if not Value
    then Dec(FSelectionCount)
    else begin
      Inc(FSelectionCount);
      Self.LastSelected := Item;
    end;
  // TODO: Обновить отображение выделения
end;

procedure TVirtualList.ClearSelection;
var
  item: PVirtualItem;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.ClearSelection %s', ['']);
  item := Self.FirstItem;
  while (item <> nil) do begin
    Self.SetSelected(item, False);
    item := item^.NextItem;
  end;
  FSelectionCount := 0;
  inherited ClearSelection;
end;

procedure TVirtualList.DeleteSelectedNodes;
var
  item, next, prev: PVirtualItem;
  node: PVirtualNode;
  data, RawData: PByte;
  size: Word;
  c: Word;
begin
  if (Self.GetFirstSelected() = nil)
    then Exit;

  size := NodeDataSize div Self.TileColumn;
  getmem(RawData, NodeDataSize * RootNodeCount);
  data := RawData;

  prev := nil;
  item := Self.FirstItem;
  while (item <> nil) do begin
    if (item^.Selected) then begin
      next := item^.NextItem;
      Dec(FTilesCount);
      freemem(item);
      if (prev <> nil) then begin
        prev^.NextItem := next;
      end else begin
        Self.FirstItem := next;
      end;
      if (next = nil) then begin
        Self.LastItem := prev;
      end;
      item := next;
    end else begin
      Move((inherited GetNodeData(item^.Node) + (size * item^.Column))^, data^, size);
      inc(data, size);
      prev := item;
      item := item^.NextItem;
    end;
  end;

  data := RawData;
  item := Self.FirstItem;
  node := inherited GetFirst();
  while (node <> nil) do begin
    Move(data^, inherited GetNodeData(node)^, NodeDataSize);
    Inc(data, NodeDataSize);
    for c := 0 to Self.TileColumn - 1 do
      if item <> nil then begin
        item^.Node := node;
        item^.Column := c;
        item := item^.NextItem;
      end else Break;
    if (item = nil)
      then Break;
    node := inherited GetNext(node);
  end;

  if (Self.LastItem = nil)
    then inherited Clear
  else begin
    item := Self.LastItem^.NextItem;
    while (item <> nil) do begin
      if (item^.Node <> Self.LastItem^.Node) then begin
        node := item^.Node;
        while (node <> nil) do begin
          inherited DeleteNode(node, False);
          node := inherited GetNext(node);
        end;
        Break;
      end;
      item := item^.NextItem;
    end;
  end;

  freemem(RawData);
  Self.LastSelected := nil;
  FSelectionCount := 0;
end;

//----------------------------------------------------------------------------------------------------------------------

function TVirtualList.GetFocusedNode(): PVirtualItem;
var
  node: PVirtualNode;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.GetFocusedNode %s', ['']);
  node := inherited FocusedNode;
  Result := Self.FirstItem;
  while ((Result <> nil) and (Result^.Node <> node))
    do Result := Result^.NextItem;
end;

procedure TVirtualList.SetFocusedNode(Item: PVirtualItem);
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.SetFocusedNode %s', ['']);
  inherited FocusedNode := Item^.Node;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualList.DoPaintNode(var PaintInfo: TVTPaintInfo);
var
  item: PVirtualItem;
  node: PVirtualNode;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.DoPaintNode %s', ['Start']);
  item := Self.FirstItem;//^.NextItem;
  while ((item <> nil) and ((item^.Node^.Index <> PaintInfo.Node^.Index) or (item^.Column <> Word(Self.Header.Columns[PaintInfo.Column].Tag))))
    do item := item^.NextItem;
  if (item <> nil) then begin
    node := PaintInfo.Node;
    PaintInfo.Node := PVirtualNode(item);
    inherited DoPaintNode(PaintInfo);
    PaintInfo.Node := node;
    //Logger.Send([lcClient, lcDebug], 'TVirtualTree.DoPaintNode %s', ['Done']);

    if (item^.Selected and (item = Self.LastSelected))
      then PaintInfo.Canvas.Pen.Color := Colors.FocusedSelectionBorderColor
      else if item^.Selected
      then PaintInfo.Canvas.Pen.Color := Colors.UnfocusedSelectionBorderColor
      else PaintInfo.Canvas.Pen.Color := Colors.BorderColor;
    //PaintInfo.Canvas.Pen.Color := clRed;
    PaintInfo.Canvas.Pen.Style := psDot;//psSolid;
    PaintInfo.Canvas.Pen.Width := 1;

    //Logger.Send([lcClient, lcDebug], 'TVirtualTree.DoPaintNode [%d,%d,%d,%d] [%d,%d]', [PaintInfo.CellRect.Left, PaintInfo.CellRect.Top,
    //PaintInfo.CellRect.Right - PaintInfo.CellRect.Left, PaintInfo.CellRect.Bottom - PaintInfo.CellRect.Top, PaintInfo.Canvas.Width, PaintInfo.Canvas.Height]);
    PaintInfo.Canvas.Line(PaintInfo.CellRect.Left+1,PaintInfo.CellRect.Bottom-1,PaintInfo.CellRect.Right-1,PaintInfo.CellRect.Bottom-1);
    PaintInfo.Canvas.Line(PaintInfo.CellRect.Right-1,PaintInfo.CellRect.Top+1,PaintInfo.CellRect.Left+1,PaintInfo.CellRect.Top+1);
    if Self.TileColumn > 1 then begin
      PaintInfo.Canvas.Line(PaintInfo.CellRect.Left+1,PaintInfo.CellRect.Top+1,PaintInfo.CellRect.Left+1,PaintInfo.CellRect.Bottom-1);
      PaintInfo.Canvas.Line(PaintInfo.CellRect.Right-1,PaintInfo.CellRect.Bottom-1,PaintInfo.CellRect.Right-1,PaintInfo.CellRect.Top+1);
    end;

    PaintInfo.Canvas.Pen.Color := Color;
    PaintInfo.Canvas.Pen.Style := psSolid;
    PaintInfo.Canvas.Line(PaintInfo.CellRect.Left,PaintInfo.CellRect.Bottom,PaintInfo.CellRect.Right,PaintInfo.CellRect.Bottom);
    PaintInfo.Canvas.Line(PaintInfo.CellRect.Right,PaintInfo.CellRect.Top,PaintInfo.CellRect.Left,PaintInfo.CellRect.Top);
    if Self.TileColumn > 1 then begin
      PaintInfo.Canvas.Line(PaintInfo.CellRect.Left,PaintInfo.CellRect.Top,PaintInfo.CellRect.Left,PaintInfo.CellRect.Bottom);
      PaintInfo.Canvas.Line(PaintInfo.CellRect.Right,PaintInfo.CellRect.Bottom,PaintInfo.CellRect.Right,PaintInfo.CellRect.Top);
      PaintInfo.Canvas.Pixels[PaintInfo.CellRect.Left+1,PaintInfo.CellRect.Top+1] := Color;
      PaintInfo.Canvas.Pixels[PaintInfo.CellRect.Left+1,PaintInfo.CellRect.Bottom-1] := Color;
      PaintInfo.Canvas.Pixels[PaintInfo.CellRect.Right-1,PaintInfo.CellRect.Bottom-1] := Color;
      PaintInfo.Canvas.Pixels[PaintInfo.CellRect.Right-1,PaintInfo.CellRect.Top+1] := Color;
    end;


    //PaintInfo.Canvas.Rectangle(PaintInfo.CellRect);
    //PaintInfo.Canvas.Line(Rect(1,1,PaintInfo.Canvas.Width-2, PaintInfo.Canvas.Height-2));
    //PaintInfo.Canvas.Rectangle(Rect(1,1,PaintInfo.Canvas.Width-2, PaintInfo.Canvas.Height-2));
    //PaintInfo.Canvas.Line(1,1,PaintInfo.Canvas.Width-2,PaintInfo.Canvas.Height-2);
    //PaintInfo.Canvas.Line(1,1,1,PaintInfo.Canvas.Height-2);
    //PaintInfo.Canvas.Line(1,PaintInfo.Canvas.Height-2,PaintInfo.Canvas.Width-2,PaintInfo.Canvas.Height-2);
    //PaintInfo.Canvas.Line(PaintInfo.Canvas.Width-4,PaintInfo.Canvas.Height-2,PaintInfo.Canvas.Width-4,1);
    //PaintInfo.Canvas.Line(0,PaintInfo.Canvas.Width,0,0);
  end;
end;

procedure TVirtualList.PrepareCell(var PaintInfo: TVTPaintInfo; WindowOrgX, MaxWidth: Integer);
var
  item: PVirtualItem;
begin
  inherited PrepareCell(PaintInfo, WindowOrgX, MaxWidth);

  item := Self.GetItemAt(PaintInfo.Node, PaintInfo.Column);
  if (item = nil)
    then Exit;
  if (item^.Selected and (item = Self.LastSelected))
  then PaintInfo.Canvas.Brush.Color := Colors.FocusedSelectionColor
  else if item^.Selected
  then PaintInfo.Canvas.Brush.Color := Colors.UnfocusedSelectionColor
  else PaintInfo.Canvas.Brush.Color := Colors.GridLineColor;
  PaintInfo.Canvas.Brush.Style := bsSolid;
  PaintInfo.Canvas.FillRect(0,0,PaintInfo.Canvas.Width, PaintInfo.Canvas.Height);
end;

procedure TVirtualList.DoPaintDropMark(Canvas: TCanvas; Node: PVirtualNode; const R: TRect);
begin

end;

//----------------------------------------------------------------------------------------------------------------------

procedure TVirtualList.HandleMouseDblClick(var Message: TLMMouse; const HitInfo: THitInfo);
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.HandleMouseDblClick %s', ['Start']);
  inherited HandleMouseDblClick(Message, HitInfo);
end;

procedure TVirtualList.HandleMouseDown(var Message: TLMMouse; var HitInfo: THitInfo);
var
  ShiftState: TShiftState;
  HitItem: PVirtualItem;
  item: PVirtualItem;
  //kinput: TKEYINPUT;
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.HandleMouseDown %s', ['Start']);
  HitItem := Self.GetItemAt(HitInfo.HitNode, HitInfo.HitColumn);
  if (HitItem = nil) then begin
    inherited HandleMouseDown(Message, HitInfo);
    Exit;
  end;

  ShiftState := KeysToShiftState(Message.Keys) * [ssShift, ssCtrl, ssAlt];
  if not (ssAlt in ShiftState) then begin
    if (not (ssCtrl in ShiftState)) and (not (ssShift in ShiftState)) then begin
      if (not HitItem^.Selected)
        then Self.ClearSelection;
      Self.SetSelected(HitItem, True);
    end else if not (ssShift in ShiftState) then begin
      Self.SetSelected(HitItem, not HitItem^.Selected);
    end else begin
      if not (ssCtrl in ShiftState)
        then Self.ClearSelection;
      if Self.LastSelected = nil
        then Self.LastSelected := Self.FirstItem;

      if Self.LastSelected^.Node^.Index < HitItem^.Node^.Index then begin
        item := Self.LastSelected;
        HitItem := HitItem;
      end else begin
        item := HitItem;
        HitItem := Self.LastSelected;
      end;
      while item <> HitItem^.NextItem do begin
        Self.SetSelected(item, True);
        item := item^.NextItem;
      end;
    end;
  end;

  // Вызываем перерисовку контрола (тутбы потом понормальному сделать...)
  if (Self.Focused) then begin
    Self.Parent.SetFocus;
    Self.SetFocus;
  end;
  inherited HandleMouseDown(Message, HitInfo);

  // Чтоже я творю-то...
  {if (ShiftState = []) then begin
    kinput.itype := INPUT_KEYBOARD;
    kinput.wVk := $11; // VK_CONTROL
    SendInput(1, @kinput, sizeof(TKEYINPUT));
    BeginDrag(TRUE);
    kinput.dwFlags := KEYEVENTF_KEYUP;
    SendInput(1, @kinput, sizeof(TKEYINPUT));
  end;} //TODO

end;

procedure TVirtualList.HandleMouseUp(Keys: PtrUInt; const HitInfo: THitInfo);
begin
  //Logger.Send([lcClient, lcDebug], 'TVirtualTree.HandleMouseUp %s', ['Start']);
  inherited HandleMouseUp(Keys, HitInfo);
end;

//----------------------------------------------------------------------------------------------------------------------



end.
