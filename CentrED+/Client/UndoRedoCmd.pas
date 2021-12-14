unit UndoRedoCmd;

interface 
uses 
  Classes, SysUtils, VirtualTrees;

type 
  IUndoRedoCommand = interface (IUnknown) 
    ['{D84BFD00-8396-11D6-B4FA-000021D960D4}'] 
    procedure Execute; 
    procedure Redo; 
    procedure Undo; 
  end; 

  IUndoRedoCommandGroup = interface (IUndoRedoCommand) 
    ['{9169AE00-839B-11D6-B4FA-000021D960D4}'] 
    function GetUndoRedoCommands: TInterfaceList; 
    property UndoRedoCommands: TInterfaceList read GetUndoRedoCommands; 
  end; 

  TUndoRedoCommandGroup = class (TInterfacedObject, IUndoRedoCommandGroup, 
          IUndoRedoCommand) 
  private 
    FList: TInterfaceList; 
    FCanRedo: Boolean; 
  public 
    constructor Create; 
    destructor Destroy; override;
    procedure Add(const AUndoRedoCommand: IUndoRedoCommand);
    procedure Execute; 
    function GetUndoRedoCommands: TInterfaceList; 
    procedure Redo; 
    procedure Undo; 
    property UndoRedoCommands: TInterfaceList read GetUndoRedoCommands; 
  end; 

  TUndoRedoManager = class (TObject) 
  private 
    FRedoList: TInterfaceList; 
    FUndoList: TInterfaceList; 
    FTransactLevel: Integer; 
    FTransaction: IUndoRedoCommandGroup; 
    function GetCanRedo: Integer; 
    function GetCanUndo: Integer; 
  public
    Enabled: Boolean;
    constructor Create; 
    destructor Destroy; override; 
    procedure BeginTransaction; 
    procedure EndTransaction; 
    procedure ExecCommand(const AUndoRedoCommand: IUndoRedoCommand); 
    procedure Redo(RedoCount: Integer = 1); 
    procedure Undo(UndoCount: Integer = 1); 
    property CanRedo: Integer read GetCanRedo; 
    property CanUndo: Integer read GetCanUndo; 
  end; 


  TUndoRedoSelectVirtualNodeCommand = class (TInterfacedObject, IUndoRedoCommand)
  private
    m_UndoRedoManag : TUndoRedoManager;
    m_VirtualTree: TBaseVirtualTree;
    m_VirtualNode: PVirtualNode;
    m_ForwardDir : Boolean;
  public
    constructor Create(UndoRedoManag: TUndoRedoManager; VirtualTree: TBaseVirtualTree; VirtualNode: PVirtualNode);
    //destructor Destroy; override;
    procedure Execute;
    procedure Redo;
    procedure Undo;
  end;

implementation 

{
**************************** TCommands Types ***********************************
}
constructor TUndoRedoSelectVirtualNodeCommand.Create(
  UndoRedoManag: TUndoRedoManager; VirtualTree: TBaseVirtualTree; VirtualNode: PVirtualNode);
begin
  m_UndoRedoManag := UndoRedoManag;
  m_VirtualTree := VirtualTree;
  m_VirtualNode := VirtualNode;
  m_ForwardDir  := VirtualTree.Selected[VirtualNode];
end;

procedure TUndoRedoSelectVirtualNodeCommand.Execute;
begin
end;

procedure TUndoRedoSelectVirtualNodeCommand.Redo;
begin
  m_UndoRedoManag.Enabled := False;
  m_VirtualTree.Selected[m_VirtualNode] := m_ForwardDir;
  m_UndoRedoManag.Enabled := True;
end;

procedure TUndoRedoSelectVirtualNodeCommand.Undo;
begin
  m_UndoRedoManag.Enabled := False;
  //m_VirtualTree.Selected[m_VirtualNode] := not m_ForwardDir;
  if m_ForwardDir
  then m_VirtualTree.Selected[m_VirtualNode] := False
  else m_VirtualTree.Selected[m_VirtualNode] := True;
  m_UndoRedoManag.Enabled := True;
end;

{ 
**************************** TUndoRedoCommandGroup ***************************** 
} 
constructor TUndoRedoCommandGroup.Create; 
begin 
  inherited Create; 
  FList:= TInterfaceList.Create; 
end; 

destructor TUndoRedoCommandGroup.Destroy; 
begin 
  FList.Free; 
  inherited Destroy; 
end; 

procedure TUndoRedoCommandGroup.Add(const AUndoRedoCommand: IUndoRedoCommand);
begin
  FList.Add(AUndoRedoCommand);
end;

procedure TUndoRedoCommandGroup.Execute; 
var 
  I: Integer; 
begin 
  for I:= 0 to FList.Count-1 do 
    (FList[I] as IUndoRedoCommand).Execute; 
end; 

function TUndoRedoCommandGroup.GetUndoRedoCommands: TInterfaceList; 
begin 
  Result:= FList; 
end; 

procedure TUndoRedoCommandGroup.Redo; 
var 
  I: Integer; 
begin 
  if FCanRedo then 
  begin 
    for I:= 0 to FList.Count-1 do
      (FList[I] as IUndoRedoCommand).Redo; 
       
    FCanRedo:= False; 
  end 
  else 
    raise Exception.Create('Must call TUndoRedoCommandGroup.Undo before calling Redo.'); 
end; 

procedure TUndoRedoCommandGroup.Undo; 
var 
  I: Integer; 
begin 
  if FCanRedo then 
    raise Exception.Create('TUndoRedoCommandGroup.Undo already called'); 

  for I:= FList.Count-1 downto 0 do
    (FList[I] as IUndoRedoCommand).Undo; 

  FCanRedo:= True; 
end; 

{ 
******************************* TUndoRedoManager ******************************* 
} 
constructor TUndoRedoManager.Create; 
begin 
  inherited Create; 
  FRedoList:= TInterfaceList.Create; 
  FUndoList:= TInterfaceList.Create;
  Enabled  := True;
end; 

destructor TUndoRedoManager.Destroy; 
begin 
  FRedoList.Free; 
  FUndoList.Free; 
  inherited Destroy; 
end; 

procedure TUndoRedoManager.BeginTransaction; 
begin 
  Inc(FTransactLevel); 
  if FTransactLevel = 1 then 
    FTransaction:= TUndoRedoCommandGroup.Create; 
end; 

procedure TUndoRedoManager.EndTransaction; 
begin 
  Dec(FTransactLevel); 
  if (FTransactLevel = 0) then 
  begin 
    if FTransaction.UndoRedoCommands.Count > 0 then 
    begin 
      FRedoList.Clear; 
      FUndoList.Add(FTransaction); 
    end; 
    FTransaction:= nil; 
  end 
  else if FTransactLevel < 0 then 
    raise Exception.Create('Unmatched TUndoRedoManager.BeginTransaction and EndTransaction'); 
end; 

procedure TUndoRedoManager.ExecCommand(const AUndoRedoCommand: IUndoRedoCommand);
begin
  if Enabled then begin
    BeginTransaction;
    try
      FTransaction.UndoRedoCommands.Add(AUndoRedoCommand);
      AUndoRedoCommand.Execute;
    finally
      EndTransaction;
    end;
  end;

end; 

function TUndoRedoManager.GetCanRedo: Integer; 
begin 
  Result:= FRedoList.Count; 
end; 

function TUndoRedoManager.GetCanUndo: Integer; 
begin 
  Result:= FUndoList.Count; 
end; 

procedure TUndoRedoManager.Redo(RedoCount: Integer = 1); 
var 
  I: Integer; 
  Item: IUndoRedoCommand; 
  RedoLast: Integer; 
begin 
  if FTransactLevel <> 0 then 
    raise Exception.Create('Cannot Redo while in Transaction'); 

  // Index of last redo item 
  RedoLast:= FRedoList.Count - RedoCount; 
  if RedoLast < 0 then 
    RedoLast:= 0; 

  for I:= FRedoList.Count-1 downto RedoLast do 
  begin 
    Item:= FRedoList[I] as IUndoRedoCommand; 
    FRedoList.Delete(I); 
    FUndoList.Add(Item); 
    Item.Redo; 
  end; 
end; 

procedure TUndoRedoManager.Undo(UndoCount: Integer = 1); 
var 
  I: Integer; 
  Item: IUndoRedoCommand; 
  UndoLast: Integer; 
begin 
  if FTransactLevel <> 0 then 
    raise Exception.Create('Cannot undo while in Transaction'); 

  // Index of last undo item 
  UndoLast:= FUndoList.Count - UndoCount; 
  if UndoLast < 0 then 
    UndoLast:= 0; 

  for I:= FUndoList.Count-1 downto UndoLast do 
  begin 
    Item:= FUndoList[I] as IUndoRedoCommand; 
    FUndoList.Delete(I); 
    FRedoList.Add(Item); 
    Item.Undo; 
  end; 
end; 


end. 
