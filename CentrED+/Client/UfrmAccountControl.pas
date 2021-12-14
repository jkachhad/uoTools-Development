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
unit UfrmAccountControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, VirtualTrees, Math, UEnhancedMemoryStream, UEnums;

type

  { TfrmAccountControl }

  TfrmAccountControl = class(TForm)
    ilToolbar: TImageList;
    ilAccesslevel: TImageList;
    tbMain: TToolBar;
    tbRefresh: TToolButton;
    tbAddUser: TToolButton;
    tbEditUser: TToolButton;
    tbDeleteUser: TToolButton;
    tbSeparator1: TToolButton;
    vstAccounts: TVirtualStringTree;
    procedure tbEditUserClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbAddUserClick(Sender: TObject);
    procedure tbDeleteUserClick(Sender: TObject);
    procedure tbRefreshClick(Sender: TObject);
    procedure vstAccountsCompareNodes(Sender: TBaseVirtualTree; Node1,
      Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
    procedure vstAccountsDblClick(Sender: TObject);
    procedure vstAccountsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vstAccountsGetHint(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var LineBreakStyle: TVTTooltipLineBreakStyle;
      var HintText: String);
    procedure GetAccountImageIndex(AccessLevel: TAccessLevel; var ImageIndex: Integer);
    procedure vstAccountsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure vstAccountsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstAccountsHeaderClick(Sender: TVTHeader; Column: TColumnIndex;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure OnModifyUserResponse(ABuffer: TEnhancedMemoryStream);
    procedure OnDeleteUserResponse(ABuffer: TEnhancedMemoryStream);
    procedure OnListUsersPacket(ABuffer: TEnhancedMemoryStream);
    function FindNode(AUsername: string): PVirtualNode;
  private
    procedure OnListModified;
  public
    lbDlgDelConfCaption: string;
    lbDlgDelConf: string;
    lbDlgAddNotiCaption: string;
    lbDlgAddNoti: string;
    lbDlgModNotiCaption: string;
    lbDlgModNoti: string;
    lbDlgInvlErrCaption: string;
    lbDlgInvlErr: string;
    lbDlgDelNotiCaption: string;
    lbDlgDelNoti: string;
    lbDlgDelfErrCaption: string;
    lbDlgDelfErr: string;
  end;

var
  frmAccountControl: TfrmAccountControl;

implementation

uses
  UdmNetwork, UPacket, UPacketHandlers, UAdminHandling, UfrmEditAccount, UfrmMain,
  UGUIPlatformUtils, Language;

type
  PAccountInfo = ^TAccountInfo;
  TAccountInfo = record
    Username: string;
    AccessLevel: TAccessLevel;
    Regions: TStringList;
  end;
  
  { TModifyUserPacket }

  TModifyUserPacket = class(TPacket)
    constructor Create(AUsername, APassword: string; AAccessLevel: TAccessLevel;
      ARegions: TStrings);
  end;
  
  { TDeleteUserPacket }

  TDeleteUserPacket = class(TPacket)
    constructor Create(AUsername: string);
  end;
  
  { TRequestUserListPacket }

  TRequestUserListPacket = class(TPacket)
    constructor Create;
  end;

{ TModifyUserPacket }

constructor TModifyUserPacket.Create(AUsername, APassword: string;
  AAccessLevel: TAccessLevel; ARegions: TStrings);
var
  regionCount: Byte;
  i: Integer;
begin
  inherited Create($03, 0);
  FStream.WriteByte($05);
  FStream.WriteStringNull(AUsername);
  FStream.WriteStringNull(APassword);
  FStream.WriteByte(Byte(AAccessLevel));

  regionCount := Min(ARegions.Count, 256);
  FStream.WriteByte(regionCount);

  for i := 0 to regionCount - 1 do
    FStream.WriteStringNull(ARegions.Strings[i]);
end;

{ TDeleteUserPacket }

constructor TDeleteUserPacket.Create(AUsername: string);
begin
  inherited Create($03, 0);
  FStream.WriteByte($06);
  FStream.WriteStringNull(AUsername);
end;

{ TRequestUserListPacket }

constructor TRequestUserListPacket.Create;
begin
  inherited Create($03, 0);
  FStream.WriteByte($07);
end;

{ TfrmAccountControl }

procedure TfrmAccountControl.FormCreate(Sender: TObject);
begin
  LanguageTranslate(Self);

  vstAccounts.NodeDataSize := SizeOf(TAccountInfo);
  
  AssignAdminPacketHandler($05, TPacketHandler.Create(0, @OnModifyUserResponse));
  AssignAdminPacketHandler($06, TPacketHandler.Create(0, @OnDeleteUserResponse));
  AssignAdminPacketHandler($07, TPacketHandler.Create(0, @OnListUsersPacket));
end;

procedure TfrmAccountControl.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmAccountControl.tbEditUserClick(Sender: TObject);
var
  selected: PVirtualNode;
  accountInfo: PAccountInfo;
  regions: TStrings;
begin
  selected := vstAccounts.GetFirstSelected;
  if selected <> nil then
  begin
    accountInfo := vstAccounts.GetNodeData(selected);
    with frmEditAccount do
    begin
      edUsername.Text := accountInfo^.Username;
      edUsername.Color := clBtnFace;
      edUsername.ReadOnly := True;
      edPassword.Text := '';
      lblPasswordHint.Visible := True;
      SetAccessLevel(accountInfo^.AccessLevel);
      SetRegions(accountInfo^.Regions);
      if ShowModal = mrOK then
      begin
        regions := GetRegions;
        dmNetwork.Send(TModifyUserPacket.Create(edUsername.Text,
          edPassword.Text, GetAccessLevel, regions));
        regions.Free;
      end;
    end;
  end;
end;

procedure TfrmAccountControl.FormDestroy(Sender: TObject);
begin
  if AdminPacketHandlers[$05] <> nil then FreeAndNil(AdminPacketHandlers[$05]);
  if AdminPacketHandlers[$06] <> nil then FreeAndNil(AdminPacketHandlers[$06]);
  if AdminPacketHandlers[$07] <> nil then FreeAndNil(AdminPacketHandlers[$07]);
end;

procedure TfrmAccountControl.FormShow(Sender: TObject);
begin
  SetWindowParent(Handle, frmMain.Handle);
  tbRefreshClick(Sender);
end;

procedure TfrmAccountControl.tbAddUserClick(Sender: TObject);
var
  regions: TStrings;
begin
  with frmEditAccount do
  begin
    edUsername.Text := '';
    edUsername.Color := clWindow;
    edUsername.ReadOnly := False;
    edPassword.Text := '';
    lblPasswordHint.Visible := False;
    cbAccessLevel.ItemIndex := 2;
    SetRegions(nil);
    if ShowModal = mrOK then
    begin
      regions := GetRegions;
      dmNetwork.Send(TModifyUserPacket.Create(edUsername.Text, edPassword.Text,
        GetAccessLevel, regions));
      regions.Free;
    end;
  end;
end;

procedure TfrmAccountControl.tbDeleteUserClick(Sender: TObject);
var
  selected: PVirtualNode;
  accountInfo: PAccountInfo;
begin
  selected := vstAccounts.GetFirstSelected;
  if selected <> nil then
  begin
    accountInfo := vstAccounts.GetNodeData(selected);
    if MessageDlg(lbDlgDelConfCaption, Format(lbDlgDelConf,
      [accountInfo^.Username]), mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      dmNetwork.Send(TDeleteUserPacket.Create(accountInfo^.Username));
  end;
end;

procedure TfrmAccountControl.tbRefreshClick(Sender: TObject);
begin
  dmNetwork.Send(TRequestUserListPacket.Create);
end;

procedure TfrmAccountControl.vstAccountsCompareNodes(Sender: TBaseVirtualTree;
  Node1, Node2: PVirtualNode; Column: TColumnIndex; var Result: Integer);
var
  accountInfo1, accountInfo2: PAccountInfo;
begin
  accountInfo1 := Sender.GetNodeData(Node1);
  accountInfo2 := Sender.GetNodeData(Node2);
  case Column of
    1: Result := CompareText(accountInfo1^.Username, accountInfo2^.Username);
    2: Result := Integer(accountInfo1^.AccessLevel) - Integer(accountInfo2^.AccessLevel);
  end;
end;

procedure TfrmAccountControl.vstAccountsDblClick(Sender: TObject);
begin
  tbEditUserClick(Sender);
end;

procedure TfrmAccountControl.vstAccountsFreeNode(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  accountInfo: PAccountInfo;
begin
  accountInfo := vstAccounts.GetNodeData(Node);
  accountInfo^.Username := '';
  if accountInfo^.Regions <> nil then FreeAndNil(accountInfo^.Regions);
end;

procedure TfrmAccountControl.vstAccountsGetHint(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex;
  var LineBreakStyle: TVTTooltipLineBreakStyle; var HintText: String);
var
  accountInfo: PAccountInfo;
begin
  if Column = 3 then
  begin
    accountInfo := Sender.GetNodeData(Node);
    if accountInfo^.Regions.Count > 0 then
      HintText := Trim(accountInfo^.Regions.Text);
  end;
end;

procedure TfrmAccountControl.GetAccountImageIndex(AccessLevel: TAccessLevel; var ImageIndex: Integer);
begin
  case AccessLevel of
      alNone: ImageIndex := 0;
      alView: ImageIndex := 1;
      alNormal: ImageIndex := 6;
      alDeveloper: ImageIndex := 8;
      alAdministrator: ImageIndex := 10;
    end;
end;

procedure TfrmAccountControl.vstAccountsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  accountInfo: PAccountInfo;
begin
  accountInfo := Sender.GetNodeData(Node);
  if Column = 0 then
  begin
    case accountInfo^.AccessLevel of
      alNone: ImageIndex := 0;
      alView: ImageIndex := 1;
      alNormal:
        begin
          if accountInfo^.Regions.Count > 0 then
            ImageIndex := 5
          else
            ImageIndex := 6;
        end;
      alDeveloper:
        begin
          if accountInfo^.Regions.Count > 0 then
            ImageIndex := 7
          else
            ImageIndex := 8;
        end;
      alAdministrator: ImageIndex := 10;
    end;
  end else if Column = 3 then
  begin
    if accountInfo^.Regions.Count > 0 then
      ImageIndex := 12;
  end;
end;

procedure TfrmAccountControl.vstAccountsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  accountInfo: PAccountInfo;
begin
  accountInfo := Sender.GetNodeData(Node);
  case Column of
    1: CellText := accountInfo^.Username;
    2: CellText := GetAccessLevel(accountInfo^.AccessLevel);
  else
    CellText := '';
  end;
end;

procedure TfrmAccountControl.vstAccountsHeaderClick(Sender: TVTHeader;
  Column: TColumnIndex; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Column in [1, 2] then
  begin
    if Sender.SortColumn <> Column then
    begin
      Sender.SortColumn := Column;
      Sender.SortDirection := sdAscending;
    end else
    begin
      case Sender.SortDirection of
        sdAscending: Sender.SortDirection := sdDescending;
        sdDescending: Sender.SortDirection := sdAscending;
      end;
    end;
    Sender.Treeview.SortTree(Sender.SortColumn, Sender.SortDirection);
  end;
end;

procedure TfrmAccountControl.OnModifyUserResponse(ABuffer: TEnhancedMemoryStream);
var
  node: PVirtualNode;
  modifyStatus: TModifyUserStatus;
  username: string;
  accountInfo: PAccountInfo;
  i, regions: Integer;
begin
  modifyStatus := TModifyUserStatus(ABuffer.ReadByte);
  username := ABuffer.ReadStringNull;
  case modifyStatus of
    muAdded:
      begin
        node := vstAccounts.AddChild(nil);
        accountInfo := vstAccounts.GetNodeData(node);
        accountInfo^.Username := username;
        accountInfo^.AccessLevel := TAccessLevel(ABuffer.ReadByte);
        accountInfo^.Regions := TStringList.Create;
        regions := ABuffer.ReadByte;
        for i := 0 to regions - 1 do
          accountInfo^.Regions.Add(ABuffer.ReadStringNull);
        OnListModified;

        Messagedlg(lbDlgAddNotiCaption, Format(lbDlgAddNoti, [username]),
          mtInformation, [mbOK], 0);
      end;
    muModified:
      begin
        node := FindNode(username);
        if node <> nil then
        begin
          accountInfo := vstAccounts.GetNodeData(node);
          accountInfo^.AccessLevel := TAccessLevel(ABuffer.ReadByte);
          accountInfo^.Regions.Clear;
          regions := ABuffer.ReadByte;
          for i := 0 to regions - 1 do
            accountInfo^.Regions.Add(ABuffer.ReadStringNull);
          OnListModified;

          Messagedlg(lbDlgModNotiCaption, Format(lbDlgModNoti, [username]),
            mtInformation, [mbOK], 0);
        end;
      end;
    muInvalidUsername:
      MessageDlg(lbDlgInvlErrCaption, Format(lbDlgInvlErr, [username]),
        mtError, [mbOK], 0);
  end;
end;

procedure TfrmAccountControl.OnDeleteUserResponse(ABuffer: TEnhancedMemoryStream);
var
  node: PVirtualNode;
  deleteStatus: TDeleteUserStatus;
  username: string;
begin
  deleteStatus := TDeleteUserStatus(ABuffer.ReadByte);
  username := ABuffer.ReadStringNull;
  case deleteStatus of
    duDeleted:
      begin
        node := FindNode(username);
        if node <> nil then
        begin
          vstAccounts.DeleteNode(node);
          OnListModified;

          Messagedlg(lbDlgDelNotiCaption, Format(lbDlgDelNoti, [username]),
            mtInformation, [mbOK], 0);
        end;
      end;
    duNotFound:
      MessageDlg(lbDlgDelfErrCaption, Format(lbDlgDelfErr, [username]),
         mtError, [mbOK], 0);
  end;
end;

procedure TfrmAccountControl.OnListUsersPacket(ABuffer: TEnhancedMemoryStream);
var
  node: PVirtualNode;
  accountInfo: PAccountInfo;
  i, j, count, regions: Integer;
begin
  vstAccounts.BeginUpdate;
  vstAccounts.Clear;
  count := ABuffer.ReadWord;
  for i := 1 to count do
  begin
    node := vstAccounts.AddChild(nil);
    accountInfo := vstAccounts.GetNodeData(node);
    accountInfo^.Username := ABuffer.ReadStringNull;
    accountInfo^.AccessLevel := TAccessLevel(ABuffer.ReadByte);
    accountInfo^.Regions := TStringList.Create;
    regions := ABuffer.ReadByte;
    for j := 0 to regions - 1 do
      accountInfo^.Regions.Add(ABuffer.ReadStringNull);
  end;
  vstAccounts.EndUpdate;
  OnListModified;
end;

function TfrmAccountControl.FindNode(AUsername: string): PVirtualNode;
var
  node: PVirtualNode;
  accountInfo: PAccountInfo;
begin
  Result := nil;
  node := vstAccounts.GetFirst;
  while (node <> nil) and (Result = nil) do
  begin
    accountInfo := vstAccounts.GetNodeData(node);
    if accountInfo^.Username = AUsername then
      Result := node;
    node := vstAccounts.GetNext(node);
  end;
end;

procedure TfrmAccountControl.OnListModified;
begin
  vstAccounts.Header.SortColumn := -1;
end;

initialization
  {$I UfrmAccountControl.lrs}

end.

