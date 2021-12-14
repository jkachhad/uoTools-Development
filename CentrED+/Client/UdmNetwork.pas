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
unit UdmNetwork;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Dialogs, lNetComponents, lNet,
  UEnhancedMemoryStream, UPacket, UEnums, ExtCtrls, dateutils, LConvEncoding;

type

  { TdmNetwork }

  TdmNetwork = class(TDataModule)
    TCPClient: TLTCPComponent;
    tmNoOp: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TCPClientConnect(aSocket: TLSocket);
    procedure TCPClientDisconnect(aSocket: TLSocket);
    procedure TCPClientError(const msg: string; aSocket: TLSocket);
    procedure TCPClientReceive(aSocket: TLSocket);
    procedure tmNoOpStartTimer(Sender: TObject);
    procedure tmNoOpTimer(Sender: TObject);
  protected
    FSendQueue: TEnhancedMemoryStream;
    FReceiveQueue: TEnhancedMemoryStream;
    FProfile: string;
    FUsername: string;
    FPassword: string;
    FAccessLevel: TAccessLevel;
    FServerStart: TDateTime;
    FDataDir: string;
    FLastPacket: TDateTime;
    procedure OnCanSend(ASocket: TLSocket);
    procedure OnConnectionHandlingPacket(ABuffer: TEnhancedMemoryStream);
    procedure ProcessQueue;
    procedure DoLogin;
  public
    property Profile: string read FProfile;
    property Username: string read FUsername;
    property AccessLevel: TAccessLevel read FAccessLevel write FAccessLevel;
    property ServerStart: TDateTime read FServerStart;
    procedure Send(APacket: TPacket);
    procedure Disconnect;
    procedure CheckClose(ASender: TForm);
  public
    ErrorCaption: string;
    WrongServer: string;
    WrongAccount: string;
    WrongPassword: string;
    NoAccess: string;
    AlreadyLogined: string;
    TCPErrorCaption: string;
    UnsuportedVersion: string;
  end; 

var
  dmNetwork: TdmNetwork;

implementation

uses
  UPacketHandlers, UPackets, UfrmMain, UfrmLogin, UfrmInitialize,
  UGameResources, UfrmAccountControl, UfrmEditAccount, UfrmDrawSettings,
  UfrmBoundaries, UfrmElevateSettings, UfrmConfirmation, UfrmMoveSettings,
  UfrmAbout, UfrmHueSettings, UfrmRadar, UfrmLargeScaleCommand, UfrmFillSettings,
  UfrmVirtualLayer, UfrmFilter, UfrmRegionControl, UfrmLightlevel, UfrmSelectionSettings,
  UfrmSurfElevateSettings, UfrmSurfStretchSettings, UfrmSurfSmoothSettings,
  Logging, Language;
  
{$I version.inc}

{ TdmNetwork }

procedure TdmNetwork.DataModuleCreate(Sender: TObject);
begin
  FSendQueue := TEnhancedMemoryStream.Create;
  FReceiveQueue := TEnhancedMemoryStream.Create;
  TCPClient.OnCanSend := @OnCanSend;
  PacketHandlers[$02] := TPacketHandler.Create(0, @OnConnectionHandlingPacket);
  DoLogin;
end;

procedure TdmNetwork.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FSendQueue);
  FreeAndNil(FReceiveQueue);
  FreeAndNil(PacketHandlers[$02]);
end;

procedure TdmNetwork.TCPClientConnect(aSocket: TLSocket);
begin
  FSendQueue.Clear;
  FReceiveQueue.Clear;
end;

procedure TdmNetwork.TCPClientDisconnect(aSocket: TLSocket);
begin
  FSendQueue.Clear;
  FReceiveQueue.Clear;
  DoLogin;
end;

procedure TdmNetwork.TCPClientError(const msg: string; aSocket: TLSocket);
var
  rumsg : string;
begin
  rumsg := CP1251ToUTF8(msg);
  MessageDlg(TCPErrorCaption, rumsg, mtError, [mbOK], 0);
  if not TCPClient.Connected then
    TCPClientDisconnect(aSocket);
end;

procedure TdmNetwork.TCPClientReceive(aSocket: TLSocket);
var
  buffer: array[0..4095] of byte;
  size: Integer;
begin
  repeat
    size := TCPClient.Get(buffer, 4096);
    if size > 0 then
      FReceiveQueue.Enqueue(buffer, size);
  until size <= 0;
  ProcessQueue;
end;

procedure TdmNetwork.tmNoOpStartTimer(Sender: TObject);
begin
  FLastPacket := Now;
end;

procedure TdmNetwork.tmNoOpTimer(Sender: TObject);
begin
  if SecondsBetween(FLastPacket, Now) > 25 then
    Send(TNoOpPacket.Create);
end;

procedure TdmNetwork.OnCanSend(ASocket: TLSocket);
var
  size: Integer;
begin
  while FSendQueue.Size > 0 do
  begin
    FLastPacket := Now;
    size := TCPClient.Send(FSendQueue.Memory^, FSendQueue.Size);
    if size > 0 then
      FSendQueue.Dequeue(size)
    else
      Break;
  end;
end;

procedure TdmNetwork.OnConnectionHandlingPacket(ABuffer: TEnhancedMemoryStream);
var
  subID: Byte;
  loginState: TLoginState;
  width, height: Word;
  flags: Cardinal;
  serverState: TServerState;
  date, time: TDateTime;
begin
  subID := ABuffer.ReadByte;
  case subID of
    $01:
      begin
        if (ABuffer.ReadCardinal - $1000) = ProtocolVersion then
        begin
          frmInitialize.lblStatus.Caption := frmInitialize.SplashAuthorization;
          Send(TLoginRequestPacket.Create(FUsername, FPassword));
        end else
        begin // sLineBreak
          MessageDlg(ErrorCaption, UnsuportedVersion, mtError, [mbOK], 0);
          Disconnect;
        end;
        Logger.Send([lcClient, lcInfo], 'Текущая версия протокола подтверждена');
      end;
    $03:
      begin
        loginState := TLoginState(ABuffer.ReadByte);
        if loginState = lsOK then
        begin
          frmInitialize.SetStatusLabel(frmInitialize.SplashInicialization);

          Application.ProcessMessages;
          FAccessLevel := TAccessLevel(ABuffer.ReadByte);
          FServerStart := IncSecond(Now, - ABuffer.ReadDWord);
          width  := ABuffer.ReadWord;
          height := ABuffer.ReadWord;
          flags  := ABuffer.ReadCardinal;

          // Для совместимости с сервером 0.7.0 (ранее тут слалось число преметов)
          if (flags and $FF000000) = 0 then // GameResourceManager.Tiledata.StaticCount
            if flags < $C000 then flags := $F0000000 else flags := $F0000008;

          if not InitGameResourceManager(FDataDir, Flags) then begin
            Logger.Send([lcClient, lcInfo], 'CentrED+ загрузка отменена, не та версия *.mul файлов.');
            Disconnect; exit;
          end;

          ResMan.InitLandscape(width, height);

          // Проверка обновлений
          frmInitialize.SetStatusLabel(frmInitialize.SplashUpdates);


          Logger.Send([lcClient, lcInfo], 'Начало загрузки CentrED+');
          frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['OpenGL Device']));
          ResMan.Landscape.UpdateWriteMap(ABuffer);                            Logger.Send([lcClient, lcInfo], 'ResMan.Landscape.UpdateWriteMap(ABuffer);');

          frmMain := TfrmMain.Create(dmNetwork);                               Logger.Send([lcClient, lcInfo], 'frmMain := TfrmMain.Create(dmNetwork);');

          frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['Windows Forms']));
          frmRadarMap := TfrmRadarMap.Create(frmMain);                         Logger.Send([lcClient, lcInfo], 'frmRadarMap := TfrmRadarMap.Create(frmMain);');
          frmLargeScaleCommand := TfrmLargeScaleCommand.Create(frmMain);       Logger.Send([lcClient, lcInfo], 'frmLargeScaleCommand := TfrmLargeScaleCommand.Create(frmMain);');
          frmRegionControl := TfrmRegionControl.Create(frmMain);               Logger.Send([lcClient, lcInfo], 'frmRegionControl := TfrmRegionControl.Create(frmMain);');
          frmAccountControl := TfrmAccountControl.Create(frmMain);             Logger.Send([lcClient, lcInfo], 'frmAccountControl := TfrmAccountControl.Create(frmMain);');
          frmEditAccount := TfrmEditAccount.Create(frmAccountControl);         Logger.Send([lcClient, lcInfo], 'frmEditAccount := TfrmEditAccount.Create(frmAccountControl);');
          frmConfirmation := TfrmConfirmation.Create(frmMain);                 Logger.Send([lcClient, lcInfo], 'frmConfirmation := TfrmConfirmation.Create(frmMain);');
          frmSelectionSettings := TfrmSelectionSettings.Create(frmMain);       Logger.Send([lcClient, lcInfo], 'frmSelectionSettings := TfrmSelectionSettings.Create(frmMain);');
          frmMoveSettings := TfrmMoveSettings.Create(frmMain);                 Logger.Send([lcClient, lcInfo], 'frmMoveSettings := TfrmMoveSettings.Create(frmMain);');
          frmElevateSettings := TfrmElevateSettings.Create(frmMain);           Logger.Send([lcClient, lcInfo], 'frmElevateSettings := TfrmElevateSettings.Create(frmMain);');
          frmSurfElevateSettings := TfrmSurfElevateSettings.Create(frmMain);   Logger.Send([lcClient, lcInfo], 'frmSurfElevateSettings := TfrmSurfElevateSettings.Create(frmMain);');
          frmSurfStretchSettings := TfrmSurfStretchSettings.Create(frmMain);   Logger.Send([lcClient, lcInfo], 'frmSurfStretchSettings := TfrmSurfStretchSettings.Create(frmMain);');
          frmSurfSmoothSettings := TfrmSurfSmoothSettings.Create(frmMain);     Logger.Send([lcClient, lcInfo], 'frmSurfSmoothSettings := TfrmSurfSmoothSettings.Create(frmMain);');
          frmDrawSettings := TfrmDrawSettings.Create(frmMain);                 Logger.Send([lcClient, lcInfo], 'frmDrawSettings := TfrmDrawSettings.Create(frmMain);');
          frmHueSettings := TfrmHueSettings.Create(frmMain);                   Logger.Send([lcClient, lcInfo], 'frmHueSettings := TfrmHueSettings.Create(frmMain);');
          frmFillSettings := TfrmFillSettings.Create(frmMain);                 Logger.Send([lcClient, lcInfo], 'frmFillSettings := TfrmFillSettings.Create(frmMain);');
          frmVirtualLayer := TfrmVirtualLayer.Create(frmMain);                 Logger.Send([lcClient, lcInfo], 'frmVirtualLayer := TfrmVirtualLayer.Create(frmMain)');
          frmBoundaries := TfrmBoundaries.Create(frmMain);                     Logger.Send([lcClient, lcInfo], 'frmBoundaries := TfrmBoundaries.Create(frmMain);');
          frmFilter := TfrmFilter.Create(frmMain);                             Logger.Send([lcClient, lcInfo], 'frmFilter := TfrmFilter.Create(frmMain);');
          frmLightlevel := TfrmLightlevel.Create(frmMain);                     Logger.Send([lcClient, lcInfo], 'frmLightlevel := TfrmLightlevel.Create(frmMain);');
          frmAbout := TfrmAbout.Create(frmMain);                               Logger.Send([lcClient, lcInfo], 'frmAbout := TfrmAbout.Create(frmMain);');
          frmMain.mnuTileListViewClick(nil);
          frmMain.Show;                                                        Logger.Send([lcClient, lcInfo], 'frmMain.Show;');
          frmInitialize.Hide;                                                  Logger.Send([lcClient, lcInfo], 'frmInitialize.Hide;');
          tmNoOp.Enabled := True;                                              Logger.Send([lcClient, lcInfo], 'tmNoOp.Enabled := True;');
          frmInitialize.SetStatusLabel(Format(frmInitialize.SplashLoading, ['Done']));
        end else
        begin
          if loginState = lsInvalidUser then
            MessageDlg(ErrorCaption, WrongAccount, mtWarning, [mbOK], 0)
          else if loginState = lsInvalidPassword then
            MessageDlg(ErrorCaption, WrongPassword, mtWarning, [mbOK], 0)
          else if loginState = lsAlreadyLoggedIn then
            MessageDlg(ErrorCaption, AlreadyLogined, mtWarning, [mbOK], 0)
          else if loginState = lsNoAccess then
            MessageDlg(ErrorCaption, NoAccess, mtWarning, [mbOK], 0);
        end;
        Logger.Send([lcClient, lcInfo], 'CentrED+ запущен.');
      end;
    $04: //Server state
      begin
        serverState := TServerState(ABuffer.ReadByte);
        if serverState = ssRunning then
        begin
          frmInitialize.UnsetModal;
          frmInitialize.Hide;
          tmNoOp.Enabled := True;
        end else
        begin
          case serverState of
            ssFrozen: frmInitialize.lblStatus.Caption := frmInitialize.SplashSuspend;
            ssOther: frmInitialize.lblStatus.Caption := ABuffer.ReadStringNull
          end;
          tmNoOp.Enabled := False;
          frmInitialize.Show;
          frmInitialize.SetModal;
        end;
      end;
  end;
end;

procedure TdmNetwork.ProcessQueue;
var
  packetHandler: TPacketHandler;
  size: Cardinal;
begin
  FReceiveQueue.Position := 0;
  while FReceiveQueue.Size >= 1 do
  begin
    packetHandler := PacketHandlers[FReceiveQueue.ReadByte];
    if packetHandler <> nil then
    begin
      size := packetHandler.PacketLength;
      if size = 0 then
      begin
        if FReceiveQueue.Size > 5 then
          size := FReceiveQueue.ReadCardinal
        else
          Break; //wait for more data
      end;

      if FReceiveQueue.Size >= size then
      begin
        FReceiveQueue.Lock(FReceiveQueue.Position, size - FReceiveQueue.Position); //prevent handler from reading too much
        packetHandler.Process(FReceiveQueue);
        FReceiveQueue.Unlock;
        FReceiveQueue.Dequeue(size);
      end else
        Break; //wait for more data
    end else
    begin
      {Writeln('Dropping client due to unknown packet: ', ANetState.Socket.PeerAddress);}
      Disconnect;
      FReceiveQueue.Clear;
    end;
  end;
end;

procedure TdmNetwork.DoLogin;
begin
  tmNoOp.Enabled := False;
  frmLogin := TfrmLogin.Create(dmNetwork);
  if frmInitialize = nil then
    frmInitialize := TfrmInitialize.Create(dmNetwork);

  FreeAndNil(frmEditAccount);
  FreeAndNil(frmAccountControl);
  FreeAndNil(frmConfirmation);
  FreeAndNil(frmSelectionSettings);
  FreeAndNil(frmMoveSettings);
  FreeAndNil(frmElevateSettings);
  FreeAndNil(frmSurfElevateSettings);
  FreeAndNil(frmSurfStretchSettings);
  FreeAndNil(frmSurfSmoothSettings);
  FreeAndNil(frmDrawSettings);
  FreeAndNil(frmHueSettings);
  FreeAndNil(frmFillSettings);
  FreeAndNil(frmVirtualLayer);
  FreeAndNil(frmBoundaries);
  FreeAndNil(frmFilter);
  FreeAndNil(frmLightlevel);
  FreeAndNil(frmAbout);
  FreeAndNil(frmRegionControl);
  FreeAndNil(frmLargeScaleCommand);
  FreeAndNil(frmRadarMap);

  if frmMain <> nil then
  begin
    frmMain.ApplicationProperties1.OnIdle := nil;
    FreeAndNil(frmMain);
  end;

  FreeAndNil(GameResourceManager);

  frmInitialize.Hide;
  while frmLogin.ShowModal = mrOK do
  begin
    LanguageTranslate(frmInitialize, self, nil);
    if TCPClient.Connect(frmLogin.edHost.Text, frmLogin.edPort.Value) then
    begin
      if frmLogin.cbProfile.ItemIndex > -1
        then FProfile := frmLogin.cbProfile.Text
        else FProfile := '---';
      FUsername := frmLogin.edUsername.Text;
      FPassword := frmLogin.edPassword.Text;
      FDataDir  := UTF8ToCP1251(frmLogin.edData.Text);
      frmInitialize.lblStatus.Caption := frmInitialize.SplashConnection;
      frmInitialize.Show;
      Break;
    end else
      MessageDlg(ErrorCaption, WrongServer, mtError, [mbOK], 0);
  end;
  frmLogin.Close;
  FreeAndNil(frmLogin);
end;

procedure TdmNetwork.Send(APacket: TPacket);
var
  source: TEnhancedMemoryStream;
begin
  if TCPClient.Connected then
  begin
    FSendQueue.Seek(0, soFromEnd);
    source := APacket.Stream;
    FSendQueue.CopyFrom(source, 0);
    OnCanSend(nil);
  end;
  APacket.Free;
end;

procedure TdmNetwork.Disconnect;
begin
  Send(TQuitPacket.Create);
end;

procedure TdmNetwork.CheckClose(ASender: TForm);
begin
  if ((frmLogin = nil) or (ASender = frmLogin)) and
     ((frmMain = nil) or (ASender = frmMain)) and
     ((frmInitialize = nil) or (not frmInitialize.Visible)) then
  begin
    Application.Terminate;
  end;
end;

initialization
  {$I UdmNetwork.lrs}

end.

