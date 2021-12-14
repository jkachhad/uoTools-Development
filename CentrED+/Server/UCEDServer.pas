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
 *      Portions Copyright 2007 Andreas Schneider
 *)
unit UCEDServer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lNet, UEnhancedMemoryStream, UConfig, ULandscape,
  UNetState, UPacket, dateutils, LConvEncoding, Language,
  {$IFDEF Linux}BaseUnix,{$ENDIF}
  {$IFDEF Windows}Windows,{$ENDIF}
  UPacketHandlers, UConnectionHandling;
  
type

  { TCEDServer }

  TCEDServer = class(TObject)
    constructor Create;
    destructor Destroy; override;
  protected
    FWorkStart: TDateTime;
    FLandscape: TLandscape;
    FTCPServer: TLTcp;
    FQuit: Boolean;
    FLastFlush: TDateTime;
    FValid: Boolean;
    procedure OnAccept(ASocket: TLSocket);
    procedure OnCanSend(ASocket: TLSocket);
    procedure OnDisconnect(ASocket: TLSocket);
    procedure OnReceive(ASocket: TLSocket);
    procedure OnError(const AError: string; ASocket: TLSocket);
    procedure ProcessBuffer(ANetState: TNetState);
    procedure CheckNetStates;
  public
    property WorkStart: TDateTime read FWorkStart;
    property Landscape: TLandscape read FLandscape;
    property TCPServer: TLTcp read FTCPServer;
    property Quit: Boolean read FQuit write FQuit;
    procedure Run;
    procedure SendPacket(ANetState: TNetState; APacket: TPacket;
      AFreePacket: Boolean = True);
    procedure Disconnect(ASocket: TLSocket);
  end;
  
var
  CEDServerInstance: TCEDServer;

implementation

uses
  Logging, UClientHandling;

{$I version.inc}

{$IFDEF Linux}
procedure OnSigInt(ASignal: cint); cdecl;
begin
  Writeln(TimeStamp, GetText('Aborting'));
  if CEDServerInstance <> nil then CEDServerInstance.Quit := True;
end;

procedure OnSigSegv(ASignal: cint); cdecl;
begin
  Writeln(TimeStamp, GetText('InternEr'));
  Halt;
  //if CEDServerInstance <> nil then CEDServerInstance.Quit := True;
end;
{$ENDIF}

{$IFDEF Windows}
function OnConsoleCtrlEvent(ACtrl: DWord): LongBool; stdcall; far;
begin
  Result := False;
  if (ACtrl = CTRL_C_EVENT) or (ACtrl = CTRL_BREAK_EVENT) then
  begin
    Writeln(TimeStamp, GetText('Aborting'));
    if CEDServerInstance <> nil then CEDServerInstance.Quit := True;
    Result := True;
  end;
end;
{$ENDIF}

{ TCEDServer }

constructor TCEDServer.Create;
begin
  inherited Create;
  FWorkStart := Now;
  FLandscape := TLandscape.Create(Config.Map.MapFile, Config.Map.StaticsFile,
    Config.Map.StaIdxFile, Config.Tiledata, Config.Radarcol, Config.Map.Width,
    Config.Map.Height, Config.Map.FormatFlags, FValid);
  FTCPServer := TLTcp.Create(nil);
  FTCPServer.OnAccept := @OnAccept;
  FTCPServer.OnCanSend := @OnCanSend;
  FTCPServer.OnDisconnect := @OnDisconnect;
  FTCPServer.OnReceive := @OnReceive;
  FTCPServer.OnError := @OnError;
  FQuit := False;
  FLastFlush := Now;
end;

destructor TCEDServer.Destroy;
begin
  if FTCPServer <> nil then
  begin
    FTCPServer.IterReset;
    if FTCPServer.Iterator <> nil then
      while FTCPServer.IterNext do
      begin
        FTCPServer.Iterator.Disconnect;
        if FTCPServer.Iterator.UserData <> nil then
        begin
          TObject(FTCPServer.Iterator.UserData).Free;
          FTCPServer.Iterator.UserData := nil;
        end;
      end;
    FreeAndNil(FTCPServer);
  end;
  if FLandscape <> nil then FreeAndNil(FLandscape);
  inherited Destroy;
end;

procedure TCEDServer.OnAccept(ASocket: TLSocket);
begin
  writeln(TimeStamp, GetText('Connects') + ' ', ASocket.PeerAddress);
  ASocket.UserData := TNetState.Create(ASocket);
  SendPacket(TNetState(ASocket.UserData), TProtocolVersionPacket.Create(ProtocolVersion));
end;

procedure TCEDServer.OnCanSend(ASocket: TLSocket);
var
  netState: TNetState;
  size: Integer;
begin
  //writeln('CanSend: ', ASocket.PeerAddress);
  netState := TNetState(ASocket.UserData);
  if netState = nil then Exit;
  while netState.SendQueue.Size > 0 do
  begin
    size := FTCPServer.Send(netState.SendQueue.Memory^, netState.SendQueue.Size, ASocket);
    if size > 0 then
      netState.SendQueue.Dequeue(size)
    else
      Break;
  end;
end;

procedure TCEDServer.OnDisconnect(ASocket: TLSocket);
var
  netState: TNetState;
begin
  writeln(TimeStamp, GetText('ConLosts') + ' ', ASocket.PeerAddress);
  if ASocket.UserData <> nil then
  begin
    netState := TNetState(ASocket.UserData);
    ASocket.UserData := nil;
    {$IFDEF NetLog}
    if netState.Account <> nil
      then writeln(TimeStamp, '$OnDisconnect("',netState.Account.Name,'")')
      else writeln(TimeStamp, '$OnDisconnect("UNKNOWN")')
    {$ENDIF}
    if netState.Account <> nil then
      SendPacket(nil, TClientDisconnectedPacket.Create(netState.Account.Name));
    netState.Free;
  end;
end;

procedure TCEDServer.OnReceive(ASocket: TLSocket);
var
  netState: TNetState;
  buffer: array[0..4095] of byte;
  size: Integer;
begin
  netState := TNetState(ASocket.UserData);
  if netState <> nil then
  begin
    repeat
      size := FTCPServer.Get(buffer, 4096, ASocket);
      if size > 0 then
        netState.ReceiveQueue.Enqueue(buffer, size);
    until size <= 0;
    ProcessBuffer(netState);
  end;
end;

procedure TCEDServer.OnError(const AError: string; ASocket: TLSocket);
begin
  writeln(TimeStamp, GetText('ErrorLbl') + ' ', ASocket.PeerAddress, ' :: ', TranslateTextA(AError));
  //OnDisconnect(ASocket);
  ASocket.Disconnect(True);
end;

procedure TCEDServer.ProcessBuffer(ANetState: TNetState);
var
  buffer: TEnhancedMemoryStream;
  packetID: Byte;
  packetHandler: TPacketHandler;
  size: Cardinal;
begin
  try
    buffer := ANetState.ReceiveQueue;
    buffer.Position := 0;
    while (buffer.Size >= 1) and ANetState.Socket.Connected do
    begin
      packetID := buffer.ReadByte;
      {$IFDEF NetLog}
      if (ANetState.Account <> nil)
        then writeln(TimeStamp, Format('NetState: [0x%.2x] <<-- "%s"', [packetID, ANetState.Account.Name]))
        else writeln(TimeStamp, Format('NetState: [0x%.2x] <<-- "NEW (%s:%d)"', [packetID, ANetState.Socket.PeerAddress, ANetState.Socket.PeerPort]));
      {$ENDIF}

      packetHandler := PacketHandlers[packetID];
      if packetHandler <> nil then
      begin
        ANetState.LastAction := Now;
        size := packetHandler.PacketLength;
        if size = 0 then
        begin
          if buffer.Size > 5 then
            size := buffer.ReadCardinal
          else
            Break; //wait for more data
        end;

        if buffer.Size >= size then
        begin
          buffer.Lock(buffer.Position, size - buffer.Position); //prevent handler from reading too much
          packetHandler.Process(buffer, ANetState);
          buffer.Unlock;
          buffer.Dequeue(size);
        end else
          Break; //wait for more data
      end else
      begin
        Writeln(TimeStamp, GetText('UnkPack1'), packetID, GetText('UnkPack2') + ' ', ANetState.Socket.PeerAddress);
        Disconnect(ANetState.Socket);
        buffer.Clear;
      end;
    end;
    ANetState.LastAction := Now;
  except
    on E: Exception do
    begin
      Logger.SendException([lcServer], 'Error processing buffer', E);
      Writeln(TimeStamp, GetText('BufferEr') + ' ', ANetState.Socket.PeerAddress);
    end;
  end;
end;

procedure TCEDServer.CheckNetStates;
var
  netState: TNetState;
begin
  FTCPServer.IterReset;
  while FTCPServer.IterNext do
  begin
    netState := TNetState(FTCPServer.Iterator.UserData);
    if netState <> nil then
    begin
      if FTCPServer.Iterator.Connected then
      begin
        if (SecondsBetween(netState.LastAction, Now) > 120) then
        begin
          if netState.Account <> nil then
            Writeln(TimeStamp, GetText('TimeOuts') + ' ', netState.Account.Name, ' (', netState.Socket.PeerAddress, ')')
          else
            Writeln(TimeStamp, GetText('TimeOuts') + ' ', netState.Socket.PeerAddress);
          Disconnect(netState.Socket);
        end;
      end else   {TODO : Unnecessary ...}
      begin
        {$IFDEF NetLog}
        Writeln(TimeStamp, GetText('$CheckNetStates - OnDisconnect'));
        {$ENDIF}
        OnDisconnect(FTCPServer.Iterator);
      end;
    end;
  end;
end;

procedure TCEDServer.Run;
begin
  if not FValid then
  begin
    Writeln(TimeStamp, GetText('BadFacet'));
    Exit;
  end;

  if FTCPServer.Listen(Config.Port) then
  begin
    repeat
      FTCPServer.CallAction;
      CheckNetStates;
      if SecondsBetween(FLastFlush, Now) >= 60 then
      begin
        FLandscape.Flush;
        Config.Flush;
        FLastFlush := Now;
      end;
      Sleep(1);
    until FQuit;
  end;
end;

procedure TCEDServer.SendPacket(ANetState: TNetState; APacket: TPacket;
  AFreePacket: Boolean = True);
var
  netState: TNetState;
begin
  if ANetState <> nil then
  begin
    {$IFDEF NetLog}
    if (ANetState.Account <> nil)
      then writeln(TimeStamp, Format('NetState: [0x%.2x] -->> "%s"', [APacket.PacketID, ANetState.Account.Name]))
      else writeln(TimeStamp, Format('NetState: [0x%.2x] -->> "NEW (%s:%d)"', [APacket.PacketID, ANetState.Socket.PeerAddress, ANetState.Socket.PeerPort]));
    {$ENDIF}

    ANetState.SendQueue.Seek(0, soFromEnd);
    ANetState.SendQueue.CopyFrom(APacket.Stream, 0);
    OnCanSend(ANetState.Socket);
  end else //broadcast
  begin
    {$IFDEF NetLog}
    write(TimeStamp, Format('NetState: [0x%.2x] -->> "BROADCAST: ', [APacket.PacketID]));
    {$ENDIF}
    FTCPServer.IterReset;
    while FTCPServer.IterNext do
    begin
      netState := TNetState(FTCPServer.Iterator.UserData);
      if (netState <> nil) and (FTCPServer.Iterator.Connected) then
      begin
        {$IFDEF NetLog}
        write('.');
        {$ENDIF}
        netState.SendQueue.Seek(0, soFromEnd);
        netState.SendQueue.CopyFrom(APacket.Stream, 0);
        OnCanSend(netState.Socket);
      end;
    end;
    {$IFDEF NetLog}
    writeln(' "');
    {$ENDIF}
  end;
  if AFreePacket then
    APacket.Free;
end;

procedure TCEDServer.Disconnect(ASocket: TLSocket);
begin
  if ASocket.Connected then
  begin
    ASocket.Disconnect;
    //OnDisconnect(ASocket);
    //Handling of the disconnect is done in CheckNetStates after each CallAction
  end;
end;

initialization
{$IFDEF Linux}
  FpSignal(SIGINT, @OnSigInt);
  FpSignal(SIGTERM, @OnSigInt); //SIGTERM should shutdown the server cleanly too
  //FpSignal(SIGSEGV, @OnSigSegv);
{$ENDIF}
{$IFDEF Windows}
  SetConsoleCtrlHandler(@OnConsoleCtrlEvent, True);
{$ENDIF}

end.

