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
unit UfrmRadar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ImagingClasses, ImagingComponents, ImagingTypes, UEnhancedMemoryStream, crc,
  StdCtrls, LConvEncoding, Logging, LCLIntf;

type

  TRadarColorMap = array of Word;

  { TfrmRadarMap }

  TfrmRadarMap = class(TForm)
    cbStayOnTop: TCheckBox;
    lblPosition: TLabel;
    pbRadar: TPaintBox;
    pnlBottom: TPanel;
    sbMain: TScrollBox;
    procedure cbStayOnTopChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure pbRadarMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbRadarMouseLeave(Sender: TObject);
    procedure pbRadarMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbRadarPaint(Sender: TObject);
  protected
    FRadar: TSingleImage;
    FRadarDependencies: TList;
    procedure OnRadarHandlingPacket(ABuffer: TEnhancedMemoryStream);
    procedure RefreshRadar(ARadarMap: TRadarColorMap);
    procedure RepaintRadar;
  public
    property Radar: TSingleImage read FRadar;
    property Dependencies: TList read FRadarDependencies;
  end; 

var
  frmRadarMap: TfrmRadarMap;
  FormMaxWidthConst : Integer;
  FormMaxHeightConst: Integer;

implementation

uses
  UdmNetwork, UGameResources, UPacketHandlers, UPackets, UfrmInitialize,
  UfrmMain, UfrmLogin, UGraphicHelper, Language;

{ TfrmRadarMap }
//{$I winapi.inc}

procedure TfrmRadarMap.FormCreate(Sender: TObject);
var IntfWidth, IntfHeight: integer;
begin
  LanguageTranslate(Self);

  FRadar := TSingleImage.CreateFromParams(ResMan.Landscape.Width,
    ResMan.Landscape.Height, ifA8R8G8B8);
  pbRadar.Width := FRadar.Width;
  pbRadar.Height := FRadar.Height;
  sbMain.ClientWidth := FRadar.Width;
  sbMain.ClientHeight := FRadar.Height;
  ClientWidth := sbMain.Width;// + sbMain.VertScrollBar.Size;
  ClientHeight := sbMain.Height + pnlBottom.Height;// + sbMain.HorzScrollBar.Size;
  Constraints.MaxWidth  := Width;
  Constraints.MaxHeight := Height;

  //LCLIntf.GetWindowSize(Handle, IntfWidth, IntfHeight); Screen.Width
  if (Width >= frmMain.Width) then begin
    Left  := frmMain.Left;
    Width := frmMain.Width;
  end;
  if (Height >= frmMain.Height) then begin
    Top   := frmMain.Top;
    Height:= frmMain.Height;
  end;

  //sbMain.HorzScrollBar.Increment:=8;
  //sbMain.VertScrollBar.Increment:=8;
  sbMain.HorzScrollBar.Range := FRadar.Width;
  sbMain.VertScrollBar.Range := FRadar.Height;

  FormMaxWidthConst  := Constraints.MaxWidth;
  FormMaxHeightConst := Constraints.MaxHeight;
  cbStayOnTopChange(Sender);

  FRadarDependencies := TList.Create;
    
  RegisterPacketHandler($0D, TPacketHandler.Create(0, @OnRadarHandlingPacket));
    
  dmNetwork.Send(TRequestRadarChecksumPacket.Create);
end;

procedure TfrmRadarMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caHide;
end;

procedure TfrmRadarMap.cbStayOnTopChange(Sender: TObject);
begin
  if cbStayOnTop.Checked
    then FormStyle := fsStayOnTop
    else FormStyle := fsNormal;
end;

procedure TfrmRadarMap.FormDestroy(Sender: TObject);
var
  confdir : string;
  radarMap: TRadarColorMap;
  x, y: Integer;
  radarMapFile: TFileStream;
begin
  RegisterPacketHandler($0D, nil);

  SetLength(radarMap, FRadar.Width * FRadar.Height);
  for x := 0 to FRadar.Width - 1 do
    for y := 0 to FRadar.Height - 1 do
      radarMap[x * FRadar.Height + y] := EncodeUOColor(PInteger(FRadar.PixelPointers[x, y])^);

  if (sprofile <> '') then
    if (frmMain.ProfileDir <> '')
      then confdir := frmMain.ProfileDir
      else confdir := frmMain.ConfigDir;

  radarMapFile := TFileStream.Create(confdir + 'RadarMap.cache', fmCreate);
  radarMapFile.Write(radarMap[0], Length(radarMap) * SizeOf(Word));
  radarMapFile.Free;

  if FRadarDependencies <> nil then FreeAndNil(FRadarDependencies);
  if FRadar <> nil then FreeAndNil(FRadar);
end;

procedure TfrmRadarMap.FormResize(Sender: TObject);
var hScroll, vScroll : Boolean;
begin
  if (Width > FormMaxWidthConst-10) and (Height > FormMaxHeightConst-10) then begin
    Width  := FormMaxWidthConst;
    Height := FormMaxHeightConst;
    Constraints.MaxWidth  := FormMaxWidthConst;
    Constraints.MaxHeight := FormMaxHeightConst;
  end;

  hScroll := (Width  < Constraints.MaxWidth);
  vScroll := (Height < Constraints.MaxHeight);

  if hScroll <> sbMain.HorzScrollBar.Visible then begin
    if hScroll
    then Constraints.MaxHeight := FormMaxHeightConst + sbMain.HorzScrollBar.Size
    else Constraints.MaxHeight := FormMaxHeightConst;
    sbMain.HorzScrollBar.Visible := hScroll;
    //FormResize(Sender); // Повторный вызов, для коррекции
  end;

  if vScroll <> sbMain.VertScrollBar.Visible then begin
    if vScroll
    then Constraints.MaxWidth  := FormMaxWidthConst  + sbMain.VertScrollBar.Size
    else Constraints.MaxWidth  := FormMaxWidthConst;
    sbMain.VertScrollBar.Visible := vScroll;
    FormResize(Sender); exit; // Повторный вызов, для коррекции
  end;

  sbMain.HorzScrollBar.Page  := sbMain.ClientWidth;
  sbMain.VertScrollBar.Page  := sbMain.ClientHeight;
end;

procedure TfrmRadarMap.pbRadarMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  frmMain.SetPos(X * 8, Y * 8);
end;

procedure TfrmRadarMap.pbRadarMouseLeave(Sender: TObject);
begin
  lblPosition.Caption := '';
end;

procedure TfrmRadarMap.pbRadarMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lblPosition.Caption := Format('X: %d, Y: %d', [X * 8, Y * 8]);
end;

procedure TfrmRadarMap.pbRadarPaint(Sender: TObject);
var
  posX, posY: Word;
begin
  DisplayImage(pbRadar.Canvas, 0, 0, FRadar);
  posX := frmMain.X div 8;
  posY := frmMain.Y div 8;
  pbRadar.Canvas.Pen.Color := clBlack;
  pbRadar.Canvas.Pen.Style := psSolid;
  pbRadar.Canvas.Brush.Color := clRed;
  pbRadar.Canvas.Brush.Style := bsSolid;
  pbRadar.Canvas.Ellipse(posX - 3, posY - 3, posX + 3, posY + 3);
  {pbRadar.Canvas.Pen.Color := clRed;
  pbRadar.Canvas.Pen.Style := psDash;
  pbRadar.Canvas.Line(0, posY, pbRadar.Width, posY);
  pbRadar.Canvas.Line(posX, 0, posX, pbRadar.Height);}
end;

procedure TfrmRadarMap.OnRadarHandlingPacket(ABuffer: TEnhancedMemoryStream);
var
  confdir: string;
  subID: Byte;
  checksum, realChecksum: Cardinal;
  radarMapFile: TFileStream;
  radarMapFileName: string;
  radarMap: TRadarColorMap;
  x, y: Integer;
begin
  subID := ABuffer.ReadByte;
  case subID of
    $01: //checksum
      begin
        checksum := ABuffer.ReadCardinal;
        realChecksum := crc32(0, nil, 0);
        if (sprofile <> '') then
          if (frmMain.ProfileDir <> '')
            then confdir := frmMain.ProfileDir
            else confdir := frmMain.ConfigDir;
        //if (sprofile <> '')
        //  then confdir := GetAppConfigDir(False) + 'Profiles' + PathDelim + UTF8ToCP1251(sprofile) + PathDelim
        //  else confdir := GetAppConfigDir(False);
        radarMapFileName := confdir + 'RadarMap.cache';
        if FileExists(radarMapFileName) then
        begin
          radarMapFile := TFileStream.Create(radarMapFileName, fmOpenRead);
          SetLength(radarMap, radarMapFile.Size div SizeOf(Word));
          radarMapFile.Read(radarMap[0], radarMapFile.Size);
          radarMapFile.Free;
          
          realChecksum := crc32(realChecksum, @radarMap[0], Length(radarMap) * SizeOf(Word));
        end;
        
        if checksum <> realChecksum then
        begin
          frmInitialize.lblStatus.Caption := frmInitialize.SplashUpdatingMiniMap;
          frmInitialize.Show;
          frmInitialize.SetModal;
          //frmMain.Enabled := False;
          dmNetwork.Send(TRequestRadarMapPacket.Create);
        end else
          RefreshRadar(radarMap);
      end;
    $02: //radar map
      begin
        SetLength(radarMap, (ABuffer.Size - ABuffer.Position) div SizeOf(Word));
        ABuffer.Read(radarMap[0], Length(radarMap) * SizeOf(Word));
        RefreshRadar(radarMap);
        //frmMain.Enabled := True;
        frmInitialize.UnsetModal;
        frmInitialize.Hide;
      end;
    $03: //update radar
      begin
        x := ABuffer.ReadWord;
        y := ABuffer.ReadWord;
        PInteger(FRadar.PixelPointers[x, y])^ := DecodeUOColor(ABuffer.ReadWord);
        RepaintRadar;
      end;
  end;
end;

procedure TfrmRadarMap.RefreshRadar(ARadarMap: TRadarColorMap);
var
  x, y: Integer;
begin
  for x := 0 to FRadar.Width - 1 do
    for y := 0 to FRadar.Height - 1 do
      PInteger(FRadar.PixelPointers[x, y])^ := DecodeUOColor(ARadarMap[x * FRadar.Height + y]);
  RepaintRadar;
end;

procedure TfrmRadarMap.RepaintRadar;
var
  i: Integer;
begin
  pbRadar.Repaint;
  for i := 0 to FRadarDependencies.Count - 1 do
    TWinControl(FRadarDependencies.Items[i]).Repaint;
end;

initialization
  {$I UfrmRadar.lrs}

end.

