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
program CentrED;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils,
  Interfaces, // this includes the LCL widgetset
  LCLType, Forms, Dialogs, UdmNetwork, UResourceManager, Graphics;

{$R CentrED.res}
//{$R CentrED.manifest.rc}

function GetApplicationName: String;
begin
  Result := 'CentrED-plus';
end;

function LoadCursorFromRes(AFileName: string; CurFormat: Boolean = True): HCURSOR;
type
  TIconHeader = packed record
    hReserved: WORD; // = 0
    hType:     WORD; // ICO = 1, CUR = 2
    hCount:    WORD;
  end;
  TIconInfo = packed record
    iWidth:    BYTE;
    iHeight:   BYTE;
    iColors:   BYTE;
    iReserved: BYTE; // = 0
    iHotspotX: WORD; // CUR file (/iPlanes for ICO)
    iHotspotY: WORD; // CUR file (/iBpp for ICO)
    iSize:     Cardinal;
    iOffset:   Cardinal;
  end;
var
  stream: TStream;
  (*dwSize: Integer;
  buffer: array of Byte;
  header: TIconHeader;
  icoinf: TIconInfo;  *) //TODO Cleanup
  cursorImage: TCursorImage;
begin
  stream := ResourceManager.GetResource(AFileName);
  (*dwSize := stream.Size;
  SetLength(buffer, dwSize + 8);
  stream.Read(buffer[0], dwSize);
  if not CurFormat then
    Result := HCURSOR(CreateIconFromResource(@buffer[0], dwSize, False, $00030000))
  else begin
    CopyMemory(@header, @buffer[0], SizeOf(TIconHeader));
    // Грузим последнее изображение, которое обычно должно являться первым и единственным в иконках
    CopyMemory(@icoinf, @buffer[SizeOf(TIconHeader) + (header.hCount - 1) * SizeOf(TIconInfo)], SizeOf(TIconInfo));
    CopyMemory(@buffer[icoinf.iOffset-4], @icoinf.iHotspotX, 2);
    CopyMemory(@buffer[icoinf.iOffset-2], @icoinf.iHotspotY, 2);
    Result := HCURSOR(CreateIconFromResource(@buffer[icoinf.iOffset-4], icoinf.iSize+4, False, $00030000))
  end;*)
  try
    cursorImage := TCursorImage.Create;
    cursorImage.LoadFromStream(stream);
    Result := cursorImage.ReleaseHandle;
  finally
    cursorImage.Free;
  end;
end;

begin
  Application.Title:='CentrED+';
  OnGetApplicationName := @GetApplicationName;
  Application.Initialize;
  {if LowerCase(ChangeFileExt(ExtractFileName(ParamStr(0)), '')) <> LowerCase(GetApplicationName) then
  begin
    MessageDlg('Bug', 'The executable file has been renamed. Rename ' +
                         'file as follows: "' +
                         GetApplicationName + '.exe"', mtError, [mbOK], 0);
    Application.Terminate;
  end;} //TODO Why?

  if Paramcount = 1 then begin
    MessageDlg('Startup options', ParamStr(1), mtError, [mbOK], 0);
  end;


  // Loading cursors ...
  Screen.Cursors[-02] := LoadCursorFromRes('Cursors/BC_NormalSelect.cur');         //crArrow
  Screen.Cursors[-19] := LoadCursorFromRes('Cursors/BC_WorkingInBackground.cur');  //crAppStart
  Screen.Cursors[-20] := LoadCursorFromRes('Cursors/BC_HelpSelect.cur');           //crHelp
  //Screen.Cursors[-12] := LoadCursorFromRes('Cursors/.cur');           //crDrag
  //Screen.Cursors[-16] := LoadCursorFromRes('Cursors/.cur');           //crMultiDrag
  //Screen.Cursors[-13] := LoadCursorFromRes('Cursors/.cur');           //crNoDrop

  Screen.Cursors[-03] := LoadCursorFromRes('Cursors/BC_PrecisionSelect.cur');      //crCross
  Screen.Cursors[-04] := LoadCursorFromRes('Cursors/BC_TextSelect.cur');           //crIBeam
  //TODO Screen.Cursors[-11] := LoadCursorFromRes('Cursors/BI_Busy.ani', False);          //crHourGlass
  Screen.Cursors[-18] := LoadCursorFromRes('Cursors/TN_Unavailable.cur');          //crNo

  Screen.Cursors[-22] := LoadCursorFromRes('Cursors/BC_Move.cur');                 //crSize
  Screen.Cursors[-06] := LoadCursorFromRes('Cursors/BC_DiagonalResize2.cur');      //crSizeNESW
  Screen.Cursors[-07] := LoadCursorFromRes('Cursors/BC_VerticalResize.cur');       //crSizeNS
  Screen.Cursors[-08] := LoadCursorFromRes('Cursors/BC_DiagonalResize1.cur');      //crSizeNWSE
  Screen.Cursors[-09] := LoadCursorFromRes('Cursors/BC_HorizontalResize.cur');     //crSizeWE
  Screen.Cursors[-10] := LoadCursorFromRes('Cursors/BC_AlternateSelect.cur');      //crUpArrow
  //Screen.Cursors[-14] := LoadCursorFromRes('Cursors/.cur');           //crHSplit
  //Screen.Cursors[-15] := LoadCursorFromRes('Cursors/.cur');           //crVSplit

  Screen.Cursors[-21] := LoadCursorFromRes('Cursors/TN_LinkSelect.cur');           //crHandPoint
  Screen.Cursors[+01] := LoadCursorFromRes('Cursors/UO_Precision.cur');
  Screen.Cursors[+02] := LoadCursorFromRes('Cursors/UO_AttackMode.cur');
  Screen.Cursors[+03] := LoadCursorFromRes('Cursors/UO_Gauntlet.cur');

  // Run the program...
  Application.CreateForm(TdmNetwork, dmNetwork);
  Application.Run;
end.

