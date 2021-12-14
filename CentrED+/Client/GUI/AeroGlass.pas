unit AeroGlass;
 
{$mode delphi}
//{$mode objfpc}{$H+}
 
interface
 
uses
  //Windows, Forms, Graphics;
  // os
  Windows, UxTheme, ShellAPI, Win32Proc, Win32Extra,
  // rtl
  Classes, SysUtils,
  // lcl
  Forms, Controls, Graphics, Themes;//, LCLProc, LCLType;

type
  _MARGINS = packed record
    cxLeftWidth    : Integer;
    cxRightWidth   : Integer;
    cyTopHeight    : Integer;
    cyBottomHeight : Integer;
  end;
 
  PMargins = ^_MARGINS;
  TMargins = _MARGINS;
 
  DwmIsCompositionEnabledFunc      = function(pfEnabled: PBoolean): HRESULT; stdcall;
  DwmExtendFrameIntoClientAreaFunc = function(destWnd: HWND; const pMarInset: PMargins): HRESULT; stdcall;
  SetLayeredWindowAttributesFunc   = function(destWnd: HWND; cKey: TColor; bAlpha: Byte; dwFlags: DWord): BOOL; stdcall;
 
const
  WS_EX_LAYERED = $80000;
  LWA_COLORKEY  = 1;
 
procedure GlassFormEx(frm: TForm; tmpMargins: TMargins; cBlurColorKey: TColor = clFuchsia);
procedure GlassForm(frm: TForm; cBlurColorKey: TColor = clFuchsia);
function WindowsAeroGlassCompatible: Boolean;

function CreateBitmap32(DC: HDC; W, H: Integer; var BitmapBits: Pointer): HBITMAP;
procedure DrawAlphaText(wnd: hwnd; DC: HDC; x,y: integer; txt: WideString);


implementation

// =============================================================================
// == Преобразование формы в AeroGlass
// =============================================================================

function WindowsAeroGlassCompatible: Boolean;
var
  osVinfo: TOSVERSIONINFO;
begin
  ZeroMemory(@osVinfo, SizeOf(osVinfo));
  OsVinfo.dwOSVersionInfoSize := SizeOf(TOSVERSIONINFO);
  if (
  (GetVersionEx(osVInfo)   = True) and
  (osVinfo.dwPlatformId    = VER_PLATFORM_WIN32_NT) and
  (osVinfo.dwMajorVersion >= 6)
  )
  then Result:=True
  else Result:=False;
end;
 
procedure GlassFormEx(frm: TForm; tmpMargins: TMargins; cBlurColorKey: TColor = clFuchsia);
var
  hDwmDLL: Cardinal;
  fDwmIsCompositionEnabled: DwmIsCompositionEnabledFunc;
  fDwmExtendFrameIntoClientArea: DwmExtendFrameIntoClientAreaFunc;
  fSetLayeredWindowAttributesFunc: SetLayeredWindowAttributesFunc;
  bCmpEnable: Boolean;
  mgn: TMargins;
begin
  { Continue if Windows version is compatible }
  if WindowsAeroGlassCompatible then begin
    { Continue if 'dwmapi' library is loaded }
    hDwmDLL := LoadLibrary('dwmapi.dll');
    if hDwmDLL <> 0 then begin
      { Get values }
      @fDwmIsCompositionEnabled        := GetProcAddress(hDwmDLL, 'DwmIsCompositionEnabled');
      @fDwmExtendFrameIntoClientArea   := GetProcAddress(hDwmDLL, 'DwmExtendFrameIntoClientArea');
      @fSetLayeredWindowAttributesFunc := GetProcAddress(GetModulehandle(user32), 'SetLayeredWindowAttributes');
      { Continue if values are <> nil }
      if (
      (@fDwmIsCompositionEnabled <> nil) and
      (@fDwmExtendFrameIntoClientArea <> nil) and
      (@fSetLayeredWindowAttributesFunc <> nil)
      )
      then begin
        { Continue if composition is enabled }
        fDwmIsCompositionEnabled(@bCmpEnable);
        if bCmpEnable = True then begin
          { Set Form Color same as cBlurColorKey }
          frm.Color := cBlurColorKey;
          { ... }
          SetWindowLong(frm.Handle, GWL_EXSTYLE, GetWindowLong(frm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
          { ... }
          fSetLayeredWindowAttributesFunc(frm.Handle, cBlurColorKey, 0, LWA_COLORKEY);
          { Set margins }
          ZeroMemory(@mgn, SizeOf(mgn));
          mgn.cxLeftWidth    := tmpMargins.cxLeftWidth;
          mgn.cxRightWidth   := tmpMargins.cxRightWidth;
          mgn.cyTopHeight    := tmpMargins.cyTopHeight;
          mgn.cyBottomHeight := tmpMargins.cyBottomHeight;
          { Extend Form }
          fDwmExtendFrameIntoClientArea(frm.Handle,@mgn);
        end;
      end;
      { Free loaded 'dwmapi' library }
      FreeLibrary(hDWMDLL);
    end;
  end;
end;

procedure GlassForm(frm: TForm; cBlurColorKey: TColor = clFuchsia);
var
  tmpMargins: TMargins;
begin
  { If all margins are -1 the whole form will be aero glass}
  tmpMargins.cxLeftWidth    := 8;
  tmpMargins.cxRightWidth   := 8;
  tmpMargins.cyBottomHeight := 25;
  tmpMargins.cyTopHeight    := 4;
  { FormName ; Margins ; TransparentColor }
  GlassFormEx(frm, tmpMargins, cBlurColorKey);
end;

// =============================================================================
// == Вывод текста и изображений на форме AeroGlass
// =============================================================================

function CreateBitmap32(DC: HDC; W, H: Integer; var BitmapBits: Pointer): HBITMAP;
var
  bi: BITMAPINFO;
begin
  ZeroMemory(@bi, sizeof(BITMAPINFO));
  with bi.bmiHeader do
  begin
    biSize := sizeof(BITMAPINFOHEADER);
    biWidth := W;
    biHeight := -H;
    biCompression := BI_RGB;
    biBitCount := 32;
    biPlanes := 1;
    biXPelsPerMeter := 0;
    biYPelsPerMeter := 0;
    biClrUsed := 0;
    biClrImportant := 0;
  end;
  Result := CreateDIBSection(DC, bi, DIB_RGB_COLORS, BitmapBits, 0, 0);
end;

type
  TDTTOpts = record
    dwSize: Longword;
    dwFlags: Longword;
    crText: Longword;
    crBorder: Longword;
    crShadow: Longword;
    eTextShadowType: Integer;
    ptShadowOffset: TPoint;
    iBorderSize: Integer;
    iFontPropId: Integer;
    iColorPropId: Integer;
    iStateId: Integer;
    fApplyOverlay: Integer;
    iGlowSize: Integer;
    pfnDrawTextCallback: Pointer;
    lParam: Integer;
  end;

var
  hTheme: THandle;

procedure DrawAlphaText(wnd: hwnd; DC: HDC; x,y: integer; txt: WideString);
var
  tr: trect;
  txtOptions: TDTTOPTS;
  hBmp: HBITMAP;
  hBmpDC: HDC;
  hFnt: HFont;
  p: pointer;
  ts: SIZE;
begin
  hTheme := OpenThemeData(wnd, 'window');

  hBmpDC := CreateCompatibleDC(0);

  hFnt := CreateFont(-MulDiv(10, GetDeviceCaps(hBmpDC, LOGPIXELSY), 72), 0, 0, 0, FW_BOLD {FW_NORMAL}, 0, 0, 0,
     DEFAULT_CHARSET, OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, DEFAULT_QUALITY, DEFAULT_PITCH, 'arial');

  SelectObject(hBmpDC, hFnt);

  GetTextExtentPointW(hBmpDC, PWideChar(txt), length(txt), ts);

  SetRect(tr, 0, 0, ts.cx + 5, ts.cy + 5);

  hBmp := CreateBitmap32(hBmpDC, tr.Right, tr.Bottom, p);
  SelectObject(hBmpDC, hBmp);

  ZeroMemory(@txtOptions, sizeof(TDTTOPTS));
  txtOptions.dwSize := sizeof(TDTTOPTS);
  txtOptions.dwFlags := DTT_COMPOSITED or DTT_GLOWSIZE or DTT_TEXTCOLOR;
  txtOptions.iGlowSize := 5;
  txtOptions.crText := $00FF0000;

  DrawThemeTextEx(hTheme, hBmpDC, 0, 0, PWideChar(txt), length(txt), DT_SINGLELINE or DT_vCENTER, @tr, @txtOptions);

  BitBlt(dc, x, y, tr.Right, tr.Bottom, hBmpDC, 0, 0, SRCCOPY);

  DeleteObject(hBmpDC);
  DeleteObject(hBmp);
  DeleteObject(hFnt);

  CloseThemeData(hTheme);
end;





end.
