unit GlowLabel;
      {******************************************************************}
      { GlowLabel                                                        }
      {                                                                  }
      { home page : http://www.winningcubed.de                           }
      { email     : martin.walter@winningcubed.de                        }
      {                                                                  }
      { date      : 15-04-2007                                           }
      {                                                                  }
      { version   : 1.0                                                  }
      {                                                                  }
      { Use of this file is permitted for commercial and non-commercial  }
      { use, as long as the author is credited.                          }
      { This file (c) 2007 Martin Walter                                 }
      {                                                                  }
      { This Software is distributed on an "AS IS" basis, WITHOUT        }
      { WARRANTY OF ANY KIND, either express or implied.                 }
      {                                                                  }
      { *****************************************************************}


interface

uses
  Windows, Classes, StdCtrls {$IFDEF USETNT}, TntStdCtrls{$ENDIF};

type
  TCustomGlowLabel = class({$IFDEF USETNT}TTntCustomLabel{$ELSE}TCustomLabel{$ENDIF})
  private
    FGlow: Boolean;
    FGlowSize: Integer;
    FOldGlowSize: Integer;
    FBoundsWithGlow: Boolean;
    procedure SetGlow(const Value: Boolean);
    procedure SetGlowSize(const Value: Integer);

    function IsGlow: Boolean;
    function GetExpansion(GlowSize: Integer): Integer;
  protected
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    procedure AdjustBounds; override;
    property Glow: Boolean read FGlow write SetGlow;
    property GlowSize: Integer read FGlowSize write SetGlowSize;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TGlowLabel = class(TCustomGlowLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    property Glow;
    property GlowSize;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;

procedure Register;

implementation

uses
  Controls, Forms, Graphics, SysUtils, Math, DwmApi, Themes, UxTheme;

function StrScanW(const Str: PWideChar; Chr: WideChar): PWideChar;
begin
  Result := Str;
  while Result^ <> Chr do
  begin
    if Result^ = #0 then
    begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;

function LastDelimiterW(const Delimiters, S: WideString): Integer;
var
  P: PWideChar;
begin
  Result := Length(S);
  P := PWideChar(Delimiters);
  while Result > 0 do
  begin
    if (S[Result] <> #0) and (StrScanW(P, S[Result]) <> nil) then
      Exit;

    Dec(Result);
  end;
end;



{ TGlowCustomLabel }

procedure TCustomGlowLabel.AdjustBounds;
const
  WordWraps: array[Boolean] of Word = (0, DT_WORDBREAK);
var
  DC: HDC;
  Rect, Bounds, CalcRect: TRect;
  AAlignment: TAlignment;
  Expand: Integer;
  DoSetBounds: Boolean;
begin
  DoSetBounds := False;
  Bounds := BoundsRect;
  Rect := Bounds;

  if (IsGlow and (csReading in ComponentState)) then
  begin
    FBoundsWithGlow := True;
    FOldGlowSize := FGlowSize;
  end;

  if FBoundsWithGlow then
  begin
    Expand := GetExpansion(FOldGlowSize);
    Inc(Rect.Left, Expand);
    Inc(Rect.Top, Expand);
    Dec(Rect.Right, Expand);
    Dec(Rect.Bottom, Expand);
    FBoundsWithGlow := False;
    DoSetBounds := True;
  end;

  if not ((csReading in ComponentState) or
          (csLoading in ComponentState)) and
     AutoSize then
  begin
    DC := GetDC(0);
    Canvas.Handle := DC;
    CalcRect.Left := 0;
    CalcRect.Top := 0;
    DoDrawText(CalcRect, (DT_EXPANDTABS or DT_CALCRECT) or WordWraps[WordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, DC);
    AAlignment := Alignment;
    if UseRightToLeftAlignment then
      ChangeBiDiModeAlignment(AAlignment);

    if AAlignment = taRightJustify then
      Rect.Left := Rect.Right - CalcRect.Right;

    Rect.Right := Rect.Left + CalcRect.Right;
    Rect.Bottom := Rect.Top + CalcRect.Bottom;
    DoSetBounds := True;
  end;

  if IsGlow then
  begin
    FBoundsWithGlow := True;
    Expand := GetExpansion(FGlowSize);
    Dec(Rect.Left, Expand);
    Dec(Rect.Top, Expand);
    Inc(Rect.Right, Expand);
    Inc(Rect.Bottom, Expand);
    FOldGlowSize := FGlowSize;
    DoSetBounds := True;
  end;

  if DoSetBounds then
    SetBounds(Rect.Left, Rect.Top, Rect.Right - Rect.Left, Rect.Bottom - Rect.Top);
end;

constructor TCustomGlowLabel.Create(AOwner: TComponent);
begin
  inherited;
  FGlow := False;
  FGlowSize := 10;
  FOldGlowSize := 0;
  FBoundsWithGlow := False;
end;

procedure TCustomGlowLabel.DoDrawText(var Rect: TRect; Flags: Integer);

  procedure DoDrawThemeTextEx(DC: HDC; const Text: WideString; TextLen: Integer;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    Options: TDTTOpts;
  begin
    FillChar(Options, SizeOf(Options), 0);
    Options.dwSize := SizeOf(Options);
    Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED;
    if IsGlow then
    begin
      Options.dwFlags := Options.dwFlags or DTT_GLOWSIZE;
      Options.iGlowSize := FGlowSize;
    end;
    Options.crText := ColorToRGB(Canvas.Font.Color);

    with ThemeServices.GetElementDetails(teEditTextNormal) do
      DrawThemeTextEx(ThemeServices.Theme[teEdit], DC, Part, State,
        PWideChar(Text), TextLen, TextFlags, @TextRect, Options);
  end;

  procedure DrawText(DC: HDC; const Text: WideString; TextLen: Integer;
    var TextRect: TRect; TextFlags: Cardinal);
  var
    LForm: TCustomForm;
    PaintOnGlass: Boolean;
    Expand: Integer;
  begin
    PaintOnGlass := ThemeServices.ThemesEnabled and DwmCompositionEnabled and
      not (csDesigning in ComponentState);
    if PaintOnGlass then
    begin
      LForm := GetParentForm(Self);
      PaintOnGlass := (LForm <> nil) and LForm.GlassFrame.FrameExtended and
        LForm.GlassFrame.IntersectsControl(Self);
    end;

    if IsGlow and (Flags and DT_CALCRECT = 0) then
    begin
      Expand := GetExpansion(FGlowSize);
      case Alignment of
        taLeftJustify: OffsetRect(TextRect, Expand, 0);
        taRightJustify: OffsetRect(TextRect, -Expand, 0);
      end;

      case Layout of
        tlTop: OffsetRect(TextRect, 0, Expand);
        tlBottom: OffsetRect(TextRect, 0, -Expand);
      end;
    end;

    if PaintOnGlass and (Flags and DT_CALCRECT = 0) then
      DoDrawThemeTextEx(DC, Text, TextLen, TextRect, TextFlags)
    else
      Windows.DrawTextW(DC, PWideChar(Text), TextLen, TextRect, TextFlags);
  end;

const
  EllipsisStr = '...';
  Ellipsis: array[TEllipsisPosition] of Longint = (0, DT_PATH_ELLIPSIS,
    DT_END_ELLIPSIS, DT_WORD_ELLIPSIS);
var
  Text, DText: WideString;
  NewRect: TRect;
  Height, Delim: Integer;
begin
  Text := Caption;
  if (Flags and DT_CALCRECT <> 0) and ((Text = '') or ShowAccelChar and
    (Text[1] = '&') and (Text[2] = #0)) then
      Text := Text + ' ';
  if not ShowAccelChar then
    Flags := Flags or DT_NOPREFIX;
  Flags := DrawTextBiDiModeFlags(Flags);
  Canvas.Font := Font;
  if (EllipsisPosition <> epNone) and not AutoSize then
  begin
    DText := Text;
    Flags := Flags and not DT_EXPANDTABS;
    Flags := Flags or Ellipsis[EllipsisPosition];
    if WordWrap and (EllipsisPosition in [epEndEllipsis, epWordEllipsis]) then
    begin
      repeat
        NewRect := Rect;
        Dec(NewRect.Right, Canvas.TextWidth(EllipsisStr));
        Windows.DrawTextW(Canvas.Handle, PWideChar(DText), Length(DText), NewRect, Flags or DT_CALCRECT);
        Height := NewRect.Bottom - NewRect.Top;
        if (Height > ClientHeight) and (Height > Canvas.Font.Height) then
        begin
          Delim := LastDelimiterW(' '#9, Text);
          if Delim = 0 then
            Delim := Length(Text);
          Dec(Delim);

          Text := Copy(Text, 1, Delim);
          DText := Text + EllipsisStr;
          if Text = '' then
            Break;
        end else
          Break;
      until False;
    end;
    if Text <> '' then
      Text := DText;
  end;
  if not Enabled then
  begin
    OffsetRect(Rect, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawText(Canvas.Handle, Text, Length(Text), Rect, Flags);
    OffsetRect(Rect, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawText(Canvas.Handle, Text, Length(Text), Rect, Flags);
  end
  else
    DrawText(Canvas.Handle, Text, Length(Text), Rect, Flags);
end;

function TCustomGlowLabel.GetExpansion(GlowSize: Integer): Integer;
begin
  Result := Ceil(GlowSize / 2) + 1;
end;

function TCustomGlowLabel.IsGlow: Boolean;
begin
  Result := FGlow and (FGlowSize > 0);
end;

procedure TCustomGlowLabel.SetGlow(const Value: Boolean);
begin
  if FGlow <> Value then
  begin
    FGlow := Value;
    AdjustBounds;
    Invalidate;
  end;
end;

procedure TCustomGlowLabel.SetGlowSize(const Value: Integer);
begin
  if FGlowSize <> Value then
  begin
    FGlowSize := Value;
    AdjustBounds;
    Invalidate;
  end;
end;


procedure Register;
begin
  RegisterComponents('MWK', [TGlowLabel]);
end;

end.
