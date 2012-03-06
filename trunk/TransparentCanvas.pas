unit TransparentCanvas;

{
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is an alpha-aware canvas class and associated classes.

  The Initial Developer of the Original Code is David Millington.
  Portions created by David Millington are Copyright (C) 2008-2012.
  All Rights Reserved.

  Contributor(s): David Millington.
}

interface

uses Windows, SysUtils, Classes, Controls, Graphics;

type
  ETransparentCanvasException = class(Exception)
  end;

  TQuadColor = record
    constructor Create(Color : TColor);
    procedure Clear;
    function WrittenByGDI : Boolean;
    procedure SetAlpha(Transparency : Byte; PreMult : Single);
    function AsColorRef : COLORREF;
    procedure SetFromColorRef(Color : COLORREF);

    case Boolean of
      True : (Blue,
              Green,
              Red,
              Alpha : Byte);
      False : (Quad : Cardinal);
  end;
  PQuadColor = ^TQuadColor;
  PPQuadColor = ^PQuadColor;

  TGDIObjects = record
  private
    FBrush : HBRUSH;
    FPen : HPEN;
    FFont : HFONT;
  public
    constructor CreateWithHandles(hBrush : HBRUSH; hPen : HPEN; hFont : HFONT);
    property Brush : HBRUSH read FBrush;
    property Pen : HPEN read FPen;
    property Font : HFONT read FFont;
  end;

  TAlphaBitmapWrapper = class(TPersistent)
  private
    FDCHandle : HDC;
    FBMPHandle, FOriginalBMP : HBitmap;
    FQuads : PQuadColor;
    FWidth, FHeight : Integer;
    FOriginalObjects : TGDIObjects;

    procedure Construct(DC: HDC; Empty: Boolean; Width, Height: Integer; Inverted : boolean = false);
    procedure CheckHandles;
    procedure Clear;
  public
    constructor CreateBlank(DC: HDC; Width, Height: Integer);
    constructor CreateForGDI(DC: HDC; Width, Height: Integer);
    constructor CreateForDrawThemeTextEx(DC: HDC; Width, Height: Integer);
    constructor Create(var ToCopy : TAlphaBitmapWrapper);
    destructor Destroy; override;

    procedure SelectObjects(const GDI : TGDIObjects);
    procedure SelectOriginalObjects;

    procedure SetAllTransparency(const Alpha: Byte);
    procedure ProcessTransparency(const Alpha: Byte); overload;
    procedure ProcessTransparency(const Alpha: Byte; TranspRect : TRect); overload;
    procedure ProcessMaskTransparency(var MaskImage: TAlphaBitmapWrapper);
    procedure BlendTo(X, Y: Integer; var Image: TAlphaBitmapWrapper; Transparency: Byte = $FF);
    procedure BlendToStretch(X, Y, StretchWidth, StretchHeight: Integer;
      var Image: TAlphaBitmapWrapper; Transparency: Byte);
    procedure BlendToDC(X, Y : Integer; DC : HDC; Transparency : Byte = $FF);

    function GetRawPixelPtr(X, Y : Integer) : PQuadColor;
    procedure SafeSetRawPixel(X, Y : Integer; Color : TQuadColor);
  published
    property Handle : HDC read FDCHandle;
    property BitmapHandle : HBitmap read FBMPHandle;
    function QuadPointer : PQuadColor;
    property Width : Integer read FWidth;
    property Height : Integer read FHeight;
  end;

  TCustomTransparentCanvas = class(TPersistent)
    class function TColorToQuadColor(Color : TColor) : TQuadColor;
    class function QuadColorToTColor(Color: TQuadColor) : TColor;
  private
    FFont : TFont;
    FBrush : TBrush;
    FPen : TPen;
    FAttachedDC : HDC;

    function GetHandle() : HDC;

    procedure SetFont(NewFont : TFont);
    procedure SetBrush(NewBrush : TBrush);
    procedure SetPen(NewPen : TPen);

    function GetPenPos : TPoint;
    procedure SetPenPos(NewPos : TPoint);

    function GetWidth : Integer;
    function GetHeight : Integer;

    // Converts to non-premultiplied alpha
    function GetPixel(X, Y : Integer) : COLORREF;
    procedure SetPixel(X, Y: Integer; Color: Cardinal); overload;
    procedure SetPixel(X, Y: Integer; Color: Cardinal; Alpha: Byte); overload;

    // Direct pre-multiplied alpha
    function GetRawPixel(X, Y : Integer) : TQuadColor;
    procedure SetRawPixel(X, Y : Integer; Color : TQuadColor);

    procedure TextOutPreVista(const Rect : TRect; const Text: string; const Alpha : Byte);
    procedure TextOutVistaPlus(const ARect : TRect; const Text: string; const Alpha : Byte);
    function CanUseDrawThemeTextEx : boolean;
  protected
    FWorkingCanvas : TAlphaBitmapWrapper;

    function OrphanAliasedFont : HFONT;
  public
    constructor Create(Width, Height : Integer); overload;
    constructor Create(Canvas: TCanvas); overload;
    constructor Create(DC : HDC; Width, Height : Integer); overload;
    constructor Create(ToCopy : TCustomTransparentCanvas); overload;
    destructor Destroy; override;

    procedure Draw(X, Y: Integer; Canvas: TCanvas; Width, Height : Integer);
    procedure DrawTo(X, Y : Integer; Canvas : TCanvas; TargetWidth, TargetHeight: Integer; Transparency : Byte = $FF); overload;
    procedure DrawTo(X, Y: Integer; DC: HDC; TargetWidth, TargetHeight: Integer; Transparency : Byte = $FF); overload;
    procedure DrawToGlass(X, Y : Integer; DC : HDC; Transparency : Byte = $FF);

    procedure MoveTo(X, Y: Integer);

    procedure RoundRect(X1, Y1, X2, Y2, XRadius, YRadius: Integer; Alpha : Byte = $FF); overload;
    procedure RoundRect(Rect : TRect; XRadius, YRadius : Integer; Alpha : Byte = $FF); overload;
    procedure Rectangle(X1, Y1, X2, Y2: Integer; Alpha : Byte = $FF); overload;
    procedure Rectangle(Rect : TRect; Alpha : Byte = $FF); overload;

    function TextExtent(const Text: string): TSize;
    function TextHeight(const Text: string): Integer;
    procedure TextOut(const X, Y: Integer; const Text: string; const Alpha : Byte = $FF);
    procedure TextRect(const Rect: TRect; const Text: string; const Alpha : Byte = $FF);
    function TextWidth(const Text: string): Integer;

    function CanDrawGlowText : boolean;
    procedure GlowTextOut(const X, Y, GlowSize: Integer; const Text: string; const Alpha : Byte = $FF);

    procedure Clear;

    property Handle: HDC read GetHandle;
    property PenPos: TPoint read GetPenPos write SetPenPos;
    property Pixels[X, Y: Integer]: COLORREF read GetPixel write SetPixel;
    property RawPixels[X, Y: Integer]: TQuadColor read GetRawPixel write SetRawPixel;
  published
    property Brush: TBrush read FBrush write SetBrush;
    property Font: TFont read FFont write SetFont;
    property Pen: TPen read FPen write SetPen;
    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
  end;

  TTransparentCanvas = class(TCustomTransparentCanvas)
  end;

  TTransparentControlCanvas = class(TCustomTransparentCanvas)
  private
    FControl : TWinControl;
    FControlDC : HDC;
  public
    constructor Create(Control : TWinControl);
    destructor Destroy; override;
  end;

implementation

uses
  Math, Themes, UxTheme;

{ TCustomTransparentCanvas }

function TCustomTransparentCanvas.CanDrawGlowText: boolean;
begin
  Result := CanUseDrawThemeTextEx;
end;

function TCustomTransparentCanvas.CanUseDrawThemeTextEx: boolean;
begin
  Result := ThemeServices.ThemesEnabled and (Win32MajorVersion >= 6);
end;

procedure TCustomTransparentCanvas.Clear;
begin
  FWorkingCanvas.Clear;
end;

constructor TCustomTransparentCanvas.Create(Width, Height : Integer);
begin
  inherited Create();
  FWorkingCanvas := TAlphaBitmapWrapper.CreateBlank(0, Width, Height);
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
  FAttachedDC := 0;
end;

constructor TCustomTransparentCanvas.Create(ToCopy: TCustomTransparentCanvas);
begin
  inherited Create();
  FWorkingCanvas := TAlphaBitmapWrapper.Create(ToCopy.FWorkingCanvas);
  FFont := TFont.Create;
  FFont.Assign(ToCopy.FFont);
  FBrush := TBrush.Create;
  FBrush.Assign(ToCopy.FBrush);
  FPen := TPen.Create;
  FPen.Assign(ToCopy.FPen);
  FAttachedDC := 0;
end;

constructor TCustomTransparentCanvas.Create(Canvas: TCanvas);
begin
  inherited Create();
  FAttachedDC := Canvas.Handle;
  FWorkingCanvas := TAlphaBitmapWrapper.CreateBlank(Canvas.Handle, Width, Height);
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
end;

constructor TCustomTransparentCanvas.Create(DC: HDC; Width, Height : Integer);
begin
  inherited Create();
  FAttachedDC := DC;
  FWorkingCanvas := TAlphaBitmapWrapper.CreateBlank(DC, Width, Height);
  FFont := TFont.Create;
  FBrush := TBrush.Create;
  FPen := TPen.Create;
end;

destructor TCustomTransparentCanvas.Destroy;
begin
  FreeAndNil(FWorkingCanvas);
  FreeAndNil(FFont);
  FreeAndNil(FBrush);
  FreeAndNil(FPen);

  inherited;
end;

procedure TCustomTransparentCanvas.Draw(X, Y: Integer; Canvas: TCanvas;
  Width, Height : Integer);
begin
  BitBlt(FWorkingCanvas.FDCHandle, X, Y, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);
  FWorkingCanvas.ProcessTransparency($FF, Rect(X, Y, X+Width, Y+Height));
end;

procedure TCustomTransparentCanvas.DrawTo(X, Y: Integer; Canvas: TCanvas; TargetWidth,
  TargetHeight: Integer; Transparency : Byte = 255);
begin
  DrawTo(X, Y, Canvas.Handle, TargetWidth, TargetHeight, Transparency);
end;

procedure TCustomTransparentCanvas.DrawTo(X, Y: Integer; DC: HDC; TargetWidth,
  TargetHeight: Integer; Transparency : Byte = 255);
var
  TempCanvas: TAlphaBitmapWrapper;
begin
  // Create a 32-bit canvas with a copy of the dc drawn in it with opaque alpha
  TempCanvas := TAlphaBitmapWrapper.CreateBlank(DC, TargetWidth, TargetHeight);
  try
    BitBlt(TempCanvas.FDCHandle, 0, 0, TargetWidth, TargetHeight, DC, 0, 0, SRCCOPY);
    TempCanvas.SetAllTransparency($FF);

    // Now blend the working image onto it at (X, Y), possibly stretched
    if (TargetWidth = Width) and (TargetHeight = Height) then begin
      FWorkingCanvas.BlendTo(X, Y, TempCanvas, Transparency);
    end else begin
      FWorkingCanvas.BlendToStretch(X, Y, TargetWidth, TargetHeight, TempCanvas, Transparency);
    end;

    // Now blit the composited image back to the DC
    BitBlt(DC, 0, 0, TargetWidth, TargetHeight, TempCanvas.FDCHandle, 0, 0, SRCCOPY);
  finally
    TempCanvas.Free;
  end;
end;

procedure TCustomTransparentCanvas.DrawToGlass(X, Y: Integer; DC: HDC; Transparency : Byte);
begin
  FWorkingCanvas.BlendToDC(X, Y, DC, Transparency);
end;

function TCustomTransparentCanvas.GetHandle: HDC;
begin
  Result := FWorkingCanvas.FDCHandle;
end;

function TCustomTransparentCanvas.GetHeight: Integer;
begin
  Result := FWorkingCanvas.FHeight;
end;

function TCustomTransparentCanvas.GetPenPos: TPoint;
begin
  GetCurrentPositionEx(FWorkingCanvas.FDCHandle, @Result);
end;

function TCustomTransparentCanvas.GetPixel(X, Y: Integer): COLORREF;
begin
  Result := GetRawPixel(X, Y).AsColorRef;
end;

function TCustomTransparentCanvas.GetRawPixel(X, Y: Integer): TQuadColor;
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  InverseY := FWorkingCanvas.FHeight - Y - 1; // 0 is top not bottom
  PQuad := FWorkingCanvas.FQuads;
  Inc(PQuad, (InverseY * Width) + X);
  Result := PQuad^;
end;

function TCustomTransparentCanvas.GetWidth: Integer;
begin
  Result := FWorkingCanvas.FWidth;
end;

procedure TCustomTransparentCanvas.GlowTextOut(const X, Y, GlowSize: Integer; const Text: string;
  const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
  TextSize : TSize;
  Options : TDTTOpts;
  Details: TThemedElementDetails;
  TextRect : TRect;
begin
  if not CanDrawGlowText then raise ETransparentCanvasException.Create('Cannot use DrawThemeTextEx');

  TextSize := TextExtent(Text);
  TempImage := TAlphaBitmapWrapper.CreateForDrawThemeTextEx(FWorkingCanvas.FDCHandle, TextSize.cx + GlowSize*2, TextSize.cy + GlowSize*2);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, Font.Handle));
    SetBkMode(TempImage.FDCHandle, TRANSPARENT);
    SetTextColor(TempImage.FDCHandle, ColorToRGB(Font.Color));

		ZeroMemory(@Options, SizeOf(Options));
		Options.dwSize := SizeOf(Options);
		Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED or DTT_GLOWSIZE;
		Options.crText := ColorToRGB(Font.Color);
		Options.iGlowSize := GlowSize;

		Details := ThemeServices.GetElementDetails(teEditTextNormal);
    TextRect := Rect(GlowSize, GlowSize, TextSize.cx + GlowSize*2, TextSize.cy + GlowSize*2);
    DrawThemeTextEx(ThemeServices.Theme[teEdit], TempImage.FDCHandle, Details.Part, Details.State,
      PChar(Text), Length(Text), DT_LEFT or DT_TOP or DT_NOCLIP, TextRect,
      Options);

    TempImage.BlendTo(X - GlowSize, Y - GlowSize, FWorkingCanvas, Alpha);
    SetBkMode(TempImage.FDCHandle, OPAQUE);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.MoveTo(X, Y: Integer);
begin
  MoveToEx(FWorkingCanvas.FDCHandle, X, Y, nil);
end;

function TCustomTransparentCanvas.OrphanAliasedFont: HFONT;
var
  FontWeight : Cardinal;
begin
  // Font output and alpha is tricky with a ClearType or antialiased font.  This method takes FFont
  // and creates a new font with the same attributes, but with ClearType and AA explicitly disabled
  if fsBold in Font.Style then
    FontWeight := FW_BOLD
  else
    FontWeight := FW_NORMAL;
  Result := CreateFont(FFont.Height, 0, 0, 0, FontWeight, Cardinal(fsItalic in Font.Style),
    Cardinal(fsUnderline in Font.Style), Cardinal(fsStrikeOut in Font.Style), DEFAULT_CHARSET,
    OUT_DEFAULT_PRECIS, CLIP_DEFAULT_PRECIS, ANTIALIASED_QUALITY, DEFAULT_PITCH, PChar(Font.Name));
end;

class function TCustomTransparentCanvas.QuadColorToTColor(Color: TQuadColor): TColor;
begin
  Result := TColor(RGB(Color.Red, Color.Blue, Color.Green));
end;

procedure TCustomTransparentCanvas.Rectangle(X1, Y1, X2, Y2: Integer; Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, X2-X1, Y2-Y1);
  TempImage.SelectObjects(TGDIObjects.CreateWithHandles(Brush.Handle, Pen.Handle, Font.Handle));
  SetWindowOrgEx(TempImage.FDCHandle, X1 - Pen.Width div 2, Y1 - Pen.Width div 2, nil);
  Windows.Rectangle(TempImage.FDCHandle, X1, Y1, X2, Y2);
  SetWindowOrgEx(TempImage.FDCHandle, 0, 0, nil);
  TempImage.ProcessTransparency(Alpha);
  TempImage.BlendTo(X1 + Pen.Width div 2, Y1 + Pen.Width div 2, FWorkingCanvas);
  TempImage.SelectOriginalObjects;
  TempImage.Free;
end;

procedure TCustomTransparentCanvas.Rectangle(Rect: TRect; Alpha: Byte);
begin
  Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, Alpha);
end;

procedure TCustomTransparentCanvas.RoundRect(Rect: TRect; XRadius, YRadius : Integer; Alpha : Byte = $FF);
begin
  RoundRect(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom, XRadius, YRadius, Alpha);
end;

procedure TCustomTransparentCanvas.RoundRect(X1, Y1, X2, Y2, XRadius, YRadius: Integer; Alpha : Byte = $FF);
var
  TempImage : TAlphaBitmapWrapper;
begin
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, X2-X1 + Pen.Width, Y2-Y1 + Pen.Width);
  TempImage.SelectObjects(TGDIObjects.CreateWithHandles(Brush.Handle, Pen.Handle, Font.Handle));
  SetWindowOrgEx(TempImage.FDCHandle, X1 - Pen.Width div 2, Y1 - Pen.Width div 2, nil);
  Windows.RoundRect(TempImage.FDCHandle, X1, Y1, X2, Y2, XRadius, YRadius);
  SetWindowOrgEx(TempImage.FDCHandle, 0, 0, nil);
  TempImage.ProcessTransparency(Alpha);
  TempImage.BlendTo(X1 - Pen.Width div 2, Y1 - Pen.Width div 2, FWorkingCanvas);
  TempImage.SelectOriginalObjects;
  TempImage.Free;
end;

procedure TCustomTransparentCanvas.SetBrush(NewBrush: TBrush);
begin
  FBrush.Assign(NewBrush);
end;

procedure TCustomTransparentCanvas.SetFont(NewFont: TFont);
begin
  FFont.Assign(NewFont);
end;

procedure TCustomTransparentCanvas.SetPen(NewPen: TPen);
begin
  FPen.Assign(NewPen);
end;

procedure TCustomTransparentCanvas.SetPenPos(NewPos: TPoint);
begin
  MoveToEx(FWorkingCanvas.FDCHandle, NewPos.X, NewPos.Y, nil);
end;

procedure TCustomTransparentCanvas.SetPixel(X, Y: Integer; Color: Cardinal);
begin
  SetPixel(X, Y, Color, $FF);
end;

procedure TCustomTransparentCanvas.SetPixel(X, Y: Integer; Color: Cardinal; Alpha: Byte);
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  InverseY := FWorkingCanvas.FHeight - Y - 1; // 0 is top not bottom
  PQuad := FWorkingCanvas.FQuads;
  Inc(PQuad, (InverseY * Width) + X);
  PQuad.Quad := Color;
  PQuad.Alpha := Alpha;
end;

procedure TCustomTransparentCanvas.SetRawPixel(X, Y: Integer; Color: TQuadColor);
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  InverseY := FWorkingCanvas.FHeight - Y - 1; // 0 is top not bottom
  PQuad := FWorkingCanvas.FQuads;
  Inc(PQuad, (InverseY * Width) + X);
  PQuad.Quad := Color.Quad;
end;

class function TCustomTransparentCanvas.TColorToQuadColor(Color: TColor): TQuadColor;
begin
  Result := TQuadColor.Create(Color);
end;

function TCustomTransparentCanvas.TextExtent(const Text: string): TSize;
var
  OldFontHandle,
  FontHandle : HFONT;
begin
  if CanUseDrawThemeTextEx then begin // Can use DrawThemeTextEx; just get text extent normally
    FWorkingCanvas.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, Font.Handle));
    GetTextExtentPoint32(FWorkingCanvas.FDCHandle, PChar(Text), Length(Text), Result);
    FWorkingCanvas.SelectOriginalObjects;
  end else begin
    // Can't use DrawThemeTextEx; use aliased font (may affect output size, so need to explicitly
    // measure using the aliased font)
    FontHandle := OrphanAliasedFont;
    try
      OldFontHandle := SelectObject(FWorkingCanvas.FDCHandle, FontHandle);
      GetTextExtentPoint32(FWorkingCanvas.FDCHandle, PChar(Text), Length(Text), Result);
      SelectObject(FWorkingCanvas.FDCHandle, OldFontHandle);
    finally
      DeleteObject(FontHandle);
    end;
  end;
end;

function TCustomTransparentCanvas.TextHeight(const Text: string): Integer;
begin
  Result := TextExtent(Text).cy;
end;

procedure TCustomTransparentCanvas.TextOut(const X, Y: Integer; const Text: string; const Alpha : Byte);
var
  TextSize : TSize;
begin
  TextSize := TextExtent(Text);
  if CanUseDrawThemeTextEx then
    TextOutVistaPlus(Rect(X, Y, X + TextSize.cx, Y + TextSize.cy), Text, Alpha)
  else
    TextOutPreVista(Rect(X, Y, X + TextSize.cx, Y + TextSize.cy), Text, Alpha);
end;

procedure TCustomTransparentCanvas.TextOutPreVista(const Rect : TRect; const Text: string;
  const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
  FontHandle : HFONT;
  TextSize : TSize;
begin
  TextSize := TextExtent(Text);
  // Clip to the rest by restricting the size it thinks the text is - the bitmap will be this size, thus clipped
  TextSize.cx := min(TextSize.cx, Rect.Right-Rect.Left);
  TextSize.cy := min(TextSize.cy, Rect.Bottom-Rect.Top);
  FontHandle := OrphanAliasedFont; // Antialiased or cleartype text works terribly when trying to fix the alpha
  TempImage := TAlphaBitmapWrapper.CreateForGDI(FWorkingCanvas.FDCHandle, TextSize.cx, TextSize.cy);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, FontHandle));
    SetBkMode(TempImage.FDCHandle, TRANSPARENT);
    SetTextColor(TempImage.FDCHandle, ColorToRGB(Font.Color));
    ExtTextOut(TempImage.FDCHandle, 0, 0, ETO_CLIPPED, nil, PChar(Text), Length(Text), nil);
    SetBkMode(TempImage.FDCHandle, OPAQUE);
    TempImage.ProcessTransparency(Alpha);
    TempImage.BlendTo(Rect.Left, Rect.Top, FWorkingCanvas);
    TempImage.SelectOriginalObjects;
  finally
    DeleteObject(FontHandle);
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.TextOutVistaPlus(const ARect : TRect; const Text: string;
  const Alpha: Byte);
var
  TempImage : TAlphaBitmapWrapper;
  TextSize : TSize;
  Options : TDTTOpts;
  Details: TThemedElementDetails;
  TextRect : TRect;
begin
  if not CanUseDrawThemeTextEx then raise ETransparentCanvasException.Create('Cannot use DrawThemeTextEx');

  TextSize := TextExtent(Text);
  // Clip by clipping the size of the rectangle it assumes the text fits in
  TextSize.cx := min(TextSize.cx, ARect.Right-ARect.Left);
  TextSize.cy := min(TextSize.cy, ARect.Bottom-ARect.Top);
  TempImage := TAlphaBitmapWrapper.CreateForDrawThemeTextEx(FWorkingCanvas.FDCHandle, TextSize.cx, TextSize.cy);
  try
    TempImage.SelectObjects(TGDIObjects.CreateWithHandles(0, 0, Font.Handle));
    SetBkMode(TempImage.FDCHandle, TRANSPARENT);
    SetTextColor(TempImage.FDCHandle, ColorToRGB(Font.Color));

		ZeroMemory(@Options, SizeOf(Options));
		Options.dwSize := SizeOf(Options);
		Options.dwFlags := DTT_TEXTCOLOR or DTT_COMPOSITED;
		Options.crText := ColorToRGB(Font.Color);
		Options.iGlowSize := 0;

		Details := ThemeServices.GetElementDetails(teEditTextNormal);
    TextRect := Rect(0, 0, TextSize.cx, TextSize.cy);
    DrawThemeTextEx(ThemeServices.Theme[teEdit], TempImage.FDCHandle, Details.Part, Details.State,
      PChar(Text), Length(Text), DT_LEFT or DT_TOP, TextRect,
      Options);

    SetBkMode(TempImage.FDCHandle, OPAQUE);
    TempImage.BlendTo(ARect.Left, ARect.Top, FWorkingCanvas, Alpha);
    TempImage.SelectOriginalObjects;
  finally
    TempImage.Free;
  end;
end;

procedure TCustomTransparentCanvas.TextRect(const Rect : TRect; const Text: string; const Alpha: Byte);
begin
  if CanUseDrawThemeTextEx then
    TextOutVistaPlus(Rect, Text, Alpha)
  else
    TextOutPreVista(Rect, Text, Alpha);
end;

function TCustomTransparentCanvas.TextWidth(const Text: string): Integer;
begin
  Result := TextExtent(Text).cx;
end;

{ TAlphaBitmapWrapper }

procedure TAlphaBitmapWrapper.BlendTo(X, Y: Integer; var Image: TAlphaBitmapWrapper; Transparency: Byte);
var
  BlendFunc : TBlendFunction;
begin
  with BlendFunc do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := Transparency; // Normally 255
    AlphaFormat := AC_SRC_ALPHA;
  end;
  AlphaBlend(Image.FDCHandle, X, Y, FWidth, FHeight, FDcHandle, 0, 0, FWidth, FHeight, BlendFunc);
end;

procedure TAlphaBitmapWrapper.BlendToDC(X, Y: Integer; DC: HDC; Transparency: Byte);
var
  BlendFunc : TBlendFunction;
begin
  with BlendFunc do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := 255;
    AlphaFormat := AC_SRC_ALPHA;
  end;
  AlphaBlend(DC, X, Y, FWidth, FHeight, FDcHandle, 0, 0, FWidth, FHeight, BlendFunc);
end;

procedure TAlphaBitmapWrapper.BlendToStretch(X, Y, StretchWidth, StretchHeight: Integer; var Image: TAlphaBitmapWrapper; Transparency: Byte);
var
  BlendFunc : TBlendFunction;
begin
  with BlendFunc do begin
    BlendOp := AC_SRC_OVER;
    BlendFlags := 0;
    SourceConstantAlpha := Transparency; // Normally 255
    AlphaFormat := AC_SRC_ALPHA;
  end;
  AlphaBlend(Image.FDCHandle, 0, 0,StretchWidth, StretchHeight, FDcHandle, X, Y, FWidth, FHeight, BlendFunc);
end;

procedure TAlphaBitmapWrapper.CheckHandles;
begin
  if FDCHandle = 0 then
    raise ETransparentCanvasException.Create('Cannot create device context');
  if FBMPHandle = 0 then
    raise ETransparentCanvasException.Create('Cannot create 32-bit bitmap');
  if FQuads = nil then
    raise ETransparentCanvasException.Create('Cannot access bitmap bits');
end;

procedure TAlphaBitmapWrapper.Clear;
begin
  ZeroMemory(FQuads, FWidth * FHeight * SizeOf(TQuadColor));
end;

procedure TAlphaBitmapWrapper.Construct(DC: HDC; Empty: Boolean; Width, Height: Integer; Inverted : boolean);
var
  BMPInfo : TBitmapInfo;
  PQuads : Pointer;
begin
  FWidth := Width;
  FHeight := Height;
  FDCHandle := CreateCompatibleDC(DC);
  ZeroMemory(@BMPInfo, SizeOf(TBitmapInfo));
  with BMPInfo.bmiHeader do begin
    biSize := SizeOf(TBitmapInfo);
    biWidth := FWidth;
    if Inverted then begin
      biHeight := -FHeight // For DrawThemeTextEx: requires inverted (bottom-up) bitmap
    end else begin
      biHeight := FHeight;
    end;
    biPlanes := 1;
    biBitCount := 32;
    biCompression := BI_RGB;
    biSizeImage := FWidth * FHeight * SizeOf(TQuadColor);
  end;
  PQuads := nil;
  FBMPHandle := 0;
  FBMPHandle := CreateDIBSection(FDCHandle, BMPInfo, DIB_RGB_COLORS, PQuads, 0, 0);
  FQuads := PQuadColor(PQuads);
  CheckHandles;
  FOriginalBMP := SelectObject(FDCHandle, FBMPHandle);
  GdiFlush; // Need to flush before any manipulation of bits
  if Empty then begin
    ZeroMemory(FQuads, Width * Height * SizeOf(TQuadColor));
  end else begin
    FillMemory(FQuads, Width * Height * SizeOf(TQuadColor), $FF);
  end;
end;

constructor TAlphaBitmapWrapper.Create(var ToCopy: TAlphaBitmapWrapper);
begin
  inherited Create();
  Construct(ToCopy.FDCHandle, true, ToCopy.FWidth, ToCopy.FHeight); // true = init to all zeroes
  ToCopy.BlendTo(0, 0, Self);
end;

constructor TAlphaBitmapWrapper.CreateBlank(DC: HDC; Width,
  Height: Integer);
begin
  inherited Create();
  Construct(DC, true, Width, Height); // true = init to all zeroes
end;

constructor TAlphaBitmapWrapper.CreateForDrawThemeTextEx(DC: HDC; Width, Height: Integer);
begin
  inherited Create();
  Construct(DC, true, Width, Height, true); // init to all zeroes; inverted (upside down) because DrawThemeTextEx needs it
end;

constructor TAlphaBitmapWrapper.CreateForGDI(DC: HDC; Width,
  Height: Integer);
begin
  inherited Create();
  Construct(DC, false, Width, Height); // false = init all bytes to $FF, so can test if written to
end;

destructor TAlphaBitmapWrapper.Destroy;
begin
  SelectOriginalObjects;
  SelectObject(FDCHandle, FOriginalBMP);
  DeleteObject(FBMPHandle);
  FBMPHandle := 0;
  DeleteObject(FDCHandle);
  FDCHandle := 0;
  inherited;
end;

procedure TAlphaBitmapWrapper.ProcessMaskTransparency(var MaskImage: TAlphaBitmapWrapper);
var
  Loop : Integer;
  PQuad,
  PMaskQuad : PQuadColor;
begin
  if not ((FWidth = MaskImage.FWidth)) and (FHeight = MaskImage.FHeight) then
    raise ETransparentCanvasException.Create('Mask images must be identical in size');

  GdiFlush; // Need to flush before any manipulation of bits
  PQuad := FQuads;
  PMaskQuad := MaskImage.FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    if (PMaskQuad.Quad and $00FFFFFF) = 0 then begin
      PQuad.SetAlpha(255, 1.0);
    end else begin
      PQuad.Quad := 0;
    end;
    Inc(PQuad);
    Inc(PmaskQuad);
  end;
end;

procedure TAlphaBitmapWrapper.ProcessTransparency(const Alpha: Byte; TranspRect: TRect);
var
  LoopX : Integer;
  PreMult : Single;
  PQuad : PQuadColor;
  LoopY: Integer;
begin
  GdiFlush; // Need to flush before any manipulation of bits
  IntersectRect(TranspRect, TranspRect, Rect(0, 0, FWidth, FHeight)); // Clip to valid bounds

  PreMult := Alpha / 255.0;
  for LoopY := TranspRect.Top to TranspRect.Bottom - 1 do begin
    PQuad := FQuads;
    Inc(PQuad, LoopY);
    for LoopX := TranspRect.Left to TranspRect.Right - 1 do begin
      if PQuad.WrittenByGDI then begin
        PQuad.SetAlpha(Alpha, PreMult);
      end else begin
        PQuad.Quad := 0;
      end;
      Inc(PQuad);
    end;
  end;
end;

procedure TAlphaBitmapWrapper.ProcessTransparency(const Alpha: Byte);
var
  Loop : Integer;
  PreMult : Single;
  PQuad : PQuadColor;
begin
  GdiFlush; // Need to flush before any manipulation of bits
  PreMult := Alpha / 255.0;
  PQuad := FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    if PQuad.WrittenByGDI then begin
      PQuad.SetAlpha(Alpha, PreMult);
    end else begin
      PQuad.Quad := 0;
    end;
    Inc(PQuad);
  end;
end;

function TAlphaBitmapWrapper.QuadPointer: PQuadColor;
begin
  Result := FQuads;
end;

function TAlphaBitmapWrapper.GetRawPixelPtr(X, Y: Integer): PQuadColor;
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then begin
    InverseY := FHeight - Y - 1; // 0 is top not bottom
    PQuad := FQuads;
    Inc(PQuad, (InverseY * FWidth) + X);
    Result := PQuad;
  end else begin
    Result := nil;
  end;
end;

procedure TAlphaBitmapWrapper.SafeSetRawPixel(X, Y: Integer; Color: TQuadColor);
var
  PQuad : PQuadColor;
  InverseY : Integer;
begin
  if (X >= 0) and (X < FWidth) and (Y >= 0) and (Y < FHeight) then begin
    InverseY := FHeight - Y - 1; // 0 is top not bottom
    PQuad := FQuads;
    Inc(PQuad, (InverseY * FWidth) + X);
    PQuad.Quad := Color.Quad;
  end;
end;

procedure TAlphaBitmapWrapper.SelectObjects(const GDI: TGDIObjects);
begin
  // This is only one layer deep - it stores the old objects in FOriginalObjects
  // If you call if more than once, it will overwrite the values in FOriginalObjects
  // If you find yourself doing this, this needs to be rewritten as a stack, and you'd
  // push and pop the handles.
  if (FOriginalObjects.FBrush <> 0) or (FOriginalObjects.FPen <> 0) or (FOriginalObjects.FFont <> 0) then
    raise ETransparentCanvasException.Create('SelectObjects has already been called');

  FOriginalObjects.FBrush := SelectObject(FDCHandle, GDI.Brush);
  FOriginalObjects.FPen := SelectObject(FDCHandle, GDI.Pen);
  FOriginalObjects.FFont := SelectObject(FDCHandle, GDI.Font);
end;

procedure TAlphaBitmapWrapper.SelectOriginalObjects;
begin
  SelectObject(FDCHandle, FOriginalObjects.FBrush);
  FOriginalObjects.FBrush := 0;
  SelectObject(FDCHandle, FOriginalObjects.FPen);
  FOriginalObjects.FPen := 0;
  SelectObject(FDCHandle, FOriginalObjects.FFont);
  FOriginalObjects.FFont := 0;
end;

procedure TAlphaBitmapWrapper.SetAllTransparency(const Alpha: Byte);
var
  Loop : Integer;
  PreMult : Single;
  PQuad : PQuadColor;
begin
  GdiFlush; // Need to flush before any manipulation of bits
  PreMult := Alpha / 255.0;
  PQuad := FQuads;
  for Loop := 0 to FWidth * FHeight - 1 do begin
    PQuad.SetAlpha(Alpha, PreMult);
    Inc(PQuad);
  end;
end;

{ TQuadColor }

function TQuadColor.AsColorRef: COLORREF;
var
  PreDiv : Single;
begin
  // contains premultiplied alpha, so un-premultiply it and return that
  if Alpha = 0 then begin
    Result := $00000000;
  end else begin
    PreDiv := 1 / (Alpha / 255.0);
    Result := RGB(Trunc(Red * PreDiv), Trunc(Green * PreDiv), Trunc(Blue * PreDiv)) or (Alpha shl 24);
  end;
end;

procedure TQuadColor.Clear;
begin
  Quad := 0;
end;

constructor TQuadColor.Create(Color: TColor);
begin
  Quad := Cardinal(ColorToRGB(Color)) or $FF000000;
end;

procedure TQuadColor.SetAlpha(Transparency: Byte; PreMult: Single);
begin
  Alpha := Transparency;
  Blue := Trunc(Blue * PreMult);
  Green := Trunc(Green * PreMult);
  Red := Trunc(Red * PreMult);
end;

procedure TQuadColor.SetFromColorRef(Color: COLORREF);
begin
  Quad := Color;
  if Alpha = 0 then begin
    Quad := 0;
  end else begin
    SetAlpha(Alpha, Alpha / 255.0);
  end;
end;

function TQuadColor.WrittenByGDI: Boolean;
begin
  Result := (Alpha = 0);
end;

{ TTransparentControlCanvas }

constructor TTransparentControlCanvas.Create(Control: TWinControl);
begin
  if Control = nil then
    raise ETransparentCanvasException.Create('Control must not be nil');
  if Control.Handle = 0 then
    raise ETransparentCanvasException.Create('Cannot access control handle');

  FControl := Control;
  FControlDC := GetWindowDC(Control.Handle);
  if FControlDC = 0 then
    raise ETransparentCanvasException.Create('Cannot obtain control device context');

  inherited Create(FControlDC, Control.Width, Control.Height);
end;

destructor TTransparentControlCanvas.Destroy;
begin
  DrawTo(0, 0, FControlDC, FControl.Width, FControl.Height);

  ReleaseDC(FControl.Handle, FControlDC);
  FControlDC := 0;
  FWorkingCanvas.FDCHandle := 0;

  inherited;
end;

{ TGDIObjects }

constructor TGDIObjects.CreateWithHandles(hBrush: HBRUSH; hPen: HPEN; hFont : HFONT);
begin
  FBrush := hBrush;
  FPen := hPen;
  FFont := hFont;
end;

end.
