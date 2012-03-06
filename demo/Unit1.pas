unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, XPMan, StdCtrls, ExtCtrls, ExtDlgs, jpeg, ComCtrls, pngimage, TransparentCanvas;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    OpenPictureDialog: TOpenPictureDialog;
    LoadedImage: TImage;
    PaintBox: TPaintBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    AlphaTrackBar: TTrackBar;
    CompositionAlphaTrackBar: TTrackBar;
    Panel2: TPanel;
    FinalColorPaintBox: TPaintBox;
    TransRawColorLabel: TLabel;
    Label4: TLabel;
    ClickCoordsLabel: TLabel;
    Label5: TLabel;
    FinalColorLabel: TLabel;
    TransRawColorPaintBox: TPaintBox;
    BaseColorLabel: TLabel;
    BaseColorPaintBox: TPaintBox;
    Label1: TLabel;
    TransColorLabel: TLabel;
    TransColorPaintBox: TPaintBox;
    Label2: TLabel;
    TabSheet2: TTabSheet;
    Bevel1: TBevel;
    PreviewPaintBox: TPaintBox;
    Button1: TButton;
    Label3: TLabel;
    TextTrackBar: TTrackBar;
    Label9: TLabel;
    Label10: TLabel;
    GridCheckBox: TCheckBox;
    Button2: TButton;
    SavePictureDialog: TSavePictureDialog;
    procedure FormCreate(Sender: TObject);
    procedure PreviewPaintBoxPaint(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PaintBoxClick(Sender: TObject);
    procedure TransColorPaintBoxPaint(Sender: TObject);
    procedure AlphaTrackBarChange(Sender: TObject);
    procedure BlurCheckBoxClick(Sender: TObject);
    procedure ColorPaintBoxPaint(Sender: TObject);
    procedure DropShadowCheckBoxClick(Sender: TObject);
    procedure GridCheckBoxClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    MouseClickPos : TPoint;
    CalcColors : Boolean;
    SaveTextRect : TRect;
    GlassCanvas : TTransparentCanvas;
    { Private declarations }
    procedure SetupWindow();
    procedure UpdateClickCoordsLabel;
    procedure UpdateColorDisplay(BaseColor : TColor; var PaintBox : TPaintBox; var TransCanvas : TTransparentCanvas);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  Math;

function ColorToText(Color : COLORREF; IncludeAlpha : Boolean = true) : string; overload;
begin
  if IncludeAlpha then begin
    Result := 'A:' + IntToStr((Color and $ff000000) shr 24) + ', ';
  end;
  Result := Result + 'R:' + IntToStr(GetRValue(Color))
    + ', G:' + IntToStr(GetGValue(Color))
    + ', B:' + IntToStr(GetBValue(Color));
end;

function ColorToText(Color : TColor) : string; overload;
begin
  Result := ColorToText(COLORREF(Color), false);
end;

function MakePoint(X, Y : Integer) : TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

procedure TForm1.AlphaTrackBarChange(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenPictureDialog.Execute then
  begin
    LoadedImage.Picture.Bitmap.LoadFromFile(OpenPictureDialog.FileName);
    MouseClickPos.X := -1; // SetupWindow resets this to the center of the image
    MouseClickPos.Y := -1;
    SetupWindow;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  BMP : TBitmap;
  SaveRect : TRect;
begin
  SaveRect := SaveTextRect;
  InflateRect(SaveRect, 40, 20);

  BMP := TBitmap.Create;
  try
    BMP.PixelFormat := pf24Bit;
    BMP.Width := SaveRect.Right-SaveRect.Left;
    BMP.Height := SaveRect.Bottom-SaveRect.Top;

    BMP.Canvas.CopyRect(Rect(0, 0, BMP.Width, BMP.Height), PaintBox.Canvas, SaveRect);
    if SavePictureDialog.Execute then
      BMP.SaveToFile(SavePictureDialog.FileName);
  finally
    BMP.Free;
  end;
end;

procedure TForm1.GridCheckBoxClick(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TForm1.ColorPaintBoxPaint(Sender: TObject);
var
  Rect : TRect;
  Paint : TPaintBox;
  RedChannel,
  GreenChannel,
  BlueChannel : TColor;
  FullColor : COLORREF;
  SegmentWidth : Integer;
begin
  Paint := Sender as TPaintBox;
  Paint.Canvas.Brush.Color := clBtnFace;
  Paint.Canvas.Pen.Color := clBtnFace;
  Paint.Canvas.FillRect(Paint.ClientRect);

  FullColor := COLORREF(Paint.Color);
  RedChannel := RGB(GetRValue(FullColor), 0, 0);
  GreenChannel := RGB(0, GetGValue(FullColor), 0);
  BlueChannel := RGB(0, 0, GetBValue(FullColor));

  // Paint 4 sections: final color, and three channels
  Rect := Paint.ClientRect;
  Rect.Top := Rect.Top + 1;
  Rect.Bottom := Rect.Bottom - 1;
  SegmentWidth := Paint.Width div 4;

  // draw the RGB colour
  Paint.Canvas.Brush.Color := TColor(FullColor and $00FFFFFF);
  Rect.Left := 0;
  Rect.Right := SegmentWidth -1;
  Paint.Canvas.FillRect(Rect);
  // Red
  Paint.Canvas.Brush.Color := RedChannel;
  Rect.Left := SegmentWidth;
  Rect.Right := SegmentWidth * 2 -1;
  Paint.Canvas.FillRect(Rect);
  // Green
  Paint.Canvas.Brush.Color := GreenChannel;
  Rect.Left := SegmentWidth *2;
  Rect.Right := SegmentWidth * 3 -1;
  Paint.Canvas.FillRect(Rect);
  // Blue
  Paint.Canvas.Brush.Color := BlueChannel;
  Rect.Left := SegmentWidth * 3;
  Rect.Right := SegmentWidth * 4 -1;
  Paint.Canvas.FillRect(Rect);
end;

procedure TForm1.DropShadowCheckBoxClick(Sender: TObject);
begin
  PaintBox.Invalidate;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  MouseClickPos.X := -1;
  MouseClickPos.Y := -1;
  DoubleBuffered := true;
  GlassCanvas := nil;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GlassCanvas.Free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  // It is far more efficient to draw once and blit many times, especially if you repaint
  // often (such as when animating a form resize.)
  // This is for the glass area at the top of the form
  if not Assigned(GlassCanvas) or (GlassCanvas.Width <> ClientWidth) then begin
    GlassCanvas.Free;
    GlassCanvas := TTransparentCanvas.Create(ClientWidth, GlassFrame.Top);

    // Draw a bunch of different shapes and text
    GlassCanvas.Font := Screen.CaptionFont;
    GlassCanvas.Font.Color := clBlack;
    GlassCanvas.TextOut(10, 0, 'This is normal text drawn to glass using TTransparentCanvas');
    GlassCanvas.GlowTextOut(10, 25, 4, 'This is glowing text drawn to glass using TTransparentCanvas');

    GlassCanvas.Pen.Color := clGreen;
    GlassCanvas.Pen.Width := 1;
    GlassCanvas.Brush.Color := clMoneyGreen;
    GlassCanvas.RoundRect(340, 0, 490, 40, 10, 10, 128);

    GlassCanvas.Pen.Width := 4;
    GlassCanvas.Pen.Color := clWhite;
    GlassCanvas.Brush.Color := clBlue;
    GlassCanvas.RoundRect(440, 10, 510, 45, 15, 15, 192);

    GlassCanvas.Pen.Width := 0;
    GlassCanvas.Brush.Color := clRed;
    GlassCanvas.Rectangle(380, 4, 580, 35, 128);

    GlassCanvas.Font.Style := [fsBold, fsItalic];
    GlassCanvas.GlowTextOut(350, 13, 4, 'Random shapes!');
    GlassCanvas.Font.Style := [fsItalic];
    GlassCanvas.GlowTextOut(480, 13, 4, 'Half-transparent text!', 128);
  end;
  GlassCanvas.DrawToGlass(0, 0, Canvas.Handle);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  SetupWindow;
end;

procedure TForm1.BlurCheckBoxClick(Sender: TObject);
begin
  Paintbox.Invalidate;
end;

procedure TForm1.TransColorPaintBoxPaint(Sender: TObject);
var
  Rect : TRect;
  Paint : TPaintBox;
  RedChannel,
  GreenChannel,
  BlueChannel,
  AlphaChannel,
  AllChannelWhite,
  AllChannelBlack : TColor;
  Alpha : Byte;
  AlphaMult,
  InverseMult : Single;
  FullColor : COLORREF;
  SegmentWidth : Integer;
  TopLeftPoints,
  BottomRightPoints : array of TPoint;
begin
  Paint := Sender as TPaintBox;
  Paint.Canvas.Brush.Color := clBtnFace;
  Paint.Canvas.Pen.Color := clBtnFace;
  Paint.Canvas.FillRect(Paint.ClientRect);

  FullColor := COLORREF(Paint.Color);
  RedChannel := RGB(GetRValue(FullColor), 0, 0);
  GreenChannel := RGB(0, GetGValue(FullColor), 0);
  BlueChannel := RGB(0, 0, GetBValue(FullColor));
  Alpha := (FullColor and $ff000000) shr 24;
  AlphaChannel := RGB(Alpha, Alpha, Alpha);
  AlphaMult := Alpha / 255.0;
  InverseMult := 1.0 - AlphaMult;
  AllChannelWhite := RGB(Trunc(255 * InverseMult + GetRValue(FullColor) * AlphaMult),
                         Trunc(255 * InverseMult + GetGValue(FullColor) * AlphaMult),
                         Trunc(255 * InverseMult + GetBValue(FullColor) * AlphaMult));
  AllChannelBlack := RGB(Trunc(GetRValue(FullColor) * AlphaMult),
                         Trunc(GetGValue(FullColor) * AlphaMult),
                         Trunc(GetBValue(FullColor) * AlphaMult));


  // Paint 5 sections: final color over black and white, and four channels
  Rect := Paint.ClientRect;
  Rect.Top := Rect.Top + 1;
  Rect.Bottom := Rect.Bottom - 1;
  SegmentWidth := Paint.Width div 5;
  // Points for the triangles in which to draw the final color over black and white
  SetLength(TopLeftPoints, 3);
  TopLeftPoints[0] := MakePoint(-1, Rect.Top-1);
  TopLeftPoints[1] := MakePoint(SegmentWidth-1, Rect.Top-1);
  TopLeftPoints[2] := MakePoint(-1, Rect.Bottom-1);
  SetLength(BottomRightPoints, 3);
  BottomRightPoints[0] := MakePoint(SegmentWidth-1, Rect.Top);
  BottomRightPoints[1] := MakePoint(SegmentWidth-1, Rect.Bottom);
  BottomRightPoints[2] := MakePoint(-1, Rect.Bottom);

  // One of the paintboxes shoing the transparent components
  SegmentWidth := Paint.Width div 5;
  // fullcolor over white
  Paint.Canvas.Brush.Color := AllChannelWhite;
  Paint.Canvas.Polygon(TopLeftPoints);
  // fullcolor over black
  Paint.Canvas.Brush.Color := AllChannelBlack;
  Paint.Canvas.Polygon(BottomRightPoints);
  // Alpha
  Paint.Canvas.Brush.Color := AlphaChannel;
  Rect.Left := SegmentWidth;
  Rect.Right := SegmentWidth * 2 -1;
  Paint.Canvas.FillRect(Rect);

  // Red
  Paint.Canvas.Brush.Color := RedChannel;
  Rect.Left := SegmentWidth * 2;
  Rect.Right := SegmentWidth * 3 -1;
  Paint.Canvas.FillRect(Rect);
  // Green
  Paint.Canvas.Brush.Color := GreenChannel;
  Rect.Left := SegmentWidth *3;
  Rect.Right := SegmentWidth * 4 -1;
  Paint.Canvas.FillRect(Rect);
  // Blue
  Paint.Canvas.Brush.Color := BlueChannel;
  Rect.Left := SegmentWidth * 4;
  Rect.Right := SegmentWidth * 5 -1;
  Paint.Canvas.FillRect(Rect);
end;

procedure TForm1.PaintBoxClick(Sender: TObject);
begin
  CalcColors := true;
  MouseClickPos := ScreenToClient(Mouse.CursorPos);
  Dec(MouseClickPos.X, PaintBox.Left);
  Dec(MouseClickPos.Y, PaintBox.Top);
  UpdateClickCoordsLabel;
  PaintBox.Invalidate; // when repaints, updates colours in side panel
end;

// This method paints the bitmap and various shapes in the main area of the form
procedure TForm1.PaintBoxPaint(Sender: TObject);
var
  TransCanvas : TTransparentCanvas;
  TextSize : TSize;
  BaseColor : TColor;
  Loop : Integer;
const
  TextX = 100;
  TextY = 100;
  TextBorder = 5;
  TextRadius = 12;
  Text = 'Caption with a transparent background';
  GridSize = 50;
  IndicatorRadius = 5;
begin
  // Draw the background image (by default, oranges) and so get the underlying (base) color
  PaintBox.Canvas.Draw(0, 0, LoadedImage.Picture.Graphic);
  if CalcColors and PtInRect(PaintBox.ClientRect, MouseClickPos) then begin
    BaseColor := PaintBox.Canvas.Pixels[MouseClickPos.X, MouseClickPos.Y];
  end else begin
    BaseColor := $00000000;
  end;

  // Draw a grid in order to check the location of the drawn objects
  if GridCheckBox.Checked then begin
    PaintBox.Canvas.Pen.Color := clRed;
    PaintBox.Canvas.Pen.Width := 1;
    PaintBox.Canvas.Pen.Style := psSolid;
    for Loop := 0 to PaintBox.Width div GridSize - 1 do begin
      PaintBox.Canvas.MoveTo(Loop * GridSize, 0);
      PaintBox.Canvas.LineTo(Loop * GridSize, PaintBox.Height);
    end;
    for Loop := 0 to PaintBox.Height div GridSize - 1 do begin
      PaintBox.Canvas.MoveTo(0, Loop * GridSize);
      PaintBox.Canvas.LineTo(PaintBox.Width, Loop * GridSize);
    end;
  end;

  // Key part: create and draw on a transparent canvas, and paint it to the paintbox
  TransCanvas := TTransparentCanvas.Create(PaintBox.Canvas.Handle, PaintBox.Width, PaintBox.Height);
  try
    TransCanvas.Pen.Width := 1;
    TransCanvas.Brush.Color := clBlack;
    TransCanvas.Font := Screen.CaptionFont;
    TransCanvas.Font.Color := clWhite;

    // Draw text on a transparent roundrect shape
    TextSize := TransCanvas.TextExtent(Text);
    TransCanvas.RoundRect(Rect(TextX, TextY, TextX + TextSize.cx + TextBorder*2, TextY + TextSize.cy + TextBorder*2),
      TextRadius, TextRadius, AlphaTrackBar.Position);
    TransCanvas.TextOut(TextX + TextBorder, TextX + TextBorder, Text, TextTrackBar.Position);
    // Remember where this was, to save it to file
    SaveTextRect := Rect(TextX, TextY, TextX + TextSize.cx + TextBorder*2, TextY + TextSize.cy + TextBorder*2);

    // Clipped text
    TransCanvas.TextRect(Rect(5, 5, 150, 15), 'Test clipped text in a rectangle', TextTrackBar.Position);

    // Draw glowing text (Vista+ text with a hazy background) if possible
    TransCanvas.Font.Color := clBlack;
    if TransCanvas.CanDrawGlowText then
      TransCanvas.GlowTextOut(60, 50, 4, 'Test glowing text', TextTrackBar.Position);

    // Draw some rectangles and rounded rectangles
    TransCanvas.Brush.Color := clRed;
    TransCanvas.Rectangle(Rect(350, 10, 450, 200), AlphaTrackBar.Position);
    TransCanvas.Pen.Color := clWhite;
    TransCanvas.Pen.Width := 5;
    TransCanvas.Brush.Color := clBlue;
    TransCanvas.RoundRect(Rect(150, 150, 420, 300), 50, 50, AlphaTrackBar.Position);
    //...add extra drawing here...

    // Kay part: draw to the normal (paintbox) TCanvas, blending the whole thing with a specific alpha
    TransCanvas.DrawTo(0, 0, PaintBox.Canvas, PaintBox.Width, PaintBox.Height, CompositionAlphaTrackBar.Position);
    UpdateColorDisplay(BaseColor, PaintBox, TransCanvas);
  finally
    TransCanvas.Free;
  end;

  // Finally, draw an indicator to show where the examined point is
  // This is a small circle around the point (two, actually: one black, one white, for visibility)
  if PtInRect(PaintBox.ClientRect, MouseClickPos) then begin
    PaintBox.Canvas.Pen.Style := psSolid;
    PaintBox.Canvas.Pen.Width := 1;
    PaintBox.Canvas.Brush.Style := bsClear;
    PaintBox.Canvas.Pen.Color := clWhite;
    // Black and white ellipse, one inside the other (so visible over most things)
    PaintBox.Canvas.Ellipse(MouseClickPos.X-IndicatorRadius+1, MouseClickPos.Y-IndicatorRadius+1,
      MouseClickPos.X+IndicatorRadius-1, MouseClickPos.Y+IndicatorRadius-1);
    PaintBox.Canvas.Pen.Color := clBlack;
    PaintBox.Canvas.Ellipse(MouseClickPos.X-IndicatorRadius, MouseClickPos.Y-IndicatorRadius,
      MouseClickPos.X+IndicatorRadius, MouseClickPos.Y+IndicatorRadius);
    PaintBox.Canvas.Pen.Style := psDot;
    PaintBox.Canvas.Brush.Style := bsSolid;
    PaintBox.Canvas.Brush.Color := clWhite;
  end;
end;

procedure TForm1.PreviewPaintBoxPaint(Sender: TObject);
begin
  PreviewPaintBox.Canvas.StretchDraw(PreviewPaintBox.ClientRect, LoadedImage.Picture.Graphic);
end;

procedure TForm1.SetupWindow;
begin
  PaintBox.Left := 0;
  PaintBox.Top := GlassFrame.Top;
  PaintBox.Width := LoadedImage.Width;
  PaintBox.Height := LoadedImage.Height;

  ClientWidth := PaintBox.Width + Panel1.Width;
  ClientHeight := Max(PaintBox.Height + PaintBox.Top, Label12.Top + Panel1.Top + Label12.Height + 8);

  // First time form opens or when an image is loaded, mousepos is set to (-1,-1)
  // Set it to show the middle of the image
  if (MouseClickPos.X < 0) or (MouseClickPos.Y < 0) then begin
    MouseClickPos.X := PaintBox.Width div 2;
    MouseClickPos.Y := PaintBox.Height div 2;
    CalcColors := true;
    UpdateClickCoordsLabel;
  end;

  Invalidate; // So preview image and main image redraw correctly
end;

procedure TForm1.UpdateClickCoordsLabel;
begin
  ClickCoordsLabel.Caption := 'Click the image to examine a pixel.'
    + ' Showing details for (X:' + IntToStr(MouseClickPos.X)
    + ', Y:' + IntToStr(MouseClickPos.Y) + ').';
end;

procedure TForm1.UpdateColorDisplay(BaseColor : TColor; var PaintBox : TPaintBox; var TransCanvas : TTransparentCanvas);
var
  TransColor,
  TransColorRaw,
  FinalColor : COLORREF;
begin
  if CalcColors and PtInRect(PaintBox.ClientRect, MouseClickPos) then begin
    TransColor := TransCanvas.Pixels[MouseClickPos.X, MouseClickPos.Y];
    TransColorRaw := COLORREF(TransCanvas.RawPixels[MouseClickPos.X, MouseClickPos.Y].Quad);
    FinalColor := COLORREF(PaintBox.Canvas.Pixels[MouseClickPos.X, MouseClickPos.Y]);
  end else begin
    TransColor := $00000000;
    TransColorRaw := $00000000;
    FinalColor := $00000000;
  end;

  if CalcColors then begin
    BaseColorLabel.Caption := ColorToText(BaseColor);
    TransColorLabel.Caption := ColorToText(TransColor);
    TransRawColorLabel.Caption := ColorToText(TransColorRaw);
    FinalColorLabel.Caption := ColorToText(FinalColor, false);
  end else begin
    BaseColorLabel.Caption := '(none)';
    TransColorLabel.Caption := '(none)';
    TransRawColorLabel.Caption := '(none)';
    FinalColorLabel.Caption := '(none)';
  end;
  BaseColorPaintBox.Color := TColor(BaseColor);
  TransColorPaintBox.Color := TColor(TransColor);
  TransRawColorPaintBox.Color := TColor(TransColorRaw);
  FinalColorPaintBox.Color := TColor(FinalColor);
  BaseColorPaintBox.Repaint;
  TransColorPaintBox.Repaint;
  TransRawColorPaintBox.Repaint;
  FinalColorPaintBox.Repaint;
end;

end.
