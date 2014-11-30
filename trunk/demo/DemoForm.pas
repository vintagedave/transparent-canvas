unit DemoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, pngimage, ExtCtrls, TransparentCanvas,
  ComCtrls, StdCtrls;

type
  TfrmDemo = class(TForm)
    LoadedImage: TImage;
    PaintBox: TPaintBox;
    AlphaTrackBar: TTrackBar;
    CompositionAlphaTrackBar: TTrackBar;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label3: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    TextTrackBar: TTrackBar;
    procedure FormPaint(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBarChange(Sender: TObject);
  private
    GlassCanvas : TTransparentCanvas;
    procedure DrawFormCanvas;
    procedure LoadAndDrawMetafile(Canvas : TTransparentCanvas);
  public

  end;

var
  frmDemo: TfrmDemo;

implementation

{$R *.dfm}

procedure TfrmDemo.FormDestroy(Sender: TObject);
begin
  GlassCanvas.Free;
end;

procedure TfrmDemo.FormPaint(Sender: TObject);
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
    GlassCanvas.RoundRect(380, 0, 490, 40, 10, 10, 128);

    GlassCanvas.Pen.Width := 4;
    GlassCanvas.Pen.Color := clWhite;
    GlassCanvas.Brush.Color := clBlue;
    GlassCanvas.RoundRect(480, 10, 510, 45, 15, 15, 192);

    GlassCanvas.Pen.Width := 0;
    GlassCanvas.Brush.Color := clRed;
    GlassCanvas.Rectangle(420, 4, 580, 35, 128);

    GlassCanvas.Font.Style := [fsBold, fsItalic];
    GlassCanvas.GlowTextOut(390, 13, 4, 'Random shapes!');
    GlassCanvas.Font.Style := [fsItalic];
    GlassCanvas.GlowTextOut(516, 13, 4, 'Half-transparent text!', taLeftJustify, 128);
  end;
  GlassCanvas.DrawToGlass(0, 0, Canvas.Handle);
end;

procedure TfrmDemo.LoadAndDrawMetafile(Canvas : TTransparentCanvas);
var
  Metafile : TMetafile;
  Path : string;
begin
  Path := ExtractFilePath(ParamStr(0)) + 'test.emf';
  if not FileExists(Path) then Exit;

  Metafile := TMetafile.Create;
  try
    Metafile.LoadFromFile(Path);
    Canvas.Draw(20, 120, Metafile, Metafile.Width, Metafile.Height, 128);
  finally
    Metafile.Free;
  end;
end;

procedure TfrmDemo.PaintBoxPaint(Sender: TObject);
begin
  DrawFormCanvas; // Creates and draws to it if it doesn't already exist
end;

procedure TfrmDemo.TrackBarChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmDemo.DrawFormCanvas;
const
  TextX = 100;
  TextY = 100;
  TextBorder = 5;
  TextRadius = 12;
  Text = 'Caption with a transparent background';
var
  TransCanvas : TTransparentCanvas;
  TextSize : TSize;
begin
  // Draw the background image to the paintbox, so we can see the transparent effects
  PaintBox.Canvas.Draw(0, 0, LoadedImage.Picture.Graphic);

// Key part: create and draw on a transparent canvas, and paint it to the paintbox
  TransCanvas := TTransparentCanvas.Create(PaintBox.Canvas.Handle, PaintBox.Width, PaintBox.Height);
  try
    TransCanvas.Pen.Width := 1;
    TransCanvas.Brush.Color := clBlack;
    TransCanvas.Font := Screen.CaptionFont;
    TransCanvas.Font.Color := clWhite;

    // Draw text on a transparent roundrect shape:
    // Get the size of the text
    TextSize := TransCanvas.TextExtent(Text);
    // Draw a transparent roundrect that big plus a border
    TransCanvas.RoundRect(Rect(TextX, TextY, TextX + TextSize.cx + TextBorder*2, TextY + TextSize.cy + TextBorder*2),
      TextRadius, TextRadius, AlphaTrackBar.Position);
    TransCanvas.TextOut(TextX + TextBorder, TextX + TextBorder, Text, taLeftJustify, TextTrackBar.Position);

    // Clipped text
    //TransCanvas.TextRect(Rect(5, 5, 150, 15), 'Test clipped text in a rectangle', TextTrackBar.Position);

    // Draw glowing text (Vista+ text with a hazy background) if possible
    TransCanvas.Font.Color := clBlack;
    if TransCanvas.CanDrawGlowText then begin
      TransCanvas.GlowTextOut(60, 50, 4, 'Test glowing text', taLeftJustify, TextTrackBar.Position);
      TransCanvas.GlowTextOutBackColor(60, 75, 4, 'Test glowing text with background color', clLime, taLeftJustify, AlphaTrackBar.Position, TextTrackBar.Position);
    end;

    // Draw some rectangles and rounded rectangles
    TransCanvas.Brush.Color := clRed;
    TransCanvas.Rectangle(Rect(350, 20, 450, 190), AlphaTrackBar.Position);
    TransCanvas.Pen.Color := clWhite;
    TransCanvas.Pen.Width := 5;
    TransCanvas.Brush.Color := clBlue;
    TransCanvas.RoundRect(Rect(150, 150, 420, 300), 50, 50, AlphaTrackBar.Position);
    TransCanvas.Ellipse(150, 150, 420, 300, AlphaTrackBar.Position);

    // Load and draw the example metafile, if it exists
    LoadAndDrawMetafile(TransCanvas);

    // Key part: draw to the normal (paintbox) TCanvas, blending the whole thing with a specific alpha
    TransCanvas.DrawTo(0, 0, PaintBox.Canvas, PaintBox.Width, PaintBox.Height, CompositionAlphaTrackBar.Position);
  finally
    TransCanvas.Free;
  end;
end;


end.
