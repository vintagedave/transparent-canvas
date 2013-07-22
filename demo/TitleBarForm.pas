unit TitleBarForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Types, TransparentCanvas, DwmApi,
  Vcl.ComCtrls;

type
  TFormGlowColor = class(TForm)
    clbText: TColorBox;
    clbGlow: TColorBox;
    Label1: TLabel;
    Label2: TLabel;
    trackGlowAlpha: TTrackBar;
    Label3: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure chkCustomDrawClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure clbTextChange(Sender: TObject);
    procedure clbGlowChange(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure trackGlowAlphaChange(Sender: TObject);
  private
    TitleCanvas : TTransparentCanvas;
    FWndFrameSize : Integer;

    procedure DrawTitleBar;
    procedure RepaintEverything;
    procedure InvalidateTitleBar;

    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMWindowPosChanging(var Message: TWMWindowPosChanging); message WM_WINDOWPOSCHANGING;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
  public
    procedure WndProc(var Message: TMessage); override;
    procedure PaintWindow(DC: HDC); override;
  end;

var
  FormGlowColor: TFormGlowColor;

implementation

{$R *.dfm}

procedure TFormGlowColor.chkCustomDrawClick(Sender: TObject);
begin
  RepaintEverything;
end;

procedure TFormGlowColor.clbGlowChange(Sender: TObject);
begin
  RepaintEverything;
end;

procedure TFormGlowColor.clbTextChange(Sender: TObject);
begin
  RepaintEverything;
end;

procedure TFormGlowColor.DrawTitleBar;
begin
  // The vast majority of this demo is based on code from Chris Rolliston:
  //  - http://delphihaven.wordpress.com/2010/04/19/setting-up-a-custom-titlebar/
  // Thanks Chris - a very useful article, and my apologies for the hacky way I've implemented
  // it here.
  // If you draw a custom title bar, don't base it on this code - go read Chris's articles, which
  // implement several features missing here (eg icons and buttons, and correct positioning
  // of client-area controls.)
  //
  // The below is all that is needed to demonstrate drawing the custom glow colour.
  // GlowTextOutBackColor's parameters are X, Y, glow size, and text (all self-explanatory);
  // then the background glow color. Text color is specified through the font.
  // GlowTextOutBackColor's last two parameters are optional - the one specified here
  // is the alpha (transparency) of the glow itself.  The unspecified one is the alpha of
  // the whole text+glow.
  if not Assigned(TitleCanvas) then begin
    TitleCanvas := TTransparentCanvas.Create(Width, GetSystemMetrics(SM_CYCAPTION) + 8); // Extra for blur
    TitleCanvas.Font.Assign(Screen.CaptionFont);
    TitleCanvas.Font.Color := clbText.Selected;
    TitleCanvas.GlowTextOutBackColor(4, 8, 4, 'Caption with different colours', clbGlow.Selected, trackGlowAlpha.Position);
  end;
  TitleCanvas.DrawToGlass(0, 0, Canvas.Handle);
end;

procedure TFormGlowColor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(TitleCanvas);
end;

procedure TFormGlowColor.FormPaint(Sender: TObject);
begin
  if not GlassFrame.Enabled then Exit;
  DrawTitleBar;
end;

procedure TFormGlowColor.RepaintEverything;
begin
  FreeAndNil(TitleCanvas);
  InvalidateTitleBar;
end;

procedure TFormGlowColor.FormCreate(Sender: TObject);
var
  R: TRect;
begin
  if DwmCompositionEnabled then
  begin
    SetRectEmpty(R);
    AdjustWindowRectEx(R, GetWindowLong(Handle, GWL_STYLE), False,
      GetWindowLong(Handle, GWL_EXSTYLE));
    FWndFrameSize := R.Right;
    GlassFrame.Top := -R.Top;
    GlassFrame.Enabled := True;
    SetWindowPos(Handle, 0, Left, Top, Width, Height, SWP_FRAMECHANGED);
    DoubleBuffered := True;
  end;
end;

procedure TFormGlowColor.trackGlowAlphaChange(Sender: TObject);
begin
  RepaintEverything;
end;

procedure TFormGlowColor.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  if not GlassFrame.Enabled then
    inherited
  else
    with Message.CalcSize_Params.rgrc[0] do
    begin
      Inc(Left, FWndFrameSize);
      Dec(Right, FWndFrameSize);
      Dec(Bottom, FWndFrameSize);
    end;
end;

procedure TFormGlowColor.WndProc(var Message: TMessage);
begin
  if GlassFrame.Enabled and HandleAllocated and DwmDefWindowProc(Handle,
    Message.Msg, Message.WParam, Message.LParam, Message.Result) then Exit;
  inherited;
end;

procedure TFormGlowColor.WMNCHitTest(var Message: TWMNCHitTest);
var
  ClientPos: TPoint;
begin
  inherited;
  if not GlassFrame.Enabled then Exit;
  case Message.Result of
    HTCLIENT: {to be dealt with below};
    HTMINBUTTON, HTMAXBUTTON, HTCLOSE:
    begin
      Message.Result := HTCAPTION; //slay ghost btns when running on Win64
      Exit;
    end;
  else Exit;
  end;
  ClientPos := ScreenToClient(Point(Message.XPos, Message.YPos));
  if ClientPos.Y > GlassFrame.Top then Exit;
  if ControlAtPos(ClientPos, True) <> nil then Exit;
  // Ignore the icon rectangle... (see original demo for details)
  if ClientPos.Y < FWndFrameSize then
    Message.Result := HTTOP
  else
    Message.Result := HTCAPTION;
end;

procedure TFormGlowColor.PaintWindow(DC: HDC);
begin
  with GetClientRect do
    ExcludeClipRect(DC, 0, GlassFrame.Top, Right, Bottom);
  inherited;
end;

procedure TFormGlowColor.InvalidateTitleBar;
var
  R: TRect;
begin
  if not HandleAllocated then Exit;
  R.Left := 0;
  R.Top := 0;
  R.Right := Width;
  R.Bottom := GlassFrame.Top;
  InvalidateRect(Handle, @R, False);
end;

procedure TFormGlowColor.WMWindowPosChanging(var Message: TWMWindowPosChanging);
const
  SWP_STATECHANGED = $8000; //see TCustomForm.WMWindowPosChanging in Forms.pas
begin
  if GlassFrame.Enabled then
    if (Message.WindowPos.flags and SWP_STATECHANGED) = SWP_STATECHANGED then
      Invalidate
    else
      InvalidateTitleBar;
  inherited;
end;

procedure TFormGlowColor.WMActivate(var Message: TWMActivate);
begin
  inherited;
  InvalidateTitleBar;
end;

end.
