object FormGlowColor: TFormGlowColor
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'FormGlowColor'
  ClientHeight = 94
  ClientWidth = 231
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 43
    Width = 52
    Height = 13
    Caption = 'Text color:'
  end
  object Label2: TLabel
    Left = 12
    Top = 71
    Width = 53
    Height = 13
    Caption = 'Glow color:'
  end
  object Label3: TLabel
    Left = 12
    Top = 99
    Width = 56
    Height = 13
    Caption = 'Glow alpha:'
  end
  object clbText: TColorBox
    Left = 74
    Top = 39
    Width = 145
    Height = 22
    Selected = clWhite
    Style = [cbStandardColors, cbExtendedColors, cbPrettyNames]
    TabOrder = 0
    OnChange = clbTextChange
  end
  object clbGlow: TColorBox
    Left = 74
    Top = 67
    Width = 145
    Height = 22
    Selected = clNavy
    Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbPrettyNames]
    TabOrder = 1
    OnChange = clbGlowChange
  end
  object trackGlowAlpha: TTrackBar
    Left = 68
    Top = 95
    Width = 158
    Height = 22
    Max = 255
    PageSize = 10
    Position = 255
    TabOrder = 2
    TickStyle = tsNone
    OnChange = trackGlowAlphaChange
  end
end
