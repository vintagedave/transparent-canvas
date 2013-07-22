program ColorOnTitleBar;

uses
  Vcl.Forms,
  TransparentCanvas in '..\TransparentCanvas.pas',
  TitleBarForm in 'TitleBarForm.pas' {FormGlowColor};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormGlowColor, FormGlowColor);
  Application.Run;
end.
