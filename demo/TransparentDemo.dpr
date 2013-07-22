program TransparentDemo;

uses
  Forms,
  TransparentCanvas in '..\TransparentCanvas.pas',
  DemoForm in 'DemoForm.pas' {frmDemo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDemo, frmDemo);
  Application.Run;
end.
