program ListBuckets;

uses
  Vcl.Forms,
  untMain in 'untMain.pas' {Form10},
  ksAwsS3 in '..\..\ksAwsS3.pas',
  synacode in '..\..\synacode.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm10, Form10);
  Application.Run;
end.
