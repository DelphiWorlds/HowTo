program RTStyleDemo;

{$I Styles\Style.inc}

uses
  System.StartUpCopy,
  FMX.Forms,
  System.IOUtils,
  FMX.Styles,
  Unit1 in 'Unit1.pas' {Form1};

{$R *.res}

begin
  // TStyleManager.SetStyleFromFile(TPath.Combine(TPath.GetDocumentsPath, 'Style.fsf'));
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


