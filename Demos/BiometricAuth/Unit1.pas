unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.BiometricAuth, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    BiometricAuth: TBiometricAuth;
    Button1: TButton;
    procedure BiometricAuthAuthenticateFail(Sender: TObject; const FailReason: TBiometricFailReason; const ResultMessage: string);
    procedure BiometricAuthAuthenticateSuccess(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.BiometricAuthAuthenticateFail(Sender: TObject; const FailReason: TBiometricFailReason; const ResultMessage: string);
begin
  ShowMessage('FAILED: ' + ResultMessage);
end;

procedure TForm1.BiometricAuthAuthenticateSuccess(Sender: TObject);
begin
  ShowMessage('Success!');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if BiometricAuth.CanAuthenticate then
    BiometricAuth.Authenticate
  else
    ShowMessage('Can not authenticate');
end;

end.
