unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, TaurusTLS;

type
  TForm1 = class(TForm)
    Button1: TButton;
    IdHTTP: TIdHTTP;
    Memo: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FIOHandler: TTaurusTLSIOHandlerSocket;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FIOHandler = nil then
    FIOHandler := TTaurusTLSIOHandlerSocket.Create(nil);
  IdHTTP.IOHandler := FIOHandler;
  Memo.Text := IdHTTP.Get('https://www.embarcadero.com');
end;

end.
