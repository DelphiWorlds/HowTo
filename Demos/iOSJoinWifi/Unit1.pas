unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  iOSapi.Foundation, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure JoinWifi(const ASSID, APassword: string);
    procedure JoinWifiHandler(error: NSError);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Macapi.Helpers,
  DW.iOSapi.NetworkExtension;

procedure TForm1.JoinWifi(const ASSID, APassword: string);
var
  LConfiguration: NEHotspotConfiguration;
begin
  LConfiguration := TNEHotspotConfiguration.Wrap(TNEHotspotConfiguration.Alloc.initWithSSID(StrToNSStr(ASSID), StrToNSStr(APassword), False));
  LConfiguration.setJoinOnce(False);
  TNEHotspotConfigurationManager.OCClass.sharedManager.applyConfiguration(LConfiguration, JoinWifiHandler);
end;

procedure TForm1.JoinWifiHandler(error: NSError);
begin
  if error <> nil then
  begin
    if error.code = NEHotspotConfigurationErrorAlreadyAssociated then
      Memo1.Lines.Add('Already connected')
    else if error.code = NEHotspotConfigurationErrorUserDenied then
      Memo1.Lines.Add('User denied')
    else
      Memo1.Lines.Add('Error: ' + NSStrToStr(error.localizedDescription));
  end
  else
    Memo1.Lines.Add('Connected!');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  JoinWifi('YourSSID', 'YourPassword');
end;

end.
