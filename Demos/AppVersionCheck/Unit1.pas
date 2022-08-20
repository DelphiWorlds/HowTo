unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation, FMX.ScrollBox,
  FMX.Memo,
  DW.AppStoreCheck;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    ButtonsLayout: TLayout;
    CheckButton: TButton;
    procedure CheckButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.CheckButtonClick(Sender: TObject);
begin
  TAppStoreCheck.Check(TAppStore.iTunes, '1303293864',
    procedure(const AAppStoreCheckResult: TAppStoreCheckResult)
    begin
      if AAppStoreCheckResult.Succeeded then
      begin
        Memo.Lines.Add('App Name: ' + AAppStoreCheckResult.AppStoreInfo.AppName);
        Memo.Lines.Add('Bundle Id: ' + AAppStoreCheckResult.AppStoreInfo.BundleId);
        Memo.Lines.Add('Seller Name: ' + AAppStoreCheckResult.AppStoreInfo.SellerName);
        Memo.Lines.Add('Version: ' + AAppStoreCheckResult.AppStoreInfo.Version);
      end
      else
        Memo.Lines.Add(Format('Check failed - %d: %s', [AAppStoreCheckResult.StatusCode, AAppStoreCheckResult.ErrorMessage]));
    end
  );
end;

end.
