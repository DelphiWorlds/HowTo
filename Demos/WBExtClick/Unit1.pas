unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser, FMX.Edit, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  DW.WebBrowserExt,
  DW.NativeImage;

type
  TForm1 = class(TForm)
    WebBrowser: TWebBrowser;
    ClickWBButton: TButton;
    URLLayout: TLayout;
    URLEdit: TEdit;
    GoButton: TButton;
    Memo: TMemo;
    MouseImage: TNativeImage;
    Layout1: TLayout;
    LeftButton: TButton;
    UpButton: TButton;
    DownButton: TButton;
    RightButton: TButton;
    WebBrowserLayout: TLayout;
    procedure GoButtonClick(Sender: TObject);
    procedure ClickWBButtonClick(Sender: TObject);
    procedure LeftButtonClick(Sender: TObject);
    procedure UpButtonClick(Sender: TObject);
    procedure DownButtonClick(Sender: TObject);
    procedure RightButtonClick(Sender: TObject);
  private
    FWBExt: TWebBrowserExt;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  DW.JavaScript;

const
  cMouseOffset = 4;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FWBExt := TWebBrowserExt.Create(WebBrowser);
end;

procedure TForm1.GoButtonClick(Sender: TObject);
begin
  WebBrowser.Navigate(URLEdit.Text);
end;

procedure TForm1.ClickWBButtonClick(Sender: TObject);
var
  LJavaScript: string;
begin
  LJavaScript := Format(cJavaScriptClickAtXY, [Round(MouseImage.Position.X + 2), Round(MouseImage.Position.Y + 2)]);
  FWBExt.ExecuteJavaScript(LJavaScript, nil);
end;

procedure TForm1.DownButtonClick(Sender: TObject);
begin
  if MouseImage.Position.Y <= (WebBrowser.Height + MouseImage.Height - cMouseOffset) then
    MouseImage.Position.Y := MouseImage.Position.Y + cMouseOffset;
end;

procedure TForm1.LeftButtonClick(Sender: TObject);
begin
  if MouseImage.Position.X >= cMouseOffset then
    MouseImage.Position.X := MouseImage.Position.X - cMouseOffset;
end;

procedure TForm1.RightButtonClick(Sender: TObject);
begin
  if MouseImage.Position.X <= (WebBrowser.Width + MouseImage.Width - cMouseOffset) then
    MouseImage.Position.X := MouseImage.Position.X + cMouseOffset;
end;

procedure TForm1.UpButtonClick(Sender: TObject);
begin
  if MouseImage.Position.Y >= cMouseOffset then
    MouseImage.Position.Y := MouseImage.Position.Y - cMouseOffset;
end;

end.
