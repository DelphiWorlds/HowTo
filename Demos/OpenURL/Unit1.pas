unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    procedure OpenURLCompletion(success: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Macapi.Helpers, Macapi.ObjectiveC,
  iOSapi.Foundation, iOSapi.UIKit;

type
  TUIApplicationOpenURLCompletionProc = procedure(success: Boolean) of object;

  UIApplicationEx = interface(UIApplication)
    ['{C1BE9D49-21ED-4723-90F0-0199A54D16FE}']
    procedure openURL(url: NSURL; options: NSDictionary; completionHandler: TUIApplicationOpenURLCompletionProc); cdecl;
  end;
  TUIApplicationEx = class(TOCGenericImport<UIApplicationClass, UIApplicationEx>) end;

function SharedApplicationEx: UIApplicationEx;
begin
  Result := TUIApplicationEx.Wrap(TUIApplication.OCClass.sharedApplication);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SharedApplicationEx.openURL(StrToNSUrl('https://www.embarcadero.com'), nil, OpenURLCompletion);
end;

procedure TForm1.OpenURLCompletion(success: Boolean);
begin
  // Take action if not successful?
end;

end.
