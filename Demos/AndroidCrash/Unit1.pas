unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Memo: TMemo;
    CrashButton: TButton;
    procedure CrashButtonClick(Sender: TObject);
  private
    //
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.JNI.GraphicsContentViewText, Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  FMX.Platform.Android,
  DW.Android.Helpers;

constructor TForm1.Create(AOwner: TComponent);
var
  LIntent: JIntent;
begin
  inherited;
  TAndroidHelperEx.HookUncaughtExceptionHandler;
  LIntent := MainActivity.getIntent;
  if LIntent.getBooleanExtra(StringToJString('CRASHED'), False) then
    Memo.Text := JStringToString(LIntent.getStringExtra(StringToJString('TRACE')));
end;

destructor TForm1.Destroy;
begin
  //
  inherited;
end;

procedure TForm1.CrashButtonClick(Sender: TObject);
begin
  TJDWUtility.JavaClass.crashTest;
end;

end.
