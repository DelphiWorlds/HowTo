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

{$IF Defined(ANDROID)}
uses
  DW.Androidapi.JNI.DWUtility, DW.Android.Helpers;
{$ENDIF}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  {$IF Defined(ANDROID)}
  TAndroidHelperEx.HookUncaughtExceptionHandler;
  Memo.Text := TAndroidHelperEx.GetCrashTrace;
  {$ENDIF}
end;

destructor TForm1.Destroy;
begin
  //
  inherited;
end;

procedure TForm1.CrashButtonClick(Sender: TObject);
begin
  {$IF Defined(ANDROID)}
  TJDWUtility.JavaClass.crashTest;
  {$ENDIF}
end;

end.
