unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TForm1 = class(TForm)
    Image1: TImage;
    SetButton: TButton;
    ClearButton: TButton;
    procedure SetButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    //
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers,
  DW.Androidapi.JNI.App, DW.Graphics.Helpers.Android;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  TJWallpaperManager.JavaClass.getInstance(TAndroidHelper.Context).clear;
end;

procedure TForm1.SetButtonClick(Sender: TObject);
begin
  TJWallpaperManager.JavaClass.getInstance(TAndroidHelper.Context).setBitmap(Image1.Bitmap.ToJBitmap);
end;

end.
