unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  DW.Mouse.iOS;

type
  TForm1 = class(TForm)
    MouseCoordsLabel: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
  private
    FMouse: TPlatformMouse;
    procedure MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseRightClickHandler(Sender: TObject; X, Y: Single);
    procedure MouseWheelHandler(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FMouse := TPlatformMouse.Create;
  FMouse.OnMouseMove := MouseMoveHandler;
  FMouse.OnMouseRightClick := MouseRightClickHandler;
  FMouse.OnMouseWheel := MouseWheelHandler;
  Memo1.Lines.Add(Format('Form size: %d x %d', [Width, Height]));
end;

procedure TForm1.MouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Single);
begin
  // NOTE: These are *absolute* screen coords, i.e. NOT form coords
  // The status bar may need to be accounted for
  MouseCoordsLabel.Text := Format('X: %.0f, Y: %.0f', [X, Y]);
end;

procedure TForm1.MouseRightClickHandler(Sender: TObject; X, Y: Single);
begin
  // NOTE: These are *absolute* screen coords, i.e. NOT form coords
  // The status bar may need to be accounted for
  Memo1.Lines.Add(Format('Right-click at X: %.0f, Y: %.0f', [X, Y]));
end;

procedure TForm1.MouseWheelHandler(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; var Handled: Boolean);
begin
  Memo1.Lines.Add(Format('WheelDelta: %d', [WheelDelta]));
end;

end.
