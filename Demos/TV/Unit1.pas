unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

{$SCOPEDENUMS ON}

type
  TKeyNavigation = (None, Home, Back, Left, Right, Up, Down, OK);

  TForm1 = class(TForm)
  private
    FPreventExit: Boolean;
    procedure KeyNavigation(const ANav: TKeyNavigation);
  public
    constructor Create(AOwner: TComponent); override;
    procedure KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  DW.OSLog;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FPreventExit := True;
end;

procedure TForm1.KeyNavigation(const ANav: TKeyNavigation);
begin
  TOSLog.d('Nav: %s', [GetEnumName(TypeInfo(TKeyNavigation), Ord(ANav))]);
end;

procedure TForm1.KeyUp(var Key: Word; var KeyChar: System.WideChar; Shift: TShiftState);
var
  LKeyNavigation: TKeyNavigation;
begin
  LKeyNavigation := TKeyNavigation.None;
  case Key of
    vkHardwareBack, vkEscape:
      LKeyNavigation := TKeyNavigation.Back;
    vkleft:
      LKeyNavigation := TKeyNavigation.Left;
    vkUp:
      LKeyNavigation := TKeyNavigation.Up;
    vkRight:
      LKeyNavigation := TKeyNavigation.Right;
    vkDown:
      LKeyNavigation := TKeyNavigation.Down;
    0, vkReturn:
    begin
      if KeyChar = #0 then
        LKeyNavigation := TKeyNavigation.OK;
    end;
  end;
  if FPreventExit and ((Key = vkHardwareBack) or (Key = vkEscape)) then
  begin
    Key := 0;
    KeyChar := #0;
  end;
  if (LKeyNavigation <> TKeyNavigation.None) then
    KeyNavigation(LKeyNavigation);
end;
end.
