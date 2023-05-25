unit DW.MouseTool;

interface

uses
  System.Types;

type
  TMouseDirection = (None, Up, Left, Right, Down);

  TMouseTool = record
    IsMouseDown: Boolean;
    MouseDownPt: TPointF;
    MouseDownTime: TDateTime;
    MousePt: TPointF;
    MouseUpPt: TPointF;
    MouseUpTime: TDateTime;
    function CalculateDirection(const ACurrentLocation: TPointF; const AMinDistance: Single = 0): TMouseDirection;
    function CalculateSpeed(const ACurrentLocation: TPointF): Single;
    procedure Down(const ALocation: TPointF);
    function IsPan(const AMinSpeed: Single; out ADirection: TMouseDirection; const AMinDistance: Single = 0): Boolean;
    function IsPull(const AMinSpeed: Single; const AMinDistance: Single = 0; const ADirection: TMouseDirection = TMouseDirection.Down): Boolean;
    procedure Move(const ALocation: TPointF);
    procedure Up(const ALocation: TPointF);
  end;

implementation

uses
  System.DateUtils, System.SysUtils;

{ TMouseTool }

function TMouseTool.CalculateDirection(const ACurrentLocation: TPointF; const AMinDistance: Single = 0): TMouseDirection;
var
  LDiffX, LDiffY: Single;
begin
  Result := TMouseDirection.None;
  LDiffX := ACurrentLocation.X - MouseDownPt.X;
  LDiffY := ACurrentLocation.Y - MouseDownPt.Y;
  if (LDiffX < AMinDistance) and (Abs(LDiffX) > Abs(LDiffY)) then
    Result := TMouseDirection.Left
  else if (LDiffX > AMinDistance) and (Abs(LDiffX) > Abs(LDiffY)) then
    Result := TMouseDirection.Right
  else if (LDiffY < AMinDistance) and (Abs(LDiffY) > Abs(LDiffX)) then
    Result := TMouseDirection.Up
  else if (LDiffY > AMinDistance) and (Abs(LDiffY) > Abs(LDiffX)) then
    Result := TMouseDirection.Down;
end;

function TMouseTool.CalculateSpeed(const ACurrentLocation: TPointF): Single;
var
  LPixelDistance: Single;
begin
  LPixelDistance := Sqrt(Sqr(ACurrentLocation.X - MouseDownPt.X) + Sqr(ACurrentLocation.Y - MouseDownPt.Y));
  Result := LPixelDistance / SecondsBetween(MouseDownTime, Now);
end;

procedure TMouseTool.Down(const ALocation: TPointF);
begin
  IsMouseDown := True;
  MouseDownPt := ALocation;
  MouseDownTime := Now;
end;

function TMouseTool.IsPan(const AMinSpeed: Single; out ADirection: TMouseDirection; const AMinDistance: Single = 0): Boolean;
begin
  ADirection := CalculateDirection(MouseUpPt, AMinDistance);
  Result := (ADirection <> TMouseDirection.None) and (CalculateSpeed(MouseUpPt) >= AMinSpeed);
end;

function TMouseTool.IsPull(const AMinSpeed: Single; const AMinDistance: Single = 0; const ADirection: TMouseDirection = TMouseDirection.Down): Boolean;
var
  LDirection: TMouseDirection;
begin
  Result := IsMouseDown and IsPan(AMinSpeed, LDirection, AMinDistance) and (LDirection = ADirection);
end;

procedure TMouseTool.Move(const ALocation: TPointF);
begin
  MousePt := ALocation;
end;

procedure TMouseTool.Up(const ALocation: TPointF);
begin
  MouseUpTime := Now;
  MouseUpPt := ALocation;
  IsMouseDown := False;
end;

end.
