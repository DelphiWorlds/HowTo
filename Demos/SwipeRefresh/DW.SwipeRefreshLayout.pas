unit DW.SwipeRefreshLayout;

interface

uses
  System.Classes, System.Types, System.UITypes,
  FMX.Layouts, FMX.Objects, FMX.Types, FMX.Ani,
  DW.MouseTool;

type
  TSwipeRefreshLayout = class(TLayout)
  private
    FImageAnimation: TFloatAnimation;
    FIsAutoReturn: Boolean;
    FIsRefreshing: Boolean;
    FMouseTool: TMouseTool;
    FSliderAnimation: TFloatAnimation;
    FSliderLayout: TLayout;
    FSliderTimer: TTimer;
    FProgressLayout: TLayout;
    FProgressCircle: TCircle;
    FProgressImage: TImage;
    FOnRefresh: TNotifyEvent;
    procedure DoRefresh;
    procedure SliderAnimationFinishHandler(Sender: TObject);
    procedure SliderTimerHandler(Sender: TObject);
    procedure ToggleSlider;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AbsoluteMouseDown(const ALocation: TPointF);
    procedure AbsoluteMouseMove(const ALocation: TPointF);
    procedure AbsoluteMouseUp(const ALocation: TPointF);
    procedure StopRefresh;
    property ProgressImage: TImage read FProgressImage;
    property OnRefresh: TNotifyEvent read FOnRefresh write FOnRefresh;
  end;

implementation

uses
  System.SysUtils, System.DateUtils, System.Math,
  FMX.Graphics;

{ TSwipeRefreshLayout }

constructor TSwipeRefreshLayout.Create(AOwner: TComponent);
begin
  inherited;
  Align := TAlignLayout.Contents;
  FSliderLayout := TLayout.Create(Self);
  FSliderLayout.Align := TAlignLayout.Top;
  FSliderLayout.Height := 0;
  FSliderLayout.Visible := False;
  FSliderLayout.Parent := Self;
  FSliderAnimation := TFloatAnimation.Create(Self);
  FSliderAnimation.PropertyName := 'Height';
  FSliderAnimation.StartFromCurrent := True;
  FSliderAnimation.Duration := 0.3;
  FSliderAnimation.OnFinish := SliderAnimationFinishHandler;
  FSliderAnimation.Parent := FSliderLayout;
  FProgressLayout := TLayout.Create(Self);
  FProgressLayout.Align := TAlignLayout.Bottom;
  FProgressLayout.Height := 53;
  FProgressLayout.Parent := FSliderLayout;
  FProgressCircle := TCircle.Create(Self);
  FProgressCircle.HitTest := False;
  FProgressCircle.Height := 50;
  FProgressCircle.Width := 50;
  FProgressCircle.Align := TAlignLayout.Center;
  FProgressCircle.Stroke.Kind := TBrushKind.None;
  FProgressCircle.Parent := FProgressLayout;
  FProgressImage := TImage.Create(Self);
  FProgressImage.HitTest := False;
  FProgressImage.Height := 32;
  FProgressImage.Width := 32;
  FProgressImage.Align := TAlignLayout.Center;
  FProgressImage.Parent := FProgressCircle;
  FImageAnimation := TFloatAnimation.Create(Self);
  FImageAnimation.PropertyName := 'RotationAngle';
  FImageAnimation.StartFromCurrent := True;
  FImageAnimation.Duration := 0.3;
  // FImageAnimation.OnFinish := SliderAnimationFinishHandler;
  FImageAnimation.Parent := FProgressImage;
  FSliderTimer := TTimer.Create(Self);
  FSliderTimer.OnTimer := SliderTimerHandler;
  FSliderTimer.Enabled := False;
end;

procedure TSwipeRefreshLayout.DoRefresh;
begin
  if Assigned(FOnRefresh) then
    FOnRefresh(Self);
end;

procedure TSwipeRefreshLayout.ToggleSlider;
begin
  if FIsRefreshing then
  begin
    FSliderLayout.Height := 0;
    FSliderLayout.Visible := True;
    FSliderAnimation.StopValue := Height / 5;
    FProgressImage.RotationAngle := 0;
    FImageAnimation.StopValue := 135;
  end
  else
    FSliderAnimation.StopValue := 0;
  FImageAnimation.Enabled := FIsRefreshing;
  FSliderAnimation.Enabled := True;
end;

procedure TSwipeRefreshLayout.SliderAnimationFinishHandler(Sender: TObject);
begin
  FImageAnimation.Enabled := False;
  FSliderAnimation.Enabled := False;
  if FIsRefreshing then
    DoRefresh;
end;

procedure TSwipeRefreshLayout.SliderTimerHandler(Sender: TObject);
begin
  FSliderTimer.Enabled := False;
  ToggleSlider;
end;

procedure TSwipeRefreshLayout.StopRefresh;
var
  LTimeRemaining: Integer;
begin
  if FIsRefreshing then
  begin
    FIsRefreshing := False;
    LTimeRemaining := 1000 - MilliSecondsBetween(FMouseTool.MouseDownTime, Now);
    if LTimeRemaining > 0 then
    begin
      FSliderTimer.Interval := LTimeRemaining;
      FSliderTimer.Enabled := True;
    end
    else
      ToggleSlider;
  end;
end;

procedure TSwipeRefreshLayout.AbsoluteMouseDown(const ALocation: TPointF);
begin
  if AbsoluteRect.Contains(ALocation) then
    FMouseTool.Down(ALocation);
end;

procedure TSwipeRefreshLayout.AbsoluteMouseMove(const ALocation: TPointF);
begin
  if FMouseTool.IsMouseDown then
  begin
    FMouseTool.Move(ALocation);
    // Allow pulls if the pull occurs within the control
    if not FIsRefreshing and AbsoluteRect.Contains(ALocation) and FMouseTool.IsPull(100, 50) then
    begin
      FIsRefreshing := True;
      ToggleSlider;
    end;
  end;
end;

procedure TSwipeRefreshLayout.AbsoluteMouseUp(const ALocation: TPointF);
begin
  FMouseTool.Up(ALocation);
end;

end.
