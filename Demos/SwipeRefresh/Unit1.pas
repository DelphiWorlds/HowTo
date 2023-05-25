unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView,
  DW.SwipeRefreshLayout;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    ListView1: TListView;
    procedure Timer1Timer(Sender: TObject);
  private
    FRefreshLayout: TSwipeRefreshLayout;
    procedure RefreshLayoutRefreshHandler(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoClick: Boolean = True); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FRefreshLayout := TSwipeRefreshLayout.Create(Self);
  FRefreshLayout.ProgressImage.Bitmap.LoadFromFile('..\..\refresh.png');
  FRefreshLayout.OnRefresh := RefreshLayoutRefreshHandler;
  FRefreshLayout.Parent := Self;
  ListView1.Items.Add.Text := 'One';
  ListView1.Items.Add.Text := 'Two';
  ListView1.Items.Add.Text := 'Three';
  ListView1.Items.Add.Text := 'Four';
  ListView1.Items.Add.Text := 'Five';
end;

procedure TForm1.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FRefreshLayout.AbsoluteMouseDown(PointF(X, Y));
end;

procedure TForm1.MouseMove(Shift: TShiftState; X, Y: Single);
begin
  inherited;
  FRefreshLayout.AbsoluteMouseMove(PointF(X, Y));
end;

procedure TForm1.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single; DoClick: Boolean);
begin
  inherited;
  FRefreshLayout.AbsoluteMouseUp(PointF(X, Y));
end;

procedure TForm1.RefreshLayoutRefreshHandler(Sender: TObject);
begin
  // TOSLog.d('TForm1.RefreshLayoutRefreshHandler');
  // Simulate doing something when pull to refresh happens..
  Timer1.Enabled := True;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  FRefreshLayout.StopRefresh;
end;

end.
