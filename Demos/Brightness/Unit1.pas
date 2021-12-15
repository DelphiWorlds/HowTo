unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.ListBox;

type
  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    procedure ComboBox1Change(Sender: TObject);
  private
    FAppBrightness: Single;
    FOriginalBrightness: Single;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  FMX.Platform,
  DW.UIHelper;

constructor TForm1.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited;
  FOriginalBrightness := TUIHelper.GetBrightness;
  FAppBrightness := FOriginalBrightness;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
  for I := 0 to 100 do
    ComboBox1.Items.Add(I.ToString);
  ComboBox1.ItemIndex := Round(FAppBrightness * 100) - 1;
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  // These measures are so that the app is a "good citizen" on iOS. Changing brightness is system-wide
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
      TUIHelper.SetBrightness(FAppBrightness);
    TApplicationEvent.WillBecomeInactive:
      TUIHelper.SetBrightness(FOriginalBrightness);
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  FAppBrightness := ComboBox1.ItemIndex / 100;
  TUIHelper.SetBrightness(FAppBrightness);
end;

end.
