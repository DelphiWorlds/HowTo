unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  Androidapi.JNI.GraphicsContentViewText,
  DW.MultiReceiver.Android, FMX.Memo.Types, FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Layouts;

type
  TMessageReceivedEvent = procedure(Sender: TObject; const Msg: string) of object;

  TLocalReceiver = class(TMultiReceiver)
  private
    FOnMessageReceived: TMessageReceivedEvent;
    procedure DoMessageReceived(const AMsg: string);
  protected
    procedure Receive(context: JContext; intent: JIntent); override;
    procedure ConfigureActions; override;
  public
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
  end;

  TForm1 = class(TForm)
    ContentLayout: TLayout;
    Memo: TMemo;
    ButtonsLayout: TLayout;
    ClearButton: TButton;
    SetAlarmButton: TButton;
    procedure SetAlarmButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
  private
    FReceiver: TLocalReceiver;
    procedure DoMessage(const AMsg: string);
    procedure ReceiverMessageReceivedHandler(Sender: TObject; const AMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.DateUtils,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  DW.Consts.Android, DW.Android.Helpers;

{ TLocalReceiver }

procedure TLocalReceiver.ConfigureActions;
begin
  IntentFilter.addAction(StringToJString(cServiceMessageAction));
  IntentFilter.addAction(StringToJString(cServiceStateAction));
end;

procedure TLocalReceiver.DoMessageReceived(const AMsg: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Self, AMsg);
end;

procedure TLocalReceiver.Receive(context: JContext; intent: JIntent);
begin
  if intent.getAction.equals(StringToJString(cServiceMessageAction)) then
    DoMessageReceived(JStringToString(intent.getStringExtra(StringToJString(cServiceBroadcastParamMessage))));
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FReceiver := TLocalReceiver.Create(True);
  FReceiver.OnMessageReceived := ReceiverMessageReceivedHandler;
end;

destructor TForm1.Destroy;
begin
  FReceiver.Free;
  inherited;
end;

procedure TForm1.DoMessage(const AMsg: string);
begin
  Memo.Lines.Add(AMsg);
end;

procedure TForm1.ReceiverMessageReceivedHandler(Sender: TObject; const AMsg: string);
begin
  DoMessage(AMsg);
end;

procedure TForm1.SetAlarmButtonClick(Sender: TObject);
var
  LAlarm: TDateTime;
begin
  LAlarm := IncSecond(Now, 15);
  TAndroidHelperEx.SetServiceAlarm('com.embarcadero.services.GenericJobService', LAlarm, True, 1234);
  DoMessage('Alarm set for ' + FormatDateTime('hh:nn:ss.zzz', LAlarm));
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  Memo.Lines.Clear;
end;

end.
