unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo,
  Androidapi.JNI.GraphicsContentViewText,
  DW.Content.Android;

type
  TSMSMessage = record
    Address: string;
    Person: Integer;
    Body: string;
    Date: TDateTime;
    MessageType: Integer;
    function DisplayValue: string;
  end;

  TSMSMessages = TArray<TSMSMessage>;

  TSMSContentRetriever = class(TCustomContentRetriever)
  private
    FMessages: TSMSMessages;
  protected
    procedure FetchRecord(const ACursor: JCursor); override;
  public
    procedure FetchAllMessages;
    property Messages: TSMSMessages read FMessages;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FRetriever: TSMSContentRetriever;
    procedure GetSMSMessages;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.DateUtils, System.Permissions,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes;

const
  cPermissionReadSMS = 'android.permission.READ_SMS';

{ TSMSMessage }

function TSMSMessage.DisplayValue: string;
begin
  Result := Format('%s: %s - %s', [FormatDateTime('yyyy-mm-dd hh:nn', Date), Address, Body.Substring(0, 50)]);
end;

{ TSMSContentRetriever }

procedure TSMSContentRetriever.FetchAllMessages;
begin
  FMessages := [];
  FetchContent('sms', ['_id', 'address', 'person', 'body', 'date', 'type']);
end;

procedure TSMSContentRetriever.FetchRecord(const ACursor: JCursor);
var
  LMessage: TSMSMessage;
begin
  // NOTE: Be careful with this code - it will likely crash if there's no such column in the cursor!
  LMessage.Address := JStringToString(ACursor.getString(GetColIndex(ACursor, 'address')));
  LMessage.Person := ACursor.getInt(GetColIndex(ACursor, 'person'));
  LMessage.Body := JStringToString(ACursor.getString(GetColIndex(ACursor, 'body')));
  LMessage.Date := UnixToDateTime(ACursor.getLong(GetColIndex(ACursor, 'date')) div 1000);
  LMessage.MessageType := ACursor.getInt(GetColIndex(ACursor, 'type'));
  FMessages := FMessages + [LMessage];
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FRetriever := TSMSContentRetriever.Create;
end;

destructor TForm1.Destroy;
begin
  FRetriever.Free;
  inherited;
end;

procedure TForm1.GetSMSMessages;
var
  LMessage: TSMSMessage;
begin
  Memo1.Lines.Clear;
  FRetriever.FetchAllMessages;
  for LMessage in FRetriever.Messages do
    Memo1.Lines.Add(LMessage.DisplayValue);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // Needs read SMS permission at runtime. Remember to add the permission to the Uses Permissions section in the Project Options!
  PermissionsService.RequestPermissions([cPermissionReadSMS],
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if AGrantResults[0] = TPermissionStatus.Granted then
        GetSMSMessages;
    end
  );
end;

end.
