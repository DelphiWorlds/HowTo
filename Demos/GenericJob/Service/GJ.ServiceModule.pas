unit GJ.ServiceModule;

interface

uses
  System.SysUtils, System.Classes, System.Android.Service,
  AndroidApi.JNI.GraphicsContentViewText, Androidapi.JNI.Os;

type
  TServiceModule = class(TAndroidIntentService)
    procedure AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
  private
    procedure DoMessage(const AMsg: string);
  public
    { Public declarations }
  end;

var
  ServiceModule: TServiceModule;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

uses
  System.DateUtils,
  Androidapi.Helpers, Androidapi.JNI.JavaTypes,
  {$IF CompilerVersion < 35}
  DW.Androidapi.JNI.SupportV4,
  {$ELSE}
  DW.Androidapi.JNI.AndroidX.LocalBroadcastManager,
  {$ENDIF}
  DW.OSLog, DW.Android.Helpers, DW.Consts.Android;

function GetLogTime: string;
begin
  Result := FormatDateTime('mm-dd hh:nn:ss.zzz', Now);
end;

procedure TServiceModule.AndroidIntentServiceHandleIntent(const Sender: TObject; const AnIntent: JIntent);
var
  LAlarm: TDateTime;
begin
  LAlarm := IncSecond(Now, 15);
  TAndroidHelperEx.SetServiceAlarm('com.embarcadero.services.GenericJobService', LAlarm, True, 1234);
  DoMessage('Next alarm for service set for ' + FormatDateTime('hh:nn:ss.zzz', LAlarm));
  // Do the work the service is to perform, here
end;

procedure TServiceModule.DoMessage(const AMsg: string);
var
  LIntent: JIntent;
begin
  TOSLog.d(AMsg);
  LIntent := TJIntent.JavaClass.init(StringToJString(cServiceMessageAction));
  LIntent.putExtra(StringToJString(cServiceBroadcastParamMessage), StringToJString(GetLogTime + ': ' + AMsg));
  TJLocalBroadcastManager.JavaClass.getInstance(TAndroidHelper.Context).sendBroadcast(LIntent);
end;

end.
