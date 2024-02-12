unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TWifiConnectStatus = (Unknown, Connected, Disconnected, Unavailable, AlreadyConnected, UserDenied);

  TWifiConnectStatusProc = reference to procedure(const Status: TWifiConnectStatus; const Msg: string);

  IWifiConnector = interface(IInterface)
    ['{D036EB90-6B1A-4BE0-A5F1-440F860E44FF}']
    procedure ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc);
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FWifiConnector: IWifiConnector;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{$IF Defined(ANDROID)}
uses
  DW.OSLog,
  Androidapi.JNI.Net, Androidapi.Helpers, Androidapi.JNI.GraphicsContentViewText, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  DW.Androidapi.JNI.DWNetworkCallback;

type
  TWifiConnector = class;

  TNetworkCallbackDelegate = class(TJavaLocal, JDWNetworkCallbackDelegate)
  private
    FCallback: JDWNetworkCallback;
    FConnector: TWifiConnector;
  protected
    property Callback: JDWNetworkCallback read FCallback;
  public
    { JDWNetworkCallbackDelegate }
    procedure onAvailable(network: JNetwork); cdecl;
    procedure onLost(network: JNetwork); cdecl;
    procedure onUnavailable; cdecl;
  public
    constructor Create(const AConnector: TWifiConnector);
  end;

  TWifiConnector = class(TInterfacedObject, IWifiConnector)
  private
    FCallbackDelegate: TNetworkCallbackDelegate;
    FConnectivityManager: JConnectivityManager;
    FStatusHandler: TWifiConnectStatusProc;
    procedure DoWifiConnectStatus(const AStatus: TWifiConnectStatus; const AMsg: string);
  protected
    procedure NetworkAvailable(const ANetwork: JNetwork); cdecl;
    procedure NetworkLost(const ANetwork: JNetwork); cdecl;
    procedure NetworkUnavailable; cdecl;
  public
    { IWifiConnector }
    procedure ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc);
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TNetworkCallbackDelegate }

constructor TNetworkCallbackDelegate.Create(const AConnector: TWifiConnector);
begin
  inherited Create;
  FConnector := AConnector;
  FCallback := TJDWNetworkCallback.JavaClass.init(TAndroidHelper.Context, Self, True);
end;

procedure TNetworkCallbackDelegate.onAvailable(network: JNetwork);
begin
  FConnector.NetworkAvailable(network)
end;

procedure TNetworkCallbackDelegate.onLost(network: JNetwork);
begin
  FConnector.NetworkLost(network)
end;

procedure TNetworkCallbackDelegate.onUnavailable;
begin
  FConnector.NetworkUnavailable;
end;

{ TWifiConnector }

constructor TWifiConnector.Create;
begin
  inherited;
  FConnectivityManager := TJConnectivityManager.Wrap(TAndroidHelper.Context.getSystemService(TJContext.JavaClass.CONNECTIVITY_SERVICE));
  FCallbackDelegate := TNetworkCallbackDelegate.Create(Self);
end;

destructor TWifiConnector.Destroy;
begin
  FCallbackDelegate.Free;
  inherited;
end;

procedure TWifiConnector.ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc);
var
  LSpecifier: JWifiNetworkSpecifier;
  LRequest: JNetworkRequest;
begin
  FStatusHandler := AHandler;
  LSpecifier := TJWifiNetworkSpecifier_Builder.JavaClass.init
    .setSsid(StringToJString(ASSID))
    .setWpa2Passphrase(StringToJString(APassword))
    .build;
  LRequest := TJNetworkRequest_Builder.JavaClass.init
    .addTransportType(TJNetworkCapabilities.JavaClass.TRANSPORT_WIFI)
    .setNetworkSpecifier(LSpecifier)
    .build;
  FConnectivityManager.requestNetwork(LRequest, FCallbackDelegate.Callback);
end;

procedure TWifiConnector.NetworkAvailable(const ANetwork: JNetwork);
begin
  FConnectivityManager.bindProcessToNetwork(ANetwork);
  DoWifiConnectStatus(TWifiConnectStatus.Connected, '');
end;

procedure TWifiConnector.NetworkLost(const ANetwork: JNetwork);
begin
  FConnectivityManager.bindProcessToNetwork(nil);
  DoWifiConnectStatus(TWifiConnectStatus.Disconnected, '');
end;

procedure TWifiConnector.NetworkUnavailable;
begin
  DoWifiConnectStatus(TWifiConnectStatus.Unavailable, '');
end;

procedure TWifiConnector.DoWifiConnectStatus(const AStatus: TWifiConnectStatus; const AMsg: string);
begin
  if Assigned(FStatusHandler) then
    FStatusHandler(AStatus, AMsg);
end;
{$ENDIF}

{$IF Defined(IOS)}
uses
  iOSapi.Foundation,
  Macapi.Helpers,
  DW.iOSapi.NetworkExtension;

type
  TWifiConnector = class(TInterfacedObject, IWifiConnector)
  private
    FStatusHandler: TWifiConnectStatusProc;
    procedure DoWifiConnectStatus(const AStatus: TWifiConnectStatus; const AMsg: string);
    procedure ConnectWifiHandler(error: NSError);
  public
    { IWifiConnector }
    procedure ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc);
  end;

{ TWifiConnector }

procedure TWifiConnector.DoWifiConnectStatus(const AStatus: TWifiConnectStatus; const AMsg: string);
begin
  if Assigned(FStatusHandler) then
    FStatusHandler(AStatus, AMsg);
end;

procedure TWifiConnector.ConnectWifiHandler(error: NSError);
begin
  if error <> nil then
  begin
    if error.code = NEHotspotConfigurationErrorAlreadyAssociated then
      DoWifiConnectStatus(TWifiConnectStatus.AlreadyConnected, '')
    else if error.code = NEHotspotConfigurationErrorUserDenied then
      DoWifiConnectStatus(TWifiConnectStatus.UserDenied, '')
    else
      DoWifiConnectStatus(TWifiConnectStatus.Unknown, NSStrToStr(error.localizedDescription));
  end
  else
    DoWifiConnectStatus(TWifiConnectStatus.Connected, '');
end;

procedure TWifiConnector.ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc);
var
  LConfiguration: NEHotspotConfiguration;
begin
  FStatusHandler := AHandler;
  LConfiguration := TNEHotspotConfiguration.Wrap(TNEHotspotConfiguration.Alloc.initWithSSID(StrToNSStr(ASSID), StrToNSStr(APassword), False));
  LConfiguration.setJoinOnce(False);
  TNEHotspotConfigurationManager.OCClass.sharedManager.applyConfiguration(LConfiguration, ConnectWifiHandler);
end;
{$ENDIF}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if FWifiConnector = nil then
    FWifiConnector := TWifiConnector.Create;
  FWifiConnector.ConnectWifi('YourSSID', 'YourPassword',
    procedure(const AStatus: TWifiConnectStatus; const AMsg: string)
    begin
      case AStatus of
        TWifiConnectStatus.AlreadyConnected:
          Memo1.Lines.Add('Already connected');
        TWifiConnectStatus.Connected:
          Memo1.Lines.Add('Connected!');
        TWifiConnectStatus.Disconnected:
          Memo1.Lines.Add('Disconnected');
        TWifiConnectStatus.Unavailable:
          Memo1.Lines.Add('Unavailable');
        TWifiConnectStatus.Unknown:
          Memo1.Lines.Add('Unhandled error: ' + AMsg);
        TWifiConnectStatus.UserDenied:
          Memo1.Lines.Add('User denied!');
      end;
    end
  );
end;

end.
