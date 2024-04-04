unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TWifiConnectStatus = (Unknown, Connected, Disconnected, Unavailable, AlreadyConnected, UserDenied);

  TWifiConnectStatusProc = reference to procedure(const Status: TWifiConnectStatus; const Msg: string);

  TSSIDPattern = (Prefix, Suffix, RegEx);

  IWifiConnector = interface(IInterface)
    ['{D036EB90-6B1A-4BE0-A5F1-440F860E44FF}']
    procedure ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc); overload;
    procedure ConnectWifi(const ASSID, APassword: string; const APattern: TSSIDPattern; const AHandler: TWifiConnectStatusProc); overload;
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
  Androidapi.JNI.Os, Androidapi.JNI.Java.Net,
  DW.Androidapi.JNI.DWNetworkCallback;

type
  {$IF CompilerVersion < 36}
  JMacAddress = interface;
  JWifiNetworkSpecifier = interface;
  JWifiNetworkSpecifier_Builder = interface;

  JMacAddressClass = interface(JObjectClass)
    ['{69E534BB-5A11-4B90-A43F-891350E811E3}']
    {class} function _GetBROADCAST_ADDRESS: JMacAddress; cdecl;
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} function _GetTYPE_BROADCAST: Integer; cdecl;
    {class} function _GetTYPE_MULTICAST: Integer; cdecl;
    {class} function _GetTYPE_UNICAST: Integer; cdecl;
    {class} function fromBytes(addr: TJavaArray<Byte>): JMacAddress; cdecl;
    {class} function fromString(addr: JString): JMacAddress; cdecl;
    {class} property BROADCAST_ADDRESS: JMacAddress read _GetBROADCAST_ADDRESS;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
    {class} property TYPE_BROADCAST: Integer read _GetTYPE_BROADCAST;
    {class} property TYPE_MULTICAST: Integer read _GetTYPE_MULTICAST;
    {class} property TYPE_UNICAST: Integer read _GetTYPE_UNICAST;
  end;

  [JavaSignature('android/net/MacAddress')]
  JMacAddress = interface(JObject)
    ['{EA71A3E6-5030-408C-8C98-0D399C41AD39}']
    function describeContents: Integer; cdecl;
    function equals(o: JObject): Boolean; cdecl;
    function getAddressType: Integer; cdecl;
    function getLinkLocalIpv6FromEui48Mac: JInet6Address; cdecl;
    function hashCode: Integer; cdecl;
    function isLocallyAssigned: Boolean; cdecl;
    function matches(baseAddress: JMacAddress; mask: JMacAddress): Boolean; cdecl;
    function toByteArray: TJavaArray<Byte>; cdecl;
    function toOuiString: JString; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(out_: JParcel; flags: Integer); cdecl;
  end;
  TJMacAddress = class(TJavaGenericImport<JMacAddressClass, JMacAddress>) end;

  JWifiNetworkSpecifierClass = interface(JNetworkSpecifierClass)
    ['{5ABD490B-DE98-44D9-B514-58E0967950DA}']
    {class} function _GetCREATOR: JParcelable_Creator; cdecl;
    {class} property CREATOR: JParcelable_Creator read _GetCREATOR;
  end;

  [JavaSignature('android/net/wifi/WifiNetworkSpecifier')]
  JWifiNetworkSpecifier = interface(JNetworkSpecifier)
    ['{70B73F1B-1EA2-40D9-9FFB-9131ED4E9C6A}']
    function describeContents: Integer; cdecl;
    function equals(obj: JObject): Boolean; cdecl;
    function getBand: Integer; cdecl;
    function hashCode: Integer; cdecl;
    function toString: JString; cdecl;
    procedure writeToParcel(dest: JParcel; flags: Integer); cdecl;
  end;
  TJWifiNetworkSpecifier = class(TJavaGenericImport<JWifiNetworkSpecifierClass, JWifiNetworkSpecifier>) end;

  JWifiNetworkSpecifier_BuilderClass = interface(JObjectClass)
    ['{C40D50C1-1BC0-4956-BB23-FEDEBE6B2621}']
    {class} function init: JWifiNetworkSpecifier_Builder; cdecl;
  end;

  [JavaSignature('android/net/wifi/WifiNetworkSpecifier$Builder')]
  JWifiNetworkSpecifier_Builder = interface(JObject)
    ['{36947CB4-1F66-41C8-8CFD-C8AA0205E7AF}']
    function build: JWifiNetworkSpecifier; cdecl;
    function setBand(band: Integer): JWifiNetworkSpecifier_Builder; cdecl;
    function setBssid(bssid: JMacAddress): JWifiNetworkSpecifier_Builder; cdecl;
    function setBssidPattern(baseAddress: JMacAddress; mask: JMacAddress): JWifiNetworkSpecifier_Builder; cdecl;
    function setIsEnhancedOpen(isEnhancedOpen: Boolean): JWifiNetworkSpecifier_Builder; cdecl;
    function setIsHiddenSsid(isHiddenSsid: Boolean): JWifiNetworkSpecifier_Builder; cdecl;
    function setSsid(ssid: JString): JWifiNetworkSpecifier_Builder; cdecl;
    function setSsidPattern(ssidPattern: JPatternMatcher): JWifiNetworkSpecifier_Builder; cdecl;
    function setWpa2EnterpriseConfig(enterpriseConfig: JWifiEnterpriseConfig): JWifiNetworkSpecifier_Builder; cdecl;
    function setWpa2Passphrase(passphrase: JString): JWifiNetworkSpecifier_Builder; cdecl;
    function setWpa3Enterprise192BitModeConfig(enterpriseConfig: JWifiEnterpriseConfig): JWifiNetworkSpecifier_Builder; cdecl;
    function setWpa3EnterpriseConfig(enterpriseConfig: JWifiEnterpriseConfig): JWifiNetworkSpecifier_Builder; cdecl;
    function setWpa3EnterpriseStandardModeConfig(enterpriseConfig: JWifiEnterpriseConfig): JWifiNetworkSpecifier_Builder; cdecl;
    function setWpa3Passphrase(passphrase: JString): JWifiNetworkSpecifier_Builder; cdecl;
  end;
  TJWifiNetworkSpecifier_Builder = class(TJavaGenericImport<JWifiNetworkSpecifier_BuilderClass, JWifiNetworkSpecifier_Builder>) end;
  {$ENDIF}
  
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
    procedure DoConnectWifi(const ABuilder: JWifiNetworkSpecifier_Builder; const APassword: string);
  protected
    procedure NetworkAvailable(const ANetwork: JNetwork); cdecl;
    procedure NetworkLost(const ANetwork: JNetwork); cdecl;
    procedure NetworkUnavailable; cdecl;
  public
    { IWifiConnector }
    procedure ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc); overload;
    procedure ConnectWifi(const ASSID, APassword: string; const APattern: TSSIDPattern; const AHandler: TWifiConnectStatusProc); overload;
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

procedure TWifiConnector.DoConnectWifi(const ABuilder: JWifiNetworkSpecifier_Builder; const APassword: string);
var
  LRequest: JNetworkRequest;
begin
  LRequest := TJNetworkRequest_Builder.JavaClass.init
    .addTransportType(TJNetworkCapabilities.JavaClass.TRANSPORT_WIFI)
    .setNetworkSpecifier(ABuilder.setWpa2Passphrase(StringToJString(APassword)).build)
    .build;
  FConnectivityManager.requestNetwork(LRequest, FCallbackDelegate.Callback);
end;

procedure TWifiConnector.ConnectWifi(const ASSID, APassword: string; const APattern: TSSIDPattern; const AHandler: TWifiConnectStatusProc);
var
  LMatcher: JPatternMatcher;
begin
  FStatusHandler := AHandler;
  case APattern of
    TSSIDPattern.RegEx:
      LMatcher := TJPatternMatcher.JavaClass.init(StringToJString(ASSID), TJPatternMatcher.JavaClass.PATTERN_ADVANCED_GLOB);
    TSSIDPattern.Suffix:
      LMatcher := TJPatternMatcher.JavaClass.init(StringToJString(ASSID), TJPatternMatcher.JavaClass.PATTERN_SUFFIX);
  else
    LMatcher := TJPatternMatcher.JavaClass.init(StringToJString(ASSID), TJPatternMatcher.JavaClass.PATTERN_PREFIX);
  end;
  DoConnectWifi(TJWifiNetworkSpecifier_Builder.JavaClass.init.setSsidPattern(LMatcher), APassword);
end;

procedure TWifiConnector.ConnectWifi(const ASSID, APassword: string; const AHandler: TWifiConnectStatusProc);
begin
  FStatusHandler := AHandler;
  DoConnectWifi(TJWifiNetworkSpecifier_Builder.JavaClass.init.setSsid(StringToJString(ASSID)), APassword);
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
  // The following variant needs the *exact* SSID
  // FWifiConnector.ConnectWifi('YourSSID', 'YourPassword',
  // This variant finds SSIDs with a prefix that matches the string
  FWifiConnector.ConnectWifi('Your', 'YourPassword', TSSIDPattern.Prefix,
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
