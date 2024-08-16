unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, System.Messaging,
  Androidapi.JNI.GraphicsContentViewText,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FChannelId: Int64;
    FHasLaunched: Boolean;
    FProgramId: Int64;
    procedure ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
    procedure CreateTvChannel;
    procedure CreateTvChannelPrograms;
    procedure HandleIntent(const AIntent: JIntent);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  Androidapi.JNI.Net, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNI.Media, Androidapi.JNI.App,
  FMX.Platform, FMX.Surfaces, FMX.Helpers.Android,
  DW.Androidapi.JNI.AndroidX.TvProvider,
  DW.OSLog;

function BitmapToJBitmap(const ABitmap: TBitmap): JBitmap;
var
  LSurface: TBitmapSurface;
begin
  Result := TJBitmap.JavaClass.createBitmap(ABitmap.Width, ABitmap.Height, TJBitmap_Config.JavaClass.ARGB_8888);
  LSurface := TBitmapSurface.Create;
  try
    LSurface.Assign(ABitmap);
    SurfaceToJBitmap(LSurface, Result);
  finally
    LSurface.Free;
  end;
end;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  TMessageManager.DefaultManager.SubscribeToMessage(TApplicationEventMessage, ApplicationEventMessageHandler);
end;

destructor TForm1.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TApplicationEventMessage, ApplicationEventMessageHandler);
  inherited;
end;

procedure TForm1.ApplicationEventMessageHandler(const Sender: TObject; const AMsg: TMessage);
begin
  case TApplicationEventMessage(AMsg).Value.Event of
    TApplicationEvent.BecameActive:
    begin
      TOSLog.d('TApplicationEvent.BecameActive');
      if not FHasLaunched then
      begin
        FHasLaunched := True;
        TOSLog.d('> from launch');
      end;
      HandleIntent(TAndroidHelper.Activity.getIntent);
    end;
  end;
end;

procedure TForm1.HandleIntent(const AIntent: JIntent);
var
  LDataUri: Jnet_Uri;
  LPath, LVideoId: string;
begin
  LDataUri := AIntent.getData;
  if LDataUri <> nil then
  begin
    // Assume the scheme applies to this app, since it received it ;-)
    LPath := JStringToString(LDataUri.getPath);
    TOSLog.d('Intent LPath: %s', [LPath]);
    if LPath.StartsWith('/video') then
    begin
      // e.g. /video/12345
      LVideoId := LPath.Substring(7);
      // LVideoId should now equal the last part of the path
      TOSLog.d('User selected video id: %s', [LVideoId]);
    end
    else if LPath.StartsWith('/channel') then
    begin
      // Do something channel related
    end;
  end
  else
    TOSLog.d('No data uri in intent');
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  CreateTvChannel; // NOTE: Do not ever remove the FIRST channel added - you can change the details, but do NOT remove it
  CreateTvChannelPrograms;
end;

procedure TForm1.CreateTvChannel;
const
  cChannelId = '1';
var
  LBuilder: JChannel_Builder;
  LChannelUri, LAppLinkUri: Jnet_Uri;
  LBitmap: TBitmap;
  LNativeBitmap: JBitmap;
begin
  LAppLinkUri := TJnet_Uri.JavaClass.parse(StringToJString('atvdemo://open/channel'));
  LBuilder := TJChannel_Builder.JavaClass.init
    .setType(TJTvContractCompat_Channels.JavaClass.TYPE_PREVIEW)
    .setAppLinkIntentUri(LAppLinkUri) // Clicking the channel icon will open the app using this uri
    .setInputId(StringToJString('com.embarcadero.ATVDemo'))
    .setInternalProviderId(StringToJString(cChannelId))
    .setDisplayName(StringToJString('Kastri Channel'));
  LChannelUri := TAndroidHelper.Context.getContentResolver.insert(TJTvContractCompat_Channels.JavaClass.CONTENT_URI, LBuilder.build.toContentValues);
  FChannelId := TJContentUris.JavaClass.parseId(LChannelUri);
  TOSLog.d('> Channel id: %d', [FChannelId]);
  TJTvContractCompat.JavaClass.requestChannelBrowsable(TAndroidHelper.Context, FChannelId);
  // TOSLog.d('requestChannelBrowsable done');
  LBitmap := TBitmap.Create;
  try
    LBitmap.LoadFromFile(TPath.Combine(TPath.GetDocumentsPath, 'Images/kastri-logo.png'));
    LNativeBitmap := BitmapToJBitmap(LBitmap);
  finally
    LBitmap.Free;
  end;
  // Alternatively, use a uri to an 80 x 80 opaque image
  TJChannelLogoUtils.JavaClass.storeChannelLogo(TAndroidHelper.Context, FChannelId, LNativeBitmap);
end;

procedure TForm1.CreateTvChannelPrograms;
const
  cEpisodeId = '12345';
var
  LBuilder: JPreviewProgram_Builder;
  LProgramUri, LIntentUri, LPosterArtUri: Jnet_Uri;
begin
  LIntentUri := TJnet_Uri.JavaClass.parse(StringToJString('atvdemo://open/video/' + cEpisodeId));
  LPosterArtUri := TJnet_Uri.JavaClass.parse(StringToJString('https://raw.githubusercontent.com/DelphiWorlds/Kastri/master/kastri-logo.png'));
  // Cannot use method chaining here as some of the methods return ancestors of JPreviewProgram_Builder
  LBuilder := TJPreviewProgram_Builder.JavaClass.init;
  LBuilder.setChannelId(FChannelId);
  LBuilder.setType(TJTvContractCompat_PreviewProgramColumns.JavaClass.TYPE_CLIP);
  LBuilder.setTitle(StringToJString('Episode 1'));
  LBuilder.setDescription(StringToJString('Test Description'));
  LBuilder.setIntentUri(LIntentUri); // Clicking the program item will send this uri to the app to play content
  LBuilder.setPosterArtUri(LPosterArtUri);
  LBuilder.setInternalProviderId(StringToJString(cEpisodeId));
  LProgramUri := TAndroidHelper.Context.getContentResolver.insert(TJTvContractCompat_PreviewPrograms.JavaClass.CONTENT_URI, LBuilder.build.toContentValues);
  FProgramId := TJContentUris.JavaClass.parseId(LProgramUri);
end;

end.
