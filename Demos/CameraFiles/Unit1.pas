unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  Androidapi.JNI.Net,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.Objects;

type
  TMediaFileItem = record
    ID: Int64;
    FileName: string;
    Uri: Jnet_Uri;
  end;

  TMediaFileItems = TArray<TMediaFileItem>;

  TForm1 = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Image1: TImage;
    Layout1: TLayout;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
  private
    FMediaFileItems: TMediaFileItems;
    procedure QueryCameraImages;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.Permissions,
  Androidapi.JNI.GraphicsContentViewText, Androidapi.JNI.Provider, Androidapi.JNIBridge, Androidapi.JNI.JavaTypes,
  Androidapi.Helpers;

type
  TAndroidFileStream = class(TBytesStream)
  public
    constructor Create(const AFile: JFile); overload;
    constructor Create(const AURI: string); overload;
    constructor Create(const AJURI: Jnet_Uri); overload;
  end;

{ TAndroidFileStream }

constructor TAndroidFileStream.Create(const AFile: JFile);
var
  LURI: Jnet_Uri;
begin
  LURI := TAndroidHelper.JFileToJURI(AFile);
  Create(LURI);
end;

constructor TAndroidFileStream.Create(const AURI: string);
var
  LURI: Jnet_Uri;
begin
  LURI := TJnet_Uri.JavaClass.parse(StringToJString(AURI));
  Create(LURI);
end;

constructor TAndroidFileStream.Create(const AJURI: Jnet_Uri);
var
  LInput: JInputStream;
  LJavaBytes: TJavaArray<Byte>;
  LBytes: TBytes;
  LBytesRead, LOffset: Integer;
begin
  LBytes := [];
  LInput := TAndroidHelper.Context.getContentResolver.openInputStream(AJURI);
  if LInput <> nil then
  try
    LJavaBytes := TJavaArray<Byte>.Create(4096);
    try
      repeat
        LBytesRead := LInput.read(LJavaBytes, 0, LJavaBytes.Length);
        if LBytesRead > 0 then
        begin
          LOffset := Length(LBytes);
          SetLength(LBytes, Length(LBytes) + LBytesRead);
          Move(LJavaBytes.Data^, LBytes[LOffset], LBytesRead);
        end;
      until LBytesRead <= 0;
    finally
      LJavaBytes.Free;
    end;
  finally
    LInput.close;
  end;
  inherited Create(LBytes);
end;

{ TForm1 }

procedure TForm1.ListBox1ItemClick(const Sender: TCustomListBox; const Item: TListBoxItem);
var
  LStream: TStream;
  LMediaFileItem: TMediaFileItem;
begin
  LMediaFileItem := FMediaFileItems[Item.Index];
  LStream := TAndroidFileStream.Create(LMediaFileItem.Uri);
  try
    Image1.Bitmap.LoadFromStream(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TForm1.QueryCameraImages;
var
  LResolver: JContentResolver;
  LProjection: TJavaObjectArray<JString>;
  LSelection: JString;
  LSelectionArgs: TJavaObjectArray<JString>;
  LCursor: JCursor;
  LIdColumn, LNameColumn, LPathColumn, I: Integer;
  LTargetUri, LContentUri: Jnet_Uri;
  LItem: TMediaFileItem;
begin
  FMediaFileItems := [];
  LResolver := TAndroidHelper.Context.getContentResolver;
  LTargetUri := TJImages_Media.JavaClass.EXTERNAL_CONTENT_URI;
  LProjection := TJavaObjectArray<JString>.Create(3);
  try
    LProjection.Items[0] := TJBaseColumns.JavaClass._ID;
    LProjection.Items[1] := TJMediaStore_MediaColumns.JavaClass.DISPLAY_NAME;
    LProjection.Items[2] := TJMediaStore_MediaColumns.JavaClass.RELATIVE_PATH;
    LSelection := TJMediaStore_MediaColumns.JavaClass.RELATIVE_PATH.concat(StringToJString(' like ?'));
    LSelectionArgs := TJavaObjectArray<JString>.Create(1);
    try
      LSelectionArgs.Items[0] := StringToJString('DCIM/Camera%');
      LCursor := LResolver.query(LTargetUri, LProjection, LSelection, LSelectionArgs, nil);
    finally
      LSelectionArgs.Free;
    end;
  finally
    LProjection.Free;
  end;
  if LCursor <> nil then
  try
    while LCursor.moveToNext do
    begin
      LIdColumn := LCursor.getColumnIndexOrThrow(TJBaseColumns.JavaClass._ID);
      LNameColumn := LCursor.getColumnIndexOrThrow(TJMediaStore_MediaColumns.JavaClass.DISPLAY_NAME);
      LItem.ID := LCursor.getLong(LIdColumn);
      LItem.FileName := JStringToString(LCursor.getString(LNameColumn));
      LItem.Uri := TJContentUris.JavaClass.withAppendedId(LTargetUri, LItem.ID);
      FMediaFileItems := FMediaFileItems + [LItem];
    end;
  finally
    LCursor.close;
  end;
  ListBox1.Items.Clear;
  for LItem in FMediaFileItems do
    ListBox1.Items.Add(LItem.FileName);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PermissionsService.RequestPermissions(['android.permission.READ_MEDIA_IMAGES'],
    procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
    begin
      if AGrantResults[0] = TPermissionStatus.Granted then
       QueryCameraImages;
    end
  );
end;

end.
