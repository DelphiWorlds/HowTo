unit DW.Content.Android;

interface

uses
  Androidapi.JNI.GraphicsContentViewText;

type
  TCustomContentRetriever = class(TObject)
  protected
    function GetColIndex(const ACursor: JCursor; const AColName: string): Integer;
    procedure FetchRecord(const ACursor: JCursor); virtual;
    procedure FetchContent(const APath: string; const AColumns: TArray<string> = []);
  end;

implementation

uses
  Androidapi.JNI.Net, Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge;

{ TCustomContentRetriever }

function TCustomContentRetriever.GetColIndex(const ACursor: JCursor; const AColName: string): Integer;
begin
  Result := ACursor.getColumnIndex(StringToJString(AColName));
end;

procedure TCustomContentRetriever.FetchContent(const APath: string; const AColumns: TArray<string> = []);
var
  LURI: Jnet_Uri;
  LProjection: TJavaObjectArray<JString>;
  LCursor: JCursor;
  I: Integer;
begin
  LURI := TJnet_Uri.JavaClass.parse(StringToJString('content://' + APath)); // e.g. sms, or sms/inbox, etc
  if Length(AColumns) > 0 then
    LProjection := TJavaObjectArray<JString>.Create(Length(AColumns))
  else
    LProjection := TJavaObjectArray<JString>.Create(1);
  try
    if Length(AColumns) > 0  then
    begin
      for I := 0 to Length(AColumns) - 1 do
        LProjection.Items[I] := StringToJString(AColumns[I]);
    end
    else
      LProjection.Items[0] := StringToJString('*');
    LCursor := TAndroidHelper.ContentResolver.query(LURI, LProjection, nil, nil, nil); // , LOrderBy
  finally
    LProjection.Free;
  end;
  if (LCursor <> nil) and LCursor.moveToFirst then
  begin
    repeat
      FetchRecord(LCursor);
    until not LCursor.moveToNext;
  end;

end;

procedure TCustomContentRetriever.FetchRecord(const ACursor: JCursor);
begin
  //
end;

end.
