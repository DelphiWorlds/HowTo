unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.PhoneDialer, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo;

type
  TForm1 = class(TForm)
    PhoneNumberEdit: TEdit;
    CallButton: TButton;
    Memo1: TMemo;
    procedure FormActivate(Sender: TObject);
    procedure CallButtonClick(Sender: TObject);
  private
    FPhoneService: IFMXPhoneDialerService;
    procedure CallStateChangedHandler(const ACallID: string; const AState: TCallState);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.TypInfo,
  System.Permissions,
  FMX.Platform,
  DW.Consts.Android, DW.Permissions.Helpers;

const
  cPermissionCallPhone = 'android.permission.CALL_PHONE';

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  if TPlatformServices.Current.SupportsPlatformService(IFMXPhoneDialerService, FPhoneService) then
    FPhoneService.OnCallStateChanged := CallStateChangedHandler
  else
    Memo1.Lines.Add('No phone dialer service');
end;

procedure TForm1.CallButtonClick(Sender: TObject);
begin
  FPhoneService.Call(PhoneNumberEdit.Text);
end;

procedure TForm1.CallStateChangedHandler(const ACallID: string; const AState: TCallState);
begin
  Memo1.Lines.Add(Format('Call state changed: %s', [GetEnumName(TypeInfo(TCallState), Ord(AState))]));
  if not ACallId.IsEmpty then
    Memo1.Lines.Add('Number: ' + ACallId);
end;

procedure TForm1.FormActivate(Sender: TObject);
begin
  PermissionsService.RequestPermissions([cPermissionReadPhoneState, cPermissionCallPhone],
    procedure(const APermissions: TPermissionArray; const AGrantResults: TPermissionStatusArray)
    begin
      if not AGrantResults.AreAllGranted then
        Memo1.Lines.Add('Cannot read phone state or make calls until permissions are granted');
    end
  );
end;

end.
