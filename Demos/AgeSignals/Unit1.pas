unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  Androidapi.JNI.JavaTypes,
  DW.PlayTasksEvents.Android;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    FCheckAgeSignalsEvents: IPlayTasksEvents;
    procedure FetchAgeSignals;
    procedure FetchFakeAgeSignals;
    procedure CheckAgeSignalsFailureHandler(const AException: JException);
    procedure CheckAgeSignalsSuccessHandler(const AResult: JObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Androidapi.Helpers, Androidapi.JNI.PlayServices.Tasks,
  DW.Androidapi.JNI.PlayServices.AgeSignals;

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  // FetchAgeSignals;  // Should be available from 01-JAN-2026
  FetchFakeAgeSignals;
end;

procedure TForm1.CheckAgeSignalsFailureHandler(const AException: JException);
begin
  Memo1.Lines.Add(Format('CheckAgeSignalsFailureHandler: %s', [JStringToString(AException.getLocalizedMessage)]));
end;

procedure TForm1.FetchAgeSignals;
var
  LAgeSignalsManager: JAgeSignalsManager;
  LTask: JTask;
begin
  if FCheckAgeSignalsEvents = nil then
    FCheckAgeSignalsEvents := TPlayTasksEvents.Create;
  LAgeSignalsManager := TJAgeSignalsManagerFactory.JavaClass.create(TAndroidHelper.Context);
  LTask := LAgeSignalsManager.checkAgeSignals(TJAgeSignalsRequest.JavaClass.builder.build);
  FCheckAgeSignalsEvents.SetTask(LTask)
    .OnSuccess(CheckAgeSignalsSuccessHandler)
    .OnFailure(CheckAgeSignalsFailureHandler);
end;

procedure TForm1.FetchFakeAgeSignals;
var
  LFakeAgeSignalsManager: JFakeAgeSignalsManager;
  LTask: JTask;
  LResult: JAgeSignalsResult;
begin
  if FCheckAgeSignalsEvents = nil then
    FCheckAgeSignalsEvents := TPlayTasksEvents.Create;
  LFakeAgeSignalsManager := TJFakeAgeSignalsManager.JavaClass.init;
  LResult := TJAgeSignalsResult.JavaClass.builder
    .setAgeLower(TJInteger.JavaClass.init(14))
    .setAgeUpper(TJInteger.JavaClass.init(20))
    .setInstallId(StringToJString('12345678'))
    .setUserStatus(TJInteger.JavaClass.init(TJAgeSignalsVerificationStatus.JavaClass.SUPERVISED_APPROVAL_DENIED))
    .setMostRecentApprovalDate(TJDate.JavaClass.init)
    .build;
  LFakeAgeSignalsManager.setNextAgeSignalsResult(LResult);
  LTask := LFakeAgeSignalsManager.checkAgeSignals(TJAgeSignalsRequest.JavaClass.builder.build);
  FCheckAgeSignalsEvents.SetTask(LTask)
    .OnSuccess(CheckAgeSignalsSuccessHandler)
    .OnFailure(CheckAgeSignalsFailureHandler);
end;

procedure TForm1.CheckAgeSignalsSuccessHandler(const AResult: JObject);
const
  cAgeStatusCaptions: array[0..4] of string = (
    'Verified', 'Supervised', 'Supervised Approval Pending', 'Supervised Approval Denied', 'Unknown'
  );
var
  LResult: JAgeSignalsResult;
  LDate: JDate;
begin
  LResult := TJAgeSignalsResult.Wrap(AResult);
  Memo1.Lines.Add(Format('Age - Lower: %d, Upper: %d', [LResult.ageLower.intValue, LResult.ageUpper.intValue]));
  Memo1.Lines.Add(Format('Status: %s', [cAgeStatusCaptions[LResult.userStatus.intValue]]));
  LDate := LResult.mostRecentApprovalDate;
  if LDate <> nil then
  begin
    Memo1.Lines.Add(Format('Most Recent Approval Date: %2d-%s-%4d',
      [LDate.getDay, FormatSettings.ShortMonthNames[LDate.getMonth + 1], LDate.getYear + 1900]));
  end;
end;

end.
