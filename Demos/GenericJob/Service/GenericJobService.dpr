program GenericJobService;

uses
  System.Android.ServiceApplication,
  GJ.ServiceModule in 'GJ.ServiceModule.pas' {ServiceModule: TAndroidIntentService};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServiceModule, ServiceModule);
  Application.Run;
end.
