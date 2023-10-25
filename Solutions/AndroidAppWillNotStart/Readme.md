# Android App Will Not Start

## Description

This issue affects a number of developers, and it may happen when you upgrade to a new version of Delphi or components used in your app, or perhaps when the device has a newer version of Android.

The following sections cater for a few scenarios for possible reasons why your app is not starting.

## Splash screen does not even show

This is when the app starts, but there is just a black screen and the "splash" does not even show.

### Configuration issue

Your app may be configured to use a specific feature (for example: Firebase Cloud Messaging), however configuration may have changed between Delphi versions, or there is something you may have missed in the Project Options. 

You may be using 3rd party code (or your own) that requires jar files that are not the "default" for a Delphi project for Android, and you have not added them to the project. Consult the documentation to discover whether any such files are required.

It's sometimes difficult to diagnose this kind of issue, however there are a couple of measures you can take:

1. Check all the required configuration, e.g. the Entitlements List and Services sections in the Project Options
2. Start a new blank application and configure for whatever features your app is using. If the app starts and shows the splash screen, check the differences between the blank app and your original app
3. Use a logcat viewer (such as Device Lens) and look for messages that indicate something has not been configured correctly. If using Device Lens, begin by filtering on your application's package name, start the output, _then_ launch the application. If there's nothing obvious, remove the application package name filter, and enter your app's name in the Text filter, launch the app and examine the output again. If you have trouble interpreting the results, you may like to ask in the `#general` channel of the Delphi Worlds Slack workspace, or ask a question in forums like Delphi Praxis, ensuring to include any warnings/errors from the logcat output that appear to be relevant.

## Splash screen shows, but the application "hangs"

This is probably the most common scenario. If a fatal error occurs when the forms/datamodules are created at startup, the application may "hang" and as a result the main form never appears.

If you have project source (inside the `begin`..`end`) like this:

```delphi
  Application.Initialize;
  Application.CreateForm(TDM, DM);
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run; 
```

Since the datamodule is being created first, consider commenting out the line where it is created, or at least examine any `DataModuleCreate` event or overridden `Create` methods in the datamodule for what might be causing the error, and perhaps comment those out instead. The cause may actually be from when one of the components on the datamodule is created, so also keep that in mind.

Commenting out the datamodule creation will at least guarantee that the form is created. If the form shows, then the cause should be in the datamodule. If not, then the cause will be during creation of the form. If so, follow the same rules above about the datamodule.

If you have a `FormCreate` event or overridden `Create` method, consider commenting out the code inside it. This should at least allow the form to show, and you will be able to determine whether or not the cause is in that method. If the cause **is** in that method, and you are still unable to determine the cause in it, consider refactoring that code into its own method, and instead use an application event handler to execute that new method when the `BecameActive` event occurs, e.g.:

```delphi
uses
  FMX.Platform;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  LService: IFMXApplicationEventService;
begin
  // Replace your startup code with this:
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, LService) then
    LService.SetApplicationEventHandler(HandleAppEvent);
end;

function TfrmMain.HandleAppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.BecameActive:
    begin
      // Declare FStarted as a private Boolean variable of your form
      if not FStarted then
        PerformStartup;
    end;
  Result := True;
end;

procedure TfrmMain.PerformStartup;
begin
  FStarted := True;
  // Put your startup code in here
end;
```

You may wish to use this technique regardless of whether you have any problems with the app starting, as it will help diagnose any issues that you may have later.

Hopefully, this will at least have your main form showing, and if an error occurs it should now be visible, rather than the app just hanging. It should also make it easier to diagnose exactly where the cause is if you're running the app via the debugger.














