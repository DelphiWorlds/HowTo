# Generic Job demo

## Description

Demonstrates a method of running code in a service at specified times. This is a **very** basic demo. If you have queries about how you might be able to extend it, please follow the support options in the [main readme](https://github.com/DelphiWorlds/HowTo/blob/main/Readme.md).

As per the `SetAlarmButtonClick` method in the main form of the application, an alarm is set for 15 seconds time, to invoke the service.

The service was created using the `Intent Local Service` option in the IDE:

<img src="../../Screenshots/IntentLocalServiceOption.png" alt="Intent Local Service" height="400">

When the alarm fires, the broadcast receiver specified in `AndroidManifest.template.xml` which is part of the Kastri base jar (see the Configuration section, below), handles the alarm and enqueues work for the service.

The `OnHandleIntent` event in the service is called when work is enqueued. In the demo, all it does is sets another alarm, so the service will have work queued every 15 seconds. You could modify this to have the work be performed at different times depending on certain conditions.

## Configuration

When creating your own project, you will need to:

* Ensure you have paths to the [Kastri](https://github.com/DelphiWorlds/Kastri) library, specifically (at a minimum) the `API`, `Core` and `Include` folders.
* Add the relevant Kastri base jar to the Libraries node under the Android target in Project Manager. For Delphi 10.4.2 or earlier, this will be `dw-kastri-base.jar` and for Delphi 11 or later: `dw-kastri-base-2.0.0.jar`. These files are located in the [`Lib` folder in Kastri](https://github.com/DelphiWorlds/Kastri/tree/master/Lib)
* Modify AndroidManifest.template.xml to add the following, after `<%receivers%>`:

```
  <receiver android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver">
      <intent-filter>
          <action android:name="com.delphiworlds.kastri.DWMultiBroadcastReceiver.ACTION_SERVICE_ALARM" />
      </intent-filter>
  </receiver>
```

