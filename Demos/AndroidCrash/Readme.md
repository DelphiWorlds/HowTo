# Android Crash demo

## Description

Demonstrates the use of the [HookUncaughtExceptionHandler method of TAndroidHelperEx](https://github.com/DelphiWorlds/Kastri/blob/b5c87d2ea70caefd2383e9327fe3bc2f8e372467/Core/DW.Android.Helpers.pas#L143) in [Kastri](https://github.com/DelphiWorlds/Kastri).

This method allows you to have the app restart, and capture information about what caused the crash when the app restarts.

## Configuration

**If using the crashTest method**, you will need to add the relevant base java library from Kastri to the Libraries node of the Android target platform (as per the demo). For Delphi 10.4.2, add [dw-kastri-base.jar](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base.jar), for Delphi 11 or later, add [dw-kastri-base-2.0.0.jar](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base-2.0.0.jar)


