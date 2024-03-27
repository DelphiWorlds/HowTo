# Join Wifi demo

## Description

Demonstrates how to join a wifi network on iOS/Android.

**For Delphi 11.x users, please use the JoinWifiDemoD11 project**

## Configuration

### iOS

You will need to create an App ID (which can be done on the [Apple Developer website](https://developer.apple.com/account)), that includes the Hotspot entitlement (called "Hotspot", about half-way down the list of entitlements), and create a Provisioning Profile that uses the App ID.

In Delphi, in the Project Options, in the Vewrsion Info section, ensure that the `CFBundleIdentifier` value matches the identifier from the App ID, mentioned above.

### Android

If constructing **your own project** (has already been configured in the demo projects):

Requires the library:

* Delphi 11.x: `dw-kastri-base-2.0.0.jar`
* Delphi 12: `dw-kastri-base-3.0.0.jar`

which can be found in the `Lib` folder of this demo. Add the library to the `Android 32-bit` target under the `Libraries` node in Project Manager

**Note**:

Due to a bug in Delphi 11.3 **ONLY**, if you need to compile for Android 64-bit, you will need to either apply [this workaround](https://docs.code-kungfu.com/books/hotfix-113-alexandria/page/fix-jar-libraries-added-to-android-64-bit-platform-target-are-not-compiled) (which will apply to **all** projects), **OR** copy the jar file(s) to _another folder_, and add them to the Libraries node of the Android 64-bit target. (Adding the same `.jar` file(s) to Android 64-bit does _not_ work)

There are two permissions required:

* Access Network State
* Change Network State

