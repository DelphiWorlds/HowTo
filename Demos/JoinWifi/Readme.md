# iOS Join Wifi demo

## Description

Demonstrates how to join a wifi network on iOS/Android

## Configuration

### iOS

You will need to create an App ID (which can be done on the [Apple Developer website](https://developer.apple.com/account)), that includes the Hotspot entitlement (called "Hotspot", about half-way down the list of entitlements), and create a Provisioning Profile that uses the App ID.

In Delphi, in the Project Options, in the Vewrsion Info section, ensure that the `CFBundleIdentifier` value matches the identifier from the App ID, mentioned above.

### Android

If constructing **your own project** (has already been configured in the demo):

Requires the library `dw-kastri-base-3.0.0.jar`, which can be found in the `Lib` folder of this demo. Add the library to the `Android 32-bit` target under the `Libraries` node in Project Manager

There are two permissions required:

* Access Network State
* Change Network State

