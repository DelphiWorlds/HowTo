# iOS Join Wifi demo

## Description

Demonstrates how to join a wifi network on iOS

## Configuration

You will need to create an App ID (which can be done on the [Apple Developer website](https://developer.apple.com/account)), that includes the Hotspot entitlement (called "Hotspot", about half-way down the list of entitlements), and create a Provisioning Profile that uses the App ID.

In Delphi, in the Project Options, in the Vewrsion Info section, ensure that the `CFBundleIdentifier` value matches the identifier from the App ID, mentioned above.
