# Biometric Auth Demo

## Description

Demonstrates the use of the `TBiometricAuth` component that ships with Delphi

Applies to Android and iOS only

## Project Configuration

If you are building your own project (these steps have been done in the demo):

### Android

#### Entitlement List (Project Options)

Ensure that the Biometric Authorization Service option is checked

#### TBiometricAuth component

Ensure that at least the `DeviceCredential` option of the `Biometric Strengths` property is selected

### iOS

No special requirements

## Running the demo

Run the demo, and tap the Authenticate button. If biometric authentication is possible on the device, a prompt should appear requesting authorization either via fingerprint or face id, depending on what is configured on your device.

