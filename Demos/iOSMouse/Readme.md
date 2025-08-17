# iOS Mouse Demo

## Description

Demonstrates how to use the [`DW.Mouse.iOS`](https://github.com/DelphiWorlds/Kastri/blob/master/Core/DW.Mouse.iOS.pas) unit from [Kastri](https://github.com/DelphiWorlds/Kastri).

NOTE: At the time of writing, this is applicable to iPad ONLY. Perhaps one day in the future Apple may add mouse support for iPhone.

## TPlatformMouse

The `DW.Mouse.iOS` unit contains the class `TPlatformMouse` that implements mouse support for iOS. It has three events:

* `OnMouseMove`
* `OnMouseRightClick`
* `OnMouseWheel`

Note that for `OnMouseMove` and `OnMouseRightClick`, the coordinates are relative to the *entire screen*, **not the form**, so the status bar may need to be accounted for if you intend to use the coordinates for anything on the form. This is because the code was designed for mimicking remote support, where the entire screen is shown on the viewer end.

Note also that for `OnMouseWheel`, the `WheelDelta` value is calculated to align with support similar to that on macOS, and may not be entirely accurate.

## Feedback

Please provide feedback about this demo as per [the main ReadMe.](https://github.com/DelphiWorlds/Playground/blob/main/Readme.md)

