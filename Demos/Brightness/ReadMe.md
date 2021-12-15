# Device Brightness Demo

## Description

Demonstrates the use of the `GetBrightness` and `SetBrightness` methods of [`TUIHelper`](https://github.com/DelphiWorlds/Kastri/blob/2a1e0dca00dd8fd0a0d3e65e44e8336913ceb9b8/Core/DW.UIHelper.pas#L32) in [Kastri](https://github.com/DelphiWorlds/Kastri).

Applies to Android and iOS only

## Running the demo

Run the demo, tap the combo and select a brightness value. Switching between apps should restore the brightness until you return to the demo

## Technical details

On iOS, changing the brightness is system-wide, so it is recommended to restore the brightness to the original setting when switching between apps. This is achieved by:

* Storing the current brightness value when the app starts
* Storing the value that is set when a selection is made in the combo
* Restoring the original brightness value when the app becomes inactive
* Restoring the selected brightness value when the app becomes active

Note that it is possible to change the brightness system-wide on Android - this may be the subject of another "How To"
