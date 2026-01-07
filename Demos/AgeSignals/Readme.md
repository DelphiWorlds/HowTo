# Age Signals demo

## Description

Demonstrates how to use the Age Signals API on Android

NOTE: This demo relies on files from the [Kastri](https://github.com/DelphiWorlds/Kastri) repo.

## Status

**The "real" Age Signals service should have been live on Jan 1st, 2026, however [this has now been delayed](https://support.google.com/googleplay/android-developer/answer/16569691).** 

Until the service goes live, it's necessary to use the "fake" API calls, as per the demo

## Configuration

### Search path

As per the description, this demo relies on files from Kastri. **You will need to add (or modify in the case of the demo) the project search path to include the `API` and `Core`folders of your local copy of Kastri**

### Android

If constructing **your own project** (has already been configured in the demo project), you will need to add the Age Signals library to the Libraries node under the Android 32-bit platform in Project Manager. 

As at 19-DEC-2025, this is: `age-signals-0.0.2.jar` in the ThirdParty/Android folder

To add it to the project, in Project Manager:

1. Expand the Target Platforms
2. Expand Android 32-bit
3. Right-click the Libraries node and click Add..
4. Select the relevant .jar file and click Open



