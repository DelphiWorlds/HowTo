# Fix broken SDK imports

If, when compiling an iOS or macOS app, you may receive an error message similar to this:

```
[dccosx64 Error] E2597 ld: file not found: /System/Library/PrivateFrameworks/AVFCapture.framework/Versions/A/AVFCapture for architecture x86_64"
```

Depending on the message, this can be caused by one of two scenarios:

1. An SDK framework needs to be added to the SDK import
2. The SDK import did not complete correctly

If the path contains: `/System/Library/PrivateFrameworks`, it is more likely to be scenario 2, otherwise scenario 1 probably applies, in which case see [this link](https://github.com/DelphiWorlds/HowTo/tree/main/Solutions/AddSDKFrameworks#readme).

For **scenario 2**, please follow these steps:

1. On the Mac, in the `~/PAServer/scratch-dir` folder delete any folders starting with: `cache-dir`
2. On your PC, delete the folder corresponding to the platform SDK under this folder: `C:\Users\(username)\Documents\Embarcadero\Studio\SDKs` (where (username) is the logged in user name), e.g. MacOSX14.0.sdk
3. In Delphi's SDK Manager, remove the affected iOS or macOS SDK
4. Re-import the SDK

This should fix the "broken" SDK
