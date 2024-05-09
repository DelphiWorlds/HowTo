# Android TV Demo

## Description

Demonstrates a couple of the considerations to take into account when creating a Delphi app for Android TV

## Project Configuration

To ensure that the app appears in the apps list on the TV, modify `AndroidManifest.template.xml` to add a category to the intent filters:

```
    <intent-filter>
        <action android:name="android.intent.action.MAIN" />
        <category android:name="android.intent.category.LAUNCHER" />
        <category android:name="android.intent.category.LEANBACK_LAUNCHER" />. <!---- Add this line, so it appears in the TV's app list -->
    </intent-filter>
```

## Handling "keystrokes" from the remote

In the demo, you may notice that the `KeyUp` method of the form has been overridden, and the key value is examined to determine which button has been pressed. As far as I can tell, there are only a few that can be detected by apps, namely:

* Back (as in navigate back)
* Left
* Up
* Right
* Down
* OK

I have included `Home`, however I am yet to successfully handle this "key".

**NOTE:** Setting `FPreventExit` in the demo stops the back button from quitting the app, however this may not be considered to be user friendly :-) 

## Future Enhancements

Enhancements might be made to this demo if I have time/interest, however feel free to [add an issue](https://github.com/DelphiWorlds/HowTo/issues) for any suggestions.
