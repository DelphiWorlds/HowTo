# Android TV demo

## Description

Demonstrates how to add a "channel" and a single program item to the home screen

**NOTE: As at August 17th 2024, this demo is very much a work in progress**

Note also that it is dependent on the [Kastri](https://github.com/DelphiWorlds/Kastri/) library

## Configuration

When building your own project based on this code:

### TV provider jar

Add `tvprovider-1.0.0.jar` to the libraries of the Android 32-bit target

### Additions to `AndroidManifest.template.xml`

```xml
    <uses-permission android:name="com.android.providers.tv.permission.WRITE_EPG_DATA" />
```

```xml
   <category android:name="android.intent.category.LEANBACK_LAUNCHER" />
```

This entry ensures that the app has an icon amongst the other icons on the device

```xml
    <!-- For when the channel is tapped -->
    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
        <data
            android:scheme="atvdemo"
            android:host="open"
            android:pathPrefix="/channel" />
    </intent-filter>
    <!-- For when a program in a channel is tapped -->
    <intent-filter>
        <action android:name="android.intent.action.VIEW" />
        <category android:name="android.intent.category.DEFAULT" />
        <category android:name="android.intent.category.BROWSABLE" />
        <data
            android:scheme="atvdemo"
            android:host="open"
            android:pathPrefix="/video" />
    </intent-filter>
```

The `scheme` parameter is a value that is usually unique

The `host` and `pathPrefix` parameters may be modified depending on your needs


