# Updating files deployed to the device

## Description

Normally, when you deploy files to an Android or iOS device using the Deployment Manager feature in Delphi, the files are not updated when you deploy a new version of the app.

Sometimes you may want certain files to be updated, regardless of whether or not they already exist.

## Solution

The solution is to use the `CopyDocuments` method of the `TStartUpCopy` type in the `DW.StartUpCopy` unit of [Kastri](https://github.com/DelphiWorlds/Kastri), which will copy the files to the device and update them when you deploy a new version of the app.

The `CopyDocuments` method can be used as follows:

```delphi
TStartUpCopy.CopyDocuments(['Test.txt']);
```

Where `Test.txt` is the name of the file to copy.

This variation of `CopyDocuments` will copy the files in the array to the root of the path represented by `TPath.GetDocumentsPath`.

The method can also be used to copy files from a subfolder of the normal deployment path, as follows:

```delphi
TStartUpCopy.CopyDocuments(['Test.txt'], 'Test');
```

This assumes that the files have been deployed to a subfolder of the normal deployment path, i.e. for Android to `.\assets\internal\Test` and for iOS to `.\StartUp\Documents\Test`.

In this case, the file `Test.txt` will be copied to a subfolder called `Test`, under `TPath.GetDocumentsPath`, which is where the original file was deployed.

Of course, `CopyDocuments` should be called **before access to the updated files is required**, e.g. an appropriate place might be in the `OnCreate` event of the main form.

## Warning

**Do not use this method if the file in the location that it is deployed to is changed by the app, as those changes will be lost**. That is unless the changes can actually be safely discarded.

## Uses

This method can be useful if, for example, the file contains instructions (or release notes, etc.) that are updated from time to time, and you want to ensure that the user always has the latest version of the file.