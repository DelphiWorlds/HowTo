# Camera Files 

## Description

Demonstrates a way of retrieving camera files on later versions of Android. I might add iOS, later

## Configuration

When using this concept in your own project, you will need to:

### Android Permissions

Enable the `Read Media Files` permission

## Explanation

Due to changes in later versions of Android of how files outside of an application be accessed, new permissions were introduced (`READ_MEDIA_FILES` and `WRITE_MEDIA_FILES`), and one of the new ways of accessing the files (instead of direct read/write) is via a content `Uri` using a `ContentResolver`.

The contents of `DCIM/Camera` is first retrieved using the `query` method of `ContentResolver`, passing which fields are desired (`ID`, `DISPLAY_NAME` and `RELATIVE_PATH`), and a condition that it should match: "`DCIM/Camera%`" (like SQL). The results in the cursor are then iterated over, fetching the desired values, and the Uri for each item is calculated using the `ID` value.

Delphi's traditional methods of accessing files cannot be used to access the files directly, so use is made of a class called `TAndroidFileStream` that is used to copy the contents of the file, then call the `LoadFromStream` method of the `Bitmap` property of the `TImage` to load the image data.