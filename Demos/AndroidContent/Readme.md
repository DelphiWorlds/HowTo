# Android Content demo

## Description

Demonstrates how to query various content on Android.

This demo shows how to query SMS messages, as requested in [this question](https://en.delphipraxis.net/topic/9221-is-there-a-way-to-save-messages-text-on-android-devices), however the code has been structured so that developers could create other descendants of `TCustomContentRetriever` from the `DW.Content.Android` unit, and query other types of content such as:

1. Contacts: `content://contacts`
   This content URI provides access to the device's contacts and related information.

2. Call Log: `content://call_log/calls`
   This content URI allows you to retrieve the call history or call log details.

3. SMS: `content://sms`
   This content URI enables you to access the SMS messages stored on the device.

4. User Dictionary: `content://user_dictionary/words`
   This content URI allows you to retrieve the words stored in the user dictionary.

5. Calendar Events: `content://com.android.calendar/events`
   This content URI provides access to the calendar events stored on the device.

6. Browser History: `content://browser/bookmarks`
   This content URI enables you to access the browser bookmarks and history.

7. Downloads: `content://downloads/all_downloads`
   This content URI allows you to retrieve information about the downloaded files.

Remember that some of these require permissions to be added to Uses Permissions, and to be requested at runtime.

## Configuration

For this particular demo, if you are using the code in your own project, remember to add the `Read SMS` permission in the Dangerous permissions section of the Uses Permissions in the Project Options.




