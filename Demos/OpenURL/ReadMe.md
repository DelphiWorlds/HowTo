# OpenURL Demo

## Description

Demonstrates the use of the variation of the `openURL` method of `UIApplication` which is now **required on iOS 18.0+**

## Technical details

This variation of `openURL` was introduced in iOS 10.0, so it really should have been added to the iOS imports by now.

Fortunately, it's possible to work around the issue by declaring a descendant of UIApplication, adding the missing method, then using the `Wrap` method on the "object id" (underlying object reference) of the `UIApplication` in order to access the new method.

[This `openURL`](https://developer.apple.com/documentation/uikit/uiapplication/1648685-openurl?language=objc) requires two additional parameters. The `options` parameter is optional, so in this example `nil` is being passed. The `completionHandler` parameter is what is known as a "block" (of code), where the value in this case can be a method of an object, so the `OpenURLCompletion` method of `TForm1` can be passed.
