# WebBrowser Click Demo

## Purpose

Demonstrates using the `ExecuteJavaScript` method of `TWebBrowserExt` to click an element using coordinates related to the `TWebBrowser`

## Description

The project contains a `TNativeImage` (see Project Configuration, below) control which is overlays a `TWebBrowser`. The image can be moved manually using the Left, Up, Down and Right buttons until it is over a clickable element, then the "Click WebBrowser" button can be tapped, which executes JavaScript that finds the element at the coordinates and invokes a "click" event on the element.

The demo was created in response to [this question on Delphi Praxis](https://en.delphipraxis.net/topic/12144-mouse-cursor).

## Project Configuration

When using this code in your own app you will need to:

* If you use TNativeImage as per the demo, install the [KastriFMX package](https://github.com/DelphiWorlds/Kastri/tree/master/Packages), which contains the "native" controls in [Kastri](https://github.com/DelphiWorlds/Kastri)
* Add the paths from the Search path in Project Options, that point to the relevant folders in Kastri

### Android

You will need to add [`dw-kastr-base-3.0.0.jar`](https://github.com/DelphiWorlds/Kastri/blob/master/Lib/dw-kastri-base-3.0.0.jar) to the Libraries node of Android 32-bit target in Project Manager. The demo uses a **copy** of this file in the `Lib` folder of the demo.