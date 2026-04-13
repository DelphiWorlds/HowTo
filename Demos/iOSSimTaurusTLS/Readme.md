# TaurusTLS on iOS Simulator demo

## Description

Demonstrates how to use TaurusTLS on iOS Simulator

## Configuration

You will need to:

- Clone or download [TaurusTLS](https://github.com/TaurusTLS-Developers/TaurusTLS)
- Either create a User Override called `TaurusTLS` (IDE options: IDE > User System Overrides) that points to the TaurusTLS source 
  **OR** update the search path in the project options to include a path to the TaurusTLS source
- Modify `TaurusTLSCompilerDefines.inc` so that it defines `STATICLOAD_OPENSSL` for iOS Simulator by commenting out an `IFDEF` thus:
  ```delphi
  {$IFDEF IOS}
    // Support for 64-bit ARM iOS Simulator was added in Delphi 11.2
    // TODO: how to detect iOS Simulator in FPC? Does it support 64-bit ARM?
    {$IFDEF CPUARM}
      //{$IFNDEF IOSSIMULATOR} // <---- Comment or remove this line
        // RLebeau: For iOS devices, OpenSSL cannot be used as an external library,
        // it must be statically linked into the app.  For the iOS simulator, this
        // is not true.  Users who want to use OpenSSL in iOS device apps will need
        // to add the static OpenSSL library to the project and then include the
        // IdSSLOpenSSLHeaders_static unit in their uses clause. It hooks up the
        // statically linked functions for the IdSSLOpenSSLHeaders unit to use...
        {$DEFINE STATICLOAD_OPENSSL}
      // {$ENDIF} // <---- Comment or remove this line
    {$ENDIF}
  {$ENDIF}
  ```
  Previously, Apple did not allow OpenSSL (and perhaps other) libraries to be linked statically. This is obviously no longer the case.

## iOS Simulator OpenSSL binaries

These have been included in this demo, in the `Lib\iOSSimulator` folder, and were built for OpenSSL 3.6.2. At present, these are not normally included in the [TaurusTLS distribution](https://github.com/TaurusTLS-Developers/OpenSSL-Distribution/releases), however they may be in the future.

If you are building your own project, **you will need these static libraries** (`libcrypto.a` and `libssl.a`) in your project search path

