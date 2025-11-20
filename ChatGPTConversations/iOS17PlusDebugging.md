# Debugging with iOS 17+

NOTE: This conversation was with Grok, but the outcome is likely the same. Please note that this contains a [follow-up question](#follow-up-question).

## Question

With the change by Apple in communications to iOS 17, is it even possible for a 3rd party (i.e. completely bypassing Xcode) to be able to have debugging without using a VM/server in the app on the device?

## Answer

Yes, it is still technically possible for a pure third-party tool (completely bypassing Xcode and any VM/debug server inside the app) to debug an iOS 17+ app on a physical device — but only for **native (Objective-C/Swift/C/C++) code**, and with extremely severe limitations that make it practically useless for day-to-day development in 2025.

Here’s exactly what Apple broke and what remains:

| Capability                                | Pre-iOS 17 (Xcode 14 and earlier) | iOS 17+ (Xcode 15+) with Apple’s changes | Status in 2025 |
|-------------------------------------------|-------------------------------------------|-----------------------------------------|----------------|
| Third-party tool installs & launches app  | Yes (ios-deploy, libimobiledevice, etc.) | No — Apple revoked the private entitlements third-party tools relied on | Permanently blocked for non-Apple-signed binaries |
| Third-party tool attaches LLDB/GDB to a running process via ptrace/mach ports | Yes (with a few workarounds)      | No — `task_set_exception_ports` + anti-ptrace hardening + mandatory code-signing checks for debugger entitlement | Effectively impossible without jailbreak or Apple-signed binary |
| Third-party tool receives debug exception messages (breakpoints, crashes) | Yes                                     | No — only processes with the `get-task-allow` + debugger entitlement signed by Apple can catch exceptions | Only Xcode / Instruments / Apple-signed debugservers |
| Remote debugging over USB without Xcode   | Possible (slow, flaky tools existed)     | Impossible for non-jailbroken devices   | Dead |

### What Apple specifically changed starting with iOS 17 / Xcode 15
1. They removed/revoked the undocumented private entitlements that tools like ios-deploy and libimobiledevice used to launch apps.
2. They added a runtime check: only a debugger that is cryptographically signed by Apple (i.e., the debugserver bundled with Xcode or Instruments) is allowed to attach to a process and receive mach exception messages.
3. `get-task-allow` alone is no longer sufficient on a release-signed or App Store build (and even on debug builds, third-party debuggers are blocked).

Result: On a stock (non-jailbroken) iOS 17+ device, the only program on your Mac that can successfully:
- launch a debug build,
- attach LLDB,
- and receive breakpoint / crash exceptions
…is Xcode itself (or command-line tools launched by Xcode, like the ones Flutter now uses under the hood with `xcodebuild` and `devicectl`).

### What still works without any VM/debug-server in the app (pure native LLDB debugging)
On a **jailbroken** device → yes, fully possible with third-party tools (checkra1n + patched debugserver, etc.).  
On a **stock** device → no credible third-party solution exists in 2025.

### Why Flutter, .NET MAUI, React Native, etc. all still work
They never relied on third-party native LLDB attachment in the first place.  
They use the VM/debug-server model (Dart VM Service, .NET diagnostic ports, JavaScript V8 inspector, etc.) that runs inside the app and communicates over a plain forwarded socket. Apple never restricted that path — only the classic ptrace/LLDB attachment path.

### Bottom line
Since iOS 17, for a stock (non-jailbroken) device:

→ Pure native third-party debugging (LLDB/GDB without Xcode and without any in-app server) is dead.  
→ Debugging via an in-app diagnostic server/VM service (the Flutter / .NET / React Native model) is the only practical way left for third-party tooling.

Apple effectively forced all cross-platform frameworks to use the “embed a debug server” model if they want seamless physical-device debugging without opening Xcode manually. That is why Flutter had to switch to Xcode-backed launching in 2023 and has never looked back.

## Follow-up question

What does the RemoteXPC service do, and what does the closed-source CoreFramework provide?

## Answer

### What is the RemoteXPC Service?

The **RemoteXPC service** refers to functionality provided by the **`remoted`** daemon (full name often `com.apple.remoted`) that runs on every modern iOS (and iPadOS) device.

Its core purpose is to **expose local XPC services over the network (or over a USB/Wi-Fi tunnel) to a connected Mac**. Normally:

- XPC (Cross-Process Communication) on Apple platforms is purely local (mach-port based, same-device only).
- `remoted` allows privileged processes on the device to register certain XPC endpoints so that a trusted host (your Mac running Xcode or devicectl) can connect to them remotely via a secure, multiplexed channel (usbmuxd for USB, or wireless in newer versions).

Starting with **iOS 17 and Xcode 15**, Apple migrated most device-management and debugging communication to a new stack built on **QUIC + RemoteXPC** (replacing older lockdownd-based protocols for many operations). This is why tools like Flutter, .NET MAUI, React Native, and even Apple's own `devicectl` now rely on it for:

- Installing apps
- Launching/debugging processes
- Forwarding arbitrary ports (e.g., Dart VM Service, .NET diagnostic ports, etc.)
- Accessing other device services (sysdiagnose, crash reports, etc.)

In short: RemoteXPC is the modern, Apple-blessed way for a Mac to talk to privileged XPC services running inside iOS without requiring the old fragile third-party hacks (like ios-deploy's ptrace tricks).

### What Does the Closed-Source CoreDevice Framework Provide?

**CoreDevice.framework** (located at `/Library/Developer/PrivateFrameworks/CoreDevice.framework` in Xcode) is a **private, closed-source Apple framework** introduced/promoted with Xcode 15. It is the host-side (macOS) counterpart to the RemoteXPC changes on the device.

It powers the new `xcrun devicectl` command-line tool and is also used internally by Xcode, xcodebuild, and third-party tools (Flutter, Visual Studio, etc.) via Xcode's automation APIs.

Key things CoreDevice provides:

| Functionality | Description |
|---------------|-----------|
| Device discovery & tunneling | Establishes secure QUIC-or-TLS-wrapped RemoteXPC connections over usbmuxd (USB or Wi-Fi) |
| App install/launch/terminate | Replaces the old lockdown + installation proxy services |
| Process debugging attachment | Starts apps under LLDB/debugserver on device (the only Apple-signed path still allowed on iOS 17+) |
| Port forwarding & service multiplexing | Critical for forwarding your framework’s debug server (Dart VM Service, .NET diagnostics, etc.) back to localhost on the Mac |
| Developer disk image management | Mounting/unmounting developer disks, enabling developer mode, etc. |
| Higher-level abstractions used by devicectl | List devices, query info, manage peripherals, etc. |

Because CoreDevice is private and closed-source, third-party tooling cannot link to it directly — instead, everyone shells out to `xcodebuild`, `devicectl`, or Xcode’s IDE automation servers, which in turn use CoreDevice under the hood.

This is exactly why, post-iOS 17, pure third-party native debugging without any in-app server became impossible: Apple now routes everything that needs deep device access through this closed CoreDevice + RemoteXPC stack, and only Apple-signed binaries (Xcode tools) are allowed to use the lowest-level entitlements.

In summary:
- **RemoteXPC (on-device)** = the secure bridge that lets your Mac talk to internal iOS XPC services.
- **CoreDevice (on-Mac, closed-source)** = the private framework that implements the modern, post-iOS 17 way of controlling and debugging devices from the host.

Together they are why cross-platform frameworks survived the iOS 17 lockdown changes while keeping seamless physical-device debugging — they just proxy through Apple’s own infrastructure instead of trying to bypass it.