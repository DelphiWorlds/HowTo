# Renewing Apple Certificates

These instructions are for developers (including, but not limited to, Delphi developers) who prefer to avoid Xcode's certificate/profile regeneration quirks, and perform (most of) the actions on the Apple Developer website.

## Certificate Signing Request (CSR)

If **you do not already have** a CSR, have misplaced one that was previously used, or have changed machines in between - on the Mac:

1. Open Keychain Access (found in Applications > Utilities or via Spotlight).
2. From the menu bar, select `Keychain Access > Certificate Assistant > Request a Certificate From a Certificate Authority`
3. In the dialog:
   - Enter your Apple ID email address in the `User Email Address` field.
   - Enter a common name (e.g., your name or company name) in the `Common Name` field.
   - Select `Saved to disk` for the CA Email Address (leave it blank).
   - Click `Continue`.
4. Save the `.certSigningRequest` file to a convenient location and click `Done`.

## Generate the New Certificate:

On the [Apple Developer website](https://developer.apple.com)

1. Sign in with your Apple ID.
2. Click `Account` in the top navigation, then select `Certificates, Identifiers & Profiles` from the sidebar (or go directly to https://developer.apple.com/account/resources/certificates/list).
3. In the Certificates section, click the `+` button to add a new certificate.
4. Select the relevant certificate type (Developer, Distribution) and click `Continue`.
5. Upload the `.certSigningRequest` file and click `Continue`.
6. Once generated, click `Download` to save the `.cer` file.
7. Double-click the downloaded `.cer` file on your Mac to install it into Keychain Access.

## Updating the Provisioning Profiles

After creating the new certificate, update each profile to use it. This regenerates the profiles.

1. In `Certificates, Identifiers & Profiles`, switch to the `Profiles` section (or go to https://developer.apple.com/account/resources/profiles/list).
2. Locate a provisioning profile in the list that is intended to use, or already uses, the certificate.
3. Click the profile name to view details, then click `Edit`.
4. In the `Certificates` section, select the new Apple Distribution certificate and deselect the old one.
5. Review other settings (e.g., app ID, devices) and make changes as needed. (There may be no changes, especially with Developer/Ad Hoc profiles that will still target the same devices)
6. Click `Generate` to create the updated profile.
7. Repeat steps 2–7 for any additional profiles that will use the certificate.

## Remove Certificates about to expire

In Keychain Access on the Mac:

1. Select `login` in the list on the left
2. Select the `My Certificates` tab
3. For the **OLD** certificate(s) (**those that you are renewing, which are about to expire**), right-click the item and click `Delete`. NOTE: If you mistakenly delete a certificate that is not about to expire, you can re-download the relevant certificate from the Apple Developer website.

## Download Updated Profiles in Xcode:

1. Open Xcode.
2. From the menu bar, select `Xcode > Settings...` (or press Command + ,).
3. In the Settings window, select the `Accounts` tab.
4. Select your Apple ID from the list on the left (add it if not present by clicking the + button and signing in).
5. In the team list on the right, select your development team.
6. Click `Download Manual Profiles` (or the downward arrow icon next to it) to fetch all current profiles from the Apple Developer portal, including the updated profiles.
7. Wait for the download to complete (you'll see a progress indicator). Xcode will install them automatically.
   
## Remove Old Profiles Using Finder:

Removing the old provisioning profiles is to avoid issues with PAServer (used by Delphi users) that can mistakenly select the wrong profile(s)

1. Open **Finder**.
2. From the menu bar, select **Go > Go to Folder...** (or press Shift + Command + G).
3. Enter `~/Library/MobileDevice/Provisioning Profiles/` and press `Enter`.
4. In the folder, locate the old provisioning profile files (they end in `.mobileprovision` and are named with UUIDs or profile names). Sort by date modified to identify recent ones, and/or use the preview pane in Finder to check the expiration date or certificate details.
5. Drag the files for your two Ad Hoc profiles to the Trash (or select and delete). **Do not delete unrelated profiles**.

## Restart PAServer

This applies to Delphi developers **ONLY**

As at February 3rd, 2026, modifying provisioning profiles can cause issues with PAServer on the Mac, so restart PAServer as a precaution.