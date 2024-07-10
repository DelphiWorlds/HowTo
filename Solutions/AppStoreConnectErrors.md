# App Store Connect Errors

## Message re: com.apple.security.network.client

![image](https://github.com/DelphiWorlds/HowTo/assets/22670829/40e89d40-b9c6-4428-b584-f91d26ae13b4)

Solution is to modify the `Entitlement.TemplateOSX.xml` file in the project to add:

```xml
<key>com.apple.security.network.client</key>
<true/>
```

Inside of the `<dict>` tag