Q: I found that I need to swipe down on the notification itself to show the buttons. Is there a way of having this expand automatically?

A:

iOS notifications do not automatically expand to show action buttons; the user needs to interact with the notification (e.g., swipe down or long-press on the notification) to reveal the action buttons. This behavior is by design to avoid accidental interactions and to keep the notification experience consistent.

However, there are a few tips and best practices you can follow to ensure users know about the actions available:

### Tips and Best Practices

1. **Clear and Concise Notification Text**:
   - Make the notification text informative and clear to prompt users to interact with it.
   - You can include text that hints at additional options being available by interacting with the notification (e.g., "Swipe down for options").

2. **Rich Media Notifications**:
   - Use rich notifications with images, videos, or other attachments to make the notification more engaging. Users are more likely to interact with visually rich notifications.
   - Note that using rich media does not automatically expand the notification but can attract more user interaction.

3. **Notification Content Extensions**:
   - Consider using a Notification Content Extension if you need to provide a custom interface for your notifications. This allows you to create a custom view that users can interact with when they expand the notification.

<br/>
<br/>
<br/>

Addendum:

I have some work in development to support custom notifications on iOS, with a goal of making the integration as easy as possible, since it requires iOS Extensions that presently need to be created/compiled in Xcode. 