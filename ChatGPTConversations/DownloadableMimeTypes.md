Preface: I asked this question in relation to how to handle downloading of files in the implementation of [`TWebBrowserExt` in Kastri](https://github.com/DelphiWorlds/Kastri/tree/master/Features/WebBrowserExt), specifically for iOS/macOS, because before iOS 14.5, there is no "standard" method for handling downloads in code. Android was much simpler (mostly) as the underlying `WebView` can be assigned a `DownloadListener` which allows handling specifically for downloading files. 

Q. Can you provide a complete list of mime types that could be considered "downloadable"?

A. There's no definitive list of "downloadable" MIME types since it largely depends on the types of files your app wants to handle as downloads. However, certain MIME types are commonly associated with files that users typically download rather than view directly in a web browser.

Here's a comprehensive list of MIME types that could be considered "downloadable":

### **Documents**
- `application/pdf` (PDF files)
- `application/msword` (Microsoft Word files)
- `application/vnd.openxmlformats-officedocument.wordprocessingml.document` (Microsoft Word Open XML)
- `application/vnd.ms-excel` (Microsoft Excel files)
- `application/vnd.openxmlformats-officedocument.spreadsheetml.sheet` (Microsoft Excel Open XML)
- `application/vnd.ms-powerpoint` (Microsoft PowerPoint files)
- `application/vnd.openxmlformats-officedocument.presentationml.presentation` (Microsoft PowerPoint Open XML)
- `text/csv` (Comma-separated values)
- `application/rtf` (Rich Text Format)
- `application/vnd.oasis.opendocument.text` (OpenDocument Text)
- `application/vnd.oasis.opendocument.spreadsheet` (OpenDocument Spreadsheet)

### **Archives**
- `application/zip` (ZIP archives)
- `application/x-rar-compressed` (RAR archives)
- `application/x-7z-compressed` (7-zip archives)
- `application/gzip` (GZIP archives)
- `application/x-tar` (TAR archives)

### **Images**
- `image/jpeg` (JPEG images)
- `image/png` (PNG images)
- `image/gif` (GIF images)
- `image/bmp` (Bitmap images)
- `image/tiff` (TIFF images)
- `image/webp` (WebP images)

### **Audio**
- `audio/mpeg` (MP3 audio)
- `audio/wav` (WAV audio)
- `audio/x-aac` (AAC audio)
- `audio/ogg` (OGG audio)
- `audio/flac` (FLAC audio)

### **Video**
- `video/mp4` (MP4 video)
- `video/x-msvideo` (AVI video)
- `video/quicktime` (MOV video)
- `video/x-matroska` (Matroska video, e.g., MKV)
- `video/webm` (WebM video)
- `video/x-flv` (FLV video)

### **Executable/Software**
- `application/vnd.android.package-archive` (Android APK files)
- `application/x-msdownload` (Windows executable files, e.g., `.exe`)
- `application/x-sh` (Shell script files)
- `application/x-csh` (C shell script files)
- `application/java-archive` (Java JAR files)
- `application/x-bzip2` (Bzip2 compressed files)

### **Fonts**
- `font/woff` (Web Open Font Format)
- `font/woff2` (Web Open Font Format 2)
- `application/x-font-ttf` (TrueType font)
- `application/x-font-otf` (OpenType font)

### **Others**
- `application/octet-stream` (Binary data, typically treated as a generic downloadable file)
- `application/x-iso9660-image` (ISO disk image files)
- `application/x-disk-image` (Disk image files)

### Important Notes:
1. **`application/octet-stream`**: This is a generic binary file MIME type and can be used for a variety of file types. It's often used when the server doesn't know the correct MIME type for a file, so it indicates that the file should be downloaded rather than displayed.

2. **MIME Type Variations**: Some MIME types have variations depending on the file type, software, or OS-specific usage. For example, certain compressed formats (e.g., `.gz`, `.bz2`) might have slightly different MIME types based on context.

3. **Custom Needs**: Depending on your application, you may need to handle additional MIME types. For example, if your app handles specific types of CAD files, e-books, or proprietary file formats, you'd include their MIME types as well.

This list provides a broad range of MIME types that are typically treated as downloadable content. You can customize it to suit the specific needs of your application.

