# iOS 26 Take Photo Issue

## Description

These are instructions to patch `FMX.MediaLibrary.iOS.pas` so that `TTakePhotoFromCameraAction` does not crash when the `NeedSaveToAlbum` property is set to `True`

## Patch Instructions

1. Copy `FMX.MediaLibrary.iOS.pas` from the `source\fmx` folder of your Delphi installation to either the same folder as your project, or somewhere in the project compile path
2. Modify the uses clause of the `implementation` section to add the following units to the end:
   ```delphi
   System.TypInfo, Macapi.ObjCRuntime
   ```
3. Add the following declaration before `TSaveImageRequest`:
   ```delphi
   ISaveImageRequest = interface(NSObject)
     ['{260AE5F7-720A-488C-806F-D65ADEDFFF87}']
     procedure writeToSavedPhotosAlbumCompletion(image: UIImage; error: NSError; contextInfo: Pointer); cdecl;
   end;
   ```
4. Modify the declaration of `TSaveImageRequest` to:
   ```delphi
   TSaveImageRequest = class(TOCLocal)
   private
     [Weak] FImageManager: TImageManagerCocoa;
     FImage: UIImage;
     FOnCompletion: TWriteImageCompletionEvent;
   protected
     function GetObjectiveCClass: PTypeInfo; override;
   public
     { ISaveImageRequest }
     procedure writeToSavedPhotosAlbumCompletion(image: UIImage; error: NSError; contextInfo: Pointer); cdecl;
   public
     constructor Create(const AImageManager: TImageManagerCocoa; const AImage: UIImage; const AHandler: TWriteImageCompletionEvent);
     procedure Save;
   end;
   ```
5. Add the following code for `TSaveImageRequest.GetObjectiveCClass`:
   ```delphi
   function TSaveImageRequest.GetObjectiveCClass: PTypeInfo;
   begin
     Result := TypeInfo(ISaveImageRequest);
   end;
   ```
6. Replace the `TSaveImageRequest.PerformResultOfSavingPhoto` and the `TSaveImageRequest.Save` methods with:
   ```delphi
   procedure TSaveImageRequest.writeToSavedPhotosAlbumCompletion(image: UIImage; error: NSError; contextInfo: Pointer);
   begin
     try
       if Assigned(FOnCompletion) then
         if error <> nil then
           FOnCompletion(False, NSStrToStr(error.localizedDescription))
         else
           FOnCompletion(True, SImageSaved);
     finally
       // ARC will remove instance of this object, when we remove the single link from FSaveQueue
       FImageManager.FSaveImageRequests.Remove(Self);
     end;
   end;

   procedure TSaveImageRequest.Save;
   begin
     UIImageWriteToSavedPhotosAlbum(NSObjectToID(FImage), GetObjectID, sel_getUid('writeToSavedPhotosAlbumCompletion:::'), nil);
   end;
   ```
7. Rebuild your project

This should fix the crash