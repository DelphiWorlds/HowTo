{$IF Defined(IOS)}
  {$R 'Style.iOS.res' 'Styles\Style.iOS.rc'}
{$ELSEIF Defined(ANDROID)}
  {$R 'Style.Android.res' 'Styles\Style.Android.rc'}
{$ELSE}
  {$R 'Style.Default.res' 'Styles\Style.Default.rc'}
{$ENDIF}

