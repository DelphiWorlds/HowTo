unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    PDFOpenDialog: TOpenDialog;
    procedure Button1Click(Sender: TObject);
  private
    procedure PrintPDF(const AFileName: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  Macapi.Helpers,
  Macapi.Foundation, Macapi.AppKit,
  DW.Macapi.PDFKit;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if PDFOpenDialog.Execute then
    PrintPDF(PDFOpenDialog.FileName);
end;

procedure TForm1.PrintPDF(const AFileName: string);
var
  LPDFDoc: PDFDocument;
  LPrintInfo: NSPrintInfo;
  LPrintOperation: NSPrintOperation;
  LApp: NSApplication;
begin
  LPDFDoc := TPDFDocument.Wrap(TPDFDocument.Alloc.initWithURL(TNSURL.Wrap(TNSURL.OCClass.fileURLWithPath(StrToNSStr(AFileName)))));
  if LPDFDoc <> nil then
  begin
    LPrintInfo := TNSPrintInfo.Wrap(TNSPrintInfo.OCClass.sharedPrintInfo);
    LPrintInfo.setHorizontalPagination(NSAutoPagination);
    LPrintInfo.setVerticalPagination(NSAutoPagination);
    LPrintInfo.setScalingFactor(1);
    LPrintOperation := LPDFDoc.printOperationForPrintInfo(LPrintInfo, kPDFPrintPageScaleToFit, True);
    LApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    LPrintOperation.runOperationModalForWindow(LApp.keyWindow, nil, nil, nil);
  end;
end;

end.
