unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Layouts, FMX.Edit,
  DW.RunProcess.Android;

type
  TForm1 = class(TForm)
    StartButton: TButton;
    Memo: TMemo;
    Layout1: TLayout;
    SendButton: TButton;
    Layout2: TLayout;
    CommandEdit: TEdit;
    procedure StartButtonClick(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
  private
    FProcess: TRunProcess;
    procedure ProcessOutputHandler(Sender: TObject; const AOutput: string);
    procedure ProcessTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  FProcess := TRunProcess.Create;
  FProcess.OnProcessOutput := ProcessOutputHandler;
  FProcess.OnProcessTerminated := ProcessTerminatedHandler;
end;

procedure TForm1.ProcessOutputHandler(Sender: TObject; const AOutput: string);
begin
  Memo.Lines.Add(AOutput);
end;

procedure TForm1.ProcessTerminatedHandler(Sender: TObject; const AExitCode: Cardinal);
begin
  Memo.Lines.Add('Process terminated with exit code: ' + AExitCode.ToString);
end;

procedure TForm1.SendButtonClick(Sender: TObject);
begin
  // Use this method only when you are certain the process being run is waiting for input
  FProcess.WriteLine(CommandEdit.Text);
end;

procedure TForm1.StartButtonClick(Sender: TObject);
//var
//  LEXEPath: string;
begin
  // The stockfish executable is related to this enquiry: https://en.delphipraxis.net/topic/5961-is-there-analog-of-cs-in-firemonkey
  // At the time of publishing this demo, I am yet to be able to make stockfish work. The executables were downloaded from:
  //   https://stockfishchess.org/download/
  // ..and added to the deployment (see Project|Deployment Manager)

  // LEXEPath := TPath.Combine(TPath.GetLibraryPath, 'stockfish');
  // FProcess.CommandLine := LEXEPath;

  // So here are some example commands you could execute:
  // FProcess.CommandLine := 'free -h';
  FProcess.CommandLine := 'df -h';
  FProcess.Run;
end;

end.
