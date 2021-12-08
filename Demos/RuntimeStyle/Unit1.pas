unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts, FMX.ListBox, FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    Label1: TLabel;
    Switch1: TSwitch;
    Layout2: TLayout;
    Label2: TLabel;
    Edit1: TEdit;
    Layout3: TLayout;
    Label3: TLabel;
    Button1: TButton;
    Layout4: TLayout;
    Label4: TLabel;
    ListBox1: TListBox;
  private
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  System.IOUtils,
  FMX.Styles;

{ TForm1 }

constructor TForm1.Create(AOwner: TComponent);
begin
  inherited;
  // Load style using "Resource" method:
  // TStyleManager.TrySetStyleFromResource('AppStyle');
  // Load style using "Deployment Manager" method:
  TStyleManager.SetStyleFromFile(TPath.Combine(TPath.GetDocumentsPath, 'AppStyle.fsf'));
  ListBox1.ItemIndex := 1;
end;

end.
