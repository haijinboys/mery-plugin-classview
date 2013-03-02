unit mProp;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
{$IFEND}
  mMain;

type
  TPropForm = class(TForm)
    BarPosLabel: TLabel;
    BarPosComboBox: TComboBox;
    ParamLabel: TLabel;
    ParamEdit: TEdit;
    AutoRefreshCheckBox: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private éŒ¾ }
  public
    { Public éŒ¾ }
  end;

function Prop(AOwner: TComponent; var APos: NativeInt;
  var AParam: string; var AAutoRefresh: Boolean): Boolean;

var
  PropForm: TPropForm;

implementation

{$R *.dfm}


uses
{$IF CompilerVersion > 22.9}
  System.Math,
{$ELSE}
  Math,
{$IFEND}
  mCommon;

function Prop(AOwner: TComponent; var APos: NativeInt;
  var AParam: string; var AAutoRefresh: Boolean): Boolean;
begin
  with TPropForm.Create(AOwner) do
    try
      BarPosComboBox.ItemIndex := APos;
      ParamEdit.Text := AParam;
      AutoRefreshCheckBox.Checked := AAutoRefresh;
      Result := ShowModal = mrOk;
      if Result then
      begin
        APos := BarPosComboBox.ItemIndex;
        AParam := ParamEdit.Text;
        AAutoRefresh := AutoRefreshCheckBox.Checked;
      end;
    finally
      Release;
    end;
end;

procedure TPropForm.FormCreate(Sender: TObject);
begin
  if Win32MajorVersion < 6 then
    with Font do
    begin
      Name := 'Tahoma';
      Size := 8;
    end;
  with Font do
  begin
    ChangeScale(FFontSize, Size);
    Name := FFontName;
    Size := FFontSize;
  end;
end;

procedure TPropForm.FormDestroy(Sender: TObject);
begin
  //
end;

procedure TPropForm.FormShow(Sender: TObject);
begin
  //
end;

procedure TPropForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

end.
