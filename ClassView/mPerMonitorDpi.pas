unit mPerMonitorDpi;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.Classes, Vcl.Forms, Winapi.MultiMon,
  Winapi.ShellScaling;
{$ELSE}
  Windows, Messages, Classes, Forms, MultiMon, ShellScaling;
{$IFEND}


const
  WM_DPICHANGED = $02E0;

type
  TDWordFiller = record
{$IFDEF CPUX64}
    Filler: array [1 .. 4] of Byte;
{$ENDIF}
  end;

  TWMDpi = record
    Msg: Cardinal;
    MsgFiller: TDWordFiller;
    YDpi: Word;
    XDpi: Word;
    WParamFiller: TDWordFiller;
    ScalledRect: PRECT;
    Result: LRESULT;
  end;

  TMonitorDpiChangedEvent = procedure(Sender: TObject; OldDPI: Integer; NewDPI: Integer) of object;

  TScaledForm = class(TForm)
  private
    { Private êÈåæ }
    FOnAfterMonitorDpiChanged: TMonitorDpiChangedEvent;
    FOnBeforeMonitorDpiChanged: TMonitorDpiChangedEvent;
  protected
    { Protected êÈåæ }
    procedure WMDpiChanged(var Message: TWMDpi); message WM_DPICHANGED;
  public
    { Public êÈåæ }
    property OnAfterMonitorDpiChanged: TMonitorDpiChangedEvent read FOnAfterMonitorDpiChanged write FOnAfterMonitorDpiChanged;
    property OnBeforeMonitorDpiChanged: TMonitorDpiChangedEvent read FOnBeforeMonitorDpiChanged write FOnBeforeMonitorDpiChanged;
  end;

implementation

{ TScaledForm }

procedure TScaledForm.WMDpiChanged(var Message: TWMDpi);
var
  R: TRect;
  OldPPI: NativeInt;
begin
  if not(csDesigning in ComponentState) then
  begin
    if (Message.YDpi = 0) or (PixelsPerInch = 0) then
    begin
      if (Application.MainForm <> nil) and (Application.MainForm.PixelsPerInch <> 0) then
        PixelsPerInch := Application.MainForm.PixelsPerInch
      else
        Exit;
    end;
    if Message.YDpi <> PixelsPerInch then
    begin
      if Assigned(FOnBeforeMonitorDpiChanged) then
        FOnBeforeMonitorDpiChanged(Self, PixelsPerInch, Message.YDpi);
      // ChangeScale(Message.YDpi, PixelsPerInch);
      R := Message.ScalledRect^;
      SetWindowPos(Self.Handle, 0, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_NOACTIVATE);
      ScaleControls(Message.YDpi, PixelsPerInch);
      Font.Height := MulDiv(Font.Height, Message.YDpi, PixelsPerInch);
      ScaleConstraints(Message.YDpi, PixelsPerInch);
      Resize;
      OldPPI := PixelsPerInch;
      PixelsPerInch := Message.YDpi;
      if Assigned(FOnAfterMonitorDpiChanged) then
        FOnAfterMonitorDpiChanged(Self, OldPPI, PixelsPerInch);
    end;
    Message.Result := 0;
  end;
end;

end.
