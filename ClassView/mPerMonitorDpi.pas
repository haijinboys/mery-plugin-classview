unit mPerMonitorDpi;

interface

// modified begin
{$HINTS OFF}
{$WARNINGS OFF}
// modified end

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Winapi.MultiMon, Winapi.ShellScaling;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, MultiMon,
  ShellScaling;
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

  TCustomScaledForm = class(TForm)
  private
    { Private éŒ¾ }
    FOnAfterMonitorDpiChanged: TMonitorDpiChangedEvent;
    FOnBeforeMonitorDpiChanged: TMonitorDpiChangedEvent;
  protected
    { Protected éŒ¾ }
    procedure WMDpiChanged(var Message: TWMDpi); message WM_DPICHANGED;
    procedure WMNCCreate(var Message: TWMNCCreate); message WM_NCCREATE;
  public
    { Public éŒ¾ }
    property OnAfterMonitorDpiChanged: TMonitorDpiChangedEvent read FOnAfterMonitorDpiChanged write FOnAfterMonitorDpiChanged;
    property OnBeforeMonitorDpiChanged: TMonitorDpiChangedEvent read FOnBeforeMonitorDpiChanged write FOnBeforeMonitorDpiChanged;
  end;

  TScaledForm = class(TCustomScaledForm)
  private
    { Private éŒ¾ }
    class var FDefaultFont: TFont;
    class constructor Create;
    class destructor Destroy;
    class procedure SetDefaultFont(const Value: TFont); static;
  protected
    { Protected éŒ¾ }
    procedure DoShow; override;
  public
    { Public éŒ¾ }
    class property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
  end;

function EnableNonClientDpiScaling(hWnd: HWND): BOOL; stdcall;

implementation

{ TCustomScaledForm }

procedure TCustomScaledForm.WMDpiChanged(var Message: TWMDpi);
var
  Y: Integer;
  OldPPI: Integer;
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
      if not Visible then
      begin
        Y := Height - ClientHeight;
        ChangeScale(Message.YDpi, PixelsPerInch);
        with Constraints do
        begin
          if MinHeight > 0 then
            MinHeight := MinHeight + Y - MulDiv(Y, Message.YDpi, PixelsPerInch);
          if MaxHeight > 0 then
            MaxHeight := MaxHeight + Y - MulDiv(Y, Message.YDpi, PixelsPerInch);
        end;
      end
      else
      begin
        if Message.YDpi < PixelsPerInch then
          ScaleConstraints(Message.YDpi, PixelsPerInch);
        with Message.ScalledRect^ do
          SetWindowPos(Self.Handle, 0, Left, Top, Right - Left, Bottom - Top, SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_NOACTIVATE);
        ScaleControls(Message.YDpi, PixelsPerInch);
        Font.Height := MulDiv(Font.Height, Message.YDpi, PixelsPerInch);
        if Message.YDpi > PixelsPerInch then
          ScaleConstraints(Message.YDpi, PixelsPerInch);
      end;
      Resize;
      OldPPI := PixelsPerInch;
      PixelsPerInch := Message.YDpi;
      if Assigned(FOnAfterMonitorDpiChanged) then
        FOnAfterMonitorDpiChanged(Self, OldPPI, PixelsPerInch);
    end;
    Message.Result := 0;
  end;
end;

procedure TCustomScaledForm.WMNCCreate(var Message: TWMNCCreate);
begin
{$IF CompilerVersion > 22.9}
  if CheckWin32Version(10) and (TOSVersion.Build >= 14393) then
{$ELSE}
  if CheckWin32Version(10) and (Win32BuildNumber >= 14393) then
{$IFEND}
    EnableNonClientDpiScaling(Self.Handle);
  inherited;
end;

{ TScaledForm }

class constructor TScaledForm.Create;
begin
  FDefaultFont := TFont.Create;
end;

class destructor TScaledForm.Destroy;
begin
  if Assigned(FDefaultFont) then
    FreeAndNil(FDefaultFont);
end;

procedure TScaledForm.DoShow;
var
  LFont: TFont;
begin
  LFont := TFont.Create;
  try
    with LFont do
    begin
      Assign(FDefaultFont);
      Height := MulDiv(Height, Self.PixelsPerInch, Screen.PixelsPerInch);
    end;
    with Font do
      if (Name <> LFont.Name) or (Height <> LFont.Height) or (Charset <> LFont.Charset) then
      begin
        ChangeScale(Abs(LFont.Height), Abs(Height));
        Assign(LFont);
      end;
  finally
    LFont.Free;
  end;
  inherited;
end;

class procedure TScaledForm.SetDefaultFont(const Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

function EnableNonClientDpiScaling; external user32 name 'EnableNonClientDpiScaling' delayed;

end.
