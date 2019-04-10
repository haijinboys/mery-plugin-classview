unit mPerMonitorDpi;

interface

// modified begin
{$HINTS OFF}
{$WARNINGS OFF}
// modified end

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Winapi.MultiMon, Winapi.ShellScaling,
  Winapi.UxTheme;
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, MultiMon,
  ShellScaling, UxTheme;
{$IFEND}


const
  themelib = 'uxtheme.dll';

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
    function GetSystemMetrics(nIndex: Integer): Integer;
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

function EnableNonClientDpiScaling(hwnd: HWND): BOOL; stdcall;
function GetSystemMetricsForDpi(nIndex: Integer; dpi: UINT): Integer; stdcall;
function OpenThemeDataForDpi(hwnd: HWND; pszClassList: LPCWSTR; dpi: UINT): HTHEME; stdcall;

implementation

{ TCustomScaledForm }

function TCustomScaledForm.GetSystemMetrics(nIndex: Integer): Integer;
begin
  if CheckWin32Version(10) and ({$IF CompilerVersion > 22.9}TOSVersion.Build{$ELSE}Win32BuildNumber{$IFEND} >= 14393) then
    Result := GetSystemMetricsForDpi(nIndex, PixelsPerInch)
  else
    Result := {$IF CompilerVersion > 22.9}Winapi.{$IFEND}Windows.GetSystemMetrics(nIndex);
end;

procedure TCustomScaledForm.WMDpiChanged(var Message: TWMDpi);
var
  Y: Integer;
  LPixelsPerInch: Integer;
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
      LPixelsPerInch := PixelsPerInch;
      PixelsPerInch := Message.YDpi;
      if not Visible then
      begin
        Y := Height - ClientHeight;
        ChangeScale(Message.YDpi, LPixelsPerInch);
        with Constraints do
        begin
          if MinHeight > 0 then
            MinHeight := MinHeight + Y - MulDiv(Y, Message.YDpi, LPixelsPerInch);
          if MaxHeight > 0 then
            MaxHeight := MaxHeight + Y - MulDiv(Y, Message.YDpi, LPixelsPerInch);
        end;
      end
      else
      begin
        if Message.YDpi < LPixelsPerInch then
          ScaleConstraints(Message.YDpi, LPixelsPerInch);
        with Message.ScalledRect^ do
          SetWindowPos(Self.Handle, 0, Left, Top, Right - Left, Bottom - Top, SWP_NOZORDER or SWP_NOOWNERZORDER or SWP_NOACTIVATE);
        ScaleControls(Message.YDpi, LPixelsPerInch);
        Font.Height := MulDiv(Font.Height, Message.YDpi, LPixelsPerInch);
        if Message.YDpi > LPixelsPerInch then
          ScaleConstraints(Message.YDpi, LPixelsPerInch);
      end;
      Resize;
      if Assigned(FOnAfterMonitorDpiChanged) then
        FOnAfterMonitorDpiChanged(Self, LPixelsPerInch, Message.YDpi);
    end;
    Message.Result := 0;
  end;
end;

procedure TCustomScaledForm.WMNCCreate(var Message: TWMNCCreate);
begin
{$IF CompilerVersion > 22.9}
  if CheckWin32Version(10) and (TOSVersion.Build >= 14393) and (TOSVersion.Build < 15063) then
{$ELSE}
  if CheckWin32Version(10) and (Win32BuildNumber >= 14393) and (Win32BuildNumber < 15063) then
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
function GetSystemMetricsForDpi(nIndex: Integer; dpi: UINT): Integer; stdcall; external user32 name 'GetSystemMetricsForDpi' delayed;
function OpenThemeDataForDpi; external themelib name 'OpenThemeDataForDpi' delayed;

end.
