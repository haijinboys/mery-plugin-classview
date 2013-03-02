// -----------------------------------------------------------------------------
// クラスビュー
//
// Copyright (c) Kuro. All Rights Reserved.
// e-mail: info@haijin-boys.com
// www:    http://www.haijin-boys.com/
// -----------------------------------------------------------------------------

library ClassView;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}


uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
{$ELSE}
  Windows,
  SysUtils,
  Classes,
{$IFEND}
  mCommon in 'mCommon.pas',
  mMain in 'mMain.pas' {MainForm},
  mProp in 'mProp.pas' {PropForm},
  mFrame in 'mFrame.pas',
  mClassView in 'mClassView.pas',
  mPlugin in 'mPlugin.pas';

const
  IDS_MENU_TEXT = 1;
  IDS_STATUS_MESSAGE = 2;
  IDI_ICON = 101;

var
  FList: TFrameList;

{$R *.res}


procedure OnCommand(hwnd: HWND); stdcall;
var
  Frame: TFrame;
begin
  Frame := FList.Find(hwnd);
  Frame.OnCommand(hwnd);
end;

function QueryStatus(hwnd: HWND; pbChecked: PBOOL): BOOL; stdcall;
var
  Frame: TFrame;
begin
  Result := False;
  Frame := FList.Find(hwnd);
  if Frame <> nil then
    Result := Frame.QueryStatus(hwnd, pbChecked);
end;

function GetMenuTextID: NativeInt; stdcall;
begin
  Result := IDS_MENU_TEXT;
end;

function GetStatusMessageID: NativeInt; stdcall;
begin
  Result := IDS_STATUS_MESSAGE;
end;

function GetIconID: NativeInt; stdcall;
begin
  Result := IDI_ICON;
end;

procedure OnEvents(hwnd: HWND; nEvent: NativeInt; lParam: LPARAM); stdcall;
var
  I: NativeInt;
  AFrame: TFrame;
begin
  if (nEvent and EVENT_CREATE) <> 0 then
  begin
    FList := TFrameList.Create;
  end
  else
  begin
    if Assigned(FList) then
    begin
      if (nEvent and EVENT_CREATE_FRAME) <> 0 then
      begin
        AFrame := TClassViewFrame.Create;
        with AFrame do
        begin
          Handle := hwnd;
          OnEvents(hwnd, nEvent, lParam);
        end;
        FList.Add(AFrame);
      end
      else if (nEvent and EVENT_CLOSE_FRAME) <> 0 then
      begin
        AFrame := FList.Find(hwnd);
        if AFrame <> nil then
          with AFrame do
          begin
            OnEvents(hwnd, nEvent, lParam);
            FList.Remove(AFrame);
            Free;
          end;
      end
      else if (nEvent and EVENT_CLOSE) <> 0 then
      begin
        for I := FList.Count - 1 downto 0 do
          FList[I].Free;
        FList.Free;
      end
      else
      begin
        AFrame := FList.Find(hwnd);
        if AFrame <> nil then
          AFrame.OnEvents(hwnd, nEvent, lParam);
      end;
    end;
  end;
end;

function PluginProc(hwnd: HWND; nMsg: NativeInt; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Frame: TFrame;
begin
  Result := 0;
  case nMsg of
    MP_GET_NAME:
      begin
        Result := Length(SName);
        if lParam <> 0 then
          lstrcpynW(PChar(lParam), PChar(SName), wParam);
      end;
    MP_GET_VERSION:
      begin
        Result := Length(SVersion);
        if lParam <> 0 then
          lstrcpynW(PChar(lParam), PChar(SVersion), wParam);
      end;
  else
    begin
      Frame := FList.Find(hwnd);
      if Frame = nil then
      begin
        hwnd := GetParent(hwnd);
        Frame := FList.Find(hwnd);
      end;
      if Frame <> nil then
        Result := Frame.PluginProc(hwnd, nMsg, wParam, lParam);
    end;
  end;
end;

exports
  OnCommand,
  QueryStatus,
  GetMenuTextID,
  GetStatusMessageID,
  GetIconID,
  OnEvents,
  PluginProc;

begin

end.
