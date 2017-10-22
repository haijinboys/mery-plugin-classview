// -----------------------------------------------------------------------------
// クラスビュー
//
// Copyright (c) Kuro. All Rights Reserved.
// e-mail: info@haijin-boys.com
// www:    https://www.haijin-boys.com/
// -----------------------------------------------------------------------------

unit mMain;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Menus, Vcl.ImgList, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ImgList, StdCtrls, ComCtrls, ExtCtrls,
{$IFEND}
  mPerMonitorDpi;

const
  MaxLineLength = 2000;
  MaxDepth = 8;

type
  TSymbolItem = class
  private
    { Private 宣言 }
    FParentName: string;
    FParentType: string;
    FSymbolName: string;
    FSymbolType: string;
    FSymbolLine: Integer;
  public
    { Public 宣言 }
    constructor Create;
    property ParentName: string read FParentName write FParentName;
    property ParentType: string read FParentType write FParentType;
    property SymbolName: string read FSymbolName write FSymbolName;
    property SymbolType: string read FSymbolType write FSymbolType;
    property SymbolLine: Integer read FSymbolLine write FSymbolLine;
  end;

  TSymbolList = class(TList)
  private
    { Private 宣言 }
    FSymbolType: string;
    FSymbolLine: Integer;
    function Get(Index: Integer): TSymbolItem; inline;
  public
    { Public 宣言 }
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TSymbolItem read Get; default;
    property SymbolType: string read FSymbolType write FSymbolType;
    property SymbolLine: Integer read FSymbolLine write FSymbolLine;
  end;

  TClassViewItem = class
  private
    { Private 宣言 }
    FNode: TTreeNode;
    FLineNum: Integer;
    FLevel: Integer;
    FSymbolName: string;
    FSymbolType: string;
  public
    { Public 宣言 }
    constructor Create(ALineNum: Integer; ALevel: Integer; const ASymbolName, ASymbolType: string);
    property Node: TTreeNode read FNode write FNode;
    property LineNum: Integer read FLineNum write FLineNum;
    property Level: Integer read FLevel write FLevel;
    property SymbolName: string read FSymbolName write FSymbolName;
    property SymbolType: string read FSymbolType write FSymbolType;
  end;

  TClassViewList = class(TList)
  private
    { Private 宣言 }
    function Get(Index: Integer): TClassViewItem; inline;
  public
    { Public 宣言 }
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TClassViewItem read Get; default;
  end;

  TMainForm = class(TScaledForm)
    PopupMenu: TPopupMenu;
    GoMenuItem: TMenuItem;
    N1: TMenuItem;
    CollapseAllMenuItem: TMenuItem;
    ExpandAllMenuItem: TMenuItem;
    N2: TMenuItem;
    RefreshMenuItem: TMenuItem;
    N3: TMenuItem;
    PropPopupMenuItem: TMenuItem;
    TreeView: TTreeView;
    SmallImageList: TImageList;
    MediumImageList: TImageList;
    LargeImageList: TImageList;
    ExtraLargeImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure GoMenuItemClick(Sender: TObject);
    procedure CollapseAllMenuItemClick(Sender: TObject);
    procedure ExpandAllMenuItemClick(Sender: TObject);
    procedure RefreshMenuItemClick(Sender: TObject);
    procedure PropPopupMenuItemClick(Sender: TObject);
    procedure TreeViewClick(Sender: TObject);
    procedure TreeViewDblClick(Sender: TObject);
    procedure TreeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private 宣言 }
    FEditor: THandle;
    FBarPos: Integer;
    FFontName: string;
    FFontSize: Integer;
    FParam: string;
    FAutoRefresh: Boolean;
    FList: TClassViewList;
    FUpdateClassView: Boolean;
    FWorkFlag: Integer;
    FWorkThread: THandle;
    FAbortThread: Boolean;
    FQueEvent: THandle;
    FMutex: THandle;
    FPoint: TPoint;
    procedure ReadIni;
    procedure WriteIni;
    procedure UpdateTreeView;
    procedure UpdateTreeViewAll;
    procedure UpdateTreeViewString;
    procedure ClassViewSelected(Node: TTreeNode; FocusView: Boolean);
  public
    { Public 宣言 }
    procedure ResetThread;
    procedure ClassViewAll;
    procedure SetFont;
    procedure SetScale(const Value: Integer);
    function SetProperties: Boolean;
    property BarPos: Integer read FBarPos write FBarPos;
    property UpdateClassView: Boolean read FUpdateClassView write FUpdateClassView;
    property Editor: THandle read FEditor write FEditor;
    property WorkHandle: THandle read FWorkThread write FWorkThread;
    property WorkFlag: Integer read FWorkFlag write FWorkFlag;
    property AbortThread: Boolean read FAbortThread write FAbortThread;
    property QueEvent: THandle read FQueEvent write FQueEvent;
    property Mutex: THandle read FMutex write FMutex;
    property AutoRefresh: Boolean read FAutoRefresh;
  end;

var
  MainForm: TMainForm;
  FSymbolList: TStringList;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.Types, System.StrUtils, System.Math, System.IniFiles,
{$ELSE}
  Types, StrUtils, Math, IniFiles,
{$IFEND}
  mCommon, mPlugin, mProp;

{$R *.dfm}


function WaitMessageLoop(Count: LongWord; var Handles: THandle;
  Milliseconds: DWORD): Integer;
var
  Quit: Boolean;
  ExitCode: Integer;
  WaitResult: DWORD;
  Msg: TMsg;
begin
  Quit := False;
  ExitCode := 0;
  repeat
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      case Msg.message of
        WM_QUIT:
          begin
            Quit := True;
            ExitCode := Integer(Msg.wParam);
            Break;
          end;
        WM_MOUSEMOVE:
          ;
        WM_LBUTTONDOWN:
          ;
      else
        DispatchMessage(Msg);
      end;
    end;
    WaitResult := MsgWaitForMultipleObjects(Count, Handles, False, Milliseconds, QS_ALLINPUT);
  until WaitResult <> WAIT_OBJECT_0 + 1;
  if Quit then
    PostQuitMessage(ExitCode);
  Result := Integer(WaitResult - WAIT_OBJECT_0);
end;

{ TSymbolItem }

constructor TSymbolItem.Create;
begin
  FParentName := '';
  FParentType := '';
  FSymbolName := '';
  FSymbolType := '';
  FSymbolLine := -1;
end;

{ TSymbolList }

constructor TSymbolList.Create;
begin
  FSymbolType := '';
  FSymbolLine := -1;
end;

destructor TSymbolList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSymbolList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited;
end;

function TSymbolList.Get(Index: Integer): TSymbolItem;
begin
  Result := TSymbolItem(inherited Get(Index));
end;

{ TClassViewItem }

constructor TClassViewItem.Create(ALineNum: Integer; ALevel: Integer;
  const ASymbolName, ASymbolType: string);
begin
  FNode := nil;
  FLineNum := ALineNum;
  FLevel := ALevel;
  FSymbolName := Trim(ASymbolName);
  FSymbolType := Trim(ASymbolType);
end;

{ TClassViewList }

destructor TClassViewList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TClassViewList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited;
end;

function TClassViewList.Get(Index: Integer): TClassViewItem;
begin
  Result := TClassViewItem(inherited Get(Index));
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  TScaledForm.DefaultFont.Assign(Font);
  FEditor := ParentWindow;
  FFontName := '';
  FFontSize := 0;
  FParam := '';
  FAutoRefresh := True;
  FList := TClassViewList.Create;
  FUpdateClassView := False;
  FWorkFlag := 0;
  FQueEvent := CreateEvent(nil, True, False, nil);
  FMutex := CreateMutex(nil, False, nil);
  FPoint.X := -1;
  FPoint.Y := -1;
  ReadIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ResetThread;
  if FQueEvent > 0 then
  begin
    CloseHandle(FQueEvent);
    FQueEvent := 0;
  end;
  if FMutex > 0 then
  begin
    CloseHandle(FMutex);
    FMutex := 0;
  end;
  WriteIni;
  if Assigned(FList) then
    FreeAndNil(FList);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  UpdateTreeView;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TMainForm.GoMenuItemClick(Sender: TObject);
var
  LNode: TTreeNode;
begin
  LNode := TreeView.GetNodeAt(FPoint.X, FPoint.Y);
  if LNode = TreeView.Selected then
    ClassViewSelected(TreeView.Selected, True);
end;

procedure TMainForm.CollapseAllMenuItemClick(Sender: TObject);
begin
  if WaitMessageLoop(1, FMutex, INFINITE) <> 0 then
    Exit;
  TreeView.FullCollapse;
  ReleaseMutex(FMutex);
end;

procedure TMainForm.ExpandAllMenuItemClick(Sender: TObject);
begin
  if WaitMessageLoop(1, FMutex, INFINITE) <> 0 then
    Exit;
  TreeView.FullExpand;
  ReleaseMutex(FMutex);
end;

procedure TMainForm.PropPopupMenuItemClick(Sender: TObject);
begin
  SetProperties;
end;

procedure TMainForm.TreeViewClick(Sender: TObject);
var
  LNode: TTreeNode;
begin
  LNode := TreeView.GetNodeAt(FPoint.X, FPoint.Y);
  if LNode = TreeView.Selected then
    ClassViewSelected(TreeView.Selected, False);
end;

procedure TMainForm.TreeViewDblClick(Sender: TObject);
var
  LNode: TTreeNode;
begin
  LNode := TreeView.GetNodeAt(FPoint.X, FPoint.Y);
  if LNode = TreeView.Selected then
    ClassViewSelected(TreeView.Selected, True);
end;

procedure TMainForm.TreeViewKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
  begin
    ClassViewSelected(TreeView.Selected, True);
    Key := 0;
  end;
end;

procedure TMainForm.TreeViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FPoint.X := X;
  FPoint.Y := Y;
  with TreeView do
    if (Button = mbRight) and (GetNodeAt(X, Y) <> nil) then
      Selected := GetNodeAt(X, Y);
end;

procedure TMainForm.ReadIni;
var
  S: string;
begin
  if not GetIniFileName(S) then
    Exit;
  with TMemIniFile.Create(S, TEncoding.UTF8) do
    try
      with TScaledForm.DefaultFont do
        if ValueExists('MainForm', 'FontName') then
        begin
          Name := ReadString('MainForm', 'FontName', Name);
          Size := ReadInteger('MainForm', 'FontSize', Size);
        end
        else if CheckWin32Version(6, 2) then
          Assign(Screen.IconFont);
      FFontName := ReadString('ClassView', 'FontName', FFontName);
      FFontSize := ReadInteger('ClassView', 'FontSize', FFontSize);
      FParam := ReadString('ClassView', 'Param', FParam);
      FAutoRefresh := ReadBool('ClassView', 'AutoRefresh', FAutoRefresh);
    finally
      Free;
    end;
end;

procedure TMainForm.RefreshMenuItemClick(Sender: TObject);
begin
  FUpdateClassView := True;
end;

procedure TMainForm.WriteIni;
var
  S: string;
begin
  if FIniFailed or (not GetIniFileName(S)) then
    Exit;
  try
    with TMemIniFile.Create(S, TEncoding.UTF8) do
      try
        WriteInteger('ClassView', 'CustomBarPos', FBarPos);
        WriteString('ClassView', 'Param', FParam);
        WriteBool('ClassView', 'AutoRefresh', FAutoRefresh);
        UpdateFile;
      finally
        Free;
      end;
  except
    FIniFailed := True;
  end;
end;

procedure TMainForm.ClassViewSelected(Node: TTreeNode; FocusView: Boolean);
var
  LClassView: Integer;
  LPos: TPoint;
begin
  if Node = nil then
    Exit;
  LClassView := Node.StateIndex;
  if not InRange(LClassView, 0, FList.Count - 1) then
    Exit;
  LPos.X := 0;
  LPos.Y := FList[LClassView].LineNum;
  if LPos.Y < 0 then
    Exit;
  Editor_Redraw(FEditor, False);
  try
    Editor_SetCaretPos(FEditor, POS_LOGICAL, @LPos);
    if FocusView then
      Editor_ExecCommand(FEditor, MEID_WINDOW_ACTIVE_PANE);
    Editor_GetCaretPos(FEditor, POS_VIEW, @LPos);
    Editor_SetScrollPos(FEditor, @LPos);
  finally
    Editor_Redraw(FEditor, True);
  end;
end;

procedure TMainForm.ResetThread;
begin
  if FWorkThread > 0 then
  begin
    FAbortThread := True;
    SetEvent(FQueEvent);
    SetThreadPriority(FWorkThread, THREAD_PRIORITY_ABOVE_NORMAL);
    WaitMessageLoop(1, FWorkThread, INFINITE);
    CloseHandle(FWorkThread);
    FWorkThread := 0;
    with TreeView.Items do
    begin
      BeginUpdate;
      try
        Clear;
      finally
        EndUpdate;
      end;
    end;
    FList.Clear;
    FAbortThread := False;
  end;
end;

procedure TMainForm.UpdateTreeView;
begin
  with TreeView do
    if PixelsPerInch >= 240 then
      Images := ExtraLargeImageList
    else if PixelsPerInch >= 192 then
      Images := LargeImageList
    else if PixelsPerInch >= 144 then
      Images := MediumImageList
    else
      Images := SmallImageList;
end;

procedure TMainForm.UpdateTreeViewAll;
var
  I, J: Integer;
  ParentItem: array [0 .. MaxDepth] of TTreeNode;
  LNode: TTreeNode;
  Item: TClassViewItem;
begin
  TreeView.Items.BeginUpdate;
  try
    for I := 0 to High(ParentItem) do
      ParentItem[I] := nil;
    for I := 0 to FList.Count - 1 do
    begin
      if FAbortThread then
        Exit;
      Item := FList[I];
      if Item.Node <> nil then
      begin
        LNode := TreeView.Items.GetNode(Item.Node.ItemId);
        if LNode <> nil then
          LNode.Text := Item.SymbolName;
      end
      else
      begin
        if ParentItem[Item.Level - 1] = nil then
          LNode := TreeView.Items.AddChild(nil, Item.SymbolName)
        else
          LNode := TreeView.Items.AddChild(ParentItem[Item.Level - 1], Item.SymbolName);
        with Item, LNode do
        begin
          ImageIndex := StrToIntDef(FSymbolList.Values[SymbolType], 10);
          SelectedIndex := ImageIndex;
        end;
        LNode.StateIndex := I;
        Item.Node := LNode;
      end;
      for J := Item.Level to MaxDepth do
        ParentItem[J] := Item.Node;
    end;
    TreeView.Selected := TreeView.Items.GetFirstNode;
  finally
    TreeView.Items.EndUpdate;
  end;
end;

function Sort(List: TStringList; Index1, Index2: Integer): Integer;
var
  L, R: string;
  P, Q: Integer;
begin
  L := List[Index1];
  R := List[Index2];
  with FSymbolList do
  begin
    P := IndexOfName(L);
    Q := IndexOfName(R);
  end;
  if P < 0 then
    P := MaxInt;
  if Q < 0 then
    Q := MaxInt;
  Result := P - Q;
  if Result = 0 then
    Result := CompareStr(List[Index1], List[Index2]);
end;

procedure TMainForm.ClassViewAll;
  function GetTempPath: string;
  var
    I: Integer;
  begin
    Result := '';
    SetLastError(ERROR_SUCCESS);
{$IF CompilerVersion > 22.9}
    I := Winapi.Windows.GetTempPath(0, nil);
{$ELSE}
    I := Windows.GetTempPath(0, nil);
{$IFEND}
    SetLength(Result, I - 1);
{$IF CompilerVersion > 22.9}
    if Winapi.Windows.GetTempPath(I, PWideChar(Result)) <> 0 then
{$ELSE}
    if Windows.GetTempPath(I, PWideChar(Result)) <> 0 then
{$IFEND}
    begin
      I := GetLongPathName(PChar(Result), nil, 0);
      SetLength(Result, I - 1);
      GetLongPathName(PChar(Result), PChar(Result), I);
    end;
  end;

  function GetTempFileName(const Prefix: string): string;
  var
    S: string;
    E: UINT;
  begin
    S := GetTempPath;
    SetLength(Result, MAX_PATH);
    SetLastError(ERROR_SUCCESS);
{$IF CompilerVersion > 22.9}
    E := Winapi.Windows.GetTempFileName(PChar(S), PChar(Prefix), 0, PChar(Result));
{$ELSE}
    E := Windows.GetTempFileName(PChar(S), PChar(Prefix), 0, PChar(Result));
{$IFEND}
    if E = 0 then
      raise EInOutError.Create(SysErrorMessage(GetLastError));
    SetLength(Result, StrLen(PChar(Result)));
  end;

  function ParseLine(const Str: string): TSymbolItem;
  var
    S: string;
    A: TStringDynArray;
  begin
    Result := nil;
    if Pos('!', Str) = 1 then
      Exit;
    Result := TSymbolItem.Create;
    with Result do
    begin
      ParentName := '';
      SymbolName := '';
      SymbolType := '';
      SymbolLine := 0;
    end;
    A := SplitString(Str, #09);
    if Length(A) > 3 then
    begin
      with Result do
      begin
        SymbolName := A[0];
        SymbolType := A[3];
        SymbolLine := StrToIntDef(Copy(A[2], 1, Length(A[2]) - 2), 0) - 1;
      end;
    end;
    if Length(A) > 4 then
    begin
      S := A[4];
      A := SplitString(S, ':');
      if Length(A) > 1 then
      begin
        Result.ParentName := A[1];
        Result.ParentType := A[0];
      end;
    end;
  end;

var
  S: string;
  I, J, Idx: Integer;
  Path: array [0 .. MAX_PATH] of Char;
  Temp, ClassViewPath, ClassViewParam: string;
  SI: TStartupInfo;
  PI: TProcessInformation;
  Item: TSymbolItem;
  List: TSymbolList;
  L, M: TStringList;
  LList: TClassViewList;
  UpdateAll: Boolean;
  FirstUpdate: Integer;
  Src, Dest: Integer;
  LNode: TTreeNode;
  LItem: TClassViewItem;
begin
  Editor_Info(FEditor, MI_GET_FILE_NAME, LPARAM(@Path));
  if Path = '' then
    Exit;
  Temp := GetTempFileName('tag');
  ClassViewPath := ExtractFilePath(ParamStr(0)) + 'Plugins\ctags.exe';
  if not FileExists2(ClassViewPath) then
    Exit;
  ClassViewParam := Format('-n --fields=fKs %s -f "%s" "%s"', [FParam, Temp, Path]);
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  CreateProcess(nil, PChar(Format('"%s" %s', [ClassViewPath, ClassViewParam])), nil, nil, True, CREATE_NO_WINDOW, nil, nil, SI, PI);
  WaitForSingleObject(PI.hProcess, INFINITE);
  CloseHandle(PI.hProcess);
  CloseHandle(PI.hThread);
  LList := TClassViewList.Create;
  try
    L := TStringList.Create;
    M := TStringList.Create;
    try
      L.LoadFromFile(Temp);
      DeleteFile(Temp);
      for I := 0 to L.Count - 1 do
      begin
        if FAbortThread then
          Break;
        Item := ParseLine(L[I]);
        if Item <> nil then
          with Item do
          begin
            if (SymbolType = 'class') or (SymbolType = 'struct') then
              S := SymbolName
            else
              S := IfThen(ParentName <> '', ParentName, SymbolType);
            Idx := M.IndexOf(S);
            if Idx < 0 then
              Idx := M.AddObject(S, TSymbolList.Create);
            List := TSymbolList(M.Objects[Idx]);
            if ParentType = '' then
              List.SymbolType := SymbolType
            else if List.SymbolType = '' then
              List.SymbolType := ParentType;
            if (SymbolType = 'class') or (SymbolType = 'struct') then
              List.SymbolLine := SymbolLine
            else
              List.Add(Item);
          end;
      end;
      M.CustomSort(Sort);
      for I := 0 to M.Count - 1 do
      begin
        if FAbortThread then
          Break;
        S := M[I];
        List := TSymbolList(M.Objects[I]);
        with List do
          LList.Add(TClassViewItem.Create(SymbolLine, 1, S, SymbolType));
        for J := 0 to List.Count - 1 do
        begin
          with List[J] do
            LList.Add(TClassViewItem.Create(SymbolLine, 2, SymbolName, SymbolType));
        end;
      end;
    finally
      for I := 0 to M.Count - 1 do
        M.Objects[I].Free;
      M.Free;
      L.Free;
    end;
    if FAbortThread then
      Exit;
    UpdateAll := False;
    FirstUpdate := FList.Count;
    if FList.Count = 0 then
    begin
      FirstUpdate := -1;
      UpdateAll := True;
      with TreeView.Items do
      begin
        BeginUpdate;
        try
          Clear;
        finally
          EndUpdate;
        end;
      end;
    end
    else
    begin
      for I := 0 to Min(FList.Count, LList.Count) - 1 do
      begin
        if FAbortThread then
          Exit;
        if FList[I].Level <> LList[I].Level then
        begin
          if not UpdateAll then
          begin
            FirstUpdate := I;
            UpdateAll := True;
          end;
        end;
        if UpdateAll then
        begin
          while True do
          begin
            if FList[I].Node = nil then
              Break;
            LNode := FList[I].Node.GetNext;
            if LNode = nil then
              Break;
            LNode.Delete;
          end;
          if FList[I].Node <> nil then
            FList[I].Node.Delete;
          Break;
        end;
      end;
    end;
    Src := 0;
    Dest := 0;
    for I := 0 to Min(FirstUpdate, LList.Count) - 1 do
    begin
      with FList[I] do
      begin
        LineNum := LList[I].LineNum;
        Level := LList[I].Level;
        SymbolName := LList[I].SymbolName;
        SymbolType := LList[I].SymbolType;
      end;
      Inc(Src);
      Inc(Dest);
    end;
    if InRange(Dest, 0, FList.Count - 1) then
    begin
      if not UpdateAll then
        UpdateAll := True;
      while True do
      begin
        if FList[Dest].Node = nil then
          Break;
        LNode := FList[Dest].Node.GetNext;
        if LNode = nil then
          Break;
        LNode.Delete;
      end;
      if FList[Dest].Node <> nil then
        FList[Dest].Node.Delete;
      for I := FList.Count - 1 downto Dest do
      begin
        LItem := FList[I];
        FList.Remove(LItem);
        LItem.Free;
      end;
    end;
    if InRange(Src, 0, LList.Count - 1) then
    begin
      for I := Src to LList.Count - 1 do
      begin
        if not UpdateAll then
          UpdateAll := True;
        with LList[I] do
        begin
          LItem := TClassViewItem.Create(LineNum, Level, SymbolName, SymbolType);
          LItem.Node := Node;
        end;
        FList.Add(LItem);
      end;
    end;
    if FAbortThread then
      Exit;
    if UpdateAll then
      UpdateTreeViewAll
    else
      UpdateTreeViewString;
  finally
    LList.Free;
  end;
end;

procedure TMainForm.SetFont;
var
  LName: array [0 .. 255] of Char;
  LSize: Integer;
  LFore, LBack: TColor;
begin
  Editor_Info(FEditor, MI_GET_FONT_NAME, LPARAM(@LName));
  LSize := Editor_Info(FEditor, MI_GET_FONT_SIZE, 0);
  LFore := TColor(Editor_Info(FEditor, MI_GET_TEXT_COLOR, COLOR_GENERAL));
  LBack := TColor(Editor_Info(FEditor, MI_GET_BACK_COLOR, COLOR_GENERAL));
  if Editor_Info(FEditor, MI_GET_INVERT_COLOR, 0) = 1 then
  begin
    LFore := GetInvertColor(LFore);
    LBack := GetInvertColor(LBack);
  end;
  with TreeView do
  begin
    with Font do
    begin
      Name := IfThen(FFontName <> '', FFontName, LName);
      Size := IfThen(FFontSize <> 0, FFontSize, LSize);
      Height := MulDiv(Height, Self.PixelsPerInch, 96);
      Color := LFore;
    end;
    Color := LBack;
  end;
end;

procedure TMainForm.UpdateTreeViewString;
var
  I: Integer;
  LNode: TTreeNode;
begin
  for I := 0 to FList.Count - 1 do
    if FList[I].Node <> nil then
    begin
      LNode := TreeView.Items.GetNode(FList[I].Node.ItemId);
      if LNode <> nil then
        LNode.Text := FList[I].SymbolName;
    end;
end;

procedure TMainForm.SetScale(const Value: Integer);
var
  P: Integer;
begin
  P := PixelsPerInch;
  PixelsPerInch := Value;
  with Font do
    Height := MulDiv(Height, Self.PixelsPerInch, P);
  SetFont;
  UpdateTreeView;
end;

function TMainForm.SetProperties: Boolean;
begin
  Result := False;
  if Prop(Self, FBarPos, FParam, FAutoRefresh) then
  begin
    WriteIni;
    FUpdateClassView := True;
    Result := True;
  end;
end;

initialization

FSymbolList := TStringList.Create;
with FSymbolList do
begin
  Values['namespace'] := '6';
  Values['typedef'] := '7';
  Values['procedure'] := '4';
  Values['function'] := '4';
  Values['method'] := '4';
  Values['const'] := '1';
  Values['variable'] := '3';
  Values['field'] := '3';
  Values['enumerator'] := '2';
  Values['macro'] := '7';
  Values['member'] := '3';
  Values['package'] := '5';
  Values['class'] := '0';
  Values['struct'] := '8';
  Values['label'] := '1';
end;

finalization

if Assigned(FSymbolList) then
  FreeAndNil(FSymbolList);

end.
