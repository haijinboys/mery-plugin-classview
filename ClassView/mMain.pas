// -----------------------------------------------------------------------------
// クラスビュー
//
// Copyright (c) Kuro. All Rights Reserved.
// e-mail: info@haijin-boys.com
// www:    http://www.haijin-boys.com/
// -----------------------------------------------------------------------------

unit mMain;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Menus, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Winapi.CommCtrl,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ComCtrls, ExtCtrls, CommCtrl,
{$IFEND}
  Native, StringBuffer, ImgList;

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
    FSymbolLine: NativeInt;
  public
    { Public 宣言 }
    constructor Create;
    property ParentName: string read FParentName write FParentName;
    property ParentType: string read FParentType write FParentType;
    property SymbolName: string read FSymbolName write FSymbolName;
    property SymbolType: string read FSymbolType write FSymbolType;
    property SymbolLine: NativeInt read FSymbolLine write FSymbolLine;
  end;

  TSymbolList = class(TList)
  private
    { Private 宣言 }
    FSymbolType: string;
    FSymbolLine: NativeInt;
    function Get(Index: Integer): TSymbolItem; inline;
  public
    { Public 宣言 }
    constructor Create;
    destructor Destroy; override;
    procedure Clear; override;
    property Items[Index: Integer]: TSymbolItem read Get; default;
    property SymbolType: string read FSymbolType write FSymbolType;
    property SymbolLine: NativeInt read FSymbolLine write FSymbolLine;
  end;

  TClassViewItem = class
  private
    { Private 宣言 }
    FNode: TTreeNode;
    FLineNum: NativeInt;
    FLevel: NativeInt;
    FSymbolName: string;
    FSymbolType: string;
  public
    { Public 宣言 }
    constructor Create(ALineNum, ALevel: NativeInt; ALineStr, AStr: string);
    property Node: TTreeNode read FNode write FNode;
    property LineNum: NativeInt read FLineNum write FLineNum;
    property Level: NativeInt read FLevel write FLevel;
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

  TMainForm = class(TForm)
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
    ImageList: TImageList;
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
    FBarPos: NativeInt;
    FFontNameSub: string;
    FFontSizeSub: NativeInt;
    FParam: string;
    FAutoRefresh: Boolean;
    FStringBuffer: TStringBuffer;
    FList: TClassViewList;
    FUpdateClassView: Boolean;
    FWorkFlag: NativeInt;
    FWorkThread: THandle;
    FAbortThread: Boolean;
    FQueEvent: THandle;
    FMutex: THandle;
    FPoint: TNativePoint;
    procedure ReadIni;
    procedure WriteIni;
    procedure UpdateTreeViewAll;
    procedure UpdateTreeViewString;
    procedure ClassViewSelected(Node: TTreeNode; FocusView: Boolean);
  public
    { Public 宣言 }
    procedure ResetThread;
    procedure ClassViewAll;
    procedure SetTreeColor;
    function SetProperties: Boolean;
    property BarPos: NativeInt read FBarPos write FBarPos;
    property UpdateClassView: Boolean read FUpdateClassView write FUpdateClassView;
    property Editor: THandle read FEditor write FEditor;
    property WorkHandle: THandle read FWorkThread write FWorkThread;
    property WorkFlag: NativeInt read FWorkFlag write FWorkFlag;
    property AbortThread: Boolean read FAbortThread write FAbortThread;
    property QueEvent: THandle read FQueEvent write FQueEvent;
    property Mutex: THandle read FMutex write FMutex;
    property AutoRefresh: Boolean read FAutoRefresh;
  end;

var
  MainForm: TMainForm;
  FFontName: string;
  FFontSize: NativeInt;
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
  Milliseconds: DWORD): NativeInt;
var
  Quit: Boolean;
  ExitCode: NativeInt;
  WaitResult: DWORD;
  Msg: TMsg;
begin
  Quit := False;
  ExitCode := 0;
  WaitResult := 0;
  repeat
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      if Msg.message = WM_QUIT then
      begin
        Quit := True;
        ExitCode := NativeInt(Msg.wParam);
        Break;
      end
      else
        DispatchMessage(Msg);
      WaitResult := MsgWaitForMultipleObjects(Count, Handles, False, Milliseconds, QS_ALLINPUT);
    end;
  until WaitResult = WAIT_OBJECT_0;
  if Quit then
    PostQuitMessage(ExitCode);
  Result := NativeInt(WaitResult - WAIT_OBJECT_0);
end;

function TrimLine(Line: PChar): string;
var
  P: PChar;
  Len: NativeInt;
begin
  P := Line;
  while (P^ = ' ') or (P^ = #09) do
    Inc(P);
  if P <> Line then
  begin
    Len := StrLen(P);
    StrMove(Line, P, Len + 1);
  end;
  P := Line;
  while True do
  begin
    P := StrScan(P, #09);
    if P = nil then
      Break;
    P^ := ' ';
    Inc(P);
  end;
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
  I: NativeInt;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited;
end;

function TSymbolList.Get(Index: Integer): TSymbolItem;
begin
  Result := TSymbolItem( inherited Get(Index));
end;

{ TClassViewItem }

constructor TClassViewItem.Create(ALineNum, ALevel: NativeInt; ALineStr,
  AStr: string);
begin
  FNode := nil;
  FLineNum := ALineNum;
  FLevel := ALevel;
  FSymbolName := ALineStr;
  FSymbolType := AStr;
  if Length(FSymbolName) > 0 then
    TrimLine(@FSymbolName[1]);
  if Length(FSymbolType) > 0 then
    TrimLine(@FSymbolType[1]);
end;

{ TClassViewList }

destructor TClassViewList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TClassViewList.Clear;
var
  I: NativeInt;
begin
  for I := 0 to Count - 1 do
    Items[I].Free;
  inherited;
end;

function TClassViewList.Get(Index: Integer): TClassViewItem;
begin
  Result := TClassViewItem( inherited Get(Index));
end;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  if Win32MajorVersion < 6 then
    with Font do
    begin
      Name := 'Tahoma';
      Size := 8;
    end;
  FEditor := ParentWindow;
  FFontNameSub := '';
  FFontSizeSub := 0;
  FParam := '';
  FAutoRefresh := True;
  FStringBuffer := TStringBuffer.Create(0);
  FList := TClassViewList.Create;
  FUpdateClassView := False;
  FWorkFlag := 0;
  FQueEvent := CreateEvent(nil, True, False, nil);
  FMutex := CreateMutex(nil, False, nil);
  FPoint.X := -1;
  FPoint.Y := -1;
  ReadIni;
  with Font do
  begin
    ChangeScale(FFontSize, Size);
    Name := FFontName;
    Size := FFontSize;
  end;
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
  if Assigned(FStringBuffer) then
    FreeAndNil(FStringBuffer);
  if Assigned(FList) then
    FreeAndNil(FList);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  //
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

procedure TMainForm.GoMenuItemClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := TreeView.GetNodeAt(FPoint.X, FPoint.Y);
  if ANode = TreeView.Selected then
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
  ANode: TTreeNode;
begin
  ANode := TreeView.GetNodeAt(FPoint.X, FPoint.Y);
  if ANode = TreeView.Selected then
    ClassViewSelected(TreeView.Selected, False);
end;

procedure TMainForm.TreeViewDblClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := TreeView.GetNodeAt(FPoint.X, FPoint.Y);
  if ANode = TreeView.Selected then
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
      FFontName := ReadString('MainForm', 'FontName', Font.Name);
      FFontSize := ReadInteger('MainForm', 'FontSize', Font.Size);
      FFontNameSub := ReadString('ClassView', 'FontName', FFontNameSub);
      FFontSizeSub := ReadInteger('ClassView', 'FontSize', FFontSizeSub);
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
  AClassView: NativeInt;
  APos: TPoint;
begin
  if Node = nil then
    Exit;
  AClassView := Node.StateIndex;
  if not InRange(AClassView, 0, FList.Count - 1) then
    Exit;
  APos.X := 0;
  APos.Y := FList[AClassView].LineNum;
  if APos.Y < 0 then
    Exit;
  Editor_Redraw(FEditor, False);
  try
    Editor_SetCaretPos(FEditor, POS_LOGICAL, @APos);
    if FocusView then
      Editor_ExecCommand(FEditor, MEID_WINDOW_ACTIVE_PANE);
    Editor_GetCaretPos(FEditor, POS_VIEW, @APos);
    Editor_SetScrollPos(FEditor, @APos);
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

procedure TMainForm.UpdateTreeViewAll;
var
  I, J: NativeInt;
  ParentItem: array [0 .. MaxDepth] of TTreeNode;
  ANode: TTreeNode;
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
        ANode := TreeView.Items.GetNode(Item.Node.ItemId);
        if ANode <> nil then
          ANode.Text := Item.SymbolName;
      end
      else
      begin
        if ParentItem[Item.Level - 1] = nil then
          ANode := TreeView.Items.AddChild(nil, Item.SymbolName)
        else
          ANode := TreeView.Items.AddChild(ParentItem[Item.Level - 1], Item.SymbolName);
        with Item, ANode do
        begin
          ImageIndex := StrToIntDef(FSymbolList.Values[SymbolType], 10);
          SelectedIndex := ImageIndex;
        end;
        ANode.StateIndex := I;
        Item.Node := ANode;
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
  P, Q: NativeInt;
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
    I: NativeInt;
  begin
    Result := '';
    SetLastError(ERROR_SUCCESS);
    I := Windows.GetTempPath(0, nil);
    SetLength(Result, I - 1);
    if Windows.GetTempPath(I, PWideChar(Result)) <> 0 then
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
    E := Windows.GetTempFileName(PChar(S), PChar(Prefix), 0, PChar(Result));
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
  S, T, U: string;
  I, P, Idx, Len: NativeInt;
  Path: array [0 .. MAX_PATH] of Char;
  Temp, ClassViewPath, ClassViewParam: string;
  SI: TStartupInfo;
  PI: TProcessInformation;
  Item: TSymbolItem;
  List: TSymbolList;
  A, B: TStringDynArray;
  L, M: TStringList;
  O: TObject;
  AList: TClassViewList;
  UpdateAll: Boolean;
  FirstUpdate: NativeInt;
  Src, Dest: NativeInt;
  ANode: TTreeNode;
  AItem: TClassViewItem;
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
  AList := TClassViewList.Create;
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
          AList.Add(TClassViewItem.Create(SymbolLine, 1, S, SymbolType));
        for P := 0 to List.Count - 1 do
        begin
          with List[P] do
            AList.Add(TClassViewItem.Create(SymbolLine, 2, SymbolName, SymbolType));
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
      for I := 0 to Min(FList.Count, AList.Count) - 1 do
      begin
        if FAbortThread then
          Exit;
        if FList[I].Level <> AList[I].Level then
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
            ANode := FList[I].Node.GetNext;
            if ANode = nil then
              Break;
            ANode.Delete;
          end;
          if FList[I].Node <> nil then
            FList[I].Node.Delete;
          Break;
        end;
      end;
    end;
    Src := 0;
    Dest := 0;
    for I := 0 to Min(FirstUpdate, AList.Count) - 1 do
    begin
      with FList[I] do
      begin
        LineNum := AList[I].LineNum;
        Level := AList[I].Level;
        SymbolName := AList[I].SymbolName;
        SymbolType := AList[I].SymbolType;
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
        ANode := FList[Dest].Node.GetNext;
        if ANode = nil then
          Break;
        ANode.Delete;
      end;
      if FList[Dest].Node <> nil then
        FList[Dest].Node.Delete;
      for I := FList.Count - 1 downto Dest do
      begin
        AItem := FList[I];
        FList.Remove(AItem);
        AItem.Free;
      end;
    end;
    if InRange(Src, 0, AList.Count - 1) then
    begin
      for I := Src to AList.Count - 1 do
      begin
        if not UpdateAll then
          UpdateAll := True;
        with AList[I] do
        begin
          AItem := TClassViewItem.Create(LineNum, Level, SymbolName, SymbolType);
          AItem.Node := Node;
        end;
        FList.Add(AItem);
      end;
    end;
    if FAbortThread then
      Exit;
    if UpdateAll then
      UpdateTreeViewAll
    else
      UpdateTreeViewString;
  finally
    AList.Free;
  end;
end;

procedure TMainForm.SetTreeColor;
var
  AName: array [0 .. 255] of Char;
  ASize: NativeInt;
  AFore, ABack: TColor;
begin
  Editor_Info(FEditor, MI_GET_FONT_NAME, LPARAM(@AName));
  ASize := Editor_Info(FEditor, MI_GET_FONT_SIZE, 0);
  AFore := TColor(Editor_Info(FEditor, MI_GET_TEXT_COLOR, COLOR_GENERAL));
  ABack := TColor(Editor_Info(FEditor, MI_GET_BACK_COLOR, COLOR_GENERAL));
  with TreeView do
  begin
    with Font do
    begin
      Name := IfThen(FFontNameSub <> '', FFontNameSub, AName);
      Size := IfThen(FFontSizeSub <> 0, FFontSizeSub, ASize);
      Color := AFore;
    end;
    Color := ABack;
  end;
end;

procedure TMainForm.UpdateTreeViewString;
var
  I: NativeInt;
  ANode: TTreeNode;
begin
  for I := 0 to FList.Count - 1 do
    if FList[I].Node <> nil then
    begin
      ANode := TreeView.Items.GetNode(FList[I].Node.ItemId);
      if ANode <> nil then
        ANode.Text := FList[I].SymbolName;
    end;
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
  Values['namespace'] := '15';
  Values['typedef'] := '20';
  Values['procedure'] := '13';
  Values['function'] := '12';
  Values['method'] := '13';
  Values['const'] := '1';
  Values['variable'] := '7';
  Values['field'] := '7';
  Values['enumerator'] := '4';
  Values['macro'] := '9';
  Values['member'] := '7';
  Values['package'] := '14';
  Values['class'] := '0';
  Values['struct'] := '18';
  Values['cursor'] := '10';
  Values['label'] := '1';
end;

finalization

if Assigned(FSymbolList) then
  FreeAndNil(FSymbolList);

end.
