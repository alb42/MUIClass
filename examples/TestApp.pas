program TestApp;
{$mode objfpc}{$H+}

uses
  SysUtils,
  Exec, Amigados, mui, muihelper, utility, intuition,
  MUIClass.Group, MUIClass.Area, MUIClass.Base,
  MUIClass.Menu, MUIClass.Window, MUIClass.Gadget, MUIClass.List,
  MUIClass.Numeric;


type
  TMyWindow = class(TMUIWindow)
    Count: Integer;
    Txt: TMUIText;
    MITest: TMUIMenuItem;
    NBtn: TMUIButton;
    Bubble: PBubble;
    Edit: TMUIString;
    Prop: TMUIScrollbar;
    Gauge: TMUIGauge;
    MyList: TMUIList;
    DirList: TMUIDirList;
    // Events
    procedure ShowEvent(Sender: TObject);
    procedure Btn1Click(Sender: TObject);
    procedure Btn2Click(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure AboutBtnClick(Sender: TObject);
    procedure MenuSelected(Sender: TObject);
    procedure ConfigStart(Sender: TObject);
    procedure QuitMe(Sender: TObject);
    procedure WinActive(Sender: TObject);
    procedure DynWindowsClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure EditAck(Sender: TObject);
    procedure PropChanged(Sender: TObject);
    procedure NumChanged(Sender: TObject);
    procedure ColChanged(Sender: TObject);

    function ConstructEvent(Sender: TObject; Pool: Pointer; Str: PChar): PChar;
    procedure DestructEvent(Sender: TObject; Pool: Pointer; Entry: PChar);
    procedure DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    function CompareEvent(Sender: TObject; Entry1, Entry2: PChar): Integer;
    function MultiTestEvent(Sender: TObject; Entry: PChar): Boolean;
    procedure ListClickEvent(Sender: TObject);
    procedure FinishedReading(Sender: TObject);
    // MUIApp Events
    procedure AppActivate(Sender: TObject);
    procedure AppDeactivate(Sender: TObject);
    procedure AppIconify(Sender: TObject);
    procedure AppRestore(Sender: TObject);
  public
    constructor Create; override;
  end;


constructor TMyWindow.Create;
var
  // not neeeded later
  Men: TMUIMenu;
  Pnl: TMUIGroup;
  MM: TMUIMenuStrip;
begin
  inherited;
  OnShow := @ShowEvent;
  Bubble := nil;
  Count := 0;

  //SizeGadget := False; // Disable SizeGadget

  Title := 'Main Window';
  ScreenTitle := 'My new supercool Window';
  OnActivate := @WinActive;


  MUIApp.OnActivate := @AppActivate;
  MUIApp.OnDeactivate := @AppDeactivate;
  MUIApp.OnIconify := @AppIconify;
  MUIApp.OnRestore := @AppRestore;


  Horizontal := False;

  Pnl := TMUIGroup.Create;
  Pnl.Horiz := True;
  Pnl.Parent := Self;

  with TMUIButton.Create do
  begin
    Contents := 'Test1';
    OnClick := @Btn1Click;
    ShortHelp := 'This is a Button. ;-)';
    Parent := Pnl;
  end;

  with TMUIButton.Create do
  begin
    Contents := 'Test2';
    OnClick := @Btn2Click;
    ShortHelp := 'And another Button. :-O';
    Parent := Pnl;
  end;

  with TMUINumericButton.Create do
  begin
    OnChange := @NumChanged;
    Parent := Pnl;
  end;

  with TMUIPopPen.Create do
  begin
    OnChange := @ColChanged;
    Parent := Pnl;
  end;

  with TMUIBalance.Create do
  begin
    FixWidth := 8;
    Parent := Pnl;
  end;

  NBtn := TMUIButton.Create;
  with NBtn do
  begin
    Contents := 'New Window';
    OnClick := @NewBtnClick;
    ShortHelp := 'This is ... you know.. :-S';
    Parent := Pnl;
  end;

  with TMUIButton.Create do
  begin
    Contents := 'About';
    ShortHelp := 'It becomes boring ... :-|';
    OnClick := @AboutBtnClick;
    Parent := Pnl;
  end;

  with TMUIRectangle.Create do
  begin
    FixHeight := 8;
    HBar := True;
    Parent := Self;
  end;

  Txt := TMUIText.Create;
  With Txt do
  begin
    Contents := 'No Button was clicked until now';
    HiChar := 'C';
    SetVMax := True;
    Font := MUIV_Font_Fixed;
    Parent := Self;
  end;

  Edit := TMUIString.Create;
  with Edit do
  begin
    //Secret := True;
    OnAcknowledge :=  @EditAck;
    Format := MUIV_String_Format_Center;
    Contents := 'Edit me and press enter';
    Parent := Self;
  end;

  Prop := TMUIScrollbar.Create;
  With Prop do
  begin
    FixHeight := 10;
    Horiz := True;

    Visible := 10;
    Entries := 110;
    First := 50;
    OnChange := @PropChanged;
    Parent:= Self;
  end;

  Gauge := TMUIGauge.Create;
  with Gauge do
  begin
    FixHeight := 10;
    Horiz := True;
    Max := 20;
    Current := 4;
    Parent := Self;
  end;
  with TMUIScale.Create do
  begin
    Horiz := True;
    Parent := Self;
  end;

  MM := TMUIMenuStrip.Create;

  // MUIApp.MenuStrip := MM; // alternative on the application, activate only one!
  Menustrip := MM;

  Men := TMUIMenu.Create;
  Men.Title := 'Test Menu';
  Men.Parent := MM;

  MITest := TMUIMenuItem.Create;
  MITest.Title := 'Test MenuItem';
  MITest.Parent := Men;
  MITest.OnTrigger := @MenuSelected;

  With TMUIMenuItem.Create do
  begin
    Title := 'Config';
    Parent := Men;
    OnTrigger := @ConfigStart;
  end;

  with TMUIMenuItem.Create do
  begin
    Title := 'Quit';
    Parent := Men;
    OnTrigger := @QuitMe;
  end;

  MyList := TMUIList.Create;
  with MyList do
  begin
    SourceStrings := ['one', 'two', 'three', 'four', 'five', 'six', 'seven'];
    OnConstruct := @ConstructEvent;
    OnDestruct := @DestructEvent;
    OnDisplay := @DisplayEvent;
    OnCompare := @CompareEvent;
    OnMultiTest := @MultiTestEvent;
    //OnChange := @ListClickEvent;
    //Parent := Self;
  end;

  with TMUIListView.Create do
  begin
    List := MyList;
    Parent := Self;
    MultiSelect := MUIV_Listview_MultiSelect_Default;
    OnDoubleClick := @ListClickEvent;
  end;

  DirList := TMUIDirList.create;
  with DirList do
  begin
    Directory := 'Sys:';
  end;

  with TMUIListView.Create do
  begin
    List := DirList;
    Parent := Self;
    MultiSelect := MUIV_Listview_MultiSelect_Default;
    OnDoubleClick := @ListClickEvent;
  end;
end;

procedure TMyWindow.ShowEvent(Sender: TObject);
begin
  writeln('Show');
end;

procedure TMyWindow.Btn1Click(Sender: TObject);
begin
  Txt.Contents := 'We Clicked to 1';
  writeln('My BaseName:' + MUIApp.Base);
  writeln('Currently active: ', HexStr(ActiveObject));
  ActiveObject := NBtn;
  NoMenus := not NoMenus; // Toggle menus
  //ToBack; // Put to back
  if Assigned(NBtn.WindowObject) then
    writeln('Window title: ', NBtn.WindowObject.Title);
  if Assigned(Bubble) then
    Txt.DeleteBubble(Bubble);
  //Bubble := Txt.CreateBubble(NBtn.RightEdge, NBtn.BottomEdge, 'Here we are', 0);
  //Prop.Decrease(2);
  //DirList.Sort;
  //DirList.Directory := 'RAM:';
end;

procedure TMyWindow.Btn2Click(Sender: TObject);
begin
  if Txt.PreParse = '' then
    Txt.PreParse := #27'i'
  else
    Txt.PreParse := '';
  Txt.Contents := '2 was Clicked';
  if Assigned(Bubble) then
    Txt.DeleteBubble(Bubble);
  Bubble := nil;
  //Prop.Increase(2);
  //MyList.Sort;
  //DirList.Directory := 'SYS:';
end;

procedure TMyWindow.NewBtnClick(Sender: TObject);
var
  w2: TMUIWindow;
begin
  Inc(Count);
  W2 := TMUIWindow.Create;
  with W2 do
  begin
    LeftEdge := Random(300);
    TopEdge := Random(300);
    OnCloseRequest := @DynWindowsClose;
    Title := 'Win ' + IntToStr(Count);
  end;
  With TMUIButton.Create do
  begin
    Contents := 'This is Window number ' + IntToStr(Count);
    Parent := W2;
  end;
  W2.Show;
end;


procedure TMyWindow.AboutBtnClick(Sender: TObject);
begin
  MUIApp.AboutMUI(Self);
  //MUIApp.OpenConfigWindow;
end;

procedure TMyWindow.ConfigStart(Sender: TObject);
begin
  MUIApp.OpenConfigWindow;
end;

procedure TMyWindow.QuitMe(Sender: TObject);
begin
  MUIApp.Terminate;
end;

procedure TMyWindow.MenuSelected(Sender: TObject);
begin
   Txt.Contents := 'Menu Selected';
end;


procedure TMyWindow.AppActivate(Sender: TObject);
begin
  writeln('App activated');
end;

procedure TMyWindow.AppDeactivate(Sender: TObject);
begin
  writeln('App deactivated');
end;

procedure TMyWindow.AppIconify(Sender: TObject);
begin
  writeln('App iconified');
end;

procedure TMyWindow.AppRestore(Sender: TObject);
begin
  writeln('App restored');
end;

procedure TMyWindow.WinActive(Sender: TObject);
begin
  writeln('Window activated');
end;

procedure TMyWindow.DynWindowsClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  if (Sender is TMUIWindow) then
    writeln('got close request from: "'+ TMUIWindow(Sender).Title + '"');
end;

procedure TMyWindow.EditAck(Sender: TObject);
begin
  writeln('Enter pressed in Edit: "' + Edit.Contents + '"')
end;

procedure TMyWindow.PropChanged(Sender: TObject);
begin
  if Sender is TMUIProp then
  begin
    writeln('Prop changed to ', IntToStr(TMUIProp(Sender).First));
    Gauge.Current := TMUIProp(Sender).First div 4;
  end;
end;

function TMyWindow.ConstructEvent(Sender: TObject; Pool: Pointer; Str: PChar): PChar;
begin
  if Str = 'five' then
    Str := 'Hello here i am';
  Result := AllocPooled(Pool, StrLen(Str) + 1);
  if Assigned(Result) then
    StrCopy(Result, Str);
end;

procedure TMyWindow.DestructEvent(Sender: TObject; Pool: Pointer; Entry: PChar);
begin
  FreePooled(Pool, Entry, StrLen(Entry) + 1);
end;

procedure TMyWindow.DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
begin
  // how to anoy a user... replace an entry only for display with
  if Entry = 'two' then
    ToPrint[0] := 'belongs together!'
  else
    ToPrint[0] := Entry;
end;

function TMyWindow.CompareEvent(Sender: TObject; Entry1, Entry2: PChar): Integer;
begin
  // lets sort by length ;-) instead ASCII sort (as MUI would do)
  Result := Length(Entry2) - Length(Entry1);
end;

function TMyWindow.MultiTestEvent(Sender: TObject; Entry: PChar): Boolean;
begin
  Result := True;
  writeln('Multitest ', Entry);
  if Entry = 'one' then
    Result := False;
end;

procedure TMyWindow.ListClickEvent(Sender: TObject);
begin
  writeln('List Clicked ' + IntToStr(TMUIListView(Sender).List.Active));
end;

procedure TMyWindow.FinishedReading(Sender: TObject);
begin
  writeln('event');
  if Sender is TMUIDirList then
  begin
    writeln('go sort');
    TMUIDirList(Sender).Sort;
  end;
end;

procedure TMyWindow.NumChanged(Sender: TObject);
begin
  if Sender is TMUINumeric then
  begin
    writeln('Numeric Changed to ', TMUINumeric(Sender).Value);
    Gauge.Current := TMUINumeric(Sender).Value;
  end;
end;

procedure TMyWindow.ColChanged(Sender: TObject);
begin
  if Sender is TMUIPenDisplay then
  begin
    writeln('Pen changed to "', TMUIPenDisplay(Sender).Spec^.ps_Buf,'"');
  end;
end;

procedure Startup;
begin
  MUIApp.Base := 'TESTAPP';
  MUIApp.Title := 'MyTestApp';
  MUIApp.Version := '$VER: MyTestApp 0.1 (19.01.2017)';
  MUIApp.Author := 'Marcus "ALB" Sackrow';
  MUIApp.Copyright := 'by me';
  MUIApp.Description := 'Test app for MUIClass';

  TMyWindow.Create;


  MUIApp.Run;
end;



begin
  Startup;
end.
