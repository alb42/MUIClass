unit MainWinUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, StrUtils, fgl, Classes, Typinfo,
  MUI, muihelper,
  MUIClass.Base, MUIClass.Window, MUIClass.Area, MUIClass.List, MUIClass.Group,
  NodeTreeUnit, MUICompUnit, MUIClass.Gadget, MUIClass.Dialog, MUIClass.Image;

type
  TItemProp = class
  private
    FActive: Boolean;
    procedure SetActive(AValue: Boolean);
  public
    Name: string;
    Value: string;
    DisplayName: string;
    DisplayValue: string;
    Additional: string;
    IsSpecial: Boolean;
    property Active: boolean read FActive write SetActive;
  end;

  TItemProps = specialize TFPGObjectList<TItemProp>;

  TEventHandler = class
    Name: string;
    Obj: TItemNode;
    Event: string;
    Header: string;
    Text: string;
  end;

  TEventHandlers = specialize TFPGObjectList<TEventHandler>;

  TMainWindow = class(TMUIWindow)
  public
    Tree: TItemTree;
    ItemList, PropList, EventList: TMUIListView;
    CurItem: TItemNode;
    CurProp,CurEvent: TItemProp;
    ItemProps, EventProps: TItemProps;
    EventHandlers: TEventHandlers;
    TestWin: TMUIWindow;
    ChooseComp: TMUICycle;
    EditPages: TMUIGroup;
    BoolLabel, IntLabel, StringLabel, StrArrayLabel, ItemName, EventLabel: TMUIText;
    IntSet, StrSet, EventName: TMUIString;
    BoolSet: TMUICycle;
    RemBtn, AutoEvent, EditEvent: TMUIButton;
    IncludeProp: TMUICheckMark;
    PropPages: TMUIRegister;
    BlockEvents: Boolean;
    // event handler
    procedure AddClick(Sender: TObject);
    procedure RemoveClick(Sender: TObject);
    procedure ShowEvent(Sender: TObject);
    procedure ItemListSelect(Sender: TObject);
    procedure PropListSelect(Sender: TObject);
    procedure PropDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    procedure EventListSelect(Sender: TObject);
    procedure EventListDblClick(Sender: TObject);
    procedure EventDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    procedure OpenStrArrayWin(Sender: TObject);
    procedure SaveClick(Sender: TObject);
    procedure IncludeChange(Sender: TObject);
    procedure AutoEventClick(Sender: TObject);
    procedure EditEventClick(Sender: TObject);
    // Update Properties of CurItem
    procedure UpdateProperties;
    procedure UpdateItemList;
    procedure CreateSource;
    function FindEventHandler(Obj: TItemNode; Event: string): TEventHandler;
  public
    // TestWin stuff
    procedure CreateTestWin;
    procedure DestroyTestWin;
    //
    procedure SetIntProp(Sender: TObject);
    procedure SetBoolProp(Sender: TObject);
    procedure SetStringProp(Sender: TObject);
    procedure SetEvent(Sender: TObject);
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

var
  MainWindow: TMainWindow;

implementation

uses
  StrArraySetUnit;

// Create Main Window
constructor TMainWindow.Create;
var
  Grp, Grp2: TMUIGroup;
  StrCycle: TStringArray;
  i: Integer;
begin
  inherited;
  // Window props
  LeftEdge := 0;
  Height := MUIV_Window_Height_Visible(80);
  Width := MUIV_Window_Width_Visible(30);
  OnShow := @ShowEvent;

  // Top Bar ()
  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self;
  end;

  SetLength(StrCycle, Length(MUIComponents));
  for i := 0 to High(MUIComponents) do
    StrCycle[i] := '    ' + MUIComponents[i].Name + '    ';

  ChooseComp := TMUICycle.Create;
  with ChooseComp do
  begin
    Entries := StrCycle;
    Parent := Grp;
  end;

  with TMUIButton.Create('Add') do
  begin
    OnClick := @AddClick;
    Parent := Grp;
  end;

  RemBtn := TMUIButton.Create('Remove');
  with RemBtn do
  begin
    OnClick := @RemoveClick;
    Parent := Grp;
  end;
  EventHandlers := TEventHandlers.Create(True);
  ItemProps := TItemProps.Create(True);
  EventProps := TItemProps.Create(True);
  // List of Items
  ItemList := TMUIListView.Create;
  with ItemList do
  begin
    List := TMUIList.Create;
    List.Font := MUIV_Font_Fixed;
    List.OnActiveChange := @ItemListSelect;
    Parent := Self;
  end;

  // *******************************
  // List of Properties of the selected item

  ItemName := TMUIText.Create;
  ItemName.Parent := Self;

  PropPages := TMUIRegister.Create;
  with PropPages do
  begin
    Titles := ['Properties', 'Events'];
    Parent := Self;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Frame := MUIV_Frame_None;
    Parent := PropPages;
  end;

  PropList := TMUIListView.Create;
  with PropList do
  begin
    List := TMUIList.Create;
    List.Font := MUIV_Font_Fixed;
    List.Format := 'BAR,P='#27'l' ;
    List.Title := #1;
    List.OnDisplay := @PropDisplay;
    List.OnActiveChange := @PropListSelect;
    Parent := Grp;
  end;

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_Frame_Group;
    FrameTitle := 'Set Property';
    Parent := Grp;
  end;
  //############ Property Pages
  EditPages := TMUIGroup.Create;
  with EditPages do
  begin
    Frame := MUIV_Frame_None;
    PageMode := True;
    Parent := Grp2;
  end;

  // Empty Group for no properties to edit
  Grp := TMUIGroup.Create;
  Grp.Frame := MUIV_Frame_None;
  Grp.Parent := EditPages;
  With TMUIRectangle.Create do
    Parent := Grp;

  // Boolean Group for boolean properties
  Grp := TMUIGroup.Create;
  Grp.Frame := MUIV_Frame_None;
  Grp.Parent := EditPages;
  Grp.Horiz := True;
  BoolLabel := TMUIText.Create;
  with BoolLabel do
  begin
    Frame := MUIV_Frame_None;
    Contents := 'Test            ';
    Parent := Grp;
  end;
  BoolSet := TMUICycle.Create;
  with BoolSet do
  begin
    Entries := ['False', 'True'];
    OnActiveChange := @SetBoolProp;
    Parent := Grp;
  end;

  // Integer Group for Integers to edit (might be extended for Special Values)
  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Frame := MUIV_Frame_None;
  Grp.Parent := EditPages;
  IntLabel := TMUIText.Create;
  with IntLabel do
  begin
    Frame := MUIV_Frame_None;
    Contents := 'Test            ';
    Parent := Grp;
  end;
  IntSet := TMUIString.Create;
  with IntSet do
  begin
    Accept := '1234567890-';
    OnAcknowledge := @SetIntProp;
    Parent := Grp;
  end;

  // String Group to edit String properties
  Grp := TMUIGroup.Create;
  Grp.Frame := MUIV_Frame_None;
  Grp.Horiz := True;
  Grp.Parent := EditPages;
  StringLabel := TMUIText.Create;
  with StringLabel do
  begin
    Frame := MUIV_Frame_None;
    Contents := 'Test            ';
    Parent := Grp;
  end;
  StrSet := TMUIString.Create;
  with StrSet do
  begin
    OnAcknowledge := @SetStringProp;
    Parent := Grp;
  end;

  // String Array Group to edit String Array properties
  Grp := TMUIGroup.Create;
  Grp.Frame := MUIV_Frame_None;
  Grp.Horiz := True;
  Grp.Parent := EditPages;
  StrArrayLabel := TMUIText.Create;
  with StrArrayLabel do
  begin
    Contents := 'Test            ';
    Parent := Grp;
  end;
  with TMUIButton.Create('Edit') do
  begin
    OnClick := @OpenStrArrayWin;
    Parent := Grp;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Frame := MUIV_Frame_None;
    Horiz := True;
    Parent := Grp2;
  end;

  IncludeProp := TMUICheckmark.Create;
  with IncludeProp do
  begin
    Disabled := True;
    OnSelected := @IncludeChange;
    Parent := Grp;
  end;

  with TMUIText.Create('Include in Source') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;
  //############ End Property Pages


  // ########### Start Event Stuff
  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Frame := MUIV_Frame_None;
    Parent := PropPages;
  end;

  EventList := TMUIListView.Create;
  with EventList do
  begin
    List := TMUIList.Create;
    List.Font := MUIV_Font_Fixed;
    List.Format := 'BAR,P='#27'l' ;
    List.Title := #1;
    List.OnDisplay := @EventDisplay;
    List.OnActiveChange := @EventListSelect;
    OnDoubleClick := @EventListDblClick;
    Parent := Grp;
  end;

  Grp2 := TMUIGroup.Create;
  with Grp2 do
  begin
    Frame := MUIV_Frame_None;
    Horiz := True;
    Parent := Grp;
  end;
  EventLabel := TMUIText.Create;
  EventLabel.Parent := Grp2;
  EventName := TMUIString.Create;
  with EventName do
  begin
    OnAcknowledge := @SetEvent;
    Reject := ' ,.-+*!"§$%&/()=?''~^°^<>|@';
    Parent := Grp2;
  end;

  AutoEvent := TMUIButton.Create('Auto');
  with AutoEvent do
  begin
    OnClick := @AutoEventClick;
    ShowMe := False;
    Weight := 50;
    Parent := Grp2;
  end;
  EditEvent := TMUIButton.Create('Edit');
  with EditEvent do
  begin
    OnClick := @EditEventClick;
    ShowMe := False;
    Weight := 50;
    Parent := Grp2;
  end;
  // ############# End Event Stuff

  with TMUIButton.Create('Save') do
  begin
    OnClick := @SaveClick;
    Parent := Self;
  end;

  Tree := TItemTree.Create;
  Tree.Name := 'Window1';
  TestWin := TMUIWindow.Create;
  Tree.Data := TestWin;
  Tree.Properties.Add('Title');
  TestWin.Title := 'Window1';

  CurItem := Tree;
end;

destructor TMainWindow.Destroy;
begin
  Tree.Free;
  ItemProps.Free;
  EventProps.Free;
  Eventhandlers.Free;
  inherited;
end;

// Add Properties to Output Source as Assignment in the Sourcecode
procedure AddProperties(Node: TItemNode; Ind: string; SL: TStringList);
var
  PT : PTypeData;
  PI : PTypeInfo;
  I,J,n : Longint;
  PP : PPropList;
  Value: Integer;
  ValueS: string;
  Obj: TObject;
  EV: TEventHandler;
begin
  // normal items
  Obj := Node.Data;
  PI := Obj.ClassInfo;
  PT := GetTypeData(PI);
  GetMem (PP, PT^.PropCount * SizeOf(Pointer));
  J := GetPropList(PI, tkAny, PP);
  for I:=0 to J-1 do
  begin
    with PP^[i]^ do
    begin
      // Only include the Properties which are marked as changed
      if (Node.Properties.IndexOf(Name) < 0) and not (PropType^.Kind = tkMethod) then
        Continue;
      // Type of the Property
      case PropType^.Kind of
        // Events
        tkMethod: begin
          EV := MainWindow.FindEventHandler(Node, Name);
          if Assigned(EV) then
          begin
            SL.Add(Ind + Name + ' := @' + EV.Event + ';');
          end;
        end;
        // Integer
        tkInteger: begin
          Value := GetOrdProp(Obj, PP^[i]);
          if Value <> Default then
            SL.Add(Ind + Name + ' := ' + IntToStr(Value) + ';');
        end;
        // Boolean
        tkBool: begin
          Value := GetOrdProp(Obj, PP^[i]);
          if Boolean(Value) <> Boolean(Default) then
            SL.Add(Ind + Name + ' := ' + BoolToStr(Boolean(Value), True) + ';');
        end;
        // String/AnsiString
        tkString, tkAString: begin
          ValueS := GetStrProp(Obj, PP^[i]);
          if ValueS <> '' then
            SL.Add(Ind + Name + ' := ''' + ValueS + ''';');
        end;
        // DynArray have to be explicitly ... sadly
        tkDynArray: begin
            // Cycle.Entries
            if (Obj is TMUICycle) and (Name = 'Entries') then
            begin
              writeln('    Length ', Length(TMUICycle(Obj).Entries));
              if Length(TMUICycle(Obj).Entries) > 0 then
              begin
                for n := 0 to High(TMUICycle(Obj).Entries) do
                begin
                  if n = 0 then
                    ValueS := '''' + TMUICycle(Obj).Entries[0] + ''''
                  else
                    ValueS := ValueS + ', ''' + TMUICycle(Obj).Entries[n] + '''';
                end;
                SL.Add(Ind + Name + ' := [' + ValueS + '];');
              end;
            end;
            // Register.Titles
            if (Obj is TMUIRegister) and (Name = 'Titles') then
            begin
              if Length(TMUIRegister(Obj).Titles) > 0 then
              begin
                for n := 0 to High(TMUIRegister(Obj).Titles) do
                begin
                  if n = 0 then
                    ValueS := TMUIRegister(Obj).Titles[0]
                  else
                    ValueS := ValueS + ', ''' + TMUIRegister(Obj).Titles[n] + '''';
                end;
                SL.Add(Ind + Name + ' := [' + ValueS + '];');
              end;
            end;
          end;
      end;
    end;
  end;
  FreeMem(PP);
end;

// Create the Source code from the settings
procedure TMainWindow.CreateSource;
var
  SL, UL: TStringList;
  Cl: TObject;
  i, j: Integer;
  FileName, Ident, str: string;
  Item: TItemNode;
  EV: TEventHandler;
begin
  FileName := 'TestSource.pas';
  Ident := ChangeFileExt(ExtractFileName(Filename), '');
  SL := TStringList.Create;
  UL := TStringList.Create;
  writeln(1);
  try
    SL.Add('unit ' + Ident + ';');
    SL.Add('{$mode objfpc}{$H+}');
    SL.Add('interface');
    SL.Add('uses');
    //
    writeln(2);
    UL.Add('MUIClass.Base');
    UL.Add('MUIClass.Window');
    for i := 0 to Tree.AllCount - 1 do
    begin
      Cl := Tree.AllChild[i].Data;
      for j := 0 to High(MUIComponents) do
      begin
        if Cl.ClassName = MUIComponents[j].MUIClass.ClassName then
        begin
          if UL.IndexOf(MUIComponents[j].AUnit) < 0 then
            UL.Add(MUIComponents[j].AUnit);
          Break;
        end;
      end;
    end;
    writeln(3);
    for i := 0 to UL.Count - 1 do
    begin
      if i = 0 then
        str := '  ' + UL[0]
      else
        str := str + ', ' + UL[i];
    end;
    writeln(4);
    SL.Add(str + ';');
    SL.Add('type');
    SL.Add('  T' + Tree.Name + ' = class(TMUIWindow)');
    writeln(5);
    for i := 1 to Tree.AllCount - 1 do
    begin
      SL.Add('    ' + Tree.AllChild[i].Name + ': ' + Tree.AllChild[i].Data.ClassName + ';');
    end;
    for i := 0 to EventHandlers.Count - 1 do
    begin
      EV := EventHandlers[i];
      SL.Add('    ' + StringReplace(EV.Header, 'T' + Tree.Name + '.', '', [rfReplaceAll]));
    end;
    writeln(5);
    SL.Add('    constructor Create; override;');
    SL.Add('  end;');
    SL.Add('var');
    SL.Add('  ' + Tree.Name + ': T' + Tree.Name + ';');
    SL.Add('implementation');
    SL.Add('');
    SL.Add('constructor T' + Tree.Name + '.Create;');
    SL.Add('begin');
    SL.Add('  inherited;');
    writeln(6);
    for i := 1 to Tree.AllCount - 1 do
    begin
      Item := Tree.AllChild[i];
      Cl := Item.Data;
      SL.Add('  ' + Item.Name + ' := ' + Cl.Classname + '.Create;');
      SL.Add('  with ' + Item.Name + ' do');
      SL.Add('  begin');
      AddProperties(Item, '    ', SL);
      if Item.Parent.Data is TMUIWindow then
        SL.Add('    Parent := Self;')
      else
        SL.Add('    Parent := ' + Item.Parent.Name + ';');
      SL.Add('  end;');
    end;
    SL.Add('end;');
    SL.Add('');
    // eventhandlers
    for i := 0 to EventHandlers.Count - 1 do
    begin
      SL.Add(EventHandlers[i].Text);
    end;
    writeln(7);
    //
    SL.Add('end.');
    writeln(8);
    SL.SaveToFile(FileName);
    writeln(9);
    SL.Clear;
    SL.Add('program ' + Ident + 'Main;');
    SL.Add('{$mode objfpc}{$H+}');
    SL.Add('uses');
    SL.Add('  MUIClass.Base, ' + Ident + ';');
    SL.Add('');
    SL.Add('begin');
    SL.Add('  ' + Tree.Name + ' := T' + Tree.Name + '.Create;');
    SL.Add('  MUIApp.Run;');
    SL.Add('end.');
    writeln(10);
    SL.SaveToFile(ExtractFilePath(Filename) + Ident + 'Main.pas');
    writeln(11);
  finally
    UL.Free;
    SL.Free;
  end;
end;

procedure TMainWindow.SaveClick(Sender: TObject);
begin
  CreateSource;
end;

procedure TMainWindow.UpdateItemList;
var
  i, OldActive: Integer;
begin
  OldActive := ItemList.List.Active;
  ItemList.List.Quiet := True;
  while ItemList.List.Entries > 0 do
    ItemList.List.Remove(MUIV_List_Remove_Last);
  Tree.UpdateNodesText;
  for i := 0 to Tree.NodesText.Count - 1 do
  begin
    ItemList.List.InsertSingle(PChar(Tree.NodesText[i]), MUIV_List_Insert_Bottom);
  end;
  ItemList.List.Quiet := False;
  MH_Set(ItemList.List.MUIObj, MUIA_List_Active, OldActive);
  EditPages.ActivePage := 0;
end;

function TMainWindow.FindEventHandler(Obj: TItemNode; Event: string): TEventHandler;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to EventHandlers.Count - 1 do
  begin
    if (Obj = EventHandlers[i].Obj) and (Event =  EventHandlers[i].Name) then
    begin
      Result := EventHandlers[i];
      Break;
    end;
  end;
end;

type
  TEventParam = record
    Style: TParamFlags;
    Name: string;
    Typ: string;
  end;
  TEventParams = array of TEventParam;

PParamFlags = ^TParamFlags;

function GetParams(MT: PTypeData): TEventParams;
var
  i, j: Integer;
  p: PByte;
  Str: string;
  Num: Integer;
begin
  SetLength(Result, MT^.ParamCount);
  P := @(MT^.ParamList[0]);
  for i := 0 to High(Result) do
  begin
    Result[i].Style := PParamFlags(P)^;
    Inc(p, SizeOf(TParamFlags));
    Num := P^;
    Inc(P);
    Str := '';
    for j := 1 to Num do
    begin
      Str := Str + Char(P^);
      Inc(P);
    end;
    Result[i].Name := str;
    //
    Num := P^;
    Inc(P);
    Str := '';
    for j := 1 to Num do
    begin
      Str := Str + Char(P^);
      Inc(P);
    end;
    Result[i].Typ := str;
  end;
end;


procedure TMainWindow.UpdateProperties;
var
  PT, MT : PTypeData;
  PI : PTypeInfo;
  I,J,n  : Longint;
  PP : PPropList;
  Obj: TObject;
  ItemProp: TItemProp;
  a, b: array of PChar;
  EV: TEVentHandler;
  PL: TEventParams;
begin
  PropList.List.Quiet := True;
  while PropList.List.Entries > 0 do
    PropList.List.Remove(MUIV_List_Remove_Last);
  EventList.List.Quiet := True;
  while EventList.List.Entries > 0 do
    EventList.List.Remove(MUIV_List_Remove_Last);
  EventProps.Clear;
  ItemProps.Clear;
  if Assigned(CurItem) and Assigned(CurItem.Data) then
  begin
    //Special Items
    ItemProp := TItemProp.Create;
    ItemProp.Name := 'Name';
    ItemProp.IsSpecial := True;
    ItemProp.Value := CurItem.Name;
    ItemProp.Active := False;
    ItemProps.Add(ItemProp);
    // normal items
    Obj := CurItem.Data;
    PI := Obj.ClassInfo;
    PT := GetTypeData(PI);
    GetMem (PP, PT^.PropCount * SizeOf(Pointer));
    J := GetPropList(PI, tkAny, PP);
    for I:=0 to J-1 do
    begin
      With PP^[i]^ do
      begin
        case PropType^.Kind of
          tkMethod: begin
            ItemProp := TItemProp.Create;
            ItemProp.Name := Name;
            MT := GetTypeData(PropType);
            PL := GetParams(MT);
            if MT^.MethodKind = mkFunction then
              ItemProp.Additional := 'function %name%('
            else
              ItemProp.Additional := 'procedure %name%(';
            for n := 0 to High(PL) do
            begin
              if pfVar in PL[n].Style then
                ItemProp.Additional := ItemProp.Additional + 'var ';
              if pfConst in PL[n].Style then
                ItemProp.Additional := ItemProp.Additional + 'const ';
              if pfOut in PL[n].Style then
                ItemProp.Additional := ItemProp.Additional + 'out ';
              if n = High(PL) then
                ItemProp.Additional := ItemProp.Additional + PL[n].Name + ': ' + PL[n].Typ
              else
                ItemProp.Additional := ItemProp.Additional + PL[n].Name + ': ' + PL[n].Typ + '; ';
            end;
            ItemProp.Additional := ItemProp.Additional + ')';
            ItemProp.IsSpecial := False;
            EV := FindEventHandler(CurItem, Name);
            if Assigned(Ev) then
              ItemProp.Value := Ev.Event
            else
              ItemProp.Value := '';
            ItemProp.Active := CurItem.Properties.IndexOf(Name) >= 0;
            EventProps.Add(ItemProp);
          end;
          tkInteger: begin
            ItemProp := TItemProp.Create;
            ItemProp.Name := Name;
            ItemProp.IsSpecial := False;
            ItemProp.Value := IntToStr(GetOrdProp(Obj, PP^[i]));
            ItemProp.Active := CurItem.Properties.IndexOf(Name) >= 0;
            ItemProps.Add(ItemProp);
          end;
          tkBool: begin
            ItemProp := TItemProp.Create;
            ItemProp.Name := Name;
            ItemProp.IsSpecial := False;
            ItemProp.Value := BoolToStr(Boolean(GetOrdProp(Obj, PP^[i])), True);
            ItemProp.Active := CurItem.Properties.IndexOf(Name) >= 0;
            ItemProps.Add(ItemProp);
          end;
          tkString, tkAString: begin
            ItemProp := TItemProp.Create;
            ItemProp.Name := Name;
            ItemProp.IsSpecial := False;
            ItemProp.Value := '''' + GetStrProp(Obj, PP^[i])+'''';
            ItemProp.Active := CurItem.Properties.IndexOf(Name) >= 0;
            ItemProps.Add(ItemProp);
          end;
          tkDynArray: begin
              if (Obj is TMUICycle) and (Name = 'Entries') then
              begin
                ItemProp := TItemProp.Create;
                ItemProp.Name := Name;
                ItemProp.IsSpecial := False;
                ItemProp.Value := '<Array ' + IntToStr(Length(TMUICycle(Obj).Entries)) + ' Entries>';
                ItemProp.Active := CurItem.Properties.IndexOf(Name) >= 0;
                ItemProps.Add(ItemProp);
              end;
              if (Obj is TMUIRegister) and (Name = 'Titles') then
              begin
                ItemProp := TItemProp.Create;
                ItemProp.Name := Name;
                ItemProp.IsSpecial := False;
                ItemProp.Value := '<Array ' + IntToStr(Length(TMUIRegister(Obj).Titles)) + ' Entries>';
                ItemProp.Active := CurItem.Properties.IndexOf(Name) >= 0;
                ItemProps.Add(ItemProp);
              end;
            end;
          else
            writeln(name, ' Type: ', PropType^.Kind);
        end;
      end;
    end;
    FreeMem(PP);
  end;

  SetLength(A, ItemProps.Count + 1);
  for i := 0 to ItemProps.Count - 1 do
    A[i] := PChar(ItemProps[i].Name);
  A[High(A)] := nil;
  PropList.List.Insert(@a[0], ItemProps.Count, MUIV_List_Insert_Bottom);
  PropList.List.Quiet := False;

  SetLength(B, EventProps.Count + 1);
  for i := 0 to EventProps.Count - 1 do
    B[i] := PChar(EventProps[i].Name);
  B[High(B)] := nil;
  EventList.List.Insert(@B[0], EventProps.Count, MUIV_List_Insert_Bottom);
  EventList.List.Quiet := False;

  EditPages.ActivePage := 0;
end;

var
  Title1: string = 'Property';
  Title2: string = 'Value';
  Title3: string = 'Event';
  Title4: string = 'Handler';

procedure TMainWindow.PropDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
var
  Idx: Integer;
  p: PLongInt;
  ItemProp: TItemProp;
begin
  P := PLongInt(ToPrint);
  Dec(P);
  Idx := P^;
  if (Idx >= 0) and (Idx < ItemProps.Count) and Assigned(Entry) then
  begin
    ItemProp := ItemProps[Idx];
    ToPrint[0] := PChar(ItemProp.DisplayName);
    ToPrint[1] := PChar(ItemProp.DisplayValue);
  end
  else
  begin
    ToPrint[0] := PChar(Title1);
    ToPrint[1] := PChar(Title2);
  end;
end;

procedure TMainWindow.EventDisplay(Sender: TObject; ToPrint: PPChar; Entry: PChar);
var
  Idx: Integer;
  p: PLongInt;
  EventProp: TItemProp;
begin
  P := PLongInt(ToPrint);
  Dec(P);
  Idx := P^;
  if (Idx >= 0) and (Idx < EventProps.Count) and Assigned(Entry) then
  begin
    EventProp := EventProps[Idx];
    ToPrint[0] := PChar(EventProp.DisplayName);
    ToPrint[1] := PChar(EventProp.DisplayValue);
  end
  else
  begin
    ToPrint[0] := PChar(Title3);
    ToPrint[1] := PChar(Title4);
  end;
end;

procedure TMainWindow.ItemListSelect(Sender: TObject);
var
  Idx: Integer;
begin
  Idx := ItemList.List.Active;
  if (Idx >= 0) and (Idx < Tree.AllCount) then
  begin
    CurItem := Tree.AllChild[Idx];
    ItemName.Contents := 'Properties of ' + CurItem.Name;
  end;
  RemBtn.Disabled := not (Idx > 0);
  UpdateProperties;
end;

procedure TMainWindow.PropListSelect(Sender: TObject);
var
  Idx: Integer;
  PT : PTypeData;
  PI : PTypeInfo;
  I,J : Longint;
  PP : PPropList;
  Obj: TObject;
  PropName: string;
begin
  BlockEvents := True;
  Idx := PropList.List.Active;
  if (Idx >= 0) and (Idx < ItemProps.Count) and Assigned(CurItem) and Assigned(CurItem.Data) then
  begin
    CurProp := ItemProps[Idx];
    //
    if StrArrayWin.Open then
      StrArrayWin.Close;
    //
    if CurProp.IsSpecial then
    begin
      PropName := CurItem.Name + '.' + CurProp.Name;
      StringLabel.Contents := PropName;
      StrSet.Reject := ' ,.-+*!"§$%&/()=?''~^°^<>|@';
      StrSet.Contents := CurProp.Value;
      EditPages.ActivePage := 3;
      IncludeProp.Disabled := True;
      Exit;
    end;
    Obj := CurItem.Data;
    PI := Obj.ClassInfo;
    PT := GetTypeData(PI);
    GetMem (PP, PT^.PropCount * SizeOf(Pointer));
    J := GetPropList(PI, tkAny, PP);
    for I:=0 to J-1 do
    begin
      With PP^[i]^ do
      begin
        if Name = CurProp.Name then
        begin
          PropName := CurItem.Name + '.' + CurProp.Name;
          case PropType^.Kind of
            tkInteger: begin
              IntLabel.Contents := PropName;
              IntSet.Contents := IntToStr(GetOrdProp(Obj, PP^[i]));
              EditPages.ActivePage := 2;
              IncludeProp.Disabled := False;
              IncludeProp.Selected := CurProp.Active;
            end;
            tkBool: begin
              BoolLabel.Contents := PropName;
              BoolSet.Active := GetOrdProp(Obj, PP^[i]);
              EditPages.ActivePage := 1;
              IncludeProp.Disabled := False;
              IncludeProp.Selected := CurProp.Active;
            end;
            tkString, tkAString: begin
              StringLabel.Contents := PropName;
              StrSet.Reject := '';
              StrSet.Contents := GetStrProp(Obj, PP^[i]);
              EditPages.ActivePage := 3;
              IncludeProp.Disabled := False;
              IncludeProp.Selected := CurProp.Active;
            end;
            tkDynArray: begin
              StrArrayLabel.Contents := PropName;
              StrArrayWin.Title := PropName;
              StrArrayWin.Obj := Obj;
              StrArrayWin.PropName := CurProp.Name;
              StrArrayWin.CurProp := CurProp;
              if (Obj is TMUICycle) and (CurProp.Name = 'Entries') then
              begin
                StrArrayWin.StrArray := TMUICycle(Obj).Entries;
                EditPages.ActivePage := 4;
                IncludeProp.Disabled := False;
                IncludeProp.Selected := CurProp.Active;
              end
              else
              if (Obj is TMUIRegister) and (CurProp.Name = 'Titles') then
              begin
                StrArrayWin.StrArray := TMUIRegister(Obj).Titles;
                EditPages.ActivePage := 4;
                IncludeProp.Disabled := False;
                IncludeProp.Selected := CurProp.Active;
              end
              else
              begin
                EditPages.ActivePage := 0;
                IncludeProp.Disabled := True;
              end;
            end;
            else
            begin
              EditPages.ActivePage := 0;
              IncludeProp.Disabled := True;
            end;
          end;
          Break;
        end;
      end;
    end;
    FreeMem(PP);
  end
  else
  begin
    EditPages.ActivePage := 0;
    IncludeProp.Disabled := True;
  end;
  BlockEvents := False;
end;

procedure TMainWindow.EventListSelect(Sender: TObject);
var
  Idx: Integer;
  EV: TEventHandler;
begin
  BlockEvents := True;
  EV := Nil;
  Idx := EventList.List.Active;
  if (Idx >= 0) and (Idx <EventProps.Count) then
  begin
    CurEvent := EventProps[Idx];
    EV := FindEventHandler(CurItem, CurEvent.Name);
    if Assigned(EV) then
    begin
      EventName.Contents := Ev.Event;
      AutoEvent.ShowMe := False;
      EditEvent.ShowMe := True;
    end
    else
    begin
      EventName.Contents := '';
      AutoEvent.ShowMe := True;
      EditEvent.ShowMe := False;
    end;
    EventLabel.Contents := CurItem.Name + '.' + CurEvent.Name;
    EventName.Disabled := False;
  end
  else
  begin
    CurEvent := nil;
    EventLabel.Contents := '';
    EventName.Contents := '';
    EventName.Disabled := True;
  end;
  BlockEvents := False;
end;

procedure TMainWindow.EventListDblClick(Sender: TObject);
begin
  EventListSelect(Sender);
  if EventName.Contents = '' then
    AutoEventClick(Sender);
  EditEventClick(Sender);
end;

procedure TMainWindow.SetEvent(Sender: TObject);
var
  Ev: TEventHandler;
  Value: string;
  SL: TStringList;
begin
  if BlockEvents then
    Exit;
  if Assigned(CurEvent) then
  begin
    EV := FindEventHandler(CurItem, CurEvent.Name);
    Value := Trim(EventName.Contents);
    if Value = '' then
    begin
      if Assigned(EV) then
        EventHandlers.Delete(Eventhandlers.IndexOf(Ev));
      CurEvent.Value := '';
      CurEvent.Active := False;
      EventList.List.Redraw(MUIV_List_Redraw_Active);
      AutoEvent.ShowMe := True;
      EditEvent.ShowMe := False;
    end
    else
    begin
      if isValidIdent(Value) then
      begin
        if not Assigned(Ev) then
        begin
          Ev := TEventHandler.Create;
          Ev.Obj := CurItem;
          Ev.Name := CurEvent.Name;
          EventHandlers.Add(Ev);
        end;
        Ev.Event := Value;
        Ev.Header := StringReplace(CurEvent.Additional, '%name%', 'T' + Tree.Name + '.' + Value, []) + ';';
        if EV.Text = '' then
          Ev.Text := EV.Header + #13#10 +'begin'#13#10#13#10+'end;'
        else
        begin
          SL := TStringList.Create;
          SL.Text := EV.Text;
          SL[0] := EV.Header;
          EV.Text := SL.Text;
          SL.Free;
        end;
        CurEvent.Value := Value;
        CurEvent.Active := True;
        EventList.List.Redraw(MUIV_List_Redraw_Active);
        AutoEvent.ShowMe := False;
        EditEvent.ShowMe := True;
      end
      else
        ShowMessage('''' + Value + ''' is not a valid method name.');
    end;
  end;
end;

procedure TMainWindow.AutoEventClick(Sender: TObject);
var
  Val: string;
begin
  if Assigned(CurItem) and Assigned(CurEvent) then
  begin
    Val := CurEvent.Name;
    if LowerCase(Copy(Val, 1, 2)) = 'on' then
      Delete(Val, 1, 2);
    Val := CurItem.Name + Val;
    EventName.Contents := Val;
    SetEvent(nil);
  end;
end;

const
  {$ifdef AROS}
  EDITOR = 'sys:EdiSyn/EdiSyn';
  {$else}
    {$ifdef Amiga68k}
    EDITOR = 'sys:Tools/EditPad';
    {$else}
    EDITOR = 'c:ed';
    {$endif}
  {$endif}

procedure TMainWindow.EditEventClick(Sender: TObject);
var
  SL: TStringList;
  EV: TEventHandler;
  TmpName: string;
  Num: Integer;
  Tmp: string;
begin
  if not Assigned(CurItem) or not Assigned(CurEvent) then
    Exit;
  EV := FindEventHandler(CurItem, CurEvent.Name);
  if not Assigned(EV) then
    Exit;
  SL := TStringList.Create;
  try
    SL.Text := EV.Text;
    SL.Insert(0,'// Edit the Eventhandler, save and close to return to MUIIDE.'#13#10'//   Do NOT change/remove the first 3 lines.');
    Num := 0;
    repeat
      TmpName := 'T:MUIGUI_' + IntToStr(Num) + '.pas';
      Inc(Num);
    until not FileExists(TmpName);
    SL.SaveToFile(TmpName);
    MUIApp.Iconified := True;
    try
      SysUtils.ExecuteProcess(EDITOR, TmpName, []);
    except
      ShowMessage('cant execute Editor');
      Exit;
    end;
    MUIApp.Iconified := False;
    try
      SL.LoadFromFile(TmpName);
    except
      ShowMessage('Unable to reload your changed file');
      Exit;
    end;
    Tmp := SL[0];
    if Copy(Tmp, 1,2) = '//' then
      SL.Delete(0);
    Tmp := SL[0];
    if Copy(Tmp, 1,2) = '//' then
      SL.Delete(0);
    SL[0] := EV.Header;
    EV.Text := SL.Text;
    DeleteFile(TmpName);
  finally
    SL.Free;
  end;
end;

procedure TMainWindow.SetIntProp(Sender: TObject);
var
  Idx: Integer;
begin
  if BlockEvents then
    Exit;
  Idx := PropList.List.Active;
  if (Idx >= 0) and (Idx < ItemProps.Count) and Assigned(CurItem) and Assigned(CurItem.Data) then
  begin
    CurProp := ItemProps[Idx];
    //
    DestroyTestWin;
    if CurItem.Properties.IndexOf(CurProp.Name) < 0 then
      CurItem.Properties.Add(CurProp.Name);
    CurProp.Active := True;
    SetOrdProp(CurItem.Data, CurProp.Name, IntSet.IntegerValue);
    CurProp.Value := IntToStr(IntSet.IntegerValue);
    PropList.List.Redraw(MUIV_List_Redraw_Active);
    CreateTestWin;
  end;
end;

procedure TMainWindow.SetBoolProp(Sender: TObject);
var
  Idx: Integer;
begin
  if BlockEvents then
    Exit;
  Idx := PropList.List.Active;
  if (Idx >= 0) and (Idx < ItemProps.Count) and Assigned(CurItem) and Assigned(CurItem.Data) then
  begin
    CurProp := ItemProps[Idx];
    //
    DestroyTestWin;
    if CurItem.Properties.IndexOf(CurProp.Name) < 0 then
      CurItem.Properties.Add(CurProp.Name);
    SetOrdProp(CurItem.Data, CurProp.Name, BoolSet.Active);
    CurProp.Value := BoolToStr(Boolean(BoolSet.Active), True);
    CurProp.Active := True;
    PropList.List.Redraw(MUIV_List_Redraw_Active);
    CreateTestWin;
    //
  end;
end;

procedure TMainWindow.SetStringProp(Sender: TObject);
var
  Idx: Integer;
  Str: string;
begin
  if BlockEvents then
    Exit;
  Idx := PropList.List.Active;
  if (Idx >= 0) and (Idx < ItemProps.Count) and Assigned(CurItem) and Assigned(CurItem.Data) then
  begin
    CurProp := ItemProps[Idx];
    //
    DestroyTestWin;
    if CurProp.IsSpecial then
    begin
      Str := StrSet.Contents;
      if IsValidIdent(Str) then
        CurItem.Name := Str
      else
        ShowMessage('''' + Str + ''' is not a valid identifier');
      UpdateItemList;
    end
    else
    begin
      if CurItem.Properties.IndexOf(CurProp.Name) < 0 then
        CurItem.Properties.Add(CurProp.Name);
      SetStrProp(CurItem.Data, CurProp.Name, StrSet.Contents);
      CurProp.Value := '''' + StrSet.Contents + '''';
      CurProp.Active := True;
      PropList.List.Redraw(MUIV_List_Redraw_Active);
    end;
    CreateTestWin;
    //
  end;
end;

procedure TMainWindow.IncludeChange(Sender: TObject);
begin
  CurProp.Active := IncludeProp.Selected;
  PropList.List.Redraw(MUIV_List_Redraw_Active);
end;

procedure TMainWindow.OpenStrArrayWin(Sender: TObject);
begin
  StrArrayWin.Show;
end;

procedure TMainWindow.CreateTestWin;
var
  Item: TItemNode;
  i: Integer;
begin
  for i := 0 to Tree.AllCount - 1 do
  begin
    Item := Tree.AllChild[i];
    if Assigned(Item.Parent) then
      TMUIWithParent(Item.Data).Parent := TMUIWithParent(Item.Parent.Data);
  end;
  TestWin.Show;
end;

procedure RemoveParent(A: TItemNode);
var
  i: Integer;
begin
  for i := 0 to A.Count - 1 do
  begin
    RemoveParent(A.Child[i]);
  end;
  if A.Data is TMUIWithParent then
    TMUIWithParent(A.Data).Parent := nil;
end;

procedure TMainWindow.DestroyTestWin;
begin
  if TestWin.HasObj then
  begin
    TestWin.Close;
    RemoveParent(Tree);
    TestWin.Parent := nil;
    TestWin.DestroyObject;
  end;
end;

// Show the Window, Update Item List and create the empty Window
procedure TMainWindow.ShowEvent(Sender: TObject);
begin
  UpdateItemList;
  ItemList.List.Active := 0;
  CreateTestWin;
end;

// Checks if an object can have a child
// (thats defined in MUIComponents, classname must fit! not only the type
// because subclasses can be that they do not allow that, or only indirect)
function CanHaveChild(Obj: TObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to High(MUIComponents) do
  begin
    if Obj.ClassName = MUIComponents[i].MUIClass.ClassName then
    begin
      Result := MUIComponents[i].HasChild;
      Break;
    end;
  end;
end;

// Add the selected Component to the list and recreate the Window
procedure TMainWindow.AddClick(Sender: TObject);
var
  Idx: Integer;
  Num: Integer;
  NName: string;
  Child: TItemNode;
begin
  // get active element
  Idx := ItemList.List.Active;
  if (Idx >= 0) and (Idx < Tree.AllCount) then
  begin
    // get thi ITem on this Element
    CurItem := Tree.AllChild[Idx];
    // Check if this element can have parents!
    while Assigned(CurItem) do
    begin
      if CanHaveChild(CurItem.Data) then
        Break;
      CurItem := CurItem.Parent;
    end;
    // if not assigned -> use the main Window as Parent
    if not Assigned(CurItem) then
      CurItem := Tree;
    // Which element to add
    Idx := ChooseComp.Active;
    if (Idx < 0) or (Idx > High(MUIComponents)) then
      Exit;
    // get the name of the component
    Num := 1;
    repeat
      NName := MUIComponents[Idx].Name + IntToStr(Num);
      Inc(Num);
    Until Tree.AllChildByName(NName) < 0;
    // destroy window
    DestroyTestWin;
    // add the component
    Child := CurItem.NewChild(NName, MUIComponents[Idx].MUIClass.Create);
    if IsPublishedProp(Child.Data, 'Contents') then
    begin
      Child.Properties.Add('Contents');
      SetStrProp(Child.Data, 'Contents', NName);
    end;
    // update the pseudo Tree
    UpdateItemList;
    // create the Window with new component
    CreateTestWin;
  end;
end;

// remove the selected item from the GUI
procedure TMainWindow.RemoveClick(Sender: TObject);
var
  Idx: Integer;
begin
  // get the element to delete
  Idx := ItemList.List.Active;
  if (Idx >= 1) and (Idx < Tree.AllCount) then
  begin
    // destroy the Window
    DestroyTestWin;
    //
    CurItem := Tree.AllChild[Idx];
    // remove the MUIClass object and all its childs
    CurItem.Data.Free;
    CurItem.Data := nil;
    // destroy the object
    CurItem.Free;
    UpdateItemList;
    // recreate the window
    CreateTestWin;
  end;
end;


procedure TItemProp.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  if Active then
  begin
    DisplayName := #27'b' + Name;
    DisplayValue := #27'b' + Value;
  end
  else
  begin
    DisplayName := Name;
    DisplayValue := Value;
  end;
end;

end.
