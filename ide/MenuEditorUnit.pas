unit menueditorunit;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, Classes,
  NodeTreeUnit,
  MUI,
  MUIClass.Base, MUIClass.Area, MUIClass.Window,
  MUIClass.Group, MUIClass.Menu;

type
  TMenuEditor = class(TMUIWindow)
  private
    ActionPanel: TMUIGroup;
    AddBtn, SubBtn, RemBtn: TMUIButton;
    CurMenu: TItemNode;
    CurMenuObj: TMUIMenuStrip;
    Tree: TItemTree;

    SelItem: TItemNode;
    SelObj: TMUIFamily;

    FOnMenuChanged: TNotifyEvent;
    FOnItemSelect: TNotifyEvent;

    procedure SubClick(Sender: TObject);
    procedure AddClick(Sender: TObject);
    procedure RemClick(Sender: TObject);

    procedure SelClick(Sender: TObject);

    procedure CreateMenus(APanel: TMUIGroup; TopMenu: TItemNode; Level: Integer);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Execute(AMenu: TItemNode);
    procedure Recreate;

    property OnMenuChanged: TNotifyEvent read FOnMenuChanged write FOnMenuChanged;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
    property SelectedItem: TItemNode read SelItem;
  end;

var
  MenuEditor: TMenuEditor;

implementation

constructor TMenuEditor.Create;
var
  Grp: TMUIGroup;
begin
  inherited;
  SelObj := nil;
  SelItem := nil;
  CurMenu := nil;
  CurMenuObj := nil;

  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Parent := Self;

  SubBtn := TMUIButton.Create('Add Sub Menu');
  SubBtn.OnClick := @SubClick;
  SubBtn.Parent := Grp;

  AddBtn := TMUIButton.Create('Add Menu Item');
  AddBtn.OnClick := @AddClick;
  AddBtn.Parent := Grp;

  RemBtn := TMUIButton.Create('Remove');
  RemBtn.OnClick := @RemClick;
  RemBtn.Parent := Grp;

  ActionPanel := TMUIGroup.Create;
  ActionPanel.Horiz := True;
  ActionPanel.Parent := Self;


end;

destructor TMenuEditor.Destroy;
begin
  inherited;
end;

procedure TMenuEditor.Execute(AMenu: TItemNode);
var
  n: TItemNode;
begin
  if Open then
    Close;
  CurMenu := AMenu;
  if not Assigned(AMenu) or not Assigned(AMenu.Data) then
    Exit;
  if not (AMenu.Data is TMUIMenuStrip) then
    Exit;
  CurMenuObj := TMUIMenuStrip(AMenu.Data);

  SelItem := AMenu;
  SelObj := CurMenuObj;

  Tree := nil;
  n := AMenu;
  repeat
    n := N.Parent;
    if n is TItemTree then
    begin
      Tree := TItemTree(n);
      Break;
    end;
  until n.Parent = nil;

  Recreate;

  AddBtn.Disabled := True;
  RemBtn.Disabled := True;
  SubBtn.Disabled := False;
  Open := True;

end;

procedure TMenuEditor.SubClick(Sender: TObject);
var
  NMenu: TMUIMenu;
  Num: Integer;
  NName: string;
  NNode: TItemNode;
begin
  if Assigned(SelItem) then
  begin
    NMenu := TMUIMenu.Create;
    Num := 1;
    repeat
      NName := 'Menu' + IntToStr(Num);
      Inc(Num);
    Until Tree.AllChildByName(NName) < 0;
    NMenu.Title := NName;
    NNode := SelItem.NewChild(NName, NMenu);
    NNode.Properties.Add('Title');
  end;
  ReCreate;
  if Assigned(FOnMenuChanged) then
    FOnMenuChanged(Self);
end;


procedure TMenuEditor.AddClick(Sender: TObject);
var
  NMenu: TMUIMenuItem;
  Num: Integer;
  NName: string;
  NNode: TItemNode;
begin
  if Assigned(SelItem) then
  begin
    NMenu := TMUIMenuItem.Create;
    Num := 1;
    repeat
      NName := 'MenuItem' + IntToStr(Num);
      Inc(Num);
    Until Tree.AllChildByName(NName) < 0;
    NMenu.Title := NName;
    NNode := SelItem.NewChild(NName, NMenu);
    NNode.Properties.Add('Title');
  end;
  ReCreate;
  if Assigned(FOnMenuChanged) then
    FOnMenuChanged(Self);
end;

procedure TMenuEditor.RemClick(Sender: TObject);
var
  ToDelete: TItemNode;
begin
  if Assigned(SelItem) and not (SelItem.Data is TMUIMenuStrip) then
  begin
    ToDelete := SelItem;
    SelItem := SelItem.Parent;
    SelObj := TMUIFamily(SelItem.Data);
    ToDelete.Free;
    ReCreate;
    if Assigned(FOnMenuChanged) then
      FOnMenuChanged(Self);
  end;
end;

procedure TMenuEditor.Recreate;
begin
  InitChange;
  ActionPanel.Free;
  //
  ActionPanel := TMUIGroup.Create;
  ActionPanel.Horiz := True;
  ActionPanel.Parent := Self;
  if Assigned(CurMenu) then
  begin
    CreateMenus(ActionPanel, CurMenu, 0);
  end;
  ExitChange;
end;

procedure TMenuEditor.CreateMenus(APanel: TMUIGroup; TopMenu: TItemNode; Level: Integer);
var
  i: Integer;
  Grp, Grp2: TMUIGroup;
  Entry: TMUIText;
  Add: string;
  Add2: string;
begin
  Add := '';
  Add2 := '';
  if TopMenu = SelItem then
    Add := #27'b';
  if TopMenu.Count > 0 then
    Add2 := ' V';
  Grp := TMUIGroup.Create;
  Grp.Horiz := Level > 1;
  Grp.Frame := MUIV_Frame_None;
  Grp.Parent := APanel;

  Entry := TMUIText.Create;
  with Entry do
  begin
    if TopMenu.Data is TMUIMenuStrip then
      Contents := Add + TopMenu.Name + ' (' + TopMenu.Data.ClassName + ')' + Add2;
    if TopMenu.Data is TMUIMenu then
      Contents := Add + TMUIMenu(TopMenu.Data).Title + ' (' + TopMenu.Name + '; ' + TopMenu.Data.ClassName + ')' + Add2;
    if TopMenu.Data is TMUIMenuItem then
      Contents := Add + TMUIMenuItem(TopMenu.Data).Title + ' (' + TopMenu.Name + '; ' + TopMenu.Data.ClassName + ')' + Add2;
    InputMode := MUIV_InputMode_Relverify;
    Parent := Grp;
    OnClick := @SelClick;
    Background.Spec := MUII_BACKGROUND;
    FixWidthTxt := 'XXXXXXXXXXXX';
    Frame := 1;
    Tag := PtrInt(TopMenu);
  end;
  if TopMenu.Count > 0 then
  begin
    Grp2 := TMUIGroup.Create;
    Grp2.Horiz := Level < 1;
    Grp2.Spacing := 0;
    Grp2.Frame := 1;
    Grp2.Parent := Grp;
    for i := 0 to TopMenu.Count - 1 do
      CreateMenus(Grp2, TopMenu.Child[i], Level + 1);
  end;
  TMUIVSpace.Create(0).Parent := Grp;
end;

procedure TMenuEditor.SelClick(Sender: TObject);
begin
  if Sender is TMUINotify then
  begin
    if (TMUINotify(Sender).Tag <> 0) then
    begin
      SelItem := TItemNode(TMUINotify(Sender).Tag);
      SelObj := TMUIFamily(SelItem.Data);
      AddBtn.Disabled := True;
      RemBtn.Disabled := True;
      SubBtn.Disabled := True;
      if SelObj is TMUIMenuItem then
      begin
        AddBtn.Disabled := False;
        RemBtn.Disabled := False;
      end
      else
      if SelObj is TMUIMenu then
      begin
        AddBtn.Disabled := False;
        RemBtn.Disabled := False;
        SubBtn.Disabled := False;
      end
      else
      if SelObj is TMUIMenuStrip then
        SubBtn.Disabled := False;
      if Assigned(FOnItemSelect) then
        FOnItemSelect(Self);
      Recreate;
    end;
  end;
end;

end.
