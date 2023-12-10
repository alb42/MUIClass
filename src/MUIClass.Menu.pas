unit MUIClass.Menu;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, icon, mui, muihelper,
  MUIClass.Base;
{$M+}
type

  // Usually it should look like this:
  // TMUIApplication.MenuStrip -> TMUIMenuStrip -> TMUIMenu -> TMenuItem
  // or
  // TMUIWindow.MenuStrip -> TMUIMenuStrip -> TMUIMenu -> TMenuItem

  TMUIMenustrip = class(TMUIFamily)
  private
    FEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True; // Enable/Disable Menu
  end;

  TMUIMenu = class(TMUIFamily)
  private
    FEnabled: Boolean;
    FTitle: string;
    procedure SetEnabled(AValue: Boolean);
    procedure SetTitle(AValue: string);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True; // Disable/Enable the full Menu
    property Title: string read FTitle write SetTitle;         // Title for the Menu
  end;

  TMUIMenuItem = class(TMUIFamily)
  private
    FEnabled: Boolean;
    FTitle: string;
    FChecked: Boolean;
    FCheckIt: Boolean;
    FCommandString: Boolean;
    FExclude: LongWord;
    FShortCut: string;
    FToggle: Boolean;
    FOnTrigger: TNotifyEvent;
    function GetChecked: Boolean;
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckit(AValue: Boolean);
    procedure SetCommandString(AValue: Boolean);
    procedure SetEnabled(AValue: Boolean);
    procedure SetExclude(AValue: LongWord);
    procedure SetShortCut(AValue: string);
    procedure SetTitle(AValue: string);
    procedure SetToggle(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    procedure AfterCreateObject; override;
  published
    property Checked: Boolean read GetChecked write SetChecked default False;                 // Check a Checkable Menu Entry (if CheckIt is True)
    property CheckIt: Boolean read FCheckIt write SetCheckIt default False;                   // If True the Item is Checkable
    property CommandString: Boolean read FCommandString write SetCommandString default False; // if True SortCut points to a full Short Cut, else only the first Char + Amiga is used
    property Enabled: Boolean read FEnabled write SetEnabled default True;                    // Enable/Disable this Menu Entry
    property Exclude: LongWord read FExclude write SetExclude default 0;                      // BitField to uncheck other menu entries when this one is checked (Radio)
    property ShortCut: string read FShortCut write SetShortCut;                               // ShortCut (just the name on the menu, the actual check you have to do yourself)
    property Title: string read FTitle write SetTitle;                                        // Title for menu Entry or '-' for a Line
    property Toggle: Boolean read FToggle write SetToggle default False;                      // Automatically toggle Checkable Entries when selected by user
    property OnTrigger: TNotifyEvent read FOnTrigger write FOnTrigger;                        // Event when an Entry is selected
  end;

implementation

const
  NM_BarLabel = -1;

{ TMUIMenuStrip }

constructor TMUIMenustrip.Create;
begin
  inherited;
  FEnabled := True;
end;

procedure TMUIMenustrip.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if not FEnabled then
    ATagList.AddTag(MUIA_Menustrip_Enabled, AsTag(FEnabled));
end;


procedure TMUIMenustrip.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_MenuStrip, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIMenustrip.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Menustrip_Enabled, FEnabled);
  end;
end;

{ TMUIMenu }

constructor TMUIMenu.Create;
begin
  inherited;
  FEnabled := True;
  FTitle := '';
end;

procedure TMUIMenu.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if not FEnabled then
    ATagList.AddTag(MUIA_Menu_Enabled, AsTag(FEnabled));
  ATagList.AddTag(MUIA_Menu_Title, AsTag(PChar(FTitle)));
end;


procedure TMUIMenu.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Menu, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIMenu.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Menu_Enabled, FEnabled);
  end;
end;

procedure TMUIMenu.SetTitle(AValue: string);
begin
  if AValue <> FTitle then
  begin
    FTitle := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Menu_Title, PChar(FTitle));
  end;
end;

{ TMUIMenuItem }

constructor TMUIMenuItem.Create;
begin
  inherited;
  FEnabled := True;
  FTitle := '';
  FChecked := False;
  FCheckIt := False;
  FCommandString := False;
  FExclude := 0;
  FShortCut := '';
  FToggle := False;
end;

procedure TMUIMenuItem.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if not FEnabled then
    ATagList.AddTag(MUIA_MenuItem_Enabled, AsTag(FEnabled));
  if FTitle = '-' then
    ATagList.AddTag(MUIA_MenuItem_Title, AsTag(NM_BarLabel))
  else
    ATagList.AddTag(MUIA_MenuItem_Title, AsTag(PChar(FTitle)));
  if FCheckIt then
    ATagList.AddTag(MUIA_MenuItem_CheckIt, AsTag(FCheckIt));
  if FChecked then
    ATagList.AddTag(MUIA_MenuItem_Checked, AsTag(FChecked));
  if FCommandString then
    ATagList.AddTag(MUIA_MenuItem_CommandString, AsTag(FCommandString));
  if FExclude > 0 then
    ATagList.AddTag(MUIA_MenuItem_Exclude, AsTag(FExclude));
  if FShortCut <> '' then
    ATagList.AddTag(MUIA_MenuItem_ShortCut, AsTag(PChar(FShortCut)));
  if FToggle then
    ATagList.AddTag(MUIA_MenuItem_Toggle, AsTag(FToggle));
end;


procedure TMUIMenuItem.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_MenuItem, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function TriggerFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIMenuItem;
begin
  try
    Result := 0;
    PasObj := TMUIMenuItem(Hook^.h_Data);
    if Assigned(PasObj.OnTrigger) then
      PasObj.OnTrigger(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;


procedure TMUIMenuItem.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Menuitem_Trigger, MUIV_EveryTime, @TriggerFunc);
end;

function TMUIMenuItem.GetChecked: Boolean;
begin
  if HasObj then
     FChecked := GetBoolValue(MUIA_MenuItem_Checked);
  Result := FChecked;
end;

procedure TMUIMenuItem.SetChecked(AValue: Boolean);
begin
  if AValue <> Checked then
  begin
    FChecked := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_Checked, FChecked);
  end;
end;

procedure TMUIMenuItem.SetCheckit(AValue: Boolean);
begin
  if AValue <> FCheckIt then
  begin
    FCheckIt := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_CheckIt, FCheckIt);
  end;
end;

procedure TMUIMenuItem.SetCommandString(AValue: Boolean);
begin
  if AValue <> FCommandString then
  begin
    FCommandString := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_CommandString, FCommandString);
  end;
end;

procedure TMUIMenuItem.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_Enabled, FEnabled);
  end;
end;

procedure TMUIMenuItem.SetExclude(AValue: LongWord);
begin
  if AValue <> FExclude then
  begin
    FExclude := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_Exclude, FExclude);
  end;
end;

procedure TMUIMenuItem.SetShortCut(AValue: string);
begin
  if AValue <> FShortCut then
  begin
    FShortCut := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_ShortCut, PChar(FShortCut));
  end;
end;

procedure TMUIMenuItem.SetTitle(AValue: string);
begin
  if AValue <> FTitle then
  begin
    FTitle := AValue;
    if Assigned(FMUIObj) then
    begin
      if FTitle = '-' then
        SetValue(MUIA_MenuItem_Title, NM_BarLabel)
      else
        SetValue(MUIA_MenuItem_Title, PChar(FTitle));
    end;
  end;
end;

procedure TMUIMenuItem.SetToggle(AValue: Boolean);
begin
  if AValue <> FToggle then
  begin
    FToggle := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_MenuItem_Toggle, FToggle);
  end;
end;


end.
