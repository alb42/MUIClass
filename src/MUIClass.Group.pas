unit MUIClass.Group;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, icon, mui, muihelper,
  MUIClass.Base, MUIClass.Area, MUIClass.List, MUIClass.Gadget;
type
  {$M+}
  TMUIGroup = class(TMUIArea)
  private
    FActivePage: Integer;   //   0
    FOnPageChange: TNotifyEvent;
    FColumns: Integer;      //   0
    FHoriz: Boolean;        //*  False
    FHorizSpacing: Integer; //   -1 = default
    FPageMode: Boolean;     //*  False
    FRows: Integer;         //   0
    FSameHeight: Boolean;   //*  False
    FSameWidth: Boolean;    //*  False
    FSpacing: Integer;      //   -1 = default
    FVertSpacing: Integer;  //   -1 = default
    function GetActivePage: Integer;
    procedure SetActivePage(AValue: Integer);
    procedure SetColumns(AValue: Integer);
    procedure SetHoriz(AValue: Boolean);
    procedure SetHorizSpacing(AValue: Integer);
    procedure SetPageMode(AValue: Boolean);
    procedure SetRows(AValue: Integer);
    procedure SetSameHeight(AValue: Boolean);
    procedure SetSameSize(AValue: Boolean);
    function GetSameSize: Boolean;
    procedure SetSameWidth(AValue: Boolean);
    procedure SetSpacing(AValue: Integer);
    procedure SetVertSpacing(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    property Childs;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure InitChange; override;  // When Application is running and you need to add/remove Children
    procedure ExitChange; override;  //   Lock/Unlock the Group before changing
    //procedure Sort; // needs a concept (maybe sort the Childlist, then apply with it)
    // Fields
  published
    property ActivePage: Integer read GetActivePage write SetActivePage default 0;     //  Active Page if PageMode is active (0 <= ActivePage < Childs.Count)
    property Columns: Integer read FColumns write SetColumns default 0;                //  Show Childs in Columns
    property Horiz: Boolean read FHoriz write SetHoriz default False;                  // True = order the Children horizontally, False = order the Children vertically (default)
    property HorizSpacing: Integer read FHorizSpacing write SetHorizSpacing default 0; //  Change Spacing between Children in horizontal direction
    property PageMode: Boolean read FPageMode write SetPageMode default False;         // Activate Pagemode, Childrens should TMUIGroup objects, each a new Page of the Group
    property Rows: Integer read FRows write SetRows default 0;                         // Show Childs in Rows
    property SameHeight: Boolean read FSameHeight write SetSameHeight default False;   // Scale all Children to the same Height
    property SameSize: Boolean read GetSameSize write SetSameSize default False;       // Scale all Children to the same Size (in principle SameHeight + SameWidth)
    property SameWidth: Boolean read FSameWidth write SetSameWidth default False;      // Scale all Children to the same Width
    property Spacing: Integer read FSpacing write SetSpacing default 0;                // Set Spacing between Children
    property VertSpacing: Integer read FVertSpacing write SetVertSpacing default 0;    // Set Spacing between Children in vertical direction
    //
    property OnPageChange: TNotifyEvent read FOnPageChange write FOnPageChange;
  end;

  TMUIListView = class(TMUIGroup)
  private
    FOnClick: TNotifyEvent;
    FDefClickColumn: Integer;       //  -1
    FOnDoubleClick: TNotifyEvent;
    FDragType: Integer;             //  None
    FInput: Boolean;                //* True
    FList: TMUIList;                //* nil
    FMultiSelect: Integer;          //* None
    FScrollerPos: Integer;          //* Default
    FOnSelectChange: TNotifyEvent;

    function GetClickColumn: Integer;
    procedure SetDefClickColumn(AValue: Integer);
    procedure SetDragType(AValue: Integer);
    procedure SetInput(AValue: Boolean);
    procedure SetList(AValue: TMUIList);
    procedure SetMultiSelect(AValue: Integer);
    procedure SetScrollerPos(AValue: Integer);

  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CreateObject; override;


    procedure DestroyObject; override;
    procedure ClearObject; override;

    property ClickColumn: Integer read GetClickColumn;
  published

    property DefClickColumn: Integer read FDefClickColumn write SetDefClickColumn default 0;
    property DragType: Integer read FDragType write SetDragType default MUIV_ListView_DragType_None;                //  MUIV_ListView_DragType_*
    property Input: Boolean read FInput write SetInput default True;                                                //I
    property List: TMUIList read FList write SetList;                                                               //I
    property MultiSelect: Integer read FMultiSelect write SetMultiSelect default MUIV_Listview_MultiSelect_None;    //I MUIV_Listview_MultiSelect_*
    property ScrollerPos: Integer read FScrollerPos write SetScrollerPos default MUIV_Listview_ScrollerPos_Default; //I MUIV_Listview_ScrollerPos_*

    property OnSelectChange: TNotifyEvent read FOnSelectChange write FOnSelectChange;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
  end;

  TMUIRegister = class(TMUIGroup)
  private
    FFrame: Boolean;
    FTitles: TStringArray;
    PCs: array of PChar;
    procedure SetFrame(AValue: Boolean);
    procedure SetTitles(AValue: TStringArray);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Frame: Boolean read FFrame write SetFrame default False; //I
    property Titles: TStringArray read FTitles write SetTitles;       //I
  end;

  TMUIVirtGroup = class(TMUIGroup)
  private
    FInput: Boolean;
    FVTop: Integer;
    FVLeft: Integer;
    procedure SetInput(AValue: Boolean);
    function GetVLeft: Integer;
    procedure SetVLeft(AValue: Integer);
    function GetVTop: Integer;
    procedure SetVTop(AValue: Integer);
    function GetVHeight: Integer;
    function GetVWidth: Integer;
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;

    property VirtHeight: Integer read GetVHeight;
    property VirtWidth: Integer read GetVWidth;
  published
    property VirtLeft: Integer read GetVLeft write SetVLeft default 0; //
    property VirtTop: Integer read GetVTop write SetVTop default 0;    //
    property Input: Boolean read FInput write SetInput default True;   //I
  end;

  TMUIScrollGroup = class(TMUIGroup)
  private
    FFreeHoriz: Boolean;
    FFreeVert: Boolean;
    FContents: TMUIVirtGroup;
    FHorizBar: TMUIScrollBar;
    FVertBar: TMUIScrollBar;
    FUseWinBorder: Boolean;
    procedure SetFreeHoriz(AValue: Boolean);
    procedure SetFreeVert(AValue: Boolean);
    procedure SetUseWinBorder(AValue: Boolean);
  protected
    procedure BeforeCreateObject; override;
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateObject; override;

    procedure DestroyObject; override;
    procedure ClearObject; override;

    property HorizBar: TMUIScrollBar read FHorizBar;
    property VertBar: TMUIScrollBar read FVertBar;
  published
    property Contents: TMUIVirtGroup read FContents;                                       //I
    property FreeHoriz: Boolean read FFreeHoriz write SetFreeHoriz default True;           //I
    property FreeVert: Boolean read FFreeVert write SetFreeVert default True;              //I
    property UseWinBorder: Boolean read FUseWinBorder write SetUseWinBorder default False; //I
  end;

  TMUIRadio = class(TMUIGroup)
  private
    FEntries: TStringArray;
    PCs: array of PChar;
    FActive: Integer;
    FOnActiveChange: TNotifyEvent;
    function GetActive: Integer;
    procedure SetActive(AValue: Integer);
    procedure SetEntries(AValue: TStringArray);

  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Active: Integer read GetActive write SetActive default 0;  //
    property Entries: TStringArray read FEntries write SetEntries;      //I
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

  TMUICycle = class(TMUIGroup)
  private
    FEntries: TStringArray;
    PCs: array of PChar;
    FActive: Integer;
    FOnActiveChange: TNotifyEvent;
    function GetActive: Integer;
    procedure SetActive(AValue: Integer);
    procedure SetEntries(AValue: TStringArray);

  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Active: Integer read GetActive write SetActive default 0; //
    property Entries: TStringArray read FEntries write SetEntries;     //I
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
  end;

  TMUIColorAdjust = class(TMUIGroup)
  private
    FRed: LongWord;
    FRedSet: Boolean;
    FGreen: LongWord;
    FGreenSet: Boolean;
    FBlue: LongWord;
    FBlueSet: Boolean;
    FModeID: LongWord;
    FModeIDSet: Boolean;
    FRGB: PLongWord;
    FOnColorChange: TNotifyEvent;
    function GetRed: LongWord;
    procedure SetRed(AValue: LongWord);
    function GetGreen: LongWord;
    procedure SetGreen(AValue: LongWord);
    function GetBlue: LongWord;
    procedure SetBlue(AValue: LongWord);
    procedure SetModeID(AValue: LongWord);
    function GetRGB: PLongWord;
    procedure SetRGB(AValue: PLongWord);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property RGB: PLongWord read GetRGB write SetRGB;
  published
    property Red: LongWord read GetRed write SetRed default 0;
    property Green: LongWord read GetGreen write SetGreen default 0;
    property Blue: LongWord read GetBlue write SetBlue default 0;
    property ModeID: LongWord read FModeID write SetModeID default 0;
    //
    property OnColorChange: TNotifyEvent read FOnColorChange write FOnColorChange;
  end;

  TMUIPalette = class(TMUIArea)
  private
    FEntries: PMUI_Palette_Entry;
    FGroupable: Boolean;
    FNames: TStringArray;
    PCs: array of PChar;
    procedure SetEntries(AValue: PMUI_Palette_Entry);
    procedure SetGroupable(AValue: Boolean);
    procedure SetNames(AVAlue: TStringArray);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Entries: PMUI_Palette_Entry read FEntries write SetEntries; //I
  published
    property Groupable: Boolean read FGroupable write SetGroupable;      //
    property Names: TStringArray read FNames write SetNames;             //
  end;

implementation

{ TMUIGroup }

constructor TMUIGroup.Create;
begin
  inherited;
  Frame := MUIV_Frame_Group;
  //
  FActivePage := 0;
  FColumns := 0;
  FHoriz := False;
  FHorizSpacing := -1;
  PageMode := False;
  FSameHeight := False;
  FSameWidth := False;
  FSpacing := -1;
  FVertSpacing := -1;
end;

procedure TMUIGroup.GetCreateTags(var ATagList: TATagList);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Childs.Count - 1 do
  begin
    if Childs[i].HasObj then
      ATagList.AddTag(MUIA_Group_Child, AsTag(Childs[i].MUIObj))
  end;
  FActivePage := Max(0, Min(Childs.Count - 1, FActivePage));
  if FActivePage > 0 then
    ATagList.AddTag(MUIA_Group_ActivePage, AsTag(FActivePage));
  if FColumns > 0 then
    ATagList.AddTag(MUIA_Group_Columns, AsTag(FColumns));
  if FHoriz then
    ATagList.AddTag(MUIA_Group_Horiz, AsTag(FHoriz));
  if FHorizSpacing >= 0 then
    ATagList.AddTag(MUIA_Group_HorizSpacing, AsTag(FHorizSpacing));
  if FPageMode then
    ATagList.AddTag(MUIA_Group_PageMode, AsTag(FPageMode));
  if FSameHeight then
    ATagList.AddTag(MUIA_Group_SameHeight, AsTag(FSameHeight));
  if FSameWidth then
    ATagList.AddTag(MUIA_Group_SameWidth, AsTag(FSameWidth));
  if FSpacing >= 0 then
    ATagList.AddTag(MUIA_Group_Spacing, AsTag(FSpacing));
  if FVertSpacing >= 0 then
    ATagList.AddTag(MUIA_Group_VertSpacing, AsTag(FVertSpacing));

end;

procedure TMUIGroup.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Group, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function PageFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIGroup;
begin
  try
    Result := 0;
    PasObj := TMUIGroup(Hook^.h_Data);
    if Assigned(PasObj.FOnPageChange) then
      PasObj.FOnPageChange(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

procedure TMUIGroup.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Group_ActivePage, MUIV_EveryTime, @PageFunc);
end;

function TMUIGroup.GetActivePage: Integer;
begin
  Result := FActivePage;
  if HasObj then
    Result := GetIntValue(MUIA_Group_ActivePage);
end;

procedure TMUIGroup.SetActivePage(AValue: Integer);
begin
  if AValue <> ActivePage then
  begin
    FActivePage := Max(0, Min(Childs.Count - 1, AValue));
    if Assigned(FMUIObj) then
    begin
      SetValue(MUIA_Group_ActivePage, FActivePage);
      FActivePage := GetIntValue(MUIA_Group_ActivePage);
    end;
  end;
end;

procedure TMUIGroup.SetColumns(AValue: Integer);
begin
  if AValue <> FColumns then
  begin
    FColumns := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Group_Columns, FColumns);
  end;
end;

procedure TMUIGroup.SetHoriz(AValue: Boolean);
begin
  if AValue <> FHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Group_Horiz', BoolToStr(AValue, True))
    else
      FHoriz := AValue;
  end;
end;

procedure TMUIGroup.SetHorizSpacing(AValue: Integer);
begin
  if AValue <> FHorizSpacing then
  begin
    FHorizSpacing := AValue;
    if Assigned(FMUIObj) then
    begin
      SetValue(MUIA_Group_HorizSpacing, FHorizSpacing);
      FHorizSpacing := GetIntValue(MUIA_Group_HorizSpacing);
    end;
  end;
end;

procedure TMUIGroup.SetPageMode(AValue: Boolean);
begin
  if AValue <> FPageMode then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Group_PageMode', BoolToStr(AValue, True))
    else
      FPageMode := AValue;
  end;
end;

procedure TMUIGroup.SetRows(AValue: Integer);
begin
  if AValue <> FRows then
  begin
    FRows := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Group_Rows, FRows);
  end;
end;

procedure TMUIGroup.SetSameHeight(AValue: Boolean);
begin
  if AValue <> FSameHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Group_SameHeight', BoolToStr(AValue, True))
    else
      FSameHeight := AValue;
  end;
end;

procedure TMUIGroup.SetSameSize(AValue: Boolean);
begin
  if AValue <> (FSameHeight and FSameWidth) then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Group_SameSize', BoolToStr(AValue, True))
    else
    begin
      FSameWidth := AValue;
      FSameHeight := AValue;
    end;
  end;
end;

function TMUIGroup.GetSameSize: Boolean;
begin
  Result := FSameHeight and FSameWidth;
end;

procedure TMUIGroup.SetSameWidth(AValue: Boolean);
begin
  if AValue <> FSameWidth then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Group_SameWidth', BoolToStr(AValue, True))
    else
      FSameWidth := AValue;
  end;
end;

procedure TMUIGroup.SetSpacing(AValue: Integer);
begin
  if AValue <> FSpacing then
  begin
    FSpacing := AValue;
    if Assigned(FMUIObj) then
    begin
      SetValue(MUIA_Group_Spacing, FSpacing);
    end;
  end;
end;

procedure TMUIGroup.SetVertSpacing(AValue: Integer);
begin
  if AValue <> FVertSpacing then
  begin
    FVertSpacing := AValue;
    if Assigned(FMUIObj) then
    begin
      SetValue(MUIA_Group_VertSpacing, FVertSpacing);
      FVertSpacing := GetIntValue(MUIA_Group_VertSpacing);
    end;
  end;
end;

procedure TMUIGroup.InitChange;
begin
  inherited;
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Group_InitChange]);
end;

procedure TMUIGroup.ExitChange;
begin
  inherited;
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Group_ExitChange]);
end;

{ TMUIListView }

constructor TMUIListView.Create;
begin
  inherited;
  Frame := MUIV_Frame_InputList;
  FDefClickColumn := -1;
  FDragType := MUIV_Listview_DragType_None;
  FInput := True;
  FList := nil;
  FMultiSelect := MUIV_Listview_MultiSelect_None;
  FScrollerPos := MUIV_Listview_ScrollerPos_Default;

end;

destructor TMUIListView.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TMUIListView.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FDefClickColumn >= 0 then
    ATagList.AddTag(MUIA_ListView_DefClickColumn, AsTag(FDefClickColumn));
  if FDragType <> MUIV_Listview_DragType_None then
    ATagList.AddTag(MUIA_ListView_DragType, AsTag(FDragType));
  if not FInput then
    ATagList.AddTag(MUIA_ListView_Input, AsTag(FInput));
  if FMultiSelect <> MUIV_Listview_MultiSelect_None then
    ATagList.AddTag(MUIA_ListView_MultiSelect, AsTag(FMultiSelect));

  if FScrollerPos <> MUIV_Listview_ScrollerPos_Default then
    ATagList.AddTag(MUIA_ListView_ScrollerPos, AsTag(FScrollerPos));
  if Assigned(FList) then
  begin
    if not FList.HasObj then
      FList.CreateObject;
    if FList.HasObj then
      ATagList.AddTag(MUIA_ListView_List, AsTag(FList.MUIObj));
  end;
end;

procedure TMUIListView.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_ListView, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIListView.DestroyObject;
begin
  inherited;
  if Assigned(FList) then
    FList.ClearObject;
end;

procedure TMUIListView.ClearObject;
begin
  inherited;
  if Assigned(FList) then
    FList.ClearObject;
end;

function ColumnClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIListView;
begin
  try
    Result := 0;
    PasObj := TMUIListView(Hook^.h_Data);
    if Assigned(PasObj.FOnClick) then
      PasObj.FOnClick(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function DoubleClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIListView;
begin
  try
    Result := 0;
    PasObj := TMUIListView(Hook^.h_Data);
    if Assigned(PasObj.FOnDoubleClick) then
      PasObj.FOnDoubleClick(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function SelectChangeFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIListView;
begin
  try
    Result := 0;
    PasObj := TMUIListView(Hook^.h_Data);
    if Assigned(PasObj.FOnSelectChange) then
      PasObj.FOnSelectChange(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;


procedure TMUIListView.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_ListView_ClickColumn, MUIV_EveryTime, @ColumnClickFunc);
  ConnectHook(MUIA_ListView_DoubleClick, MUI_TRUE, @DoubleClickFunc);
  ConnectHook(MUIA_ListView_SelectChange, MUI_TRUE, @SelectChangeFunc);
end;

procedure TMUIListView.SetScrollerPos(AValue: Integer);
begin
  if AValue <> FScrollerPos then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ListView_ScrollerPos', IntToStr(AValue))
    else
      FScrollerPos := AValue;
  end;
end;

procedure TMUIListView.SetList(AValue: TMUIList);
begin
  if AValue <> FList then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ListView_List', HexStr(AValue))
    else
      FList := AValue;
  end;
end;

function TMUIListView.GetClickColumn: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_ListView_ClickColumn);
end;

procedure TMUIListView.SetDefClickColumn(AValue: Integer);
begin
  if AValue <> FDefClickColumn then
  begin
    FDefClickColumn := AValue;
    if HasObj then
      SetValue(MUIA_ListView_DefClickColumn, AsTag(FDefClickColumn));
  end;
end;

procedure TMUIListView.SetDragType(AValue: Integer);
begin
  if AValue <> FDragType then
  begin
    FDragType := AValue;
    if HasObj then
      SetValue(MUIA_ListView_DragType, AsTag(FDragType));
  end;
end;

procedure TMUIListView.SetInput(AValue: Boolean);
begin
  if AValue <> FInput then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ListView_Input', BoolToStr(AValue, True))
    else
      FInput := AValue;
  end;
end;

procedure TMUIListView.SetMultiSelect(AValue: Integer);
begin
  if AValue <> FMultiSelect then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ListView_MultiSelect', IntToStr(AValue))
    else
      FMultiSelect := AValue;
  end;
end;

{ TMUIRegister }

constructor TMUIRegister.Create;
begin
  inherited;
  FFrame := False;
  SetLength(FTitles, 0);
end;

var
  EmptyStr: string = ' ';

procedure TMUIRegister.GetCreateTags(var ATagList: TATagList);
var
  I: Integer;
begin
  inherited;
  if FFrame then
    ATagList.AddTag(MUIA_Register_Frame, AsTag(FFrame));
  if Length(Titles) > 0 then
  begin
    SetLength(PCs, Length(FTitles) + 1);
    for i := 0 to High(FTitles) do
      PCs[i] := PChar(FTitles[i]);
    PCs[High(PCs)] := nil;
    ATagList.AddTag(MUIA_Register_Titles, AsTag(@(PCs[0])));
  end
  else
  begin
    SetLength(PCs, 2);
    PCs[0] := PChar(EmptyStr);
    PCs[1] := nil;
    ATagList.AddTag(MUIA_Register_Titles, AsTag(@(PCs[0])));
  end;

end;

procedure TMUIRegister.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Register, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIRegister.SetFrame(AValue: Boolean);
begin
  if AValue <> FFrame then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Register_Frame', BoolToStr(AValue, True))
    else
      FFrame := AValue;
  end;
end;

procedure TMUIRegister.SetTitles(AValue: TStringArray);
begin
  if Assigned(FMUIObj) then
    ComplainIOnly(Self, 'MUIA_Register_Frame', '<string array>')
  else
    FTitles := Copy(AValue);
end;

{ TMUIVirtGroup }

constructor TMUIVirtGroup.Create;
begin
  inherited;
  FInput := False;
end;

procedure TMUIVirtGroup.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FInput then
    ATagList.AddTag(MUIA_VirtGroup_Input, AsTag(FInput));
end;

procedure TMUIVirtGroup.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_VirtGroup, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function TMUIVirtGroup.GetVLeft: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_Virtgroup_Left);
end;

procedure TMUIVirtGroup.SetVLeft(AValue: Integer);
begin
  if AValue <> FVLeft then
  begin
    FVLeft := AValue;
    if HasObj then
      SetValue(MUIA_Virtgroup_Left, AsTag(FVLeft));
  end;
end;

function TMUIVirtGroup.GetVTop: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_Virtgroup_Top);
end;

procedure TMUIVirtGroup.SetVTop(AValue: Integer);
begin
  if AValue <> FVTop then
  begin
    FVTop := AValue;
    if HasObj then
      SetValue(MUIA_Virtgroup_Top, AsTag(FVTop));
  end;
end;

function TMUIVirtGroup.GetVHeight: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_Virtgroup_Height);
end;

function TMUIVirtGroup.GetVWidth: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_Virtgroup_Width);
end;

procedure TMUIVirtGroup.SetInput(AValue: Boolean);
begin
  if AValue <> FInput then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_VirtGroup_Input', BoolToStr(AValue, True))
    else
      FInput := AValue;
  end;
end;

{ TMUIScrollGroup }

constructor TMUIScrollGroup.Create;
begin
  inherited;
  FContents := TMUIVirtGroup.Create;
  FFreeHoriz := True;
  FFreeVert := True;
  FHorizBar := TMUIScrollBar.Create;
  FVertBar := TMUIScrollBar.Create;
end;

destructor TMUIScrollGroup.Destroy;
begin
  FContents.Free;
  FHorizBar.ClearObject;
  FHorizBar.Free;
  FVertBar.ClearObject;
  FVertBar.Free;
  inherited;
end;

procedure TMUIScrollGroup.BeforeCreateObject;
begin
  inherited;
  FContents.CreateObject;
end;

procedure TMUIScrollGroup.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FContents.HasObj then
    ATagList.AddTag(MUIA_ScrollGroup_Contents, AsTag(FContents.MUIObj));
  if not FFreeHoriz then
    ATagList.AddTag(MUIA_ScrollGroup_FreeHoriz, AsTag(FFreeHoriz));
  if not FFreeVert then
    ATagList.AddTag(MUIA_ScrollGroup_FreeVert, AsTag(FFreeVert));
end;

procedure TMUIScrollGroup.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_ScrollGroup, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIScrollGroup.AfterCreateObject;
begin
  inherited;
  if Assigned(FHorizBar) then
    FHorizBar.MUIObj := GetPointerValue(MUIA_Scrollgroup_HorizBar);
  if Assigned(FVertBar) then
    FVertBar.MUIObj := GetPointerValue(MUIA_Scrollgroup_VertBar);
end;

procedure TMUIScrollGroup.DestroyObject;
begin
  inherited;
  if Assigned(FContents) then
    FContents.ClearObject;
  if Assigned(FHorizBar) then
    FHorizBar.ClearObject;
  if Assigned(FVertBar) then
    FHorizBar.ClearObject;
end;

procedure TMUIScrollGroup.ClearObject;
begin
  inherited;
  if Assigned(FContents) then
    FContents.ClearObject;
  if Assigned(FHorizBar) then
    FHorizBar.ClearObject;
  if Assigned(FVertBar) then
    FHorizBar.ClearObject;
end;

procedure TMUIScrollGroup.SetFreeHoriz(AValue: Boolean);
begin
  if AValue <> FFreeHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ScrollGroup_FreeHoriz', BoolToStr(AValue, True))
    else
      FFreeHoriz := AValue;
  end;
end;

procedure TMUIScrollGroup.SetFreeVert(AValue: Boolean);
begin
  if AValue <> FFreeVert then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ScrollGroup_FreeVert', BoolToStr(AValue, True))
    else
      FFreeVert := AValue;
  end;
end;

procedure TMUIScrollGroup.SetUseWinBorder(AValue: Boolean);
begin
  if AValue <> FUseWinBorder then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_ScrollGroup_UseWinBorder', BoolToStr(AValue, True))
    else
      FUseWinBorder := AValue;
  end;
end;

{ TMUIRadio }

constructor TMUIRadio.Create;
begin
  inherited;
  SetLength(FEntries, 0);
end;

procedure TMUIRadio.GetCreateTags(var ATagList: TATagList);
var
  I: Integer;
begin
  inherited;
  if Length(FEntries) > 0 then
  begin
    SetLength(PCs, Length(FEntries) + 1);
    for i := 0 to High(FEntries) do
      PCs[i] := PChar(FEntries[i]);
    PCs[High(PCs)] := nil;
    ATagList.AddTag(MUIA_Radio_Entries, AsTag(@(PCs[0])));
  end;
end;

procedure TMUIRadio.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Radio, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function RadioFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIRadio;
begin
  try
    Result := 0;
    PasObj := TMUIRadio(Hook^.h_Data);
    if Assigned(PasObj.FOnActiveChange) then
      PasObj.FOnActiveChange(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

procedure TMUIRadio.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Radio_Active, MUIV_EveryTime, @RadioFunc);
end;


function TMUIRadio.GetActive: Integer;
begin
  Result := FActive;
  if HasObj then
    Result := GetIntValue(MUIA_Radio_Active);
end;

procedure TMUIRadio.SetActive(AValue: Integer);
begin
  if AValue <> Active then
  begin
    FActive := AValue;
    if HasObj then
      SetValue(MUIA_Radio_Active, AsTag(FActive));
  end;
end;

procedure TMUIRadio.SetEntries(AValue: TStringArray);
begin
  if AValue <> FEntries then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Radio_Entries', '<string array>')
    else
      FEntries := Copy(AValue);
  end;
end;

{ TMUICycle }

constructor TMUICycle.Create;
begin
  inherited;
  SetLength(FEntries, 0);
  Frame := MUIV_Frame_Button;
  Font := MUIV_Font_Button;
  CycleChain := 1;
end;

procedure TMUICycle.GetCreateTags(var ATagList: TATagList);
var
  I: Integer;
begin
  inherited;
  if Length(FEntries) > 0 then
  begin
    SetLength(PCs, Length(FEntries) + 1);
    for i := 0 to High(FEntries) do
      PCs[i] := PChar(FEntries[i]);
    PCs[High(PCs)] := nil;
    ATagList.AddTag(MUIA_Cycle_Entries, AsTag(@(PCs[0])));
  end;
  if FActive > 0 then
    ATagList.AddTag(MUIA_Cycle_Active, AsTag(Min(Integer(FActive), Integer(High(FEntries)))));
end;

procedure TMUICycle.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Cycle, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function CycleFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUICycle;
begin
  try
    Result := 0;
    PasObj := TMUICycle(Hook^.h_Data);
    if Assigned(PasObj.FOnActiveChange) then
      PasObj.FOnActiveChange(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

procedure TMUICycle.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Cycle_Active, MUIV_EveryTime, @CycleFunc);
end;


function TMUICycle.GetActive: Integer;
begin
  Result := FActive;
  if HasObj then
    Result := GetIntValue(MUIA_Cycle_Active);
end;

procedure TMUICycle.SetActive(AValue: Integer);
begin
  if AValue <> Active then
  begin
    FActive := AValue;
    if HasObj then
      SetValue(MUIA_Cycle_Active, AsTag(FActive));
  end;
end;

procedure TMUICycle.SetEntries(AValue: TStringArray);
begin
  if AValue <> FEntries then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Cycle_Entries', '<string array>')
    else
      FEntries := Copy(AValue);
  end;
end;

{ TMUIColorAdjust }

constructor TMUIColorAdjust.Create;
begin
  inherited;
  FRed := 0;
  FRedSet := False;
  FGreen := 0;
  FGreenSet := False;
  FBlue := 0;
  FBlueSet := False;
  FModeID := 0;
  FModeIDSet := False;
  FRGB := nil;
end;

procedure TMUIColorAdjust.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FRedSet then
    ATagList.AddTag(MUIA_ColorAdjust_Red, AsTag(FRed));
  if FGreenSet then
    ATagList.AddTag(MUIA_ColorAdjust_Green, AsTag(FGreen));
  if FBlueSet then
    ATagList.AddTag(MUIA_ColorAdjust_Blue, AsTag(FBlue));
  if FModeIDSet then
    ATagList.AddTag(MUIA_ColorAdjust_ModeID, AsTag(FModeID));
  if Assigned(FRGB) then
    ATagList.AddTag(MUIA_ColorAdjust_RGB, AsTag(FRGB));
end;

procedure TMUIColorAdjust.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_ColorAdjust, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function ColFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIColorAdjust;
begin
  try
    Result := 0;
    PasObj := TMUIColorAdjust(Hook^.h_Data);
    if Assigned(PasObj.FOnColorChange) then
      PasObj.FOnColorChange(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

procedure TMUIColorAdjust.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_ColorAdjust_RGB, MUIV_EveryTime, @ColFunc);
end;

function TMUIColorAdjust.GetRed: LongWord;
begin
  Result := FRed;
  if HasObj then
    Result := LongWord(GetIntValue(MUIA_Coloradjust_Red));
end;

procedure TMUIColorAdjust.SetRed(AValue: LongWord);
begin
  FRed := AValue;
  FRedSet := True;
  if HasObj then
    SetValue(MUIA_ColorAdjust_Red, AsTag(FRed));
end;

function TMUIColorAdjust.GetGreen: LongWord;
begin
  Result := FGreen;
  if HasObj then
    Result := LongWord(GetIntValue(MUIA_Coloradjust_Green));
end;


procedure TMUIColorAdjust.SetGreen(AValue: LongWord);
begin
  FGreen := AValue;
  FGreenSet := True;
  if HasObj then
    SetValue(MUIA_ColorAdjust_Green, AsTag(FGreen));
end;

function TMUIColorAdjust.GetBlue: LongWord;
begin
  Result := FBlue;
  if HasObj then
    Result := LongWord(GetIntValue(MUIA_Coloradjust_Blue));
end;

procedure TMUIColorAdjust.SetBlue(AValue: LongWord);
begin
  FBlue := AValue;
  FBlueSet := True;
  if HasObj then
    SetValue(MUIA_ColorAdjust_Blue, AsTag(FBlue));
end;

procedure TMUIColorAdjust.SetModeID(AValue: LongWord);
begin
  FModeID := AValue;
  FModeIDSet := True;
  if HasObj then
    SetValue(MUIA_ColorAdjust_ModeID, AsTag(FModeID));
end;

function TMUIColorAdjust.GetRGB: PLongWord;
begin
  Result := FRGB;
  if HasObj then
    Result := GetPointerValue(MUIA_Coloradjust_RGB);
end;


procedure TMUIColorAdjust.SetRGB(AValue: PLongWord);
begin
  if AValue <> FRGB then
  begin
    FRGB := AValue;
    if HasObj then
      SetValue(MUIA_ColorAdjust_RGB, AsTag(FRGB));
  end;
end;

{ TMUIPalette }

constructor TMUIPalette.Create;
begin
  inherited;
  FEntries := nil;
  FGroupable := True;
end;

procedure TMUIPalette.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if Assigned(FEntries) then
    ATagList.AddTag(MUIA_Palette_Entries, AsTag(FEntries));
  if not FGroupable then
    ATagList.AddTag(MUIA_Palette_Groupable, AsTag(FGroupable));
end;

procedure TMUIPalette.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Palette, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIPalette.SetEntries(AValue: PMUI_Palette_Entry);
begin
  if AValue <> FEntries then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Palette_Entries', HexStr(AValue))
    else
      FEntries := AValue;
  end;
end;

procedure TMUIPalette.SetGroupable(AValue: Boolean);
begin
  if AValue <> FGroupable then
  begin
    FGroupable := AValue;
    if HasObj then
      SetValue(MUIA_Palette_Groupable, AsTag(FGroupable));
  end;
end;

procedure TMUIPalette.SetNames(AValue: TStringArray);
var
  i: Integer;
begin
  FNames := Copy(AValue);
  if HasObj then
  begin
    SetLength(PCs, Length(FNames) + 1);
    for i := 0 to High(FNames) do
      PCs[i] := PChar(FNames[i]);
    PCs[High(PCs)] := nil;
    SetValue(MUIA_Palette_Names, AsTag(@(PCs[0])));
  end;
end;

end.
