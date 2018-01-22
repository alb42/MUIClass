unit MUIClass.Group;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area, MUIClass.List;

type
  TMUIGroup = class(TMUIArea)
  private
    FActivePage: Integer;   //   0
    FColumns: Integer;      //   0
    FHoriz: Boolean;        //*  False
    FHorizSpacing: Integer; //   -1 = default
    FPageMode: Boolean;     //*  False
    FRows: Integer;         //   0
    FSameHeight: Boolean;   //*  False
    FSameWidth: Boolean;    //*  False
    FSpacing: Integer;      //   -1 = default
    FVertSpacing: Integer;  //   -1 = default
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
  public
    property Childs;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure InitChange;  // When Application is running and you need to add/remove Children
    procedure ExitChange;  //   Lock/Unlock the Group before changing
    //procedure Sort; // needs a concept (maybe sort the Childlist, then apply with it)
    // Fields
    property ActivePage: Integer read FActivePage write SetActivePage;         // Active Page if PageMode is active (0 <= ActivePage < Childs.Count)
    property Columns: Integer read FColumns write SetColumns;                  // Show Childs in Columns
    property Horiz: Boolean read FHoriz write SetHoriz;                        // True = order the Children horizontally, False = order the Children vertically (default)
    property HorizSpacing: Integer read FHorizSpacing write SetHorizSpacing;   // Change Spacing between Children in horizontal direction
    property PageMode: Boolean read FPageMode write SetPageMode;               // Activate Pagemode, Childrens should TMUIGroup objects, each a new Page of the Group
    property Rows: Integer read FRows write SetRows;                           // Show Childs in Rows
    property SameHeight: Boolean read FSameHeight write SetSameHeight;         // Scale all Children to the same Height
    property SameSize: Boolean read GetSameSize write SetSameSize;             // Scale all Children to the same Size (in principle SameHeight + SameWidth)
    property SameWidth: Boolean read FSameWidth write SetSameWidth;            // Scale all Children to the same Width
    property Spacing: Integer read FSpacing write SetSpacing;                  // Set Spacing between Children
    property VertSpacing: Integer read FVertSpacing write SetVertSpacing;      // Set Spacing between Children in vertical direction
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

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CreateObject; override;
    procedure AfterCreateObject; override;

    procedure DestroyObject; override;
    procedure ClearObject; override;

    property ClickColumn: Integer read GetClickColumn;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property DefClickColumn: Integer read FDefClickColumn write SetDefClickColumn;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
    property DragType: Integer read FDragType write SetDragType;                        // MUIV_ListView_DragType_*
    property Input: Boolean read FInput write SetInput;
    property List: TMUIList read FList write SetList;
    property MultiSelect: Integer read FMultiSelect write SetMultiSelect;               // MUIV_Listview_MultiSelect_*
    property ScrollerPos: Integer read FScrollerPos write SetScrollerPos;               // MUIV_Listview_ScrollerPos_*
    property OnSelectChange: TNotifyEvent read FOnSelectChange write FOnSelectChange;
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

procedure TMUIGroup.SetActivePage(AValue: Integer);
begin
  if AValue <> FActivePage then
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
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Group_InitChange]);
end;

procedure TMUIGroup.ExitChange;
begin
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
  Result := 0;
  PasObj := TMUIListView(Hook^.h_Data);
  if Assigned(PasObj.FOnClick) then
    PasObj.FOnClick(PasObj);
end;

function DoubleClickFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIListView;
begin
  Result := 0;
  PasObj := TMUIListView(Hook^.h_Data);
  if Assigned(PasObj.FOnDoubleClick) then
    PasObj.FOnDoubleClick(PasObj);
end;

function SelectChangeFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIListView;
begin
  Result := 0;
  PasObj := TMUIListView(Hook^.h_Data);
  if Assigned(PasObj.FOnSelectChange) then
    PasObj.FOnSelectChange(PasObj);
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
    begin
      SetValue(MUIA_ListView_DragType, AsTag(FDragType));
    end;
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



end.
