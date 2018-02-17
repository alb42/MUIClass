unit MUIClass.List;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, agraphics, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area, MUIClass.Image;
{$M+}
type
  TDisplayEvent = procedure(Sender: TObject; ToPrint: PPChar; Entry: PChar) of object;
  TCompareEvent = function(Sender: TObject; Entry1, Entry2: PChar): Integer of object;
  TConstructEvent = function(Sender: TObject; Pool: Pointer; Str: PChar): PChar of object;
  TDestructEvent = procedure(Sender: TObject; Pool: Pointer; Entry: PChar) of object;
  TMultiTestEvent = function(Sender: TObject; Entry: PChar): Boolean of object;

  TListImage = record
  end;
  PListImage = ^TListImage;
  TStringArray = array of string;
  TMUIList = class(TMUIArea)
  private
    DisplayHook: THook;
    CompareHook: THook;
    ConstructHook: THook;
    DestructHook: THook;
    MultiTestHook: THook;
    //
    FActive: Integer;
    FAdjustHeight: Boolean;
    FAdjustWidth: Boolean;
    FAutoVisible: Boolean;
    FDragSortable: Boolean;
    FOnDrop: TNotifyEvent;
    FFormat: string;
    FMinLineHeight: Integer;
    FQuiet: Boolean;
    FShowDropMarks: Boolean;
    FSourceArray: array of PChar;
    FSourceStrings: TStringArray;
    FTitle: string;
    FPool: Pointer;
    FPoolPuddleSize: LongWord;
    FPoolThreshSize: LongWord;

    FOnDisplay: TDisplayEvent;
    FOnCompare: TCompareEvent;
    FOnConstruct: TConstructEvent;
    FOnDestruct: TDestructEvent;
    FOnMultiTest: TMultiTestEvent;
    FOnActiveChange: TNotifyEvent;

    function GetActive: Integer;
    procedure SetActive(AValue: Integer);
    procedure SetAdjustHeight(AValue: Boolean);
    procedure SetAdjustWidth(AValue: Boolean);
    procedure SetAutoVisible(AValue: Boolean);
    procedure SetDragSortable(AValue: Boolean);
    function GetDropMark: Integer;
    function GetEntries: Integer;
    function GetFirst: Integer;
    procedure SetFormat(AValue: string);
    function GetInsertPosition: Integer;
    procedure SetMinLineHeight(AValue: Integer);
    procedure SetQuiet(AValue: Boolean);
    procedure SetShowDropMarks(AValue: Boolean);
    procedure SetSourceArray(AValue: TStringArray);
    procedure SetTitle(AValue: string);
    function GetVisible: Integer;
    procedure SetPool(AValue: Pointer);
    procedure SetPoolPuddleSize(AValue: LongWord);
    procedure SetPoolThreshSize(AValue: LongWord);

    procedure SetOnDisplay(AValue: TDisplayEvent);
    procedure SetOnCompare(AValue: TCompareEvent);
    procedure SetOnConstruct(AValue: TConstructEvent);
    procedure SetOnDestruct(AValue: TDestructEvent);
    procedure SetOnMultiTest(AValue: TMultiTestEvent);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
    // Method
    function CreateImage(Bitmap: TMUIBitmap; Flags: Longword): PListImage;
    procedure DeleteImage(Img: PListImage);
    class function ListImageToString(Img: PListImage): string;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure Exchange(Pos1, Pos2: Integer);
    function GetEntry(Pos: Integer): Pointer;
    procedure Insert(Entries: PPChar; Count: Integer; Pos: Integer);
    procedure InsertSingle(Entry: PChar; Pos: Integer);
    procedure Jump(Pos: Integer);
    procedure Move(FromPos, ToPos: Integer);
    procedure NextSelected(var Pos: Integer);
    procedure Redraw(Pos: Integer);
    procedure Remove(Pos: Integer);
    procedure Select(Pos, SelType: Integer; var State: Integer);
    procedure Sort;
    function TestPos(x, y: Integer): TMUI_List_TestPos_Result;
    // Fields
    property DropMark: Integer read GetDropMark;
    property Entries: Integer read GetEntries;
    property First: Integer read GetFirst;
    property InsertPosition: Integer read GetInsertPosition;
    property Pool: Pointer read FPool write SetPool;                              //I
    property PoolPuddleSize: LongWord read FPoolPuddleSize write FPoolPuddleSize; //I
    property PoolThreshSize: LongWord read FPoolThreshSize write FPoolThreshSize; //I
    property Quiet: Boolean read FQuiet write SetQuiet;
    property Visible: Integer read GetVisible;
  published
    property Active: Integer read GetActive write SetActive default 0;                     //
    property AdjustHeight: Boolean read FAdjustHeight write SetAdjustHeight default False; //I
    property AdjustWidth: Boolean read FAdjustWidth write SetAdjustWidth default False;    //I
    property AutoVisible: Boolean read FAutoVisible write SetAutoVisible default False;    //I
    property DragSortable: Boolean read FDragSortable write SetDragSortable default False; //
    property Format: string read FFormat write SetFormat;                                  //
    property MinLineHeight: Integer read FMinLineHeight write SetMinLineHeight default 0;  //I
    property ShowDropMarks: Boolean read FShowDropMarks write SetShowDropMarks default True;
    property SourceStrings: TStringArray read FSourceStrings write SetSourceArray;         //I
    property Title: string read FTitle write SetTitle;                                     // #1 for multicolumn lists -> DisplayHook will be called
    //Events
    property OnActiveChange: TNotifyEvent read FOnActiveChange write FOnActiveChange;
    property OnDisplay: TDisplayEvent read FOnDisplay write SetOnDisplay;
    property OnCompare: TCompareEvent read FOnCompare write SetOnCompare;
    property OnConstruct: TConstructEvent read FOnConstruct write SetOnConstruct;
    property OnDestruct: TDestructEvent read FOnDestruct write SetOnDestruct;
    property OnMultiTest: TMultiTestEvent read FOnMultiTest write SetOnMultiTest;
    property OnDrop: TNotifyEvent read FOnDrop write FOnDrop;
  end;

  TMUIFloatText = class(TMUIList)
  private
    FJustify: Boolean;
    FSkipChars: string;
    FTabSize: Integer;
    FText: string;
    procedure SetJustify(AValue: Boolean);
    procedure SetSkipChars(AValue: string);
    procedure SetTabSize(AValue: Integer);
    procedure SetText(AValue: string);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Justify: Boolean read FJustify write SetJustify default False;
    property SkipChars: string read FSkipChars write SetSkipChars;
    property TabSize: Integer read FTabSize write SetTabSize default 8;
    property Text: string read FText write SetText;
  end;


  TMUIVolumeList = class(TMUIList)
  public
    procedure CreateObject; override;
  end;

  TMUIDirList = class(TMUIList)
  private
    FAcceptPattern: string;
    FDirectory: string;
    FDrawersOnly: Boolean;
    FFilesOnly: Boolean;
    FFilterDrawers: Boolean;
    FMultiSelDirs: Boolean;
    FRejectIcons: Boolean;
    FRejectPattern: string;
    FSortDirs: Integer;
    FOnStatusValid: TNotifyEvent;
    FOnStatusInvalid: TNotifyEvent;

    procedure SetAcceptPattern(AValue: string);
    procedure SetDirectory(AValue: string);
    procedure SetDrawersOnly(AValue: Boolean);
    procedure SetFilesOnly(AValue: Boolean);
    procedure SetFilterDrawers(AValue: Boolean);
    procedure SetMultiSelDirs(AValue: Boolean);
    function GetNumBytes: Integer;
    function GetNumDrawers: Integer;
    function GetNumFiles: Integer;
    function GetPath: string;
    procedure SetRejectIcons(AValue: Boolean);
    procedure SetRejectPattern(AValue: string);
    procedure SetSortDirs(AValue: Integer);
    function GetStatus: Integer;
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure ReRead;
    // Fields
    property NumBytes: Integer read GetNumBytes;
    property NumDrawers: Integer read GetNumDrawers;
    property NumFiles: Integer read GetNumFiles;
    property Path: string read GetPath;
    property Status: Integer read GetStatus;
  published
    property AcceptPattern: string read FAcceptPattern write SetAcceptPattern;
    property Directory: string read FDirectory write SetDirectory;
    property DrawersOnly: Boolean read FDrawersOnly write SetDrawersOnly default False;
    property FilesOnly: Boolean read FFilesOnly write SetFilesOnly default False;
    property FilterDrawers: Boolean read FFilterDrawers write SetFilterDrawers default False;
    // FilterHook
    property MultiSelDirs: Boolean read FMultiSelDirs write SetMultiSelDirs default False;
    property RejectIcons: Boolean read FRejectIcons write SetRejectIcons default False;
    property RejectPattern: string read FRejectPattern write SetRejectPattern;
    property SortDirs: Integer read FSortDirs write SetSortDirs default MUIV_Dirlist_SortDirs_First; // MUIV_Dirlist_*

    property OnStatusValid: TNotifyEvent read FOnStatusValid write FOnStatusValid;
    property OnStatusInvalid: TNotifyEvent read FOnStatusInvalid write FOnStatusInvalid;
  end;

implementation

{ TMUIList }

constructor TMUIList.Create;
begin
  inherited;
  Frame := MUIV_Frame_InputList;
  FOnCompare := nil;
  FOnDisplay := nil;
  FOnConstruct := nil;
  FOnDestruct := nil;
  FOnMultiTest := nil;
  //
  FActive := -1;
  FAdjustHeight := False;
  FAdjustWidth := False;
  FAutoVisible := False;
  FDragSortable := False;
  FFormat := '';
  FMinLineHeight := 0;
  FQuiet := False;
  FShowDropMarks := True;
  SetLength(FSourceStrings, 0);
  SetLength(FSourceArray, 0);
  FTitle := '';
  FPool := nil;
  FPoolPuddleSize := 2008;
  FPoolThreshSize := 1024;
end;


procedure TMUIList.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FActive >= 0 then
    ATagList.AddTag(MUIA_List_Active, AsTag(FActive));
  if FAdjustHeight then
    ATagList.AddTag(MUIA_List_AdjustHeight, AsTag(FAdjustHeight));
  if FAdjustWidth then
    ATagList.AddTag(MUIA_List_AdjustWidth, AsTag(FAdjustWidth));
  if FAutoVisible then
    ATagList.AddTag(MUIA_List_AutoVisible, AsTag(FAutoVisible));
  if FDragSortable then
    ATagList.AddTag(MUIA_List_DragSortable, AsTag(FDragSortable));
  if FFormat <> '' then
    ATagList.AddTag(MUIA_List_Format, AsTag(PChar(FFormat)));
  if FMinLineHeight > 0 then
    ATagList.AddTag(MUIA_List_MinLineHeight, AsTag(FMinLineHeight));
  if not FShowDropMarks then
    ATagList.AddTag(MUIA_List_ShowDropMarks, AsTag(FShowDropMarks));
  if Length(FSourceArray) > 0 then
    ATagList.AddTag(MUIA_List_SourceArray, AsTag(@(FSourceArray[0])));
  if FTitle <> '' then
  begin
    if FTitle = #1 then
      ATagList.AddTag(MUIA_List_Title, MUI_TRUE)
    else
      ATagList.AddTag(MUIA_List_Title, AsTag(PChar(FTitle)));
  end;
  if Assigned(FPool) then
    ATagList.AddTag(MUIA_List_Pool, AsTag(FPool));
  if FPoolPuddleSize <> 2008 then
    ATagList.AddTag(MUIA_List_PoolPuddleSize, AsTag(FPoolPuddleSize));
  if FPoolThreshSize <> 1024 then
    ATagList.AddTag(MUIA_List_PoolThreshSize, AsTag(FPoolThreshSize));

  if Assigned(FOnDisplay) then
    ATagList.AddTag(MUIA_List_DisplayHook, AsTag(@DisplayHook));
  if Assigned(FOnCompare) then
    ATagList.AddTag(MUIA_List_CompareHook, AsTag(@CompareHook));
  if Assigned(FOnConstruct) then
    ATagList.AddTag(MUIA_List_ConstructHook, AsTag(@ConstructHook));
  if Assigned(FOnDestruct) then
    ATagList.AddTag(MUIA_List_DestructHook, AsTag(@DestructHook));
  if Assigned(FOnMultiTest) then
    ATagList.AddTag(MUIA_List_MultiTestHook, AsTag(@MultiTestHook));
end;

procedure TMUIList.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_List, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function DropFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIList;
begin
  Result := 0;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnDrop) then
    PasObj.FOnDrop(PasObj);
end;

function ActiveFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIList;
begin
  Result := 0;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnActiveChange) then
    PasObj.FOnActiveChange(PasObj);
end;

procedure TMUIList.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_List_DropMark, MUIV_EveryTime, @DropFunc);
  ConnectHook(MUIA_List_Active, MUIV_EveryTime, @ActiveFunc);
end;

function TMUIList.GetActive: Integer;
begin
  Result := -1;
  if HasObj then
    Result := GetIntValue(MUIA_List_Active);
end;

procedure TMUIList.SetActive(AValue: Integer);
begin
  if AValue <> Active then
  begin
    FActive := AValue;
    if HasObj then
      SetValue(MUIA_List_Active, AsTag(FActive));
  end;
end;

procedure TMUIList.SetAdjustHeight(AValue: Boolean);
begin
  if AValue <> FAdjustHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_List_AdjustHeight', BoolToStr(AValue, True))
    else
      FAdjustHeight := AValue;
  end;
end;

procedure TMUIList.SetAdjustWidth(AValue: Boolean);
begin
  if AValue <> FAdjustWidth then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_List_AdjustWidth', BoolToStr(AValue, True))
    else
      FAdjustWidth := AValue;
  end;
end;

procedure TMUIList.SetAutoVisible(AValue: Boolean);
begin
  if AValue <> FAutoVisible then
  begin
    FAutoVisible := AValue;
    if HasObj then
      SetValue(MUIA_List_AutoVisible, AsTag(FAutoVisible));
  end;
end;

procedure TMUIList.SetDragSortable(AValue: Boolean);
begin
  if AValue <> FDragSortable then
  begin
    FDragSortable := AValue;
    if HasObj then
      SetValue(MUIA_List_DragSortable, AsTag(FDragSortable));
  end;
end;

function TMUIList.GetDropMark: Integer;
begin
  Result := -1;
  if HasObj then
    Result := GetIntValue(MUIA_List_DropMark);
end;

function TMUIList.GetEntries: Integer;
begin
  Result := -1;
  if HasObj then
    Result := GetIntValue(MUIA_List_Entries);
end;

function TMUIList.GetFirst: Integer;
begin
  Result := -1;
  if HasObj then
    Result := GetIntValue(MUIA_List_First);
end;

procedure TMUIList.SetFormat(AValue: string);
begin
  if AValue <> FFormat then
  begin
    FFormat := AValue;
    if HasObj then
      SetValue(MUIA_List_Format, AsTag(PChar(FFormat)));
  end;
end;

function TMUIList.GetInsertPosition: Integer;
begin
  Result := -1;
  if HasObj then
    Result := GetIntValue(MUIA_List_InsertPosition);
end;

procedure TMUIList.SetMinLineHeight(AValue: Integer);
begin
  if AValue <> FMinLineHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_List_MinLineHeight', IntToStr(AValue))
    else
      FMinLineHeight := AValue;
  end;
end;

procedure TMUIList.SetQuiet(AValue: Boolean);
begin
  FQuiet := AValue;
  if HasObj then
    SetValue(MUIA_List_Quiet, AsTag(FQuiet));
end;

procedure TMUIList.SetShowDropMarks(AValue: Boolean);
begin
  if AValue <> FShowDropMarks then
  begin
    FShowDropMarks := AValue;
    if HasObj then
      SetValue(MUIA_List_ShowDropMarks, AsTag(FShowDropMarks));
  end;
end;

procedure TMUIList.SetSourceArray(AValue: TStringArray);
var
  i: Integer;
begin
  if Assigned(FMUIObj) then
    ComplainIOnly(Self, 'MUIA_List_SourceArray', IntToStr(Length(AValue)))
  else
  begin
    FSourceStrings := Copy(AValue);
    SetLength(FSourceArray, Length(FSourceStrings) + 1);
    for i := 0 to High(FSourceStrings) do
      FSourceArray[i] := PChar(FSourceStrings[i]);
    FSourceArray[High(FSourceArray)] := nil;
  end;
end;

procedure TMUIList.SetTitle(AValue: string);
begin
  if AValue <> FTitle then
  begin
    FTitle := AValue;
    if HasObj then
    begin
      if FTitle = #1 then
        SetValue(MUIA_List_Title, MUI_TRUE)
      else
        SetValue(MUIA_List_Title, AsTag(PChar(FTitle)));
    end;
  end;
end;

function TMUIList.GetVisible: Integer;
begin
  Result := -1;
  if HasObj then
    Result := GetIntValue(MUIA_List_Visible);
end;

function TMUIList.CreateImage(Bitmap: TMUIBitmap; Flags: Longword): PListImage;
begin
  Result := nil;
  if HasObj then
  begin
    if not Bitmap.HasObj then
      Bitmap.CreateObject;
    if Bitmap.HasObj then
      Result := PListImage(DoMethod(MUIObj, [MUIM_List_CreateImage, AsTag(Bitmap.MUIObj), AsTag(Flags)]));
  end;
end;

procedure TMUIList.DeleteImage(Img: PListImage);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_DeleteImage, AsTag(Img)]);
end;

class function TMUIList.ListImageToString(Img: PListImage): string;
begin
  Result := #27'O[' + HexStr(Img) + ']';
end;

procedure TMUIList.Exchange(Pos1, Pos2: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Exchange, Pos1, Pos2]);
end;

function TMUIList.GetEntry(Pos: Integer): Pointer;
begin
  Result := nil;
  if HasObj then
    Result := Pointer(DoMethod(MuiObj, [MUIM_List_GetEntry, Pos, AsTag(@Result)]));
end;

procedure TMUIList.Insert(Entries: PPChar; Count: Integer; Pos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Insert, AsTag(Entries), Count, Pos]);
end;

procedure TMUIList.InsertSingle(Entry: PChar; Pos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_InsertSingle, AsTag(Entry), Pos])
end;

procedure TMUIList.Jump(Pos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Jump, Pos]);
end;

procedure TMUIList.Move(FromPos, ToPos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Move, FromPos, ToPos]);
end;

procedure TMUIList.NextSelected(var Pos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_NextSelected, AsTag(@Pos)]);
end;

procedure TMUIList.Redraw(Pos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Redraw, AsTag(Pos)]);
end;

procedure TMUIList.Remove(Pos: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Remove, AsTag(Pos)]);
end;

procedure TMUIList.Select(Pos, SelType: Integer; var State: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Select, AsTag(Pos), AsTag(SelType), AsTag(@State)]);
end;

procedure TMUIList.Sort;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Sort]);
end;

function TMUIList.TestPos(x, y: Integer): TMUI_List_TestPos_Result;
begin
  Result.Entry := -1;
  Result.Column := -1;
  Result.flags := 0;
  Result.xOffset := 0;
  Result.yOffset := 0;
  if HasObj then
    DoMethod(MUIObj, [MUIM_List_Select, AsTag(x), AsTag(y), AsTag(@Result)]);
end;

function DisplayFunc(Hook: PHook; CArray: PPChar; Entry: PChar): PtrInt;
var
  PasObj: TMUIList;
begin
  Result := 0;
  CArray[0] := nil;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnDisplay) then
    PasObj.FOnDisplay(PasObj, CArray, Entry);
end;

procedure TMUIList.SetOnDisplay(AValue: TDisplayEvent);
begin
  FOnDisplay := AValue;
  MH_SetHook(DisplayHook, THookFunc(@DisplayFunc), Self);
  if HasObj then
  begin
    if Assigned(FOnDisplay) then
      SetValue(MUIA_List_DisplayHook, AsTag(@DisplayHook))
    else
      SetValue(MUIA_List_DisplayHook, AsTag(nil));
  end;
end;

function CompareFunc(Hook: PHook; Entry1: PChar; Entry2: PChar): PtrInt;
var
  PasObj: TMUIList;
begin
  Result := 0;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnCompare) then
    Result := PasObj.FOnCompare(PasObj, Entry1, Entry2);
end;

procedure TMUIList.SetOnCompare(AValue: TCompareEvent);
begin
  FOnCompare := AValue;
  MH_SetHook(CompareHook, THookFunc(@CompareFunc), Self);
  if HasObj then
  begin
    if Assigned(FOnCompare) then
      SetValue(MUIA_List_CompareHook, AsTag(@CompareHook))
    else
      SetValue(MUIA_List_CompareHook, AsTag(nil));
  end;
end;

function ConstructFunc(Hook: PHook; Pool: Pointer; Str: PChar): Pointer;
var
  PasObj: TMUIList;
begin
  Result := nil;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnConstruct) then
    Result := PasObj.FOnConstruct(PasObj, Pool, Str);
end;

procedure TMUIList.SetOnConstruct(AValue: TConstructEvent);
begin
  FOnConstruct := AValue;
  MH_SetHook(ConstructHook, THookFunc(@ConstructFunc), Self);
  if HasObj then
  begin
    if Assigned(FOnConstruct) then
      SetValue(MUIA_List_ConstructHook, AsTag(@ConstructHook))
    else
      SetValue(MUIA_List_ConstructHook, AsTag(MUIV_List_ConstructHook_String));
  end;
end;

function DestructFunc(Hook: PHook; Pool: Pointer; Entry: PChar): PtrInt;
var
  PasObj: TMUIList;
begin
  Result := 0;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnDestruct) then
    PasObj.FOnDestruct(PasObj, Pool, Entry);
end;

procedure TMUIList.SetOnDestruct(AValue: TDestructEvent);
begin
  FOnDestruct := AValue;
  MH_SetHook(DestructHook, THookFunc(@DestructFunc), Self);
  if HasObj then
  begin
    if Assigned(FOnDestruct) then
      SetValue(MUIA_List_DestructHook, AsTag(@DestructHook))
    else
      SetValue(MUIA_List_DestructHook, AsTag(MUIV_List_DestructHook_String));
  end;
end;

function MultiTestFunc(Hook: PHook; Dummy: Pointer; Entry: PChar): PtrInt;
var
  PasObj: TMUIList;
begin
  Result := MUI_TRUE;
  PasObj := TMUIList(Hook^.h_Data);
  if Assigned(PasObj.FOnMultiTest) then
    Result := AsTag(PasObj.FOnMultiTest(PasObj, Entry));
end;

procedure TMUIList.SetOnMultiTest(AValue: TMultiTestEvent);
begin
  FOnMultiTest := AValue;
  MH_SetHook(MultiTestHook, THookFunc(@MultiTestFunc), Self);
  if HasObj then
  begin
    if Assigned(FOnMultiTest) then
      SetValue(MUIA_List_MultiTestHook, AsTag(@MultiTestHook))
    else
      SetValue(MUIA_List_MultiTestHook, AsTag(nil));
  end;
end;

procedure TMUIList.SetPool(AValue: Pointer);
begin
  if AValue <> FPool then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_List_Pool', HexStr(AValue))
    else
      FPool := AValue;
  end;
end;

procedure TMUIList.SetPoolPuddleSize(AValue: LongWord);
begin
  if AValue <> FPoolPuddleSize then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_List_PoolPuddleSize', IntToStr(AValue))
    else
      FPoolPuddleSize := AValue;
  end;
end;

procedure TMUIList.SetPoolThreshSize(AValue: LongWord);
begin
  if AValue <> FPoolThreshSize then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_List_PoolThreshSize', IntToStr(AValue))
    else
      FPoolThreshSize := AValue;
  end;
end;

{ TMUIFloatText }

constructor TMUIFloatText.Create;
begin
  inherited;
  FJustify := False;
  FSkipChars := '';
  FTabSize := 0;
  FText := '';
end;

procedure TMUIFloatText.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FJustify then
    ATagList.AddTag(MUIA_FloatText_Justify, AsTag(FJustify));
  if FSkipChars <> '' then
    ATagList.AddTag(MUIA_FloatText_SkipChars, AsTag(PChar(FSkipChars)));
  if FTabSize > 0 then
    ATagList.AddTag(MUIA_FloatText_TabSize, AsTag(FTabSize));
  if FText <> '' then
    ATagList.AddTag(MUIA_FloatText_Text, AsTag(PChar(FText)));
end;

procedure TMUIFloatText.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_FloatText, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIFloatText.SetJustify(AValue: Boolean);
begin
  if AValue <> FJustify then
  begin
    FJustify := AValue;
    if HasObj then
      SetValue(MUIA_FloatText_Justify, AsTag(FJustify));
  end;
end;

procedure TMUIFloatText.SetSkipChars(AValue: string);
begin
  if AValue <> FSkipChars then
  begin
    FSkipChars := AValue;
    if HasObj then
      SetValue(MUIA_FloatText_SkipChars, AsTag(PChar(FSkipChars)));
  end;
end;

procedure TMUIFloatText.SetTabSize(AValue: Integer);
begin
  if AValue <> FTabSize then
  begin
    FTabSize := AValue;
    if HasObj then
      SetValue(MUIA_FloatText_TabSize, AsTag(FTabSize));
  end;
end;

procedure TMUIFloatText.SetText(AValue: string);
begin
  if AValue <> FText then
  begin
    FText := AValue;
    if HasObj then
      SetValue(MUIA_FloatText_Text, AsTag(PChar(FText)));
  end;
end;

{ TMUIVolumeList }

procedure TMUIVolumeList.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_VolumeList, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

{ TMUIDirList }

constructor TMUIDirList.Create;
begin
  inherited;
  FAcceptPattern := '';
  FDirectory := '';
  FDrawersOnly := False;
  FFilesOnly := False;
  FFilterDrawers := False;
  FMultiSelDirs := False;
  FRejectIcons := False;
  FRejectPattern := '';
  FSortDirs := MUIV_Dirlist_SortDirs_First;
end;

procedure TMUIDirList.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FAcceptPattern <> '' then
    ATagList.AddTag(MUIA_DirList_AcceptPattern , AsTag(PChar(FAcceptPattern)));
  if FDirectory <> '' then
    ATagList.AddTag(MUIA_DirList_Directory , AsTag(PChar(FDirectory)));
  if FDrawersOnly then
    ATagList.AddTag(MUIA_DirList_DrawersOnly, AsTag(FDrawersOnly));
  if FFilesOnly then
    ATagList.AddTag(MUIA_DirList_FilesOnly, AsTag(FFilesOnly));
  if FFilterDrawers then
    ATagList.AddTag(MUIA_DirList_FilterDrawers, AsTag(FFilterDrawers));
  if FMultiSelDirs then
    ATagList.AddTag(MUIA_DirList_MultiSelDirs, AsTag(FMultiSelDirs));
  if FRejectIcons then
    ATagList.AddTag(MUIA_DirList_RejectIcons, AsTag(FRejectIcons));
  if FRejectPattern <> '' then
    ATagList.AddTag(MUIA_DirList_RejectPattern , AsTag(PChar(FRejectPattern)));
  ATagList.AddTag(MUIA_DirList_SortDirs, AsTag(MUIV_Dirlist_SortDirs_First));
  ATagList.AddTag(MUIA_DirList_SortType, AsTag(MUIV_Dirlist_SortType_Name));
end;

procedure TMUIDirList.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_DirList, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function StatusValidFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIDirList;
begin
  Result := 0;
  PasObj := TMUIDirList(Hook^.h_Data);
  if Assigned(PasObj.FOnStatusValid) then
    PasObj.FOnStatusValid(PasObj);
end;

function StatusInvalidFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIDirList;
begin
  Result := 0;
  PasObj := TMUIDirList(Hook^.h_Data);
  if Assigned(PasObj.FOnStatusInvalid) then
    PasObj.FOnStatusInvalid(PasObj);
end;

procedure TMUIDirList.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Dirlist_Status, MUIV_Dirlist_Status_Valid, @StatusValidFunc);
  ConnectHook(MUIA_Dirlist_Status, MUIV_Dirlist_Status_Invalid, @StatusInvalidFunc);
end;

procedure TMUIDirList.SetAcceptPattern(AValue: string);
begin
  if AValue <> FAcceptPattern then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_MUIDirList_AcceptPattern', AValue)
    else
      FAcceptPattern := AValue;
  end;
end;

procedure TMUIDirList.SetDirectory(AValue: string);
begin
  if AValue <> FDirectory then
  begin
    FDirectory := AValue;
    if HasObj then
      SetValue(MUIA_DirList_Directory, AsTag(PChar(FDirectory)));
  end;
end;

procedure TMUIDirList.SetDrawersOnly(AValue: Boolean);
begin
  if AValue <> FDrawersOnly then
  begin
    FDrawersOnly := AValue;
    if HasObj then
      SetValue(MUIA_DirList_DrawersOnly, AsTag(FDrawersOnly));
  end;
end;

procedure TMUIDirList.SetFilesOnly(AValue: Boolean);
begin
  if AValue <> FFilesOnly then
  begin
    FFilesOnly := AValue;
    if HasObj then
      SetValue(MUIA_DirList_FilesOnly, AsTag(FFilesOnly));
  end;
end;

procedure TMUIDirList.SetFilterDrawers(AValue: Boolean);
begin
  if AValue <> FFilterDrawers then
  begin
    FFilterDrawers := AValue;
    if HasObj then
      SetValue(MUIA_DirList_FilterDrawers, AsTag(FFilterDrawers));
  end;
end;

procedure TMUIDirList.SetMultiSelDirs(AValue: Boolean);
begin
  if AValue <> FMultiSelDirs then
  begin
    FMultiSelDirs := AValue;
    if HasObj then
      SetValue(MUIA_DirList_MultiSelDirs, AsTag(FMultiSelDirs));
  end;
end;

function TMUIDirList.GetNumBytes: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_DirList_NumBytes);
end;

function TMUIDirList.GetNumDrawers: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_DirList_NumDrawers);
end;

function TMUIDirList.GetNumFiles: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_DirList_NumFiles);
end;

function TMUIDirList.GetPath: string;
begin
  Result := '';
  if HasObj then
    Result := GetStringValue(MUIA_DirList_Path);
end;

procedure TMUIDirList.SetRejectIcons(AValue: Boolean);
begin
  if AValue <> FRejectIcons then
  begin
    FRejectIcons := AValue;
    if HasObj then
      SetValue(MUIA_DirList_RejectIcons, AsTag(FRejectIcons));
  end;
end;

procedure TMUIDirList.SetRejectPattern(AValue: string);
begin
  if AValue <> FRejectPattern then
  begin
    FRejectPattern := AValue;
    if HasObj then
      SetValue(MUIA_DirList_RejectPattern, AsTag(PChar(FRejectPattern)));
  end;
end;

procedure TMUIDirList.SetSortDirs(AValue: Integer);
begin
  if AValue <> FSortDirs then
  begin
    FSortDirs := AValue;
    if HasObj then
      SetValue(MUIA_DirList_SortDirs, AsTag(FSortDirs));
  end;
end;

function TMUIDirList.GetStatus: Integer;
begin
  Result := MUIV_Dirlist_Status_Invalid;
  if HasObj then
    Result := GetIntValue(MUIA_DirList_Status);
end;

procedure TMUIDirList.ReRead;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_DirList_ReRead]);
end;


end.
