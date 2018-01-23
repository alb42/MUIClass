unit MUIClass.Gadget;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area;

type
  TMUIGadget = class(TMUIArea)
  private
    function GetGadget: PGadget;
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Gadget: PGadget read GetGadget;
  end;

  TMUIString = class(TMUIArea)
  private
    FAccept: string;
    FAdvanceOnCR: Boolean;
    FOnAcknowledge: TNotifyEvent;
    FContents: string;
    FFormat: Integer;
    FInteger: Integer;
    FIntegerSet: Boolean;
    FMaxLen: Integer;
    FReject: string;
    FSecret: Boolean;
    procedure SetAccept(AValue: string);
    procedure SetAdvanceOnCR(AValue: Boolean);
    function GetBufferPos: Integer;
    procedure SetBufferPos(AValue: Integer);
    function GetContents: string;
    procedure SetContents(AValue: string);
    function GetDisplayPos: Integer;
    procedure SetDisplayPos(AValue: Integer);
    procedure SetFormat(AValue: Integer);
    function GetInteger: Integer;
    procedure SetInteger(AValue: Integer);
    procedure SetMaxLen(AValue: Integer);
    procedure SetReject(AValue: string);
    procedure SetSecret(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Accept: string read FAccept write SetAccept;
    property AdvanceOnCR: Boolean read FAdvanceOnCR write SetAdvanceOnCR;
    property OnAcknowledge: TNotifyEvent read FOnAcknowledge write FOnAcknowledge;
    // AttachedList -> needs to be implemented first
    property BufferPos: Integer read GetBufferPos write SetBufferPos;
    property Contents: string read GetContents write SetContents;
    property DisplayPos: Integer read GetDisplayPos write SetDisplayPos;
    // EditHook -> own Event? (does not work in AROS)
    property Format: Integer read FFormat write SetFormat;              //MUIV_StringFormat_*
    property IntegerValue: Integer read GetInteger write SetInteger;
    // LonelyEditHook -> see EditHook
    property MaxLen: Integer read FMaxLen write SetMaxLen; // maximal length of the string in the Gadget WITHOUT #0 at the end! (class add one to it ;-))
    property Reject: string read FReject write SetReject;
    property Secret: Boolean read FSecret write FSecret;
  end;


  TMUIProp = class(TMUIArea)
  private
    FEntries: Integer;
    FFirst: Integer;
    FOnFirstChange: TNotifyEvent;
    FHoriz: Boolean;
    FVisible: Integer;
    FUseWinBorder: Integer;
    procedure SetEntries(AValue: Integer);
    function GetFirst: Integer;
    procedure SetFirst(AValue: Integer);
    procedure SetHoriz(AValue: Boolean);
    procedure SetVisible(AValue: Integer);
    procedure SetUseWinBorder(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure Increase(Amount: Integer);
    procedure Decrease(Amount: Integer);
    // Fields
    property Entries: Integer read FEntries write SetEntries;
    property First: Integer read GetFirst write SetFirst;
    property Horiz: Boolean read FHoriz write SetHoriz;
    property OnFirstChange: TNotifyEvent read FOnFirstChange write FOnFirstChange;
    property UseWinBorder: Integer read FUseWinBorder write SetUseWinBorder;
    property Visible: Integer read FVisible write SetVisible;                 // MUIV_Prop_UseWinBorder_*
  end;

  TMUIScrollbar = class(TMUIProp)
  private
    FType: Integer; //* Default
    procedure SetHoriz(AValue: Boolean);
    procedure SetType(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
    procedure DoFirstOpen; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Horiz: Boolean read FHoriz write SetHoriz;
    property SType: Integer read FType write SetType;
  end;

implementation

{ TMUIGadget }

constructor TMUIGadget.Create;
begin
  inherited;
end;

procedure TMUIGadget.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
end;

procedure TMUIGadget.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Gadget, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function TMUIGadget.GetGadget: PGadget;
begin
  Result := nil;
  if HasObj then
    Result := GetPointerValue(MUIA_Gadget_Gadget);
end;


{ TMUIString }

constructor TMUIString.Create;
begin
  inherited;
  FAccept := '';
  FAdvanceOnCR := False;
  FContents := '';
  FFormat := MUIV_String_Format_Left;
  FInteger := 0;
  FIntegerSet := False;
  MaxLen := 0;
  FReject := '';
  FSecret := False;
  Frame := MUIV_Frame_String;
end;

procedure TMUIString.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FAccept <> '' then
    ATagList.AddTag(MUIA_String_Accept, AsTag(PChar(FAccept)));
  if FAdvanceOnCR then
    ATagList.AddTag(MUIA_String_AdvanceOnCR, AsTag(FAdvanceOnCR));
  if FContents <> '' then
    ATagList.AddTag(MUIA_String_Contents, AsTag(PChar(FContents)));
  if FFormat <> MUIV_String_Format_Left then
    ATagList.AddTag(MUIA_String_Format, AsTag(FFormat));
  if FIntegerSet then
    ATagList.AddTag(MUIA_String_Integer, AsTag(FInteger));
  if FMaxLen > 0 then
    ATagList.AddTag(MUIA_String_MaxLen, AsTag(FMaxLen + 1)); // one more, mui wants that with #0... how stupid!
  if FReject <> '' then
    ATagList.AddTag(MUIA_String_Reject, AsTag(PChar(FReject)));
  if FSecret then
    ATagList.AddTag(MUIA_String_Secret, AsTag(FSecret));
end;

procedure TMUIString.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_String, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function AckFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIString;
begin
  Result := 0;
  PasObj := TMUIString(Hook^.h_Data);
  if Assigned(PasObj.FOnAcknowledge) then
    PasObj.FOnAcknowledge(PasObj);
end;

procedure TMUIString.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_String_Acknowledge, MUIV_EveryTime, @AckFunc);
end;

procedure TMUIString.SetAdvanceOnCR(AValue: Boolean);
begin
  if AValue <> FAdvanceOnCR then
  begin
    FAdvanceOnCR := AValue;
    if HasObj then
      SetValue(MUIA_String_AdvanceOnCR, AsTag(AValue));
  end;
end;

procedure TMUIString.SetAccept(AValue: string);
begin
  if AValue <> FAccept then
  begin
    FAccept := AValue;
    if HasObj then
      SetValue(MUIA_String_Accept, AsTag(PChar(AValue)));
  end;
end;

function TMUIString.GetBufferPos: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_String_BufferPos);
end;

procedure TMUIString.SetBufferPos(AValue: Integer);
begin
  if HasObj then
    SetValue(MUIA_String_BufferPos, AValue);
end;

function TMUIString.GetContents: string;
var
  PC: PChar;
begin
  Result := FContents;
  if HasObj then
  begin
    PC := GetPointerValue(MUIA_String_Contents);
    if Assigned(PC) then
      Result := PC
    else
      Result := '';
  end;
end;

procedure TMUIString.SetContents(AValue: string);
begin
  FContents := AValue;
  if HasObj then
    SetValue(MUIA_String_Contents, PChar(FContents));
end;

function TMUIString.GetDisplayPos: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_String_DisplayPos);
end;

procedure TMUIString.SetDisplayPos(AValue: Integer);
begin
  if HasObj then
    SetValue(MUIA_String_DisplayPos, AValue);
end;

procedure TMUIString.SetFormat(AValue: PtrInt);
begin
  if AValue <> FFormat then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_String_Format', IntToStr(AValue))
    else
      FFormat := AValue;
  end;
end;

function TMUIString.GetInteger: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_String_Integer);
end;

procedure TMUIString.SetInteger(AValue: Integer);
begin
  FInteger := AValue;
  FIntegerSet := True;
  if HasObj then
    SetValue(MUIA_String_Integer, AValue);
end;

procedure TMUIString.SetMaxLen(AValue: Integer);
begin
  if AValue <> FMaxLen then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_String_MaxLen', IntToStr(AValue))
    else
      FMaxLen := AValue;
  end;
end;

procedure TMUIString.SetReject(AValue: string);
begin
  if AValue <> FReject then
  begin
    FReject := AValue;
    if HasObj then
      SetValue(MUIA_String_Reject, AsTag(PChar(AValue)));
  end;
end;

procedure TMUIString.SetSecret(AValue: Boolean);
begin
  if AValue <> FSecret then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_String_Secret', BoolToStr(AValue, True))
    else
      FSecret := AValue;
  end;
end;

{ TMUIProp }

constructor TMUIProp.Create;
begin
  inherited;
  Frame := MUIV_Frame_Prop;
  //
  FEntries := 0;
  FFirst := 0;
  FVisible := 0;
  FHoriz := False;
  FUseWinBorder := MUIV_Prop_UseWinBorder_None;
end;

procedure TMUIProp.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if Entries > 0 then
    ATagList.AddTag(MUIA_Prop_Entries, AsTag(FEntries));
  if FHoriz then
    ATagList.AddTag(MUIA_Prop_Horiz, AsTag(FHoriz));
  if FFirst > 0 then
    ATagList.AddTag(MUIA_Prop_First, AsTag(FFirst));
  if FVisible > 0 then
    ATagList.AddTag(MUIA_Prop_Visible, AsTag(FVisible));
  if FUseWinBorder > MUIV_Prop_UseWinBorder_None then
    ATagList.AddTag(MUIA_Prop_UseWinBorder, AsTag(FUseWinBorder));
end;

procedure TMUIProp.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Prop, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function PropFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIProp;
begin
  Result := 0;
  PasObj := TMUIProp(Hook^.h_Data);
  if Assigned(PasObj.FOnFirstChange) then
    PasObj.FOnFirstChange(PasObj);
end;

procedure TMUIProp.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Prop_First, MUIV_EveryTime, @PropFunc);
end;


procedure TMUIProp.SetEntries(AValue: Integer);
begin
  if AValue <> FEntries then
  begin
    FEntries := AValue;
    if HasObj then
      SetValue(MUIA_Prop_Entries, AsTag(AValue));
  end;
end;

procedure TMUIProp.SetHoriz(AValue: Boolean);
begin
  if AValue <> FHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Prop_Horiz', BoolToStr(AValue, True))
    else
      FHoriz := AValue;
  end;
end;

function TMUIProp.GetFirst: Integer;
begin
  Result := FFirst;
  if HasObj then
    Result := GetIntValue(MUIA_Prop_First);
end;

procedure TMUIProp.SetFirst(AValue: Integer);
begin
  if AValue <> FFirst then
  begin
    FFirst := AValue;
    if HasObj then
      SetValue(MUIA_Prop_First, AsTag(AValue));
  end;
end;

procedure TMUIProp.SetVisible(AValue: Integer);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    if HasObj then
      SetValue(MUIA_Prop_Visible, AsTag(AValue));
  end;
end;

procedure TMUIProp.SetUseWinBorder(AValue: Integer);
begin
  if AValue <> FUseWinBorder then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Prop_UseWinBorder', IntToStr(AValue))
    else
      FUseWinBorder := AValue;
  end;
end;

procedure TMUIProp.Increase(Amount: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Prop_Increase, Amount]);
end;

procedure TMUIProp.Decrease(Amount: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Prop_Decrease, Amount]);
end;

{ TMUIScrollbar }

constructor TMUIScrollbar.Create;
begin
  inherited;
  FHoriz := False;
  FType := MUIV_Scrollbar_Type_Default;
end;

procedure TMUIScrollbar.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FHoriz then
    ATagList.AddTag(MUIA_Group_Horiz, AsTag(FHoriz));
  if FType <> MUIV_Scrollbar_Type_Default then
    ATagList.AddTag(MUIA_Scrollbar_Type, AsTag(FType));
end;

procedure TMUIScrollbar.AfterCreateObject;
begin
  inherited;
end;

procedure TMUIScrollbar.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Scrollbar, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIScrollbar.DoFirstOpen;
begin
  inherited;
  if FFirstOpen then
  begin
    // Nasty hack, because Amiga MUI has a big bug, that it do not accept
    // these Values as I but they are... so we set them here ;-)
    SetValue(MUIA_Prop_Entries, AsTag(Entries));
    SetValue(MUIA_Prop_Visible, AsTag(Visible));
    SetValue(MUIA_Prop_First, AsTag(First));
    FFirstOpen := False;
  end;
end;

procedure TMUIScrollbar.SetHoriz(AValue: Boolean);
begin
  if AValue <> FHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Group_Horiz', BoolToStr(AValue, True))
    else
      FHoriz := AValue;
  end;
end;

procedure TMUIScrollbar.SetType(AValue: Integer);
begin
  if AValue <> FType then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Scrollbar_Type', IntToStr(AValue))
    else
      FType := AValue;
  end;
end;



end.
