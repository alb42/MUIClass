unit MUIClass.Numeric;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, agraphics, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area;

type
  TMUINumeric = class(TMUIArea)
  private
    FCheckAllSizes: Boolean;
    FDefault: Integer;
    FFormat: string;
    FMin, FMax: Integer;
    FReverse: Boolean;
    FRevLeftRight: Boolean;
    FRevUpDown: Boolean;
    FValue: Integer;
    FOnValueChange: TNotifyEvent;
    procedure SetCheckAllSizes(AValue: Boolean);
    procedure SetDefault(AValue: Integer);
    procedure SetFormat(AValue: string);
    procedure SetMin(AValue: Integer);
    procedure SetMax(AValue: Integer);
    procedure SetReverse(AValue: Boolean);
    procedure SetRevLeftRight(AValue: Boolean);
    procedure SetRevUpDown(AValue: Boolean);
    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure Decrease;
    procedure Increase;
    function ScaleToValue(ScaleMin, ScaleMax, Scale: Integer): Integer;
    procedure SetDefault;
    function Stringify(Value: Integer): string;
    function ValueToScale(ScaleMin, ScaleMax: Integer): Integer;
    // Fields
  published
    property CheckAllSizes: Boolean read FCheckAllSizes write SetCheckAllSizes default False;
    property Default: Integer read FDefault write SetDefault default 0;
    property Format: string read FFormat write SetFormat;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 0;
    property Reverse: Boolean read FReverse write SetReverse default False;
    property RevLeftRight: Boolean read FRevLeftRight write SetRevLeftRight default False;
    property RevUpDown: Boolean read FRevUpDown write SetRevUpDown default False;
    property Value: Integer read GetValue write SetValue default 0;
    // Event
    property OnValueChange: TNotifyEvent read FOnValueChange write FOnValueChange;
  end;

  TMUIKnob = class(TMUINumeric)
  public
    procedure CreateObject; override;
  end;

  TMUILevelmeter = class(TMUINumeric)
  private
    FLabel: string;
    procedure SetLabel(AValue: string);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property LLabel: string read FLabel write SetLabel;
  end;

  TMUINumericbutton = class(TMUINumeric)
  public
    procedure CreateObject; override;
  end;

  TMUISlider = class(TMUINumeric)
  private
    FHoriz: Boolean;
    FQuiet: Boolean;
    procedure SetHoriz(AValue: Boolean);
    procedure SetQuiet(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property Horiz: Boolean read FHoriz write SetHoriz default False; //I
    property Quiet: Boolean read FQuiet write SetQuiet default False; //I
  end;

implementation

{ TMUINumeric }

constructor TMUINumeric.Create;
begin
  inherited;
  FCheckAllSizes := False;
  FDefault := 0;
  FFormat := '';
  FMin := 0;
  FMax := 100;
  FReverse := False;
  FRevLeftRight := False;
  FRevUpDown := False;
end;

procedure TMUINumeric.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FCheckAllSizes then
    ATagList.AddTag(MUIA_Numeric_CheckAllSizes, AsTag(FCheckAllSizes));
  if FDefault <> 0 then
    ATagList.AddTag(MUIA_Numeric_Default, AsTag(FDefault));
  if FFormat <> '' then
    ATagList.AddTag(MUIA_Numeric_Format, AsTag(PChar(FFormat)));
  if FMin <> 0 then
    ATagList.AddTag(MUIA_Numeric_Min, AsTag(FMin));
  if FMax <> 100 then
    ATagList.AddTag(MUIA_Numeric_Max, AsTag(FMax));
  if FReverse then
    ATagList.AddTag(MUIA_Numeric_Reverse, AsTag(FReverse));
  if FRevLeftRight then
    ATagList.AddTag(MUIA_Numeric_RevLeftRight, AsTag(FRevLeftRight));
  if FRevUpDown then
    ATagList.AddTag(MUIA_Numeric_RevUpDown, AsTag(FRevUpDown));
end;

procedure TMUINumeric.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Numeric, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function ValueFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUINumeric;
begin
  Result := 0;
  PasObj := TMUINumeric(Hook^.h_Data);
  if Assigned(PasObj.FOnValueChange) then
    PasObj.FOnValueChange(PasObj);
end;

procedure TMUINumeric.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Numeric_Value, MUIV_EveryTime, @ValueFunc);
end;

procedure TMUINumeric.SetCheckAllSizes(AValue: Boolean);
begin
  if AValue <> FCheckAllSizes then
  begin
    FCheckAllSizes := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_CheckAllSizes, FCheckAllSizes);
  end;
end;

procedure TMUINumeric.SetDefault(AValue: Integer);
begin
  if AValue <> FDefault then
  begin
    FDefault := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_Default, FDefault);
  end;
end;

procedure TMUINumeric.SetFormat(AValue: string);
begin
  if AValue <> FFormat then
  begin
    FFormat := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_Format, AsTag(PChar(FFormat)));
  end;
end;

procedure TMUINumeric.SetMin(AValue: Integer);
begin
  if AValue <> FMin then
  begin
    FMin := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_Min, FMin);
  end;
end;

procedure TMUINumeric.SetMax(AValue: Integer);
begin
  if AValue <> FMax then
  begin
    FMax := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_Max, FMax);
  end;
end;

procedure TMUINumeric.SetReverse(AValue: Boolean);
begin
  if AValue <> FReverse then
  begin
    FReverse := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_Reverse, FReverse);
  end;
end;

procedure TMUINumeric.SetRevLeftRight(AValue: Boolean);
begin
  if AValue <> FRevLeftRight then
  begin
    FRevLeftRight := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_RevLeftRight, FRevLeftRight);
  end;
end;

procedure TMUINumeric.SetRevUpDown(AValue: Boolean);
begin
  if AValue <> FRevUpDown then
  begin
    FRevUpDown := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_RevUpDown, FRevUpDown);
  end;
end;

function TMUINumeric.GetValue: Integer;
begin
  Result := FValue;
  if HasObj then
    Result := GetIntValue(MUIA_Numeric_Value);
end;

procedure TMUINumeric.SetValue(AValue: Integer);
begin
  if AValue <> FValue then
  begin
    FValue := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Numeric_Value, FValue);
  end;
end;

procedure TMUINumeric.Decrease;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Numeric_Decrease]);
end;

procedure TMUINumeric.Increase;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Numeric_Increase]);
end;

function TMUINumeric.ScaleToValue(ScaleMin, ScaleMax, Scale: Integer): Integer;
begin
  Result := 0;
  if HasObj then
    Result := DoMethod(MUIObj, [MUIM_Numeric_ScaleToValue, ScaleMin, ScaleMax, Scale]);
end;

procedure TMUINumeric.SetDefault;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Numeric_SetDefault])
  else
    FValue := FDefault;
end;

function TMUINumeric.Stringify(Value: Integer): string;
var
  PC: PChar;
begin
  Result := '';
  if HasObj then
  begin
    PC := PChar(DoMethod(MUIObj, [MUIM_Numeric_Stringify, Value]));
    if Assigned(PC) then
      Result := PC;
  end;
end;

function TMUINumeric.ValueToScale(ScaleMin, ScaleMax: Integer): Integer;
begin
  Result := 0;
  if HasObj then
    Result := DoMethod(MUIObj, [MUIM_Numeric_ValueToScale, ScaleMin, ScaleMax]);
end;


{ TMUIKnob }

procedure TMUIKnob.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Knob, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

{ TMUILevelmeter }

constructor TMUILevelmeter.Create;
begin
  inherited;
  FLabel := '';
end;

procedure TMUILevelmeter.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FLabel = '' then
    ATagList.AddTag(MUIA_Levelmeter_Label, AsTag(PChar(FLabel)));
end;

procedure TMUILevelmeter.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Levelmeter, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUILevelmeter.SetLabel(AValue: string);
begin
  if AValue <> FLabel then
  begin
    FLabel := Copy(AValue, 1, 6); // Max 6 chars
    if Assigned(FMUIObj) then
      SetValue(MUIA_Levelmeter_Label, AsTag(PChar(FLabel)));
  end;
end;

{ TMUINumericbutton }

procedure TMUINumericbutton.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Numericbutton, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

{ TMUISlider }

constructor TMUISlider.Create;
begin
  inherited;
  FHoriz := False;
end;

procedure TMUISlider.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FHoriz then
    ATagList.AddTag(MUIA_Slider_Horiz, AsTag(FHoriz));
end;

procedure TMUISlider.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Slider, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUISlider.SetHoriz(AValue: Boolean);
begin
  if AValue <> FHoriz then
  begin
    FHoriz := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Slider_Horiz, FHoriz);
  end;
end;

procedure TMUISlider.SetQuiet(AValue: Boolean);
begin
  if AValue <> FQuiet then
  begin
    FQuiet := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Slider_Quiet, FQuiet);
  end;
end;

end.
