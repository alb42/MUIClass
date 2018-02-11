unit MUIClass.PopString;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, agraphics, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Group, MUIClass.Area, MUIClass.Gadget;

type
  TOpenPopEvent = function(Sender: TObject): Boolean of object;
  TClosePopEvent = procedure(Sender: TObject; Success: Boolean) of object;
  TMUIPopString = class(TMUIGroup)
  private
    FToggle: Boolean;
    FButton: TMUIArea;
    FString: TMUIString;
    OpenHook: PHook;
    CloseHook: PHook;
    FOnOpen: TOpenPopEvent;
    FOnClose: TClosePopEvent;

    procedure SetToggle(AValue: Boolean);
    procedure SetButton(AValue: TMUIArea);
    procedure SetString(AValue: TMUIString);
    procedure SetOnOpen(AOnOpen: TOpenPopEvent);
    procedure SetOnClose(AOnClose: TClosePopEvent);
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

    // Methods
    procedure Open;
    procedure Close(Result: Boolean);
    // Fields
  published
    property Toggle: Boolean read FToggle write SetToggle;           //
    property Button: TMUIArea read FButton write SetButton;          //I
    property StringObj: TMUIString read FString write SetString;     //I
    //Events
    property OnOpen: TOpenPopEvent read FOnOpen write SetOnOpen;
    property OnClose: TClosePopEvent read FOnClose write SetOnClose;
  end;

  TMUIPopObject = class(TMUIPopString)
  private
    FFollow: Boolean;
    FLight: Boolean;
    FObject: TMUIArea;
    StrObjHook: PHook;
    ObjStrHook: PHook;
    FOnStrObj: TOpenPopEvent;
    FOnObjStr: TNotifyEvent;
    FVolatile: Boolean;
    procedure SetFollow(AValue: Boolean);
    procedure SetLight(AValue: Boolean);
    procedure SetObject(AValue: TMUIArea);
    procedure SetOnStrObj(AOnStrObj: TOpenPopEvent);
    procedure SetOnObjStr(AValue: TNotifyEvent);
    procedure SetVolatile(AValue: Boolean);
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
  published
    property Follow: Boolean read FFollow write SetFollow default True;       //
    property Light: Boolean read FLight write SetLight default True;          //
    property PopObject: TMUIArea read FObject write SetObject;                //I
    property Volatile: Boolean read FVolatile write SetVolatile default True; //
    // Events
    property OnStrObj: TOpenPopEvent read FOnStrObj write SetOnStrObj;
    property OnObjStr: TNotifyEvent read FOnObjStr write SetOnObjStr;
    // WindowHook
  end;

  TMUIPoplist = class(TMUIPopObject)
  private
    FLArray: TStringArray;
    PCs: array of PChar;
    procedure SetLArray(AValue: TStringArray);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
  published
    property LArray: TStringArray read FLArray write SetLArray;
  end;

  TMUIPopASL = class(TMUIPopObject)
  private
    FASLType: LongWord;
    procedure SetASLType(AValue: LongWord);
    function GetActive: Boolean;
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Active: Boolean read GetActive;
  published
    property ASLType: LongWord read FASLType write SetASLType default 0; //I ASL_*Request
    // StartHook/StopHook
  end;

implementation

{ TMUIPopString }

constructor TMUIPopString.Create;
begin
  inherited;
  FToggle := False;
  FButton := nil;
  FString := nil;
  OpenHook := nil;
end;

destructor TMUIPopString.Destroy;
begin
  if Assigned(FButton) then
    FButton.ClearObject;
  FButton.Free;
  if Assigned(FString) then
    FString.ClearObject;
  FString.Free;
  inherited;
end;

procedure TMUIPopString.BeforeCreateObject;
begin
  inherited;
  if Assigned(FButton) then
    FButton.CreateObject;
  if Assigned(FString) then
    FString.CreateObject;
end;

procedure TMUIPopString.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FToggle then
    ATagList.AddTag(MUIA_PopString_Toggle, AsTag(FToggle));
  if Assigned(FButton) and FButton.HasObj then
    ATagList.AddTag(MUIA_PopString_Button, AsTag(FButton.MUIObj));
  if Assigned(FString) and FString.HasObj then
    ATagList.AddTag(MUIA_PopString_String, AsTag(FString.MUIObj));
  if Assigned(OpenHook) then
    ATagList.AddTag(MUIA_PopString_OpenHook, AsTag(OpenHook));
  if Assigned(CloseHook) then
    ATagList.AddTag(MUIA_PopString_CloseHook, AsTag(CloseHook));
end;

procedure TMUIPopString.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_PopString, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIPopString.AfterCreateObject;
var
  Obj: PObject_;
begin
  inherited;
  if not Assigned(FButton) then
  begin
    Obj := GetPointerValue(MUIA_PopString_Button);
    if Assigned(Obj) then
    begin
      FButton := TMUIArea.Create;
      FButton.MUIObj := Obj;
    end;
  end;
  if not Assigned(FString) then
  begin
    Obj := GetPointerValue(MUIA_PopString_String);
    if Assigned(Obj) then
    begin
      FString := TMUIString.Create;
      FString.MUIObj := Obj;
    end;
  end;
end;

procedure TMUIPopString.DestroyObject;
begin
  inherited;
  if Assigned(FButton) then
    FButton.ClearObject;
  if Assigned(FString) then
    FString.ClearObject;
end;

procedure TMUIPopString.ClearObject;
begin
  inherited;
  if Assigned(FButton) then
    FButton.ClearObject;
  if Assigned(FString) then
    FString.ClearObject;
end;

procedure TMUIPopString.SetToggle(AValue: Boolean);
begin
  if AValue <> FToggle then
  begin
    FToggle := AValue;
    if HasObj then
      SetValue(MUIA_PopString_Toggle, FToggle);
  end;
end;

procedure TMUIPopString.SetButton(AValue: TMUIArea);
begin
  if AValue <> FButton then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_PopString_Button', HexStr(AValue))
    else
      FButton := AValue;
  end;
end;

procedure TMUIPopString.SetString(AValue: TMUIString);
begin
  if AValue <> FString then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_PopString_String', HexStr(AValue))
    else
      FString := AValue;
  end;
end;

procedure TMUIPopString.Open;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_PopString_Open]);
end;

procedure TMUIPopString.Close(Result: Boolean);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_PopString_Close, AsTag(Result)]);
end;

function OpenFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIPopString;
begin
  Result := AsTag(False);
  PasObj := TMUIPopString(Hook^.h_Data);
  if Assigned(PasObj.FOnOpen) then
    Result := AsTag(PasObj.FOnOpen(PasObj));
end;

procedure TMUIPopString.SetOnOpen(AOnOpen: TOpenPopEvent);
begin
  if AOnOpen <> FOnOpen then
  begin
    FOnOpen := AOnOpen;
    if Assigned(FOnOpen) then
    begin
      if not Assigned(OpenHook) then
        OpenHook := HookList.GetNewHook;
      MH_SetHook(OpenHook^, @OpenFunc, Self);
      if HasObj then
        SetValue(MUIA_PopString_OpenHook, AsTag(OpenHook));
    end;
  end;
end;

function CloseFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
type
  TCloseMsg = record
    S: PObject_;
    Res: Integer;
  end;
var
  CMsg: ^TCloseMsg;
  PasObj: TMUIPopString;
begin
  writeln('onClose');
  Result := 0;
  PasObj := TMUIPopString(Hook^.h_Data);
  if Assigned(PasObj.FOnClose) then
  begin
    CMsg := Msg;
    writeln('MSG: ', CMsg^.Res);
    PasObj.FOnClose(PasObj, CMsg^.Res <> 0);
  end;
end;

procedure TMUIPopString.SetOnClose(AOnClose: TClosePopEvent);
begin
  if AOnClose <> FOnClose then
  begin
    FOnClose := AOnClose;
    if Assigned(FOnClose) then
    begin
      if not Assigned(CloseHook) then
        CloseHook := HookList.GetNewHook;
      MH_SetHook(CloseHook^, @CloseFunc, Self);
      if HasObj then
        SetValue(MUIA_PopString_CloseHook, AsTag(CloseHook));
    end;
  end;
end;

{ TMUIPoplist }

constructor TMUIPoplist.Create;
begin
  inherited;
  SetLength(FLArray, 0);
end;

procedure TMUIPoplist.GetCreateTags(var ATagList: TATagList);
var
  I: Integer;
begin
  inherited;
  if Length(FLArray) > 0 then
  begin
    SetLength(PCs, Length(FLArray) + 1);
    for i := 0 to High(FLArray) do
      PCs[i] := PChar(FLArray[i]);
    PCs[High(PCs)] := nil;
    ATagList.AddTag(MUIA_Poplist_Array, AsTag(@(PCs[0])));
  end;
end;

procedure TMUIPoplist.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Poplist, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIPoplist.SetLArray(AValue: TStringArray);
begin
  if Assigned(FMUIObj) then
    ComplainIOnly(Self, 'MUIA_Poplist_Array', '<string array>')
  else
    FLArray := Copy(AValue);
end;

{ TMUIPopObject }

constructor TMUIPopObject.Create;
begin
  inherited;
  FFollow := True;
  FLight := True;
  FObject := nil;
  StrObjHook := nil;
  ObjStrHook := nil;
  FVolatile := True;
end;

destructor TMUIPopObject.Destroy;
begin
  if Assigned(FObject) then
    FObject.ClearObject;
  FObject.Free;
  inherited;
end;

procedure TMUIPopObject.BeforeCreateObject;
begin
  inherited;
  if Assigned(FObject) then
    FObject.CreateObject;
end;

procedure TMUIPopObject.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if not FFollow then
    ATagList.AddTag(MUIA_PopObject_Follow, AsTag(FFollow));
  if not FLight then
    ATagList.AddTag(MUIA_PopObject_Light, AsTag(FLight));
  if Assigned(FObject) and FObject.HasObj then
    ATagList.AddTag(MUIA_PopObject_Object, AsTag(FObject.MUIObj));
  if Assigned(StrObjHook) then
    ATagList.AddTag(MUIA_PopObject_StrObjHook, AsTag(StrObjHook));
  if Assigned(ObjStrHook) then
    ATagList.AddTag(MUIA_PopObject_ObjStrHook, AsTag(ObjStrHook));
  if not FVolatile then
    ATagList.AddTag(MUIA_PopObject_Volatile, AsTag(FVolatile));
end;

procedure TMUIPopObject.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_PopObject, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIPopObject.AfterCreateObject;
var
  Obj: PObject_;
begin
  inherited;
  if not Assigned(FObject) then
  begin
    Obj := GetPointerValue(MUIA_PopObject_Object);
    if Assigned(Obj) then
    begin
      FObject := TMUIArea.Create;
      FObject.MUIObj := Obj;
    end;
  end;
end;

procedure TMUIPopObject.DestroyObject;
begin
  inherited;
  if Assigned(FObject) then
    FObject.ClearObject;
end;

procedure TMUIPopObject.ClearObject;
begin
  inherited;
  if Assigned(FObject) then
    FObject.ClearObject;
end;

procedure TMUIPopObject.SetFollow(AValue: Boolean);
begin
  if FFollow <> AValue then
  begin
    FFollow := AValue;
    if HasObj then
      SetValue(MUIA_PopObject_Follow, AsTag(FFollow));
  end;
end;

procedure TMUIPopObject.SetLight(AValue: Boolean);
begin
  if FLight <> AValue then
  begin
    FLight := AValue;
    if HasObj then
      SetValue(MUIA_PopObject_Light, AsTag(FLight));
  end;
end;

procedure TMUIPopObject.SetObject(AValue: TMUIArea);
begin
  if AValue <> FObject then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_PopObject_Object', HexStr(AValue))
    else
      FObject := AValue;
  end;
end;

function StrObjFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIPopObject;
begin
  Result := AsTag(False);
  PasObj := TMUIPopObject(Hook^.h_Data);
  if Assigned(PasObj.FOnStrObj) then
    Result := AsTag(PasObj.FOnStrObj(PasObj));
end;

function ObjStrFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIPopObject;
begin
  Result := 0;
  PasObj := TMUIPopObject(Hook^.h_Data);
  if Assigned(PasObj.FOnObjStr) then
    PasObj.FOnObjStr(PasObj);
end;

procedure TMUIPopObject.SetOnStrObj(AOnStrObj: TOpenPopEvent);
begin
  if AOnStrObj <> FOnStrObj then
  begin
    FOnStrObj := AOnStrObj;
    if Assigned(FOnStrObj) then
    begin
      if not Assigned(StrObjHook) then
        StrObjHook := HookList.GetNewHook;
      MH_SetHook(StrObjHook^, @StrObjFunc, Self);
      if HasObj then
        SetValue(MUIA_PopObject_StrObjHook, AsTag(StrObjHook));
    end;
  end;
end;

procedure TMUIPopObject.SetOnObjStr(AValue: TNotifyEvent);
begin
  if AValue <> FOnObjStr then
  begin
    FOnObjStr := AValue;
    if Assigned(FOnObjStr) then
    begin
      if not Assigned(ObjStrHook) then
        ObjStrHook := HookList.GetNewHook;
      MH_SetHook(ObjStrHook^, @ObjStrFunc, Self);
      if HasObj then
        SetValue(MUIA_PopObject_ObjStrHook, AsTag(ObjStrHook));
    end;
  end;
end;

procedure TMUIPopObject.SetVolatile(AValue: Boolean);
begin
  if FVolatile <> AValue then
  begin
    FVolatile := AValue;
    if HasObj then
      SetValue(MUIA_PopObject_Volatile, AsTag(FVolatile));
  end;
end;

{ TMUIPopASL }

constructor TMUIPopASL.Create;
begin
  inherited;
  FASLType := 0;
end;

procedure TMUIPopASL.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FASLType <> 0 then
    ATagList.AddTag(MUIA_PopASL_Type, AsTag(FASLType));
end;

procedure TMUIPopASL.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_PopASL, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function TMUIPopASL.GetActive: Boolean;
begin
  Result := False;
  if HasObj then
    Result := GetBoolValue(MUIA_PopASL_Active);
end;

procedure TMUIPopASL.SetASLType(AValue: LongWord);
begin
  if Assigned(FMUIObj) then
    ComplainIOnly(Self, 'MUIA_PopASL_Type', IntToStr(AValue))
  else
    FASLType := AValue;
end;


end.
