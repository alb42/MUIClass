unit MUIClass.Base;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, icon,
  {$ifndef AmigaOS4} // OS4 still no commodities unit
  Commodities,
  {$endif}
  mui, muihelper,
  tagsparamshelper;

{$M+}
type
  {$ifdef AmigaOS4}
  PCxObj = Pointer;
  {$endif}

  TIOnlyEvent = procedure(AClass: TObject; Field, Value: string) of object;
  TExceptionEvent = procedure(Sender: TObject; E: Exception) of object;

  TMUITimer = class;

  THookList = class
  private
    FList: Classes.TList;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetNewHook: PHook;
    procedure RemoveHook(AHook: PHook);
  end;

  TMUIRootClass = class
  private
    function GetHasObj: Boolean;
  protected
    HookList: THookList;
    FFirstOpen: Boolean;
    FMUIObj: PObject_;
    procedure GetCreateTags(var ATagList: TATagList); virtual;
    //
    procedure DoFirstOpen; virtual;
    //
    procedure SetValue(Tag: LongWord; Value: PtrUInt); virtual; overload;
    procedure SetValue(Tag: LongWord; Value: PtrInt); virtual; overload;
    procedure SetValue(Tag: LongWord; Value: Boolean); virtual; overload;
    procedure SetValue(Tag: LongWord; Value: Pointer); virtual; overload;
    function GetIntValue(Tag: LongWord): PtrInt; virtual;
    function GetBoolValue(Tag: LongWord): Boolean; virtual;
    function GetPointerValue(Tag: LongWord): Pointer; virtual;
    function GetStringValue(Tag: LongWord): string; virtual;

    procedure ConnectHookObject(AObj: PObject_; MUIField: PtrUInt; TriggerValue: PtrUInt; HookFunc: THookFunc);
    procedure ConnectHook(MUIField: PtrUInt; TriggerValue: PtrUInt; HookFunc: THookFunc);

    procedure ClearObject; virtual;
    procedure BeforeCloseWindow; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure CreateObject; virtual; abstract;
    procedure DestroyObject; virtual;

    property MUIObj: PObject_ read FMUIObj write FMUIObj;
    property HasObj: Boolean read GetHasObj;
  end;

  TChildList = specialize TFPGObjectList<TMUIRootclass>;

  TMUINotify = class(TMUIRootClass)
  private
    FHelpLine: Integer;
    FHelpNode: string;
    FChilds: TChildList;
    procedure SetHelpLine(AValue: Integer);
    procedure SetHelpNode(AValue: string);
  protected
    FExchangeMode: Boolean;
    procedure InitChange; virtual;
    procedure ExitChange; virtual;
    procedure BeforeCreateObject; virtual;
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; virtual;
    procedure DoFirstOpen; override;
    procedure BeforeCloseWindow; override;
    property Childs: TChildList read FChilds;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure DestroyObject; override;
    procedure ClearObject; override;

    procedure AddChild(AChild: TMUINotify); virtual;
    procedure RemoveChild(AChild: TMUINotify); virtual;

    class function GetPasObject(AMUIObj: PObject_): TMUINotify;

    property HelpLine: Integer read FHelpLine write SetHelpLine;
    property HelpNode: string read FHelpNode write SetHelpNode;
  end;

  TTimerList = specialize TFPGObjectList<TMUITimer>;

  TMUIApplication = class(TMUINotify)
  private
    FToDestroy: TChildList;
    FTimerList: TTimerList;
    FActiveTimer: Boolean;
    FTerminated: Boolean;
    FMainWindow: TMUINotify;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FOnIdle: TNotifyEvent;
    FAuthor: string;          //* ''
    FBase: string;            //* ''
    FBrokerPri: Integer;      //  0
    FCopyright: string;       //* ''
    FDescription: string;     //* ''
    FMyDiskObject: Pointer;
    FDiskObject: Pointer;     // App.info
    FOnDoubleStart: TNotifyEvent;
    FHelpFile: string;        // ''
    FOnIconify: TNotifyEvent;
    FOnRestore: TNotifyEvent;
    FSingleTask: Boolean;     //* False
    FSleepCount: Integer;
    FUseCommodities: Boolean; //* True
    FTitle: string;           //* ''
    FVersion: string;         //* ''
    FMenuStrip: TMUINotify; //
    FOnException: TExceptionEvent;
    procedure SetActive(AValue: Boolean);
    function GetActive: Boolean;
    procedure SetAuthor(AValue: string);
    procedure SetBase(AValue: string);
    function GetBase: string;
    function GetBroker: PCxObj;
    procedure SetBrokerPri(AValue: Integer);
    procedure SetCopyright(AValue: string);
    procedure SetDescription(AValue: string);
    procedure SetDiskObject(AValue: Pointer);
    function GetForceQuit: Boolean;
    procedure SetHelpFile(AValue: string);
    function GetIconified: Boolean;
    procedure SetIconified(AValue: Boolean);
    procedure SetSingleTask(AValue: Boolean);
    function GetSleep: Boolean;
    procedure SetSleep(AValue: Boolean);
    procedure SetUseCommodities(AValue: Boolean);
    procedure SetTitle(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetMenuStrip(AValue: TMUINotify);
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure CreateObject; override;
    procedure AfterCreateObject; override;

    procedure DestroyObject; override;

    procedure Run;

    procedure Terminate;

    property Terminated: Boolean read FTerminated;
    property MainWindow: TMUINotify read FMainWindow; // TMUIWindow
  public
    property Childs;
  public
    procedure AddToDestroy(AObj: TMUINotify);
    procedure DoException(E: Exception); virtual;
    // MUI Methods
    procedure AboutMUI(Window: TMUINotify = nil); // TMUIWindow
    procedure AddInputHandler(IhNode: PMUI_InputHandlerNode);
    procedure RemInputHandler(IhNode: PMUI_InputHandlerNode);
    procedure CheckRefresh;
    procedure InputBuffered;
    procedure Load(FromEnvarc: Boolean);
    procedure OpenConfigWindow;
    procedure Save(ToEnvarc: Boolean);
    procedure ShowHelp(Window: TMUINotify; HelpFileName: string; Node: string; LineNum: Integer); overload; // TMUIWindow
    procedure ShowHelp(Node: string; LineNum: Integer); overload;

    // MUI Fields
    property Broker: PCxObj read GetBroker;
    property DiskObject: Pointer read FDiskObject write SetDiskObject;
    property ForceQuit: Boolean read GetForceQuit;
    property Iconified: Boolean read GetIconified write SetIconified;
    property Sleep: Boolean read GetSleep write SetSleep;

  published
    property Active: Boolean read GetActive write SetActive;
    property Author: string read FAuthor write SetAuthor;
    property Base: string read GetBase write SetBase;
    // Broker Hook
    // BrokerPort
    property BrokerPri: Integer read FBrokerPri write SetBrokerPri;
        // Commands
    property Copyright: string read FCopyright write SetCopyright;
    property Description: string read FDescription write SetDescription;
    // Drop object
    property HelpFile: string read FHelpFile write SetHelpFile;
    property Menustrip: TMUINotify read FMenuStrip write SetMenuStrip; // TMUIMenuStrip
    // RexxMsg, RexxString (AROS no support)
    property SingleTask: Boolean read FSingleTask write SetSingleTask;
    property Title: string read FTitle write SetTitle;
    property UseCommodities: Boolean read FUseCommodities write SetUseCommodities;
    // Usedclasses
    // UseRexx (No REXX in AROS)
    property Version: string read FVersion write SetVersion;

    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;
    property OnDoubleStart: TNotifyEvent read FOnDoubleStart write FOnDoubleStart;
    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnIconify: TNotifyEvent read FOnIconify write FOnIconify;
    property OnRestore: TNotifyEvent read FOnRestore write FOnRestore;
    property OnException: TExceptionEvent read FOnException write FOnException;
  end;

  TMUIWithParent = class(TMUINotify)
  private
    FParent: TMUINotify;
  protected
    procedure SetParent(AParent: TMUINotify); virtual;
  public
    destructor Destroy; override;
    procedure DestroyObject; override;
    property Parent: TMUINotify read FParent write SetParent;
  end;

  TMUIFamily = class(TMUIWithParent)
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    property Childs;
  end;

  TMUISemaphore = class(TMUIWithParent)
  public
    procedure CreateObject; override;
    // Methods
    function Attempt: Boolean;
    function AtteptShared: Boolean;
    procedure Obtain;
    procedure ObtainShared;
    procedure Release;
  end;

  TMUIDataspace = class(TMUISemaphore)
  private
    FPool: Pointer;
    procedure SetPool(AValue: Pointer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Method
    function Add(Data: Pointer; Len: Integer; Id: LongWord): Boolean;
    procedure Clear;
    function Find(Id: LongWord): Pointer;
    function Merge(DS: TMUIDataSpace): Integer;
    function ReadIFF(IFFHandle: Pointer): Integer;
    function WriteIFF(IFFHandle: Pointer; typ: LongWord; id: LongWord): Integer;
    function Remove(Id: LongWord): Boolean;
    //Field
    property Pool: Pointer read FPool write SetPool;
  end;

  TMUITimer = class
  private
    FEnabled: boolean;
    FInterval: Integer;
    FLastStart: Int64;
    FOnTimer: TNotifyEvent;
    procedure SetEnabled(AValue: boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Interval: Integer read FInterVal write FInterval;
    property OnTimer: TNotifyEvent read FOnTimer write FOnTimer;
  end;

procedure ComplainIOnly(AClass: TObject; Field, Value: string);

var
  MUIApp: TMUIApplication = nil;
  OnIOnlyWarning: TIOnlyEvent = nil;

implementation


uses
  MUIClass.Window, MUIClass.Dialog;

procedure ComplainIOnly(AClass: TObject; Field, Value: string);
begin
  SysDebugLn('Warning: ' + AClass.Classname + ' tries to set ' + Field + ' to Value ' + Value + ', but Object is already created and field is not writeable.');
  if Assigned(OnIOnlyWarning) then
    OnIOnlyWarning(AClass, Field, Value);
end;

{ THookList }

constructor THookList.Create;
begin
  FList := Classes.TList.Create;
end;

destructor THookList.Destroy;
var
  Hook: PHook;
  i: Integer;
begin
  // dispose all the saved Hooks
  for i := 0 to FList.Count - 1 do
  begin
    Hook := PHook(FList[i]);
    Dispose(Hook);
  end;
  FList.Free;
  inherited;
end;

function THookList.GetNewHook: PHook;
begin
  New(Result);
  FList.Add(Result);
end;

procedure THookList.RemoveHook(AHook: PHook);
begin
  if Assigned(AHook) and (FList.IndexOf(AHook) >= 0) then
  begin
    FList.Remove(AHook);
    Dispose(AHook);
  end;
end;


{ TMUIRootClass }

constructor TMUIRootClass.Create;
begin
  FFirstOpen := True;
  FMuiObj := nil;
  HookList := THookList.Create;
end;


destructor TMUIRootClass.Destroy;
begin
  HookList.Free;
  inherited;
end;

procedure TMUIRootClass.DestroyObject;
begin
  if Assigned(FMUIObj) then
    MUI_DisposeObject(FMUIObj);
  FMUIObj := nil;
end;

// warning clears all childs, because we destroyed them already
procedure TMUIRootClass.ClearObject;
begin
  FMUIObj := nil;
end;

procedure TMUIRootClass.DoFirstOpen;
begin
end;

procedure TMUIRootClass.GetCreateTags(var ATagList: TATagList);
begin
end;

function TMUIRootClass.GetHasObj: Boolean;
begin
  Result := Assigned(FMUIObj);
end;


procedure TMUIRootClass.SetValue(Tag: LongWord; Value: PtrUInt);
begin
  if Assigned(FMUIObj) then
    MH_Set(FMUIObj, Tag, Value);
end;

procedure TMUIRootClass.SetValue(Tag: LongWord; Value: PtrInt);
begin
  if Assigned(FMUIObj) then
    MH_Set(FMUIObj, Tag, Value);
end;

procedure TMUIRootClass.SetValue(Tag: LongWord; Value: Boolean);
begin
  if Assigned(FMUIObj) then
    MH_Set(FMUIObj, Tag, AsTag(Value));
end;

procedure TMUIRootClass.SetValue(Tag: LongWord; Value: Pointer);
begin
  if Assigned(FMUIObj) then
    MH_Set(FMUIObj, Tag, AsTag(Value));
end;

function TMUIRootClass.GetIntValue(Tag: LongWord): PtrInt;
begin
  Result := 0;
  if Assigned(FMUIObj) then
    Result := PtrInt(MH_Get(FMUIObj, Tag));
end;

function TMUIRootClass.GetBoolValue(Tag: LongWord): Boolean;
begin
  Result := False;
  if Assigned(FMUIObj) then
    Result := LongBool(MH_Get(FMUIObj, Tag));
end;

function TMUIRootClass.GetPointerValue(Tag: LongWord): Pointer;
begin
  Result := nil;
  if Assigned(FMUIObj) then
    Result := Pointer(MH_Get(FMUIObj, Tag));
end;

function TMUIRootClass.GetStringValue(Tag: LongWord): string;
var
  PC: PChar;
begin
  Result := '';
  PC := GetPointerValue(Tag);
  if Assigned(PC) then
    Result := string(PC);
end;

procedure TMUIRootClass.BeforeCloseWindow;
begin

end;


procedure TMUIRootClass.ConnectHookObject(AObj: PObject_; MUIField: PtrUInt; TriggerValue: PtrUInt; HookFunc: THookFunc);
var
  Hook: PHook;
begin
  Hook := HookList.GetNewHook;

  MH_SetHook(Hook^, HookFunc, Self);
  DoMethod(AObj, [MUIM_Notify, MUIField, TriggerValue, AsTag(AObj), 2, MUIM_CallHook, AsTag(Hook)]);
end;


procedure TMUIRootClass.ConnectHook(MUIField: PtrUInt; TriggerValue: PtrUInt; HookFunc: THookFunc);
begin
  ConnectHookObject(FMUIObj, MUIField, TriggerValue, HookFunc);
end;

{ TMUINotify }

constructor TMUINotify.Create;
begin
  inherited;
  FChilds := TChildList.Create(False);
  FHelpLine := 0;
  FHelpNode := '';
end;


destructor TMUINotify.Destroy;
var
  i: Integer;
begin
  for i := FChilds.Count - 1 downto 0 do
  begin
    FChilds[i].Free;
  end;
  FChilds.Free;
  inherited;
end;

procedure TMUINotify.BeforeCreateObject;
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
    FChilds[i].CreateObject;
end;

procedure TMUINotify.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FHelpLine <> 0 then
    ATagList.AddTag(MUIA_HelpLine, AsTag(FHelpLine));
  if FHelpNode <> '' then
    ATagList.AddTag(MUIA_HelpNode, AsTag(PChar(FHelpNode)));
end;

procedure TMUINotify.DestroyObject;
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
    FChilds[i].DestroyObject;
  inherited;
end;

procedure TMUINotify.ClearObject;
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
    FChilds[i].ClearObject;
  inherited;
end;

procedure TMUINotify.DoFirstOpen;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FChilds.Count - 1 do
    FChilds[i].DoFirstOpen;
end;

procedure TMUINotify.BeforeCloseWindow;
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
    FChilds[i].BeforeCloseWindow;
end;

procedure TMUINotify.AddChild(AChild: TMUINotify);
var
  RightMode: Boolean;
begin
  if Assigned(AChild) and (FChilds.IndexOf(AChild) < 0) then
  begin
    FChilds.Add(AChild);
    if HasObj then
    begin
      RightMode := FExchangeMode;
      if not RightMode then
        InitChange;
      //
      DoMethod(FMUIObj, [NativeUInt(OM_ADDMEMBER), AsTag(AChild.MUIObj)]);
      //
      if not RightMode then
        ExitChange;
    end;
  end;
end;

procedure TMUINotify.RemoveChild(AChild: TMUINotify);
var
  RightMode: Boolean;
begin
  if Assigned(AChild) and (FChilds.IndexOf(AChild) >= 0) then
  begin
    FChilds.Remove(AChild);
    if HasObj then
    begin
      RightMode := FExchangeMode;
      if not RightMode then
        InitChange;
      //
      DoMethod(FMUIObj, [NativeUInt(OM_REMMEMBER), AsTag(AChild.MUIObj)]);
      //
      if not RightMode then
        ExitChange;
    end;
  end;
end;

procedure TMUINotify.InitChange;
begin
  FExchangeMode := True;
end;

procedure TMUINotify.ExitChange;
begin
  FExchangeMode := False;
end;

procedure TMUINotify.AfterCreateObject;
begin
  if Assigned(FMUIObj) then
    SetValue(MUIA_UserData, Self);
end;

class function TMUINotify.GetPasObject(AMUIObj: PObject_): TMUINotify;
var
  p: TObject;
begin
  Result := nil;
  if Assigned(AMUIObj) then
  begin
    p := TObject(MH_Get(AMUIObj, MUIA_UserData));
    if p is TMUINotify then
      Result := TMUINotify(p);
  end;
end;

procedure TMUINotify.SetHelpLine(AValue: Integer);
begin
  if AValue <> FHelpLine then
  begin
    FHelpLine := AValue;
    if HasObj then
      SetValue(MUIA_HelpLine, AsTag(FHelpLine));
  end;
end;

procedure TMUINotify.SetHelpNode(AValue: string);
begin
  if AValue <> FHelpNode then
  begin
    FHelpNode := AValue;
    if HasObj then
      SetValue(MUIA_HelpNode, AsTag(PChar(FHelpNode)));
  end;
end;

{ TMUIApplication }

constructor TMUIApplication.Create;
begin
  inherited;
  FToDestroy := TChildList.Create(False);
  FTimerList := TTimerList.Create(False);
  FActiveTimer := False;
  FTerminated := False;
  FMainWindow := nil;
  // Inits
  FAuthor := '';
  FBase := '';
  FBrokerPri := 0;
  FCopyright := '';
  FDescription := '';
  FMyDiskObject := GetDiskObject(PChar(ParamStr(0)));
  FDiskObject := FMyDiskObject;
  FHelpFile := '';
  FSingleTask := False;
  FSleepCount := 0;
  FUseCommodities := True;
  FTitle := ExtractFilename(ParamStr(0));
  FVersion := '';
  FMenuStrip := nil;
end;

destructor TMUIApplication.Destroy;
var
  i: Integer;
begin
  FreeDiskObject(FMyDiskObject);
  FMenuStrip.Free;
  for i := 0 to FToDestroy.Count - 1 do
    FToDestroy[i].Free;
  FToDestroy.Free;
  while FTimerList.Count > 0 do
  begin
    FTimerList[0].Free;
  end;
  inherited;
end;

procedure TMUIApplication.GetCreateTags(var ATagList: TATagList);
var
  i: Integer;
begin
  inherited;
  for i := 0 to FChilds.Count - 1 do
  begin
    if FChilds[i].HasObj then
      ATagList.AddTag(MUIA_Application_Window, AsTag(FChilds[i].MUIObj));
  end;
  if FAuthor <> '' then
    ATagList.AddTag(MUIA_Application_Author, AsTag(PChar(FAuthor)));
  if FBase <> '' then
    ATagList.AddTag(MUIA_Application_Base, AsTag(PChar(FBase)));
  if FBrokerPri <> 0 then
    ATagList.AddTag(MUIA_Application_BrokerPri, AsTag(FBrokerPri));
  if FCopyright <> '' then
    ATagList.AddTag(MUIA_Application_Copyright, AsTag(PChar(FCopyright)));
  if FDescription <> '' then
    ATagList.AddTag(MUIA_Application_Description, AsTag(PChar(FDescription)));
  ATagList.AddTag(MUIA_Application_DiskObject, AsTag(FDiskObject));
  if FHelpFile <> '' then
    ATagList.AddTag(MUIA_Application_HelpFile, AsTag(PChar(FHelpFile)));
  if FSingleTask then
    ATagList.AddTag(MUIA_Application_SingleTask, AsTag(FSingleTask));
  if not FUseCommodities then
    ATagList.AddTag(MUIA_Application_UseCommodities, AsTag(FUseCommodities));
  ATagList.AddTag(MUIA_Application_Title, AsTag(PChar(FTitle)));
  if FVersion <> '' then
    ATagList.AddTag(MUIA_Application_Version, AsTag(PChar(FVersion)));
  if Assigned(FMenuStrip) then
  begin
    FMenuStrip.CreateObject;
    ATagList.AddTag(MUIA_Application_MenuStrip, AsTag(FMenuStrip.MuiObj));
  end;
end;

procedure TMUIApplication.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Application, TagList.GetTagPointer);
    AfterCreateObject;
  end;
end;

procedure TMUIApplication.DestroyObject;
begin
  if Assigned(FMenuStrip) then
    FMenuStrip.ClearObject;
  inherited;
end;


function ActivateFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIApplication;
begin
  try
    Result := 0;
    PasObj := TMUIApplication(Hook^.h_Data);
    if Assigned(PasObj.FOnActivate) then
      PasObj.FOnActivate(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function DeactivateFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIApplication;
begin
  try
    Result := 0;
    PasObj := TMUIApplication(Hook^.h_Data);
    if Assigned(PasObj.FOnDeactivate) then
      PasObj.FOnDeactivate(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function DoubleStartFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIApplication;
begin
  try
    Result := 0;
    PasObj := TMUIApplication(Hook^.h_Data);
    if Assigned(PasObj.FOnDoubleStart) then
      PasObj.FOnDoubleStart(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function IconifyFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIApplication;
begin
  try
    Result := 0;
    PasObj := TMUIApplication(Hook^.h_Data);
    if Assigned(PasObj.FOnIconify) then
      PasObj.FOnIconify(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function RestoreFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIApplication;
begin
  try
    Result := 0;
    PasObj := TMUIApplication(Hook^.h_Data);
    if Assigned(PasObj.FOnRestore) then
      PasObj.FOnRestore(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;


procedure TMUIApplication.AfterCreateObject;
begin
  inherited;
  ConnectHook(MUIA_Application_Active, MUI_TRUE, @ActivateFunc);
  ConnectHook(MUIA_Application_Active, MUI_FALSE, @DeactivateFunc);
  ConnectHook(MUIA_Application_DoubleStart, MUI_TRUE, @DoubleStartFunc);
  ConnectHook(MUIA_Application_Iconified, MUI_TRUE, @IconifyFunc);
  ConnectHook(MUIA_Application_Iconified, MUI_FALSE, @RestoreFunc);
end;


procedure TMUIApplication.DoException(E: Exception);
begin
  if Assigned(FOnException) then
    FOnException(Self, E)
  else
    if MessageBox('Exception', 'Exception: ' + E.Message + #10 + 'To prevent Data corruption you should close the program.', ['Ignore', 'Close Program']) = 0 then
      Terminate;
end;

procedure TMUIApplication.Run;
var
  Sigs: LongInt;
  i: Integer;
  t1: Int64;
begin
  if Childs.Count = 0 then
  begin
    ShowMessage('No Windows to open, Exit');
    Exit;
  end;
  // Create the objects
  CreateObject;
  if not HasObj then
  begin
    ShowMessage('Unable to create application');
    Exit;
  end;
  // connect the close event to first Window
  FMainWindow := TMUINotify(Childs[0]);
  //
  TMUIWindow(FMainWindow).Show;
  if not TMUIWindow(FMainWindow).Open then
  begin
    ShowMessage('Unable to  open Window');
    Exit;
  end;
  FTerminated := False;
  while not FTerminated  do
  begin
    // poll loop
    try
      if Integer(DoMethod(MUIApp.MUIObj, [MUIM_Application_NewInput, AsTag(@sigs)])) = MUIV_Application_ReturnID_Quit then
        Break;
    except
      On E:Exception do
      begin
        if FTerminated then
          Break;
      end;
    end;
    //
    if (FToDestroy.Count > 0) then
    begin
      for i := 0 to FToDestroy.Count - 1 do
        FToDestroy[i].Free;
      FToDestroy.Clear;
    end;
    //
    if FTerminated then
      Break;
    if Sigs <> 0 then
    begin
      if Assigned(FOnIdle) or (FActiveTimer) then
      begin
        if Assigned(FOnIdle) then
          FOnIdle(Self);
        t1 := GetTickCount64;
        I := 0;
        while i < FTimerList.Count do
        begin
          if FTimerList[i].Enabled and Assigned(FTimerList[i].OnTimer) then
          begin
            if t1 > FTimerList[i].FLastStart + FTimerList[i].InterVal then
            begin
              FTimerList[i].FLastStart := t1;
              FTimerList[i].OnTimer(Self);
            end;
          end;
          Inc(i);
        end;
      end
      else
      begin
        Sigs := Wait(sigs or SIGBREAKF_CTRL_C);
        if (Sigs and SIGBREAKF_CTRL_C) <>0 then
          Break;
      end;
    end;
  end;

  // Close Window
  TMUIWindow(FMAinWindow).Close;
  // Free MUI Objects
  DestroyObject;
  ClearObject;
end;

procedure TMUIApplication.Terminate;
begin
  FTerminated := True;
end;

procedure TMUIApplication.AddToDestroy(AObj: TMUINotify);
begin
  if Assigned(AObj) then
    FToDestroy.Add(AObj);
end;

procedure TMUIApplication.SetActive(AValue: Boolean);
begin
  if HasObj then
    SetValue(MUIA_Application_Active, AValue);
end;

function TMUIApplication.GetActive: Boolean;
begin
  Result := True;
  if HasObj then
    Result := GetBoolValue(MUIA_Application_Active);
end;

procedure TMUIApplication.SetAuthor(AValue: string);
begin
  if AValue <> FAuthor then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_Author', AValue)
    else
      FAuthor := AValue;
  end;
end;

procedure TMUIApplication.SetBase(AValue: string);
begin
  if AValue <> FBase then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_Base', AValue)
    else
      FBase := AValue;
  end;
end;

function TMUIApplication.GetBase: string;
begin
  Result := FBase;
  if HasObj then
    Result := GetStringValue(MUIA_Application_Base);
end;

function TMUIApplication.GetBroker: PCxObj;
begin
  Result := nil;
  if HasObj then
    Result := GetPointerValue(MUIA_Application_Broker);
end;

procedure TMUIApplication.SetBrokerPri(AValue: Integer);
begin
  if AValue <> FBrokerPri then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_BrokerPri', IntToStr(AValue))
    else
      FBrokerPri := AValue;
  end;
end;

procedure TMUIApplication.SetCopyright(AValue: string);
begin
  if AValue <> FCopyright then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_Copyright', AValue)
    else
      FCopyright := AValue;
  end;
end;

procedure TMUIApplication.SetDescription(AValue: string);
begin
  if AValue <> FDescription then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_Description', AValue)
    else
      FDescription := AValue;
  end;
end;

procedure TMUIApplication.SetDiskObject(AValue: Pointer);
begin
  if AValue <> FDiskObject then
  begin
    FDiskObject := AValue;
    if HasObj then
      SetValue(MUIA_Application_DiskObject, AValue);
  end;
end;

function TMUIApplication.GetForceQuit: Boolean;
begin
  Result := False;
  if HasObj then
    Result := GetBoolValue(MUIA_Application_ForceQuit);
end;

procedure TMUIApplication.SetHelpFile(AValue: string);
begin
  if AValue <> FHelpFile then
  begin
    FHelpFile := AValue;
    if HasObj then
      SetValue(MUIA_Application_HelpFile, AsTag(PChar(FHelpFile)));
  end;
end;

function TMUIApplication.GetIconified: Boolean;
begin
  Result := False;
  if HasObj then
    Result := GetBoolValue(MUIA_Application_Iconified);
end;

procedure TMUIApplication.SetIconified(AValue: Boolean);
begin
  if AValue <> Iconified then
  begin
    if HasObj then
      SetValue(MUIA_Application_Iconified, AsTag(AValue));
  end;
end;

procedure TMUIApplication.SetSingleTask(AValue: Boolean);
begin
  if AValue <> FSingleTask then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_SetSingleTask', BoolToStr(AValue, True))
    else
      FSingleTask := AValue;
  end;
end;

function TMUIApplication.GetSleep: Boolean;
begin
  Result := FSleepCount > 0;
end;

procedure TMUIApplication.SetSleep(AValue: Boolean);
begin
  if HasObj then
  begin
    SetValue(MUIA_Application_Sleep, AsTag(AValue));
    if AValue then
      Inc(FSleepCount)
    else
      Dec(FSleepCount);
  end;
end;

procedure TMUIApplication.SetUseCommodities(AValue: Boolean);
begin
  if AValue <> FUseCommodities then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_UseCommodities', BoolToStr(AValue, True))
    else
      FUseCommodities := AValue;
  end;
end;

procedure TMUIApplication.SetTitle(AValue: string);
begin
  if AValue <> FTitle then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_Title', Title)
    else
      FTitle := Copy(AValue, 1, 30);
  end;
end;

procedure TMUIApplication.SetVersion(AValue: string);
begin
  if AValue <> FVersion then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_Version', Version)
    else
      FVersion := AValue;
  end;
end;

procedure TMUIApplication.AboutMUI(Window: TMUINotify = nil);
var
  Win: PObject_;
begin
  if HasObj then
  begin
    Win := nil;
    if Assigned(Window) then
      Win := Window.MUIObj;
    DoMethod(FMUIObj, [MUIM_Application_AboutMUI, AsTag(Win)]);
  end;
end;

procedure TMUIApplication.AddInputHandler(IhNode: PMUI_InputHandlerNode);
begin
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Application_AddInputHandler, AsTag(IhNode)]);
end;

procedure TMUIApplication.RemInputHandler(IhNode: PMUI_InputHandlerNode);
begin
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Application_RemInputHandler, AsTag(IhNode)]);
end;

procedure TMUIApplication.CheckRefresh;
begin
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Application_CheckRefresh]);
end;

procedure TMUIApplication.InputBuffered;
begin
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Application_InputBuffered]);
end;

procedure TMUIApplication.Load(FromEnvarc: Boolean);
begin
  if HasObj then
  begin
    {$ifdef AROS}
    if FromEnvarc then
      DoMethod(FMUIObj, [MUIM_Application_Load, AsTag(MUIV_Application_Load_ENVARC)])
    else
      DoMethod(FMUIObj, [MUIM_Application_Load, AsTag(MUIV_Application_Load_ENV)])
    {$endif}
  end;
end;

procedure TMUIApplication.OpenConfigWindow;
begin
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Application_OpenConfigWindow, 0, 0]);
end;

procedure TMUIApplication.Save(ToEnvarc: Boolean);
begin
  if HasObj then
  begin
    {$ifdef AROS}
    if ToEnvarc then
      DoMethod(FMUIObj, [MUIM_Application_Save, AsTag(MUIV_Application_Load_ENVARC)])
    else
      DoMethod(FMUIObj, [MUIM_Application_Save, AsTag(MUIV_Application_Load_ENV)])
    {$endif}
  end;
end;


procedure TMUIApplication.ShowHelp(Window: TMUINotify; HelpFileName: string; Node: string; LineNum: Integer);
var
  Win: PObject_;
begin
  Win := nil;
  if Assigned(Win) then
    Win := Window.MUIObj;
  if HasObj then
    DoMethod(FMUIObj, [MUIM_Application_ShowHelp, AsTag(Win), AsTag(PChar(HelpFileName)), AsTag(PChar(Node)), LineNum]);
end;

procedure TMUIApplication.ShowHelp(Node: string; LineNum: Integer);
begin
  ShowHelp(nil, '', Node, LineNum);
end;

procedure TMUIApplication.SetMenuStrip(AValue: TMUINotify);
begin
  if AValue <> FMenuStrip then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Application_MenuStrip', HexStr(AValue))
    else
      FMenuStrip := AValue;
  end;
end;

{ TMUIWithParent }


destructor TMUIWithParent.Destroy;
begin
  if Assigned(FParent) then
    FParent.RemoveChild(Self);
  inherited;
end;

procedure TMUIWithParent.DestroyObject;
begin
  if not Assigned(FParent) then
    inherited;
  FMUIObj := nil;
end;


procedure TMUIWithParent.SetParent(AParent: TMUINotify);
begin
  if Assigned(FParent) then
    FParent.RemoveChild(Self);
  FParent := AParent;
  if Assigned(FParent) then
  begin
    if (FParent.HasObj) and not HasObj then
      CreateObject;
    FParent.AddChild(self);
  end;
  if (FParent = nil) and HasObj then
    DestroyObject;
end;

{ TMUIFamily }

procedure TMUIFamily.GetCreateTags(var ATagList: TATagList);
var
  i: Integer;
begin
  inherited;
  for i := 0 to Childs.Count - 1 do
  begin
    Childs[i].CreateObject;
    if Childs[i].HasObj then
      ATagList.AddTag(MUIA_Family_Child, AsTag(Childs[i].MUIObj));
  end;
end;

{ TMUIFamily }

procedure TMUISemaphore.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Semaphore, TagList.GetTagPointer);
    AfterCreateObject;
  end;
end;

function TMUISemaphore.Attempt: Boolean;
begin
  if HasObj then
    Result := Boolean(DoMethod(MUIObj, [MUIM_Semaphore_Attempt]));
end;

function TMUISemaphore.AtteptShared: Boolean;
begin
  if HasObj then
    Result := Boolean(DoMethod(MUIObj, [MUIM_Semaphore_AttemptShared]));
end;

procedure TMUISemaphore.Obtain;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Semaphore_Obtain]);
end;

procedure TMUISemaphore.ObtainShared;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Semaphore_ObtainShared]);
end;

procedure TMUISemaphore.Release;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Semaphore_Release]);
end;

{ TMUIDataspace }

constructor TMUIDataspace.Create;
begin
  inherited;
  FPool := nil;
end;

procedure TMUIDataspace.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if Assigned(FPool) then
    ATagList.AddTag(MUIA_Dataspace_Pool, AsTag(FPool));
end;

procedure TMUIDataspace.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Dataspace, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIDataspace.SetPool(AValue: Pointer);
begin
  if AValue <> FPool then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Dataspace_Pool', HexStr(AValue))
    else
      FPool := AValue;
  end;
end;

function TMUIDataspace.Add(Data: Pointer; Len: Integer; Id: LongWord): Boolean;
begin
  Result := False;
  if HasObj then
    Result := Boolean(DoMethod(MUIObj, [MUIM_Dataspace_Add, AsTag(Data), AsTag(Len), AsTag(Id)]));
end;

procedure TMUIDataspace.Clear;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Dataspace_Clear]);
end;

function TMUIDataspace.Find(Id: LongWord): Pointer;
begin
  Result := nil;
  if HasObj then
    Result := Pointer(DoMethod(MUIObj, [MUIM_Dataspace_Find, AsTag(Id)]));
end;

function TMUIDataspace.Merge(DS: TMUIDataSpace): Integer;
begin
  Result := 0;
  if HasObj and DS.HasObj then
    Result := DoMethod(MUIObj, [MUIM_Dataspace_Add, AsTag(DS.MUIObj)]);
end;

function TMUIDataspace.ReadIFF(IFFHandle: Pointer): Integer;
begin
  Result := -1;
  if HasObj then
    Result := DoMethod(MUIObj, [MUIM_Dataspace_ReadIFF, AsTag(IFFHandle)]);
end;

function TMUIDataspace.WriteIFF(IFFHandle: Pointer; typ: LongWord; ID: LongWord): Integer;
begin
  Result := -1;
  if HasObj then
    Result := DoMethod(MUIObj, [MUIM_Dataspace_WriteIFF, AsTag(IFFHandle), AsTag(Typ), AsTag(ID)]);
end;

function TMUIDataspace.Remove(Id: LongWord): Boolean;
begin
  Result := False;
  if HasObj then
    Result := Boolean(DoMethod(MUIObj, [MUIM_Dataspace_Remove, AsTag(Id)]));
end;

constructor TMUITimer.Create;
begin
  FEnabled := False;
  FInterval := 1000;
  FLastStart := 0;
  MUIApp.FtimerList.Add(Self);
end;

destructor TMUITimer.Destroy;
begin
  Enabled := False;
  MUIApp.FTimerList.Remove(Self);
  inherited;
end;

procedure TMUITimer.SetEnabled(AValue: boolean);
var
  i: Integer;
begin
  FEnabled := AValue;
  if FEnabled then
    FLastStart := GetTickCount64;
  MUIApp.FActiveTimer := False;
  for i := 0 to MUIApp.FTimerList.Count - 1 do
  begin
    if MUIApp.FTimerList[i].Enabled then
    begin
      MUIApp.FActiveTimer := True;
      Break;
    end;
  end;
end;

initialization
  MUIApp := TMUIApplication.Create;
finalization
  MUIApp.Free;
end.
