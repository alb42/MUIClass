unit MUIClass.Window;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Exec,
  Utility, Intuition, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base;
{$M+}
type
  TCloseAction = (caNone, caClose, caFree); // return values for OnCloseRequest, default is caClose

  TCloseReqEvent = procedure(Sender: TObject; var CloseAction: TCloseAction) of object;

  TMUIWindow = class(TMUIWithParent)
  private
    property Parent;
  private
    FGroupObj: PObject_;
    FHoriz: Boolean;
    FOpen: Boolean;
    FOnActivate: TNotifyEvent;
    FOnDeactivate: TNotifyEvent;
    FAltLeftEdge: Integer;
    FAltTopEdge: Integer;
    FAltHeight: Integer;
    FAltWidth: Integer;
    FAppWindow: Boolean;
    FBackdrop: Boolean;
    FBorderless: Boolean;
    FCloseGadget: Boolean;
    FOnCloseRequest: TCloseReqEvent;
    FDefaultObject: TMUINotify;
    FDepthGadget: Boolean;
    FDragBar: Boolean;
    FLeftEdge: Integer;
    FTopEdge: Integer;
    FHeight: Integer;
    FWidth: Integer;
    FID: LongWord;
    FMenuStrip: TMUINotify;
    FNoMenus: Boolean;
    FPublicScreen: string;
    FScreen: PScreen;
    FScreenTitle: string;
    FSizeGadget: Boolean;
    FTitle: string;
    FUBBS, FULBS, FURBS: Boolean;
    FSizeRight: Boolean;
    FOnShow: TNotifyEvent;
    FRefWindow: TMUIWindow;
    function GetActivate: Boolean;
    procedure SetActivate(AValue: Boolean);
    function GetActiveObject: TMUINotify;
    procedure SetActiveObject(AValue: TMUINotify);
    procedure SetAltLeftEdge(AValue: Integer);
    procedure SetAltTopEdge(AValue: Integer);
    procedure SetAltHeight(AValue: Integer);
    procedure SetAltWidth(AValue: Integer);
    procedure SetAppWindow(AValue: Boolean);
    procedure SetBackdrop(AValue: Boolean);
    procedure SetBorderless(AValue: Boolean);
    procedure SetCloseGadget(AValue: Boolean);
    function GetDefaultObject: TMUINotify;
    procedure SetDefaultObject(AValue: TMUINotify);
    procedure SetDepthGadget(AValue: Boolean);
    procedure SetDragBar(AValue: Boolean);
    function GetLeftEdge: Integer;
    function GetTopEdge: Integer;
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetLeftEdge(AValue: Integer);
    procedure SetTopEdge(AValue: Integer);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetID(AValue: LongWord);
    procedure SetMenuStrip(AValue: TMUINotify);
    procedure SetNoMenus(AValue: Boolean);
    procedure SetOpen(AOpen: Boolean);
    procedure SetPublicScreen(AValue: string);
    function GetScreen: PScreen;
    procedure SetScreen(AValue: PScreen);
    procedure SetScreenTitle(AValue: string);
    procedure SetSizeGadget(AValue: Boolean);
    procedure SetSizeRight(AValue: Boolean);
    function GetSleep: Boolean;
    procedure SetSleep(AValue: Boolean);
    procedure SetTitle(AValue: string);
    procedure SetUBBS(AValue: Boolean);
    procedure SetULBS(AValue: Boolean);
    procedure SetURBS(AValue: Boolean);
    function GetWindow: PWindow;
    procedure SetRefWindow(AValue: TMUIWindow);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    property Childs;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateObject; override;
    procedure AfterCreateObject; override;
    procedure DestroyObject; override;

    procedure AddChild(AChild: TMUINotify); override;
    procedure RemoveChild(AChild: TMUINotify); override;

    procedure Show; //  open the Window
    procedure Close; // Close the Window
    property GroupObj: PObject_ read FGroupObj;
  public
    // MUI Methods
    procedure ScreenToBack;   // put the screen (with this Window) to back
    procedure ScreenToFront;  // put the screen (with this Window) to Front
    procedure Snapshot;       // Snapshot the window
    procedure Unsnapshot;     // Unsnapshot the window
    procedure ToBack;   // put the Window to back
    procedure ToFront;  // put the Window to Front
    procedure InitChange; override;
    procedure ExitChange; override;
    // MUI Fields
    property Activate: Boolean read GetActivate write SetActivate;                   //  Activate the Window (True activate the Window, False does nothing)
    property ActiveObject: TMUINotify read GetActiveObject write SetActiveObject;    //  Currently active Object (can be nil!), can be also set
    property DefaultObject: TMUINotify read GetDefaultObject write SetDefaultObject; //  Default object gets the keyboard input if not other takes it
    property Open: Boolean read FOpen write SetOpen default False;                   //  Open the Window (default False)
    property Screen: PScreen read GetScreen write SetScreen;                         //  Set the Screen to open the Window or get the Screen structure the currently window is open
    property Window: PWindow read GetWindow;
    property Sleep: Boolean read GetSleep write SetSleep;                            //  Put Window to sleep mode, with Busy Pointer
  published
    property AltLeftEdge: Integer read FAltLeftEdge write SetAltLeftEdge default -1;   //I alternate (when click the zoom button) Left Edge (default = -1, let system decide)
    property AltTopEdge: Integer read FAltTopEdge write SetAltTopEdge default -1;      //I alternate (when click the zoom button) Top Edge (default = -1, let system decide)
    property AltHeight: Integer read FAltHeight write SetAltHeight default -1;         //I alternate (when click the zoom button) Height (default = -1, minimum)
    property AltWidth: Integer read FAltWidth write SetAltWidth default -1;            //I alternate (when click the zoom button) Width (default = -1, minimum)
    property AppWindow: Boolean read FAppWindow write SetAppWindow default False;      //I Create the Window as AppWindow default False
    property Backdrop: Boolean read FBackdrop write SetBackdrop default False;         //I Create the Window as backdrop window (default False)
    property Borderless: Boolean read FBorderless write SetBorderless default False;   //I Create the Window without border (default False)
    property CloseGadget: Boolean read FCloseGadget write SetCloseGadget default True; //I  Create the Window with CloseGadget (default True)
    property DepthGadget: Boolean read FDepthGadget write SetDepthGadget default True; //I Create the Window with DepthGadget (default True)
    property SizeGadget: Boolean read FSizeGadget write SetSizeGadget default True;    //I Create the Window with SizeGadget (default True)
    property SizeRight: Boolean read FSizeRight write SetSizeRight default False;      //I SizeGadget is in the right window border (default False)
    property DragBar: Boolean read FDragBar write SetDragBar default True;             //I Create the Window with DragBar (default True)
    property LeftEdge: Integer read GetLeftEdge write SetLeftEdge default MUIV_Window_LeftEdge_Centered; //I Set inital LeftEdge of Window, read current LeftEdge (Default MUIV_Window_LeftEdge_Centered);
    property TopEdge: Integer read GetTopEdge write SetTopEdge default MUIV_Window_TopEdge_Centered;     //I Set inital TopEdge of Window, read current TopEdge (Default MUIV_Window_TopEdge_Centered);
    property Height: Integer read GetHeight write SetHeight default MUIV_Window_Height_Default;          //I Set inital Height of Window, read current Height (Default MUIV_Window_Height_Default;
    property Width: Integer read GetWidth write SetWidth default MUIV_Window_Width_Default;              //I Set inital Width of Window, read current Width (Default MUIV_Window_Width_Default);
    property ID: LongWord read FID write SetID default 0;                              //  unique ID in the App to identify the Window for snapshots and so on use number or MAKEID()
    property Horizontal: Boolean read FHoriz write FHoriz;    // Alignment of the included Group
    // inputevent
    property Menustrip: TMUINotify read FMenuStrip write SetMenuStrip; // TMUIMenuStrip
    // NeedMouseObject/MouseObject -> not in AROS
    property NoMenus: Boolean read FNoMenus write SetNoMenus default False;        //  Disable the global or window menu
    property PublicScreen: string read FPublicScreen write SetPublicScreen;        //I  Force to a Public Screen
    property RefWindow: TMUIWindow read FRefWindow write SetRefWindow;             //  Set the Window relative to an other Window
    // RootObject handled internally
    property ScreenTitle: string read FScreenTitle write SetScreenTitle;              //  Title in the Screen bar shown, when the Window is active
    property Title: string read FTitle write SetTitle;                                //  Set Window Title
    property UseBottomBorderScroller: Boolean read FUBBS write SetUBBS default False; //  Use Bottom Border scroller if any Object needs it
    property UseLeftBorderScroller: Boolean read FULBS write SetULBS default False;   //  Use Left Border scroller if any Object needs it
    property UseRightBorderScroller: Boolean read FURBS write SetURBS default False;  //  Use Right Border scroller if any Object needs it
    // EVents
    property OnShow: TNotifyEvent read FOnShow write FOnShow;                           // Event when the Window is opened
    property OnActivate: TNotifyEvent read FOnActivate write FOnActivate;               // Event when the Window gets activated
    property OnDeactivate: TNotifyEvent read FOnDeactivate write FOnDeactivate;         // Event when the Window gets activated
    property OnCloseRequest: TCloseReqEvent read FOnCloseRequest write FOnCloseRequest; // Ask the user what to do on Close button click
  end;

  { TMUIAboutMUI }

  TMUIAboutMUI = class(TMUIWindow)
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    procedure CreateObject; override;
  end;


implementation

{ TMUIWindow }

constructor TMUIWindow.Create;
begin
  inherited;
  if  MUIApp.HasObj then
    Parent := nil
  else
    Parent := MUIApp;
  //
  FGroupObj := nil;
  FHoriz := False;
  //
  FOpen := False;
  FAltLeftEdge := -1;
  FAltTopEdge := -1;
  FAltHeight := -1;
  FAltWidth := -1;
  FAppWindow := False;
  FBackdrop := False;
  FBorderless := False;
  FCloseGadget := True;
  FDefaultObject := nil;
  FCloseGadget := True;
  FDepthGadget := True;
  FDragBar := True;
  FLeftEdge := MUIV_Window_LeftEdge_Centered;
  FTopEdge := MUIV_Window_TopEdge_Centered;
  FHeight := MUIV_Window_Height_Default;
  FWidth :=  MUIV_Window_Width_Default;
  FID := 0;
  FMenustrip := nil;
  FNoMenus := False;
  FPublicScreen := '';
  Screen := nil;
  FScreenTitle := '';
  FSizeGadget := True;
  FTitle := '';
  FUBBS := False;
  FULBS := False;
  FURBS := False;
  FSizeRight := False;
  FRefWindow := nil;
end;

destructor TMUIWindow.Destroy;
begin
  FMenuStrip.Free;
  inherited;
end;


procedure TMUIWindow.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FOpen then
    ATagList.AddTag(MUIA_Window_Open, AsTag(True));
  if FAltLeftEdge <> -1 then
    ATagList.AddTag(MUIA_Window_AltLeftEdge, AsTag(FAltLeftEdge));
  if FAltTopEdge <> -1 then
    ATagList.AddTag(MUIA_Window_AltTopEdge, AsTag(FAltTopEdge));
  if FAltHeight <> -1 then
    ATagList.AddTag(MUIA_Window_AltHeight, AsTag(FAltHeight));
  if FAltWidth <> -1 then
    ATagList.AddTag(MUIA_Window_AltWidth, AsTag(FAltWidth));
  if FAppWindow then
    ATagList.AddTag(MUIA_Window_AppWindow, AsTag(FAppWindow));
  if FBackdrop then
    ATagList.AddTag(MUIA_Window_Backdrop, AsTag(FBackdrop));
  if FBorderless then
    ATagList.AddTag(MUIA_Window_Borderless, AsTag(FBorderless));
  if not FCloseGadget then
    ATagList.AddTag(MUIA_Window_CloseGadget, AsTag(FCloseGadget));
  if Assigned(FDefaultObject) then
    ATagList.AddTag(MUIA_Window_DefaultObject, AsTag(FDefaultObject));
  if not FDepthGadget then
    ATagList.AddTag(MUIA_Window_DepthGadget, AsTag(FDepthGadget));
  if not FDragBar then
    ATagList.AddTag(MUIA_Window_DragBar, AsTag(FDragBar));
  if FLeftEdge <> MUIV_Window_LeftEdge_Centered then
    ATagList.AddTag(MUIA_Window_LeftEdge, AsTag(FLeftEdge));
  if FTopEdge <> MUIV_Window_TopEdge_Centered then
    ATagList.AddTag(MUIA_Window_TopEdge, AsTag(FTopEdge));
  if FHeight <> MUIV_Window_Height_Default then
    ATagList.AddTag(MUIA_Window_Height, AsTag(FHeight));
  if FWidth <> MUIV_Window_Width_Default then
    ATagList.AddTag(MUIA_Window_Width, AsTag(FWidth));
  if FID > 0 then
    ATagList.AddTag(MUIA_Window_ID, AsTag(FID));
  if Assigned(FMenuStrip) then
  begin
    FMenuStrip.CreateObject;
    ATagList.AddTag(MUIA_Window_MenuStrip, AsTag(FMenuStrip.MuiObj));
  end;
  if FNoMenus then
    ATagList.AddTag(MUIA_Window_NoMenus, AsTag(FNoMenus));
  if FPublicScreen <> '' then
    ATagList.AddTag(MUIA_Window_PublicScreen, AsTag(PChar(FPublicScreen)));
  if Assigned(FScreen) then
    ATagList.AddTag(MUIA_Window_Screen, AsTag(FScreen));
  if FScreenTitle <> '' then
    ATagList.AddTag(MUIA_Window_ScreenTitle, AsTag(PChar(FScreenTitle)));
  if not FSizeGadget then
    ATagList.AddTag(MUIA_Window_SizeGadget, AsTag(FSizeGadget));
  if not FSizeRight then
    ATagList.AddTag(MUIA_Window_SizeRight, AsTag(FSizeRight));
  if FTitle <> '' then
    ATagList.AddTag(MUIA_Window_Title, AsTag(PChar(FTitle)));
  if FUBBS then
    ATagList.AddTag(MUIA_Window_UseBottomBorderScroller, AsTag(FUBBS));
  if FULBS then
    ATagList.AddTag(MUIA_Window_UseLeftBorderScroller, AsTag(FULBS));
  if FURBS then
    ATagList.AddTag(MUIA_Window_UseRightBorderScroller, AsTag(FURBS));
  if Assigned(FRefWindow) then
  begin
    FRefWindow.CreateObject;
    ATagList.AddTag(MUIA_Window_RefWindow, AsTag(FRefWindow.MUIObj));
  end;
end;

procedure TMUIWindow.CreateObject;
var
  TagList: TATagList;
  i: Integer;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    //
    // Create the GroupObject
    TagList.AddTag(MUIA_Group_Horiz, AsTag(FHoriz));
    for i := 0 to Childs.Count - 1 do
    begin
      if Childs[i].HasObj then
        TagList.AddTag(MUIA_Group_Child, AsTag(Childs[i].MUIObj))
    end;
    FGroupObj := MUI_NewObjectA(MUIC_Group, TagList.GetTagPointer);
    //
    // Create The Actual Window
    TagList.Clear;
    GetCreateTags(TagList);
    TagList.AddTag(MUIA_Window_RootObject, AsTag(FGroupObj));

    FMUIObj := MUI_NewObjectA(MUIC_Window, TagList.GetTagPointer);
    AfterCreateObject;
  end;
end;

procedure TMUIWindow.DestroyObject;
begin
  if Assigned(FMenuStrip) then
    FMenuStrip.ClearObject;
  inherited;
end;

function ActivateFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIWindow;
begin
  Unused(Obj);
  Unused(Msg);
  try
    Result := 0;
    PasObj := TMUIWindow(Hook^.h_Data);
    if Assigned(PasObj.FOnActivate) then
      PasObj.FOnActivate(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function DeactivateFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIWindow;
begin
  Unused(Obj);
  Unused(Msg);
  try
    Result := 0;
    PasObj := TMUIWindow(Hook^.h_Data);
    if Assigned(PasObj.FOnDeactivate) then
      PasObj.FOnDeactivate(PasObj);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function CloseReqFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIWindow;
  CloseAction: TCloseAction;
begin
  Unused(Obj);
  Unused(Msg);
  CloseAction := caClose;
  Result := 0;
  PasObj := TMUIWindow(Hook^.h_Data);
  try
    // ask the user if we should close it ;)
    if Assigned(PasObj.FOnCloseRequest) then
      PasObj.FOnCloseRequest(PasObj, CloseAction);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
  // User has no objection, close the Window
  case CloseAction of
    caNone: begin
    end;
    caClose: PasObj.Close;
    caFree:
    begin
      PasObj.Close;
      MUIApp.AddToDestroy(PasObj); // Async destroy
    end;
  end;
end;

procedure TMUIWindow.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Window_Activate, MUI_TRUE, @ActivateFunc);
  ConnectHook(MUIA_Window_Activate, MUI_FALSE, @DeactivateFunc);
  ConnectHook(MUIA_Window_CloseRequest, MUI_TRUE, @CloseReqFunc);
end;

procedure TMUIWindow.SetOpen(AOpen: Boolean);
begin
  if FOpen <> AOpen then
  begin
    FOpen := AOpen;
    // we try to open but not created until now -> try to create it now
    if AOpen and not HasObj then
    begin
      Parent := nil; // no parent!
      CreateObject; // Create
      Parent := MUIApp; // Parent to Application -> must be included
    end;
    // and try to Open it
    if HasObj then
    begin
      if not AOpen then
        BeforeCloseWindow;
      SetValue(MUIA_Window_Open, AsTag(FOpen));
      FOpen := GetBoolValue(MUIA_Window_Open);
      // call the Event when Window is opened
      if FOpen then
      begin
        DoFirstOpen;
        if Assigned(FOnShow) then
          FOnShow(Self);
      end;
      // we are the Main Window and got closed -> close application
      if not AOpen and (Self = MUIApp.MainWindow) then
        MUIApp.Terminate;
    end;
  end;
end;


procedure TMUIWindow.Show;
begin
  Open := True;
end;

procedure TMUIWindow.Close;
begin
  Open := False;
end;

procedure TMUIWindow.AddChild(AChild: TMUINotify);
begin
  if Assigned(AChild) and (Childs.IndexOf(AChild) < 0) then
  begin
    Childs.Add(AChild);
    if HasObj then
      DoMethod(FGroupObj, [NativeUInt(OM_ADDMEMBER), AsTag(AChild.MUIObj)]);
  end;
end;

procedure TMUIWindow.RemoveChild(AChild: TMUINotify);
begin
  if Assigned(AChild) and (Childs.IndexOf(AChild) >= 0) then
  begin
    if HasObj then
      DoMethod(FGroupObj, [NativeUInt(OM_REMMEMBER), AsTag(AChild.MUIObj)]);
    Childs.Remove(AChild);
  end;
end;

function TMUIWindow.GetActivate: Boolean;
begin
  Result := False;
  if HasObj then
    Result := GetBoolValue(MUIA_Window_Activate);
end;

procedure TMUIWindow.SetActivate(AValue: Boolean);
begin
  if AValue and HasObj then
    SetValue(MUIA_Window_Activate, MUI_TRUE);
end;

function TMUIWindow.GetActiveObject: TMUINotify;
var
  ActObj: PObject_;
begin
  Result := nil;
  if HasObj then
  begin
    ActObj := GetPointerValue(MUIA_Window_ActiveObject);
    if Assigned(ActObj) then
      Result := GetPasObject(ActObj);
  end;
end;

procedure TMUIWindow.SetActiveObject(AValue: TMUINotify);
begin
  if HasObj and Assigned(AValue) and AValue.HasObj then
    SetValue(MUIA_Window_ActiveObject, AsTag(AVAlue.MUIObj));
end;

procedure TMUIWindow.SetAltLeftEdge(AValue: Integer);
begin
  if AValue <> FAltLeftEdge then
  begin
    FAltLeftEdge := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Window_AltLeftEdge, AsTag(FAltLeftEdge));
      FAltLeftEdge := GetIntValue(MUIA_Window_AltLeftEdge);
    end;
  end;
end;

procedure TMUIWindow.SetAltTopEdge(AValue: Integer);
begin
  if AValue <> FAltTopEdge then
  begin
    FAltTopEdge := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Window_AltTopEdge, AsTag(FAltTopEdge));
      FAltTopEdge := GetIntValue(MUIA_Window_AltTopEdge);
    end;
  end;
end;

procedure TMUIWindow.SetAltHeight(AValue: Integer);
begin
  if AValue <> FAltHeight then
  begin
    FAltHeight := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Window_AltHeight, AsTag(FAltHeight));
      FAltHeight := GetIntValue(MUIA_Window_AltHeight);
    end;
  end;
end;

procedure TMUIWindow.SetAltWidth(AValue: Integer);
begin
  if AValue <> FAltWidth then
  begin
    FAltWidth := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Window_AltWidth, AsTag(FAltWidth));
      FAltWidth := GetIntValue(MUIA_Window_AltWidth);
    end;
  end;
end;

procedure TMUIWindow.SetAppWindow(AValue: Boolean);
begin
  if AValue <> FAppWindow then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_AppWindow', BoolToStr(AValue, True))
    else
      FAppWindow := AValue;
  end;
end;

procedure TMUIWindow.SetBackdrop(AValue: Boolean);
begin
  if AValue <> FBackdrop then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_Backdrop', BoolToStr(AValue, True))
    else
      FBackdrop := AValue;
  end;
end;

procedure TMUIWindow.SetBorderless(AValue: Boolean);
begin
  if AValue <> FBorderless then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_Borderless', BoolToStr(AValue, True))
    else
      FBorderless := AValue;
  end;
end;

procedure TMUIWindow.SetCloseGadget(AValue: Boolean);
begin
  if AValue <> FCloseGadget then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_CloseGadget', BoolToStr(AValue, True))
    else
      FCloseGadget := AValue;
  end;
end;

function TMUIWindow.GetDefaultObject: TMUINotify;
var
  ActObj: PObject_;
begin
  Result := FDefaultObject;
  if HasObj then
  begin
    ActObj := GetPointerValue(MUIA_Window_DefaultObject);
    if Assigned(ActObj) then
      Result := GetPasObject(ActObj);
  end;
end;

procedure TMUIWindow.SetDefaultObject(AValue: TMUINotify);
begin
  FDefaultObject := AValue;
  if HasObj and Assigned(AValue) and AValue.HasObj then
    SetValue(MUIA_Window_DefaultObject, AsTag(AVAlue.MUIObj));
end;

procedure TMUIWindow.SetDepthGadget(AValue: Boolean);
begin
  if AValue <> FDepthGadget then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_DepthGadget', BoolToStr(AValue, True))
    else
      FDepthGadget := AValue;
  end;
end;

procedure TMUIWindow.SetDragBar(AValue: Boolean);
begin
  if AValue <> FDragBar then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_DragBar', BoolToStr(AValue, True))
    else
      FDragBar := AValue;
  end;
end;

function TMUIWindow.GetLeftEdge: Integer;
begin
  Result := FLeftEdge;
  if HasObj then
    Result := GetIntValue(MUIA_Window_LeftEdge);
end;

function TMUIWindow.GetTopEdge: Integer;
begin
  Result := FTopEdge;
  if HasObj then
    Result := GetIntValue(MUIA_Window_TopEdge);
end;

function TMUIWindow.GetHeight: Integer;
begin
  Result := FHeight;
  if HasObj then
    Result := GetIntValue(MUIA_Window_Height);
end;

function TMUIWindow.GetWidth: Integer;
begin
  Result := FWidth;
  if HasObj then
    Result := GetIntValue(MUIA_Window_Width);
end;

procedure TMUIWindow.SetLeftEdge(AValue: Integer);
begin
  if AValue <> FLeftEdge then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_LeftEdge', IntToStr(AValue))
    else
      FLeftEdge := AValue;
  end;
end;

procedure TMUIWindow.SetTopEdge(AValue: Integer);
begin
  if AValue <> FTopEdge then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_TopEdge', IntToStr(AValue))
    else
      FTopEdge := AValue;
  end;
end;


procedure TMUIWindow.SetHeight(AValue: Integer);
begin
  if AValue <> FHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_Height', IntToStr(AValue))
    else
      FHeight := AValue;
  end;
end;

procedure TMUIWindow.SetWidth(AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_Width', IntToStr(AValue))
    else
      FWidth := AValue;
  end;
end;

procedure TMUIWindow.SetID(AValue: LongWord);
begin
  if AValue <> FID then
  begin
    FID := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Window_ID, AsTag(FID));
      FID := GetIntValue(MUIA_Window_ID);
    end;
  end;
end;

procedure TMUIWindow.SetMenuStrip(AValue: TMUINotify);
begin
  if AValue <> FMenuStrip then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_MenuStrip', HexStr(AValue))
    else
      FMenuStrip := AValue;
  end;
end;

procedure TMUIWindow.SetNoMenus(AValue: Boolean);
begin
  if AValue <> FNoMenus then
  begin
    FNoMenus := AValue;
    if HasObj then
      SetValue(MUIA_Window_NoMenus, AsTag(FNoMenus));
  end;
end;


procedure TMUIWindow.SetPublicScreen(AValue: string);
begin
  if AValue <> FPublicScreen then
  begin
    FPublicScreen := AValue;
    if HasObj then
      SetValue(MUIA_Window_PublicScreen, AsTag(PChar(FPublicScreen)));
  end;
end;

function TMUIWindow.GetScreen: PScreen;
begin
  Result := FScreen;
  if HasObj then
    Result := GetPointerValue(MUIA_Window_Screen);
end;

procedure TMUIWindow.SetScreen(AValue: PScreen);
begin
  if AValue <> FScreen then
  begin
    FScreen := AValue;
    if HasObj then
      SetValue(MUIA_Window_Screen, AsTag(FScreen));
  end;
end;

procedure TMUIWindow.SetScreenTitle(AValue: string);
begin
  if AValue <> FScreenTitle then
  begin
    FScreenTitle := AValue;
    if HasObj then
      SetValue(MUIA_Window_ScreenTitle, AsTag(PChar(FScreenTitle)));
  end;
end;

procedure TMUIWindow.SetSizeGadget(AValue: Boolean);
begin
  if AValue <> FSizeGadget then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_SizeGadget', BoolToStr(AValue, True))
    else
      FSizeGadget := AValue;
  end;
end;

procedure TMUIWindow.SetSizeRight(AValue: Boolean);
begin
  if AValue <> FSizeRight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Window_SizeRight', BoolToStr(AValue, True))
    else
      FSizeRight := AValue;
  end;
end;

function TMUIWindow.GetSleep: Boolean;
begin
  Result := False;
  if HasObj then
    Result := GetBoolValue(MUIA_Window_Sleep);
end;

procedure TMUIWindow.SetSleep(AValue: Boolean);
begin
  if HasObj then
    SetValue(MUIA_Window_Sleep, AValue);
end;

procedure TMUIWindow.SetTitle(AValue: string);
begin
  if AValue <> FTitle then
  begin
    FTitle := AValue;
    if HasObj then
      SetValue(MUIA_Window_Title, AsTag(PChar(FTitle)));
  end;
end;

procedure TMUIWindow.SetUBBS(AValue: Boolean);
begin
  if AValue <> FUBBS then
  begin
    FUBBS := AValue;
    if HasObj then
      SetValue(MUIA_Window_UseBottomBorderScroller, AsTag(FUBBS));
  end;
end;

procedure TMUIWindow.SetULBS(AValue: Boolean);
begin
  if AValue <> FULBS then
  begin
    FULBS := AValue;
    if HasObj then
      SetValue(MUIA_Window_UseLeftBorderScroller, AsTag(FULBS));
  end;
end;

procedure TMUIWindow.SetURBS(AValue: Boolean);
begin
  if AValue <> FURBS then
  begin
    FURBS := AValue;
    if HasObj then
      SetValue(MUIA_Window_UseRightBorderScroller, AsTag(FURBS));
  end;
end;

procedure TMUIWindow.SetRefWindow(AValue: TMUIWindow);
begin
  FRefWindow := AValue;
  if HasObj and Assigned(AValue) and AValue.HasObj then
    SetValue(MUIA_Window_RefWindow, AsTag(FRefWindow.MUIObj));
end;

function TMUIWindow.GetWindow: PWindow;
begin
  Result := nil;
  if HasObj then
    Result := GetPointerValue(MUIA_Window_Window);
end;

procedure TMUIWindow.ScreenToBack;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Window_ScreenToBack]);
end;

procedure TMUIWindow.ScreenToFront;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Window_ScreenToFront]);
end;

procedure TMUIWindow.Snapshot;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Window_Snapshot, 1]);
end;

procedure TMUIWindow.Unsnapshot;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Window_Snapshot, 0]);
end;

procedure TMUIWindow.ToBack;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Window_ToBack]);
end;

procedure TMUIWindow.ToFront;
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Window_ToFront]);
end;

procedure TMUIWindow.InitChange;
begin
  inherited;
  if HasObj then
    DoMethod(FGroupObj, [MUIM_Group_InitChange]);
end;

procedure TMUIWindow.ExitChange;
begin
  inherited;
  if HasObj then
    DoMethod(FGroupObj, [MUIM_Group_ExitChange]);
end;

{ TMUIAboutMUI }

procedure TMUIAboutMUI.GetCreateTags(var ATagList: TATagList);
begin
  //inherited; // do not add the standard window stuff
  //ATagList.AddTag(MUIA_Aboutmui_Application, AsTag(MUIApp.MUIObj)); // do not finish properly! removed for now!
  if Assigned(FRefWindow) then
  begin
    FRefWindow.CreateObject;
    ATagList.AddTag(MUIA_Window_RefWindow, AsTag(FRefWindow.MUIObj));
  end;
end;

procedure TMUIAboutMUI.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    //
    TagList.Clear;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_AboutMUI, TagList.GetTagPointer);
    AfterCreateObject;
  end;
end;


end.
