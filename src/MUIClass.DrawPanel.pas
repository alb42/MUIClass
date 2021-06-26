unit MUIClass.DrawPanel;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, Types,
  Utility, Intuition, icon, AGraphics, inputevent,
  keymap, layers,
  mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area;
{$M+}

type
  TMUIMouseBtn = (mmbLeft, mmbMiddle, mmbRight);
  TMUIShiftState = set of (mssShift, mssCtrl, mssLShift, mssRShift, mssLAlt, mssRAlt, mssLAmiga, mssRAmiga);

  TMUIDrawEvent = procedure(Sender: TObject; Rp: PRastPort; DrawRect: TRect) of object;
  TMUIMouseEvent = procedure(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean) of object;
  TMUIMouseWheel = procedure(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean) of object;
  TMUIMouseMove = procedure(Sender: TObject; X,Y: Integer; var EatEvent: Boolean) of object;
  TMUIKeyEvent = procedure(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean) of object;

  TMouseClickTime = record
    LSecs, LMicros: LongWord;
    MSecs, MMicros: LongWord;
    RSecs, RMicros: LongWord;
  end;

  TDrawBuffer = class
  private
    li: PLayer_Info;
    Bitmap: PBitmap;
    Layer: PLayer;
    FRP: PRastPort;
    FWidth: Integer;
    FHeight: Integer;
    OwnsRP: Boolean;
  private
    procedure SetPenA(Pen: LongInt);
    function GetPenA: LongInt;
    procedure SetPenB(Pen: LongInt);
    function GetPenB: LongInt;

    procedure SetPenPos(APenPos: TPoint);
    function GetPenPos: TPoint;
  public
    constructor Create(AWidth, AHeight, ADepth: Integer; AFriend: PBitmap = nil); virtual; overload;
    constructor Create(ARP: PRastPort); virtual; overload;
    destructor Destroy; override;

    procedure Clear(Pen: Integer = 0);
    procedure Line(const x1, y1, x2, y2: Integer);
    procedure DrawRect(const x1, y1, x2, y2: Integer); overload;
    procedure DrawRect(const ARect: Types.TRect); overload;
    procedure Draw3DBox(const ARect: Types.TRect; IsUp: Boolean = True);
    procedure FillRect(const x1, y1, x2, y2: Integer); overload;
    procedure FillRect(const ARect: Types.TRect); overload;

    procedure DrawText(x, y: Integer; AText: string); overload;
    procedure DrawText(AText: string); overload;

    procedure DrawImage(x, y: Integer; Src: TDrawBuffer); overload;
    procedure DrawImage(x, y: Integer; SrcRP: PRastPort); overload;

    procedure DrawToRastPort(x, y: Integer; DestRP: PRastPort); overload;
    procedure DrawToRastPort(x, y, SrcWidth, SrcHeight: Integer; DestRP: PRastPort); overload;


    property RP: PRastPort read FRP;
    property Width: Integer read FWidth;
    property Height: Integer read FHeight;


    property APen: Integer read GetPenA write SetPenA;
    property BPen: Integer read GetPenB write SetPenB;
    property PenPos: TPoint read GetPenPos write SetPenPos;
  end;

  TMUIDrawPanel = class(TMUIArea)
  private
    // Min Max
    FMinWidth: Integer;
    FMinHeight: Integer;
    FMaxWidth: Integer;
    FMaxHeight: Integer;
    FDefWidth: Integer;
    FDefHeight: Integer;
    FMouseClickTime: TMouseClickTime;
    EHNode: TMUI_EventHandlerNode;
    FMouseInObject: Boolean;
    // Events
    FOnDrawObject: TMUIDrawEvent;
    FOnMUIMouseDown: TMUIMouseEvent;
    FOnMUIMouseUp: TMUIMouseEvent;
    FOnMUIDblClick: TMUIMouseEvent;
    FOnMUIMouseWheel: TMUIMouseWheel;
    FOnMUIMouseMove: TMUIMouseMove;
    FOnMUIKeyDown: TMUIKeyEvent;
    FOnMUIKeyUp: TMUIKeyEvent;
    FOnMUIMouseLeave: TNotifyEvent;
  protected
    function MUIEvent(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt; virtual;
    //
    function DoSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt; virtual;
    function DoCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt; virtual;
    function DoAskMinMax(cl: PIClass; Obj: PObject_; Msg: PMUIP_AskMinMax): PtrUInt; virtual;
    function DoDraw(cl: PIClass; Obj: PObject_; Msg: PMUIP_Draw): PtrUInt; virtual;
    function DoHandleEvent(cl: PIClass; Obj: PObject_; Msg: PMUIP_HandleEvent): PtrUInt; virtual;

    procedure DoDrawObject(Rp: PRastPort; DrawRect: TRect); virtual;

    procedure ResetDblClickTime;
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;

    procedure CreateObject; override;

    procedure RedrawObject;
  published
    property FillArea;
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;
    property DefWidth: Integer read FDefWidth write FDefWidth;
    property DefHeight: Integer read FDefHeight write FDefHeight;

    // Events
    property OnDrawObject: TMUIDrawEvent read FOnDrawObject write FOnDrawObject;
    property OnMouseDown: TMUIMouseEvent read FOnMUIMouseDown write FOnMUIMouseDown;
    property OnMouseUp: TMUIMouseEvent read FOnMUIMouseUp write FOnMUIMouseUp;
    property OnDblClick: TMUIMouseEvent read FOnMUIDblClick write FOnMUIDblClick;
    property OnMouseWheel: TMUIMouseWheel read FOnMUIMouseWheel write FOnMUIMouseWheel;
    property OnMouseMove: TMUIMouseMove read FOnMUIMouseMove write FOnMUIMouseMove;
    property OnMouseLeave: TNotifyEvent read FOnMUIMouseLeave write FOnMUIMouseLeave;
    property OnKeyDown: TMUIKeyEvent read FOnMUIKeyDown write FOnMUIKeyDown;
    property OnKeyUp: TMUIKeyEvent read FOnMUIKeyUp write FOnMUIKeyUp;
  end;

var
  MUIPBType: PMUI_CustomClass = nil;

implementation

constructor TDrawBuffer.Create(AWidth, AHeight, ADepth: Integer; AFriend: PBitmap);
begin
  OwnsRP := True;
  FWidth := AWidth;
  FHeight := AHeight;
  li := NewLayerInfo(); // Layerinfo we also need
  if Assigned(AFriend) then
    Bitmap := AllocBitMap(AWidth, AHeight, ADepth, BMF_DISPLAYABLE or BMF_MINPLANES, AFriend)
  else
    Bitmap := AllocBitMap(AWidth, AHeight, ADepth, BMF_DISPLAYABLE or BMF_MINPLANES, nil);
  Layer := CreateUpFrontLayer(li, Bitmap, 0, 0, AWidth - 1, AHeight - 1, LAYERSIMPLE, nil);
  FRP := Layer^.RP;
  APen := 0;
  BPen := 0;
end;

constructor TDrawBuffer.Create(ARP: PRastPort);
begin
  OwnsRP := False;
  FRP := ARP;
  FWidth := ARP^.Bitmap^.BytesPerRow div 8 * ARP^.Bitmap^.Depth;
  FHeight := ARP^.Bitmap^.Rows;
end;

destructor TDrawBuffer.Destroy;
begin
  if OwnsRP then
  begin
    DeleteLayer(0, Layer);
    DisposeLayerInfo(li);
    FreeBitMap(Bitmap);
  end;
  inherited;
end;

procedure TDrawBuffer.SetPenA(Pen: LongInt);
begin
  SetAPen(FRP, Pen);
end;

function TDrawBuffer.GetPenA: LongInt;
begin
  Result := GetAPen(FRP);
end;

procedure TDrawBuffer.SetPenB(Pen: LongInt);
begin
  SetBPen(FRP, Pen);
end;

function TDrawBuffer.GetPenB: LongInt;
begin
  Result := GetBPen(FRP);
end;

procedure TDrawBuffer.Line(const x1, y1, x2, y2: Integer);
begin
  AGraphics.GfxMove(FRP, x1, y1);
  AGraphics.Draw(FRP, X2, y2);
end;

procedure TDrawBuffer.DrawRect(const x1, y1, x2, y2: Integer);
begin
  AGraphics.GfxMove(FRP, x1, y1);
  AGraphics.Draw(FRP, x2, y1);
  AGraphics.Draw(FRP, x2, y2);
  AGraphics.Draw(FRP, x1, y2);
  AGraphics.Draw(FRP, x1, y1);
end;

procedure TDrawBuffer.DrawRect(const ARect: Types.TRect);
begin
  DrawRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TDrawBuffer.Draw3DBox(const ARect: Types.TRect; IsUp: Boolean = True);
var
  SPen: LongInt;
begin
  SPen := APen;
  if IsUp then
    APen := 2
  else
    APen := 1;
  GFXMove(FRP, ARect.Left, ARect.Bottom);
  AGraphics.Draw(FRP, ARect.Left, ARect.Top);
  AGraphics.Draw(FRP, ARect.Right, ARect.Top);
  if IsUp then
    APen := 1
  else
    APen := 2;
  AGraphics.Draw(FRP, ARect.Right, ARect.Bottom);
  AGraphics.Draw(FRP, ARect.Left, ARect.Bottom);
  APen := SPen;
end;

procedure TDrawBuffer.Clear(Pen: Integer = 0);
begin
  SetRast(FRP, Pen);
end;

procedure TDrawBuffer.FillRect(const x1, y1, x2, y2: Integer);
begin
  RectFill(FRP, x1, y1, x2, y2);
end;

procedure TDrawBuffer.FillRect(const ARect: Types.TRect);
begin
  RectFill(FRP, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
end;

procedure TDrawBuffer.DrawText(x, y: Integer; AText: string);
begin
  GfxMove(FRP, x, y);
  AGraphics.GFXText(FRP, PChar(AText + #0), Length(AText));
end;

procedure TDrawBuffer.DrawText(AText: string); overload;
begin
  AGraphics.GFXText(FRP, PChar(AText + #0), Length(AText));
end;

procedure TDrawBuffer.SetPenPos(APenPos: TPoint);
begin
  GFXMove(FRP, APenPos.X, APenPos.Y);
end;

function TDrawBuffer.GetPenPos: TPoint;
begin
  Result.X := FRP^.cp_x;
  Result.Y := FRP^.cp_y;
end;

procedure TDrawBuffer.DrawImage(x,y: Integer; Src: TDrawBuffer);
begin
  ClipBlit(Src.RP, 0, 0, FRP, x, y, Src.Width, Src.Height, $00C0);
end;

procedure TDrawBuffer.DrawImage(x,y: Integer; SrcRP: PRastPort);
begin
  ClipBlit(SrcRP, 0, 0, FRP, x, y, SrcRP^.Bitmap^.BytesPerRow div 8 * SrcRP^.Bitmap^.Depth , SrcRP^.Bitmap^.Rows, $00C0);
end;

procedure TDrawBuffer.DrawToRastPort(x, y: Integer; DestRP: PRastPort);
begin
  ClipBlit(FRP, 0, 0, DestRP, x, y, Width, Height, $00C0);
end;

procedure TDrawBuffer.DrawToRastPort(x, y, SrcWidth, SrcHeight: Integer; DestRP: PRastPort);
begin
  ClipBlit(FRP, 0, 0, DestRP, x, y, SrcWidth, SrcHeight, $00C0);
end;


//############################# DrawPanel ##############################

// Constructor
constructor TMUIDrawPanel.Create;
begin
  inherited Create;
  FMouseInObject := False;
  // Basic min max settings
  FMinWidth := 0;
  FMinHeight := 0;
  FMaxWidth := MUI_MAXMAX;
  FMaxHeight := MUI_MAXMAX;
  FDefWidth := 100;
  FDefHeight := 100;
  Frame := MUIV_Frame_Text;
  Background.Spec := MUII_BACKGROUND;
  Font := MUIV_Font_Button;
  FillArea := True;
  InnerLeft := 0;
  InnerTop := 0;
  InnerBottom := 0;
  InnerRight := 0;
end;

procedure TMUIDrawPanel.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
end;

procedure TMUIDrawPanel.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMuiObj := NewObjectA(MUIPBType^.mcc_Class, nil, TagList.GetTagPointer);
    Pointer(INST_DATA(MUIPBType^.mcc_Class, Pointer(FMUIObj))^) := Self;
    AfterCreateObject;
  end;
end;

// Call to redraw the object
procedure TMUIDrawPanel.RedrawObject;
begin
  if HasObj then
    MUI_Redraw(FMUIObj, MADF_DRAWOBJECT);
end;
const
  MUI_EHF_GUIMODE = 1 shl 1;

// OM_SETUP
function TMUIDrawPanel.DoSetup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Setup): PtrUInt;
begin
  Result := DoSuperMethodA(cl, obj, msg);
  EHNode.ehn_Priority := 0;
  EHNode.ehn_Flags := MUI_EHF_GUIMODE;
  EHNode.ehn_Object := obj;
  EHNode.ehn_Class := cl;
  EHNode.ehn_Events := IDCMP_MOUSEBUTTONS or IDCMP_MOUSEMOVE or IDCMP_RAWKEY;
  {$ifdef AmigaOS4}
  EHNode.ehn_Events := EHNode.ehn_Events or IDCMP_EXTENDEDMOUSE;
  {$endif}
  DoMethod(OBJ_win(obj), [MUIM_Window_AddEventHandler, PtrUInt(@EHNode)]);
end;

// OM_CLEANUP
function TMUIDrawPanel.DoCleanup(cl: PIClass; Obj: PObject_; Msg: PMUIP_Cleanup): PtrUInt;
begin
  DoMethod(OBJ_win(obj), [MUIM_Window_RemEventHandler, PtrUInt(@EHNode)]);
  Result := DoSuperMethodA(cl,obj,msg);
end;

// MUIM_ASKMINMAX
function TMUIDrawPanel.DoAskMinMax(cl: PIClass; Obj: PObject_; Msg: PMUIP_AskMinMax): PtrUInt;
begin
  // let our superclass first fill in what it thinks about sizes.
  // this will e.g. add the size of frame and inner spacing.
  Result := DoSuperMethodA(cl, obj, msg);
  // now add the values specific to our object. note that we
  // indeed need to *add* these values, not just set them!
  msg^.MinMaxInfo^.MinWidth  := msg^.MinMaxInfo^.MinWidth + FMinWidth;
  msg^.MinMaxInfo^.DefWidth  := msg^.MinMaxInfo^.DefWidth + FDefHeight;
  msg^.MinMaxInfo^.MaxWidth  := IfThen(msg^.MinMaxInfo^.MaxWidth + FMaxWidth >= MUI_MAXMAX, MUI_MAXMAX, msg^.MinMaxInfo^.MaxWidth + FMaxWidth);

  msg^.MinMaxInfo^.MinHeight := msg^.MinMaxInfo^.MinHeight + FMinHeight;
  msg^.MinMaxInfo^.DefHeight := msg^.MinMaxInfo^.DefHeight + FDefHeight;
  msg^.MinMaxInfo^.MaxHeight := IfThen(msg^.MinMaxInfo^.MaxHeight + FMaxHeight >= MUI_MAXMAX, MUI_MAXMAX, msg^.MinMaxInfo^.MaxHeight + FMaxHeight);
end;

// MUIM_DRAW
function TMUIDrawPanel.DoDraw(cl: PIClass; Obj: PObject_; Msg: PMUIP_Draw): PtrUInt;
var
  Clip: Pointer;
  Ri: PMUI_RenderInfo;
  Rp: PRastPort;
  DrawRect: TRect;
begin
  // let it draw itself
  Result := DoSuperMethodA(cl,obj,msg);
  // if MADF_DRAWOBJECT isn't set, we shouldn't draw anything.
  // MUI just wanted to update the frame or something like that.
  if (Msg^.flags and MADF_DRAWOBJECT) = 0 then
    Exit;
  // get render info
  Ri := MUIRenderInfo(Obj);
  if not Assigned(Ri) then
    Exit;
  // get rastport for drawing
  Rp := Obj_Rp(Obj);
  if not Assigned(Rp) then
    Exit;

  DrawRect.Left := Obj_mLeft(Obj);
  DrawRect.Top := Obj_mTop(Obj);
  DrawRect.Width := Obj_mWidth(Obj);
  DrawRect.Height := Obj_mHeight(Obj);
  // install the clip region (do not draw over the border)
  clip := MUI_AddClipping(Ri, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height);
  DoDrawObject(Rp, DrawRect);
  MUI_RemoveClipRegion(Ri, Clip);
end;

procedure TMUIDrawPanel.DoDrawObject(Rp: PRastPort; DrawRect: TRect);
begin
  try
    if Assigned(FOnDrawObject) then
      FOnDrawObject(Self, Rp, DrawRect);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

procedure TMUIDrawPanel.ResetDblClickTime;
begin
  FMouseClickTime.LSecs := 0;
  FMouseClickTime.LMicros := 0;
  FMouseClickTime.MSecs := 0;
  FMouseClickTime.MMicros := 0;
  FMouseClickTime.RSecs := 0;
  FMouseClickTime.RMicros := 0;
end;

// MUIM_HANDLEEVENT
function TMUIDrawPanel.DoHandleEvent(cl: PIClass; Obj: PObject_; Msg: PMUIP_HandleEvent): PtrUInt;
var
  InObject: Boolean;
  EatMe: Boolean;
  RelX, RelY: Integer;
  // Keys
  Code: Word;
  IsKeyUp: Boolean;
  Qual: Word;
  Mss: TMUIShiftState;
  Buff: array[0..10] of char;
  ie: TInputEvent;
  IAddr: Pointer;
  {$ifdef AmigaOS4}
  WheelData: PIntuiWheelData;
  {$endif}
begin
  Result := DoSuperMethodA(cl,obj,msg);
  EatMe := False;
  // is the Pointer in the Object? mouse down only accept there
  InObject :=  OBJ_IsInObject(Msg^.Imsg^.MouseX, Msg^.Imsg^.MouseY, Obj) and not Boolean(MH_Get(Obj_Win(Obj), MUIA_Window_Sleep));
  case Msg^.imsg^.IClass of
    // Mouse Buttons
    IDCMP_MOUSEBUTTONS:
    begin
      RelX := Msg^.imsg^.MouseX - obj_Left(obj);
      RelY := Msg^.imsg^.MouseY - obj_Top(obj);
      case Msg^.imsg^.Code of
        SELECTDOWN:
        begin
          if not InObject then
            Exit;
          if Assigned(FOnMUIMouseDown) then
            FOnMUIMouseDown(Self, mmbLeft, RelX, RelY, EatMe);
        end;
        SELECTUP:
        begin
          if Assigned(FOnMUIMouseUp) then
            FOnMUIMouseUp(Self, mmbLeft, RelX, RelY, EatMe);
          if DoubleClick(FMouseClickTime.LSecs, FMouseClickTime.LMicros, Msg^.imsg^.Seconds, Msg^.imsg^.Micros) then
          begin
            if Assigned(FOnMUIDblClick) and InObject then
              FOnMUIDblClick(Self, mmbLeft, RelX, RelY, EatMe);
            FMouseClickTime.LSecs := 0;
            FMouseClickTime.LMicros := 0;
          end
          else
          begin
            if InObject then
            begin
              FMouseClickTime.LSecs := Msg^.imsg^.Seconds;
              FMouseClickTime.LMicros := Msg^.imsg^.Micros;
            end;
          end;

        end;
        MENUDOWN:
        begin
          if not InObject then
            Exit;
          if Assigned(FOnMUIMouseDown) then
            FOnMUIMouseDown(Self, mmbRight, RelX, RelY, EatMe);
        end;
        MENUUP:
        begin
          if Assigned(FOnMUIMouseUp) then
            FOnMUIMouseUp(Self, mmbRight, RelX, RelY, EatMe);
          if DoubleClick(FMouseClickTime.RSecs, FMouseClickTime.RMicros, Msg^.imsg^.Seconds, Msg^.imsg^.Micros) then
          begin
            if Assigned(FOnMUIDblClick) and InObject then
              FOnMUIDblClick(Self, mmbRight, RelX, RelY, EatMe);
            FMouseClickTime.RSecs := 0;
            FMouseClickTime.RMicros := 0;
          end
          else
          begin
            if InObject then
            begin
              FMouseClickTime.RSecs := Msg^.imsg^.Seconds;
              FMouseClickTime.RMicros := Msg^.imsg^.Micros;
            end;
          end;

        end;
      end;
    end;
    {$ifdef AmigaOS4}
    IDCMP_EXTENDEDMOUSE:
    begin
      if Msg^.imsg^.Code = IMSGCODE_INTUIWHEELDATA then
      begin
        RelX := Msg^.imsg^.MouseX - obj_Left(obj);
        RelY := Msg^.imsg^.MouseY - obj_Top(obj);
        WheelData := PIntuiWheelData(Msg^.imsg^.IAddress);
        // Mouse wheel with Value 120 (from the other interfaces)
        if Assigned(FOnMUIMouseWheel) then
          FOnMUIMouseWheel(Self, WheelData^.WheelY > 0, EatMe);
      end;
    end;
    {$endif}
    // Mouse Move
    IDCMP_MOUSEMOVE:
    begin
      if FMouseInObject and not InObject then
      begin
        FMouseInObject := False;
        if Assigned(FOnMUIMouseLeave) then
          FOnMUIMouseLeave(Self);
      end;
      if not InObject then
        Exit;
      FMouseInObject := True;
      RelX := Msg^.imsg^.MouseX - obj_Left(obj);
      RelY := Msg^.imsg^.MouseY - obj_Top(obj);
      if Assigned(FOnMUIMouseMove) then
        FOnMUIMouseMove(Self, RelX, RelY, EatMe);
    end;
    // Raw Key
    IDCMP_RAWKEY:
    begin
      // Gather Values
      IsKeyUp := (Msg^.iMsg^.Code and IECODE_UP_PREFIX) <> 0;
      Code := Msg^.iMsg^.Code and not IECODE_UP_PREFIX;
      Qual := Msg^.iMsg^.Qualifier;
      IAddr := Msg^.iMsg^.IAddress; 
      // Mouse Wheel
      if (Code = $7A) or (Code = $7B) then
      begin
        if Assigned(FOnMUIMouseWheel) then
          FOnMUIMouseWheel(Self, Code = $7A, EatMe);
      end
      else
      begin
        // Check the shiftstate
        Mss := [];
        if (Qual and IEQUALIFIER_CONTROL) <> 0 then
          Mss := Mss + [mssCtrl];
        if (Qual and IEQUALIFIER_LSHIFT) <> 0 then
          Mss := Mss + [mssLShift, mssShift];
        if (Qual and IEQUALIFIER_RSHIFT) <> 0 then
          Mss := Mss + [mssRShift, mssShift];
        if (Qual and IEQUALIFIER_LALT) <> 0 then
          Mss := Mss + [mssLAlt];
        if (Qual and IEQUALIFIER_RALT) <> 0 then
          Mss := Mss + [mssRAlt];
        if (Qual and IEQUALIFIER_LCOMMAND) <> 0 then
          Mss := Mss + [mssLAmiga];
        if (Qual and IEQUALIFIER_RCOMMAND) <> 0 then
          Mss := Mss + [mssRAmiga];
        // Get the actual char for it
        Buff[0] := #0;
        ie.ie_Class := IECLASS_RAWKEY;
        ie.ie_SubClass := 0;
        ie.ie_Code := Code;
        ie.ie_Qualifier := Qual and (not (IEQUALIFIER_CONTROL or IEQUALIFIER_LALT));
        ie.ie_position.ie_addr := PPointer(IAddr)^;
        ie.ie_NextEvent := nil;
        MapRawKey(@ie, @Buff[0], 1, nil);
        // send message
        if IsKeyUp then
        begin
          if Assigned(FOnMUIKeyUp) then
            FOnMUIKeyUp(Self, Mss, Code, Buff[0], EatMe);
        end
        else
        begin
          if Assigned(FOnMUIKeyDown) then
            FOnMUIKeyDown(Self, Mss, Code, Buff[0], EatMe);
        end;
      end;
    end;
  end;
  if EatMe then
    Result := MUI_EventHandlerRC_Eat;
end;


function TMUIDrawPanel.MUIEvent(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt;
begin
  case Msg^.MethodID of
    MUIM_Setup: Result := DoSetup(cl, Obj, Pointer(Msg));
    MUIM_Cleanup: Result := DoCleanup(cl, Obj, Pointer(Msg));
    //
    MUIM_AskMinMax: Result := DoAskMinMax(cl, Obj, Pointer(Msg));
    //
    MUIM_Draw: Result := DoDraw(cl, Obj, Pointer(Msg));
    MUIM_HANDLEEVENT: Result := DoHandleEvent(cl, Obj, Pointer(Msg));
    else
      Result := DoSuperMethodA(cl, obj, msg);
  end;
end;

// Dispatcher
// send everything to the object
function MPBDispatcher(cl: PIClass; Obj: PObject_; Msg: intuition.PMsg): PtrUInt;
var
  MUIPB: TMUIDrawPanel;
begin
  try
    case Msg^.MethodID of
      MUIM_Setup,
      MUIM_Cleanup,
      MUIM_AskMinMax,
      MUIM_Draw,
      MUIM_HANDLEEVENT:
      begin
        MUIPB := TMUIDrawPanel(INST_DATA(cl, Pointer(obj))^); // get class
        if Assigned(MUIPB) then
          MPBDispatcher := MUIPB.MUIEvent(cl, Obj, Msg)                       // call the class dispatcher
        else
          MPBDispatcher := DoSuperMethodA(cl, obj, msg);     // Still not assigned just use default
      end;
      else
        MPBDispatcher := DoSuperMethodA(cl, obj, msg);
    end;
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

// create CustomClass
procedure MakePaintBoxClass;
begin
  MUIPBType := MH_CreateCustomClass(nil, MUIC_Area, nil, SizeOf(Pointer), @MPBDispatcher);
end;

// Destroy CustomClass
procedure FreePaintBoxClass;
begin
  if Assigned(MUIPBType) then
    MUI_DeleteCustomClass(MUIPBType);
end;

initialization
  MakePaintBoxClass;
finalization
  FreePaintBoxClass;
end.
