unit MUIClass.Area;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, agraphics, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Window;

type
  TSpecDesc = class;
  // Bubble Handle
  TBubble = record
  end;
  PBubble = ^TBubble;
  TBubbleList = specialize TFPGList<PBubble>;

  TMUIRGB = record
    Red: LongWord;
    Green: LongWord;
    Blue: LongWord;
  end;
  PMUIRGB = ^TMUIRGB;

  TMUIArea = class(TMUIWithParent)
  private
    FBubbleList: TBubbleList;
    FBackground: TSpecDesc;
    FOnClick: TNotifyEvent;

    FControlChar: Char;
    FCycleChain: Integer;
    FDisabled: Boolean;
    FDraggable: Boolean;
    FDropable: Boolean;
    FFillArea: Boolean;
    FFixHeight: Integer;
    FFixHeightTxt: string;
    FFixWidth: Integer;
    FFixWidthTxt: string;
    FFont: PtrInt;
    FFrame: PtrInt;
    FFramePhantomHoriz: Boolean;
    FFrameTitle: string;
    FHorizDisappear: Integer;
    FVertDisappear: Integer;
    FHorizWeight: Integer;
    FVertWeight: Integer;
    FInnerLeft: Integer;
    FInnerTop: Integer;
    FInnerRight: Integer;
    FInnerBottom: Integer;
    FInputMode: Integer;
    FMaxHeight: Integer;
    FMaxWidth: Integer;
    FSelected: Boolean;
    FShortHelp: string;
    FShowSelState: Boolean;
    FWeight: Integer;
    function GetLeftEdge: Integer;
    function GetTopEdge: Integer;
    function GetRightEdge: Integer;
    function GetBottomEdge: Integer;
    procedure SetControlChar(AValue: Char);
    function GetCycleChain: Integer;
    procedure SetCycleChain(AValue: Integer);
    procedure SetDisabled(AValue: Boolean);
    procedure SetDraggable(AValue: Boolean);
    procedure SetDropable(AValue: Boolean);
    procedure SetFillArea(AValue: Boolean);
    procedure SetFixHeight(AValue: Integer);
    procedure SetFixHeightTxt(AValue: string);
    procedure SetFixWidth(AValue: Integer);
    procedure SetFixWidthTxt(AValue: string);
    procedure SetFont(AValue: PtrInt);
    procedure SetFrame(AValue: PtrInt);
    procedure SetFramePhantomHoriz(AValue: Boolean);
    procedure SetFrameTitle(AValue: string);
    function GetHeight: Integer;
    function GetWidth: Integer;
    procedure SetHorizDisappear(AValue: Integer);
    procedure SetHorizWeight(AValue: Integer);
    procedure SetVertDisappear(AValue: Integer);
    procedure SetVertWeight(AValue: Integer);
    function GetInnerLeft: Integer;
    procedure SetInnerLeft(AValue: Integer);
    function GetInnerTop: Integer;
    procedure SetInnerTop(AValue: Integer);
    function GetInnerRight: Integer;
    procedure SetInnerRight(AValue: Integer);
    function GetInnerBottom: Integer;
    procedure SetInnerBottom(AValue: Integer);
    procedure SetInputMode(AValue: Integer);
    procedure SetMaxHeight(AValue: Integer);
    procedure SetMaxWidth(AValue: Integer);
    function GetPressed: Boolean;
    function GetSelected: Boolean;
    procedure SetSelected(AValue: Boolean);
    procedure SetShortHelp(AValue: string);
    procedure SetShowSelState(AValue: Boolean);
    procedure SetWeight(AValue: Integer);
    function GetWindow: PWindow;
    function GetWindowObject: TMUIWindow;
  protected
    procedure AfterCreateObject; override;
    procedure BeforeCloseWindow; override;
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    property FillArea: Boolean read FFillArea write SetFillArea;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateObject; override;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    // MUI Methods
    function CreateBubble(x,y: Integer; Txt: string; Flags: Integer): PBubble;
    procedure DeleteBubble(Bubble: PBubble);
    procedure DeleteAllBubbles;
    // MUI Fields
    property Background: TSpecDesc read FBackground;
    property LeftEdge: Integer read GetLeftEdge;
    property TopEdge: Integer read GetTopEdge;
    property RightEdge: Integer read GetRightEdge;
    property BottomEdge: Integer read GetBottomEdge;
    // ContextMenu/ContextMenuTrigger (need some special care, destroying?)
    property ControlChar: Char read FControlChar write SetControlChar;
    property CycleChain: Integer read GetCycleChain write SetCycleChain;
    property Disabled: Boolean read FDisabled write SetDisabled;
    property Draggable: Boolean read FDraggable write SetDraggable;
    property Dropable: Boolean read FDropable write SetDropable;
    property FixHeight: Integer read FFixHeight write SetFixHeight;
    property FixHeightTxt: string read FFixHeightTxt write SetFixHeightTxt;
    property FixWidth: Integer read FFixWidth write SetFixWidth;
    property FixWidthTxt: string read FFixWidthTxt write SetFixWidthTxt;
    property Font: PtrInt read FFont write SetFont;                             // MUIV_Font_* or PtrInt(PTextFont)
    property Frame: PtrInt read FFrame write SetFrame;                          // MUIV_Frame_*
    property FramePhantomHoriz: Boolean read FFramePhantomHoriz write SetFramePhantomHoriz;
    property FrameTitle: string read FFrameTitle write SetFrameTitle;
    property Height: Integer read GetHeight;
    property Width: Integer read GetWidth;
    property HorizDisappear: Integer read FHorizDisappear write SetHorizDisappear;
    property VertDisappear: Integer read FVertDisappear write SetVertDisappear;
    property HorizWeight: Integer read FHorizWeight write SetHorizWeight;
    property VertWeight: Integer read FVertWeight write SetVertWeight;
    property InnerLeft: Integer read GetInnerLeft write SetInnerLeft;
    property InnerTop: Integer read GetInnerTop write SetInnerTop;
    property InnerRight: Integer read GetInnerRight write SetInnerRight;
    property InnerBottom: Integer read GetInnerBottom write SetInnerBottom;
    property InputMode: Integer read FInputMode write SetInputMode;            // MUIV_InputMode_*
    property MaxHeight: Integer read FMaxHeight write SetMaxHeight;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth;
    property Pressed: Boolean read GetPressed;
    property Selected: Boolean read GetSelected write SetSelected;
    property ShortHelp: string read FShortHelp write SetShortHelp;
    property ShowSelState: Boolean read FShowSelState write SetShowSelState;  // defaults to true
    // Timer -> event?
    property Weight: Integer read FWeight write SetWeight;
    property Window: PWindow read GetWindow;
    property WindowObject: TMUIWindow read GetWindowObject;
  end;

  TMUIRectangle = class(TMUIArea)
  private
    FBarTitle: string;
    FHBar: Boolean;
    FVBar: Boolean;
    procedure SetBarTitle(AValue: string);
    procedure SetHBar(AValue: Boolean);
    procedure SetVBar(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property BarTitle: string read FBarTitle write SetBarTitle;
    property HBar: Boolean read FHBar write SetHBar;
    property VBar: Boolean read FVBar write SetVBar;
  end;

  TMUIBalance = class(TMUIArea)
  private
    FQuiet: Boolean;
    procedure SetQuiet(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Quiet: Boolean read FQuiet write SetQuiet;  // If set to TRUE the balance object will show its frame only if the mouse is located over the object. Otherwise it will be invisible.
  end;

  TMUIGauge = class(TMUIArea)
  private
    FCurrent: Integer;
    FDivide: LongWord;
    FHoriz: Boolean;
    FMax: Integer;
    procedure SetHoriz(AValue: Boolean);
    procedure SetCurrent(AValue: Integer);
    procedure SetDivide(AValue: LongWord);
    procedure SetMax(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Current: Integer read FCurrent write SetCurrent;
    property Divide: LongWord read FDivide write SetDivide;
    property Max: Integer read FMax write SetMax;
    property Horiz: Boolean read FHoriz write SetHoriz;
  end;

  TMUIScale = class(TMUIArea)
  private
    FHoriz: Boolean;
    procedure SetHoriz(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Horiz: Boolean read FHoriz write SetHoriz;
  end;

  TMUIColorfield = class(TMUIArea)
  private
    FBlue: LongWord;
    FBlueSet: Boolean;
    FRed: LongWord;
    FRedSet: Boolean;
    FGreen: LongWord;
    FGreenSet: Boolean;
    FPen: LongWord;
    procedure SetBlue(AValue: LongWord);
    procedure SetRed(AValue: LongWord);
    procedure SetGreen(AValue: LongWord);
    function GetPen: LongWord;
    procedure SetPen(AValue: LongWord);
    function GetRGB: TMUIRGB;
    procedure SetRGB(AValue: TMUIRGB);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    property Blue: LongWord read FBlue write SetBlue;
    property Red: LongWord read FRed write SetRed;
    property Green: LongWord read FGreen write SetGreen;
    property Pen: LongWord read GetPen write SetPen;
    property RGB: TMUIRGB read GetRGB write SetRGB;
  end;

  //
  TMUIText = class(TMUIArea)
  private
    FContents: string;  //  ''
    FHiChar: char;      //*  #0
    FPreParse: string;  //  ''
    FSetMax: Boolean;   //* False
    FSetMin: Boolean;   //* False
    FSetVMax: Boolean;  //* True
    procedure SetContents(AContents: string);
    procedure SetHiChar(AHiChar: char);
    procedure SetPreParse(AValue: string);
    procedure SetSetMax(AValue: Boolean);
    procedure SetSetMin(AValue: Boolean);
    procedure SetSetVMax(AValue: Boolean);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    constructor Create(AContents: string); virtual;
    procedure CreateObject; override;
    property Contents: string read FContents write SetContents;
    property HiChar: char read FHiChar write SetHiChar;
    property PreParse: string read FPreParse write SetPreParse;
    property SetMax: Boolean read FSetMax write SetSetMax;
    property SetMin: Boolean read FSetMin write SetSetMin;
    property SetVMax: Boolean read FSetVMax write SetSetVMax;
  end;

  TMUIPenDisplay = class(TMUIArea)
  private
    FSpec: PMUI_PenSpec;
    FOnSpecChange: TNotifyEvent;
    function GetPen: LongWord;
    procedure SetSpec(AValue: PMUI_PenSpec);
    function GetSpec: PMUI_PenSpec;
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    procedure CreateObject; override;
    // Methods
    procedure SetColormap(ColorMap: Integer);
    procedure SetMUIPen(MUIPen: Integer);
    procedure SetRGB(Red, Green, Blue: LongWord);
    procedure SetRGB8(Red, Green, Blue: Byte);
    // Fields
    property Pen: LongWord read GetPen;
    // Reference? what it does?
    property Spec: PMUI_PenSpec read GetSpec write SetSpec;
    property OnSpecChange: TNotifyEvent read FOnSpecChange write FOnSpecChange;
  end;

  TMUIPopPen = class(TMUIPenDisplay)
  public
    procedure CreateObject; override;
  end;

  TMUIButton = class(TMUIText)
  public
    constructor Create; override;
    constructor Create(ATitle: string); override;
  end;

  TMUIHSpace = class(TMUIRectangle)
  public
    constructor Create; override;
    constructor Create(Space: Integer); virtual;
  end;

  TMUIVSpace = class(TMUIRectangle)
  public
    constructor Create; override;
    constructor Create(Space: Integer); virtual;
  end;

  TMUIHBar = class(TMUIRectangle)
  public
    constructor Create; override;
    constructor Create(Space: Integer); virtual;
  end;

  TMUIVBar = class(TMUIRectangle)
  public
    constructor Create; override;
    constructor Create(Space: Integer); virtual;
  end;

  TMUIBarTitle = class(TMUIRectangle)
  public
    constructor Create; override;
    constructor Create(ALabel: string); virtual;
  end;

  TSpecDesc = class
  private
    FText: string;
    FSpec: PtrInt;
    FIsSet: Boolean;
    procedure SetFSpec(ASpec: PtrInt);
  public
    constructor Create; virtual;
    procedure SetStdPattern(APattern: Integer);        // MUII_BACKGROUND ... MUII_FILLBACK2 , MUII_ButtonBack, MUII_TextBack
    procedure SetRGB(R,G,B: Byte); overload;           //2: r,g,b
    procedure SetRGB32(R,G,B: LongWord); overload;     //2: r,g,b
    procedure SetBoopsiName(AName: string);            //3: name of external BOOPSI image class
    procedure SetMUIBrush(AName: string);              //4: Name of external MUI Brush
    procedure SetPicture(PicPath: string);             //5: Path to external file
    procedure SetPreConf(ABack: Integer);              //6: MUII_WindowBack ... MUII_Count - 1
    procedure SetString(AText: string);                // your own string must include the '<num>:';
    property Spec: PtrInt read FSpec write SetFSpec;
    property IsSet: Boolean read FIsSet;
  end;



  function ColComToMUI(c: Byte): LongWord; inline;
  function MUIToColComp(c: LongWord): Byte; inline;

implementation

function ColComToMUI(c: Byte): LongWord; inline;
begin
  Result := c shl 24 or c shl 16 or c shl 8 or c;
end;

function MUIToColComp(c: LongWord): Byte; inline;
begin
  Result := (c shr 24) and $FF;
end;

{ TMUIArea }

constructor TMUIArea.Create;
begin
  inherited;
  FBubbleList := TBubbleList.Create;
  FBackground := TSpecDesc.Create;
  FCycleChain := 0;
  FDisabled := False;
  FDraggable := False;
  FDropable := False;
  FFillArea := True;
  FFixHeight := 0;
  FFixHeightTxt := '';
  FFixWidth := 0;
  FFixWidthTxt := '';
  FFont := 0;
  FFrame := 0;
  FFramePhantomHoriz := False;
  FFrameTitle := '';
  FHorizDisappear := 0;
  FHorizWeight := 100;
  FVertDisappear := 0;
  FVertWeight := 100;
  FInnerLeft := -1;
  FInnerTop := -1;
  FInnerRight := -1;
  FInnerBottom := -1;
  FInputMode := MUIV_InputMode_None;
  FMaxHeight := 0;
  FMaxWidth := 0;
  FSelected := False;
  FShortHelp := '';
  FShowSelState := True;
  FWeight := 100;
end;

destructor TMUIArea.Destroy;
begin
  DeleteAllBubbles;
  FBubbleList.Free;
  FBackground.Free;
  inherited;
end;

procedure TMUIArea.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FBackground.IsSet then
    ATagList.AddTag(MUIA_Background, AsTag(FBackground.FSpec));
  if FCycleChain <> 0 then
    ATagList.AddTag(MUIA_CycleChain, AsTag(FCycleChain));
  if FDisabled then
    ATagList.AddTag(MUIA_Disabled, AsTag(FDisabled));
  if FDraggable then
    ATagList.AddTag(MUIA_Draggable, AsTag(FDraggable));
  if FDropable then
    ATagList.AddTag(MUIA_Dropable, AsTag(FDropable));
  if not FFillArea then
    ATagList.AddTag(MUIA_FillArea, AsTag(FFillArea));
  if FFixHeight > 0 then
    ATagList.AddTag(MUIA_FixHeight, AsTag(FFixHeight));
  if FFixHeightTxt <> '' then
    ATagList.AddTag(MUIA_FixHeightTxt, AsTag(PChar(FFixHeightTxt)));
  if FFixWidth > 0 then
    ATagList.AddTag(MUIA_FixWidth, AsTag(FFixWidth));
  if FFixWidthTxt <> '' then
    ATagList.AddTag(MUIA_FixWidthTxt, AsTag(PChar(FFixWidthTxt)));
  if FFont <> 0 then
    ATagList.AddTag(MUIA_Font, AsTag(FFont));
  if FFrame <> 0 then
    ATagList.AddTag(MUIA_Frame, AsTag(FFrame));
  if FFramePhantomHoriz then
    ATagList.AddTag(MUIA_FramePhantomHoriz, AsTag(FFramePhantomHoriz));
  if FFrameTitle <> '' then
    ATagList.AddTag(MUIA_FrameTitle, AsTag(PChar(FFrameTitle)));
  if FHorizDisappear <> 0 then
    ATagList.AddTag(MUIA_HorizDisappear, AsTag(FHorizDisappear));
  if FHorizWeight <> 100 then
    ATagList.AddTag(MUIA_HorizWeight, AsTag(FHorizWeight));
  if FVertDisappear <> 0 then
    ATagList.AddTag(MUIA_VertDisappear, AsTag(FVertDisappear));
  if FVertWeight <> 100 then
    ATagList.AddTag(MUIA_VertWeight, AsTag(FVertWeight));
  if FInnerLeft >= 0 then
    ATagList.AddTag(MUIA_InnerLeft, AsTag(FInnerLeft));
  if FInnerTop >= 0 then
    ATagList.AddTag(MUIA_InnerTop, AsTag(FInnerTop));
  if FInnerRight >= 0 then
    ATagList.AddTag(MUIA_InnerRight, AsTag(FInnerRight));
  if FInnerBottom >= 0 then
    ATagList.AddTag(MUIA_InnerBottom, AsTag(FInnerBottom));
  if FInputMode <> MUIV_InputMode_None then
    ATagList.AddTag(MUIA_InputMode, AsTag(FInputMode));
  if FMaxHeight > 0 then
    ATagList.AddTag(MUIA_MaxHeight, AsTag(FMaxHeight));
  if FMaxWidth > 0 then
    ATagList.AddTag(MUIA_MaxWidth, AsTag(FMaxWidth));
  if FSelected then
    ATagList.AddTag(MUIA_Selected, AsTag(FSelected));
  if FShortHelp <> '' then
    ATagList.AddTag(MUIA_ShortHelp, AsTag(PChar(FShortHelp)));
  if not FShowSelState then
    ATagList.AddTag(MUIA_ShowSelState, AsTag(FShowSelState));
  if FWeight <> 100 then
    ATagList.AddTag(MUIA_Weight, AsTag(FWeight));
end;

procedure TMUIArea.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Area, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function PressFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIArea;
begin
  Result := 0;
  PasObj := TMUIArea(Hook^.h_Data);
  if Assigned(PasObj.FOnClick) then
    PasObj.FOnClick(PasObj);
end;

procedure TMUIArea.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_Pressed, MUI_FALSE, @PressFunc);
end;

procedure TMUIArea.BeforeCloseWindow;
begin
  DeleteAllBubbles;
  inherited;
end;

function TMUIArea.GetLeftEdge: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_LeftEdge);
end;

function TMUIArea.GetTopEdge: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_TopEdge);
end;

function TMUIArea.GetRightEdge: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_RightEdge);
end;

function TMUIArea.GetBottomEdge: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_BottomEdge);
end;

procedure TMUIArea.SetControlChar(AValue: Char);
begin
  if AValue <> FControlChar then
  begin
    FControlChar := AValue;
    if HasObj then
    begin
      SetValue(MUIA_ControlChar, AsTag(LongWord(FControlChar)));
    end;
  end;
end;

function TMUIArea.GetCycleChain: Integer;
begin
  Result := FCycleChain;
  if HasObj then
    Result := GetIntValue(MUIA_CycleChain);
end;

procedure TMUIArea.SetCycleChain(AValue: Integer);
begin
  if AValue <> FCycleChain then
  begin
    FCycleChain := AValue;
    if HasObj then
    begin
      SetValue(MUIA_CycleChain, AsTag(FCycleChain));
    end;
  end;
end;

procedure TMUIArea.SetDisabled(AValue: Boolean);
begin
  if AValue <> FDisabled then
  begin
    FDisabled := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Disabled, AsTag(FDisabled));
    end;
  end;
end;

procedure TMUIArea.SetDraggable(AValue: Boolean);
begin
  if AValue <> FDraggable then
  begin
    FDraggable := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Draggable, AsTag(FDraggable));
    end;
  end;
end;

procedure TMUIArea.SetDropable(AValue: Boolean);
begin
  if AValue <> FDropable then
  begin
    FDropable := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Dropable, AsTag(FDropable));
    end;
  end;
end;

procedure TMUIArea.SetFillArea(AValue: Boolean);
begin
  if AValue <> FFillArea then
  begin
    FFillArea := AValue;
    if HasObj then
    begin
      SetValue(MUIA_FillArea, AsTag(FFillArea));
    end;
  end;
end;

procedure TMUIArea.SetFixHeight(AValue: Integer);
begin
  if AValue <> FFixHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FixHeight', IntToStr(AValue))
    else
      FFixHeight := AValue;
  end;
end;

procedure TMUIArea.SetFixHeightTxt(AValue: string);
begin
  if AValue <> FFixHeightTxt then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FixHeightTxt', AValue)
    else
      FFixHeightTxt := AValue;
  end;
end;

procedure TMUIArea.SetFixWidth(AValue: Integer);
begin
  if AValue <> FFixWidth then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FixWidth', IntToStr(AValue))
    else
      FFixWidth := AValue;
  end;
end;

procedure TMUIArea.SetFixWidthTxt(AValue: string);
begin
  if AValue <> FFixWidthTxt then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FixWidthTxt', AValue)
    else
      FFixWidthTxt := AValue;
  end;
end;

procedure TMUIArea.SetFont(AValue: PtrInt);
begin
  if AValue <> FFont then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Font', IntToStr(AValue))
    else
      FFont := AValue;
  end;
end;

procedure TMUIArea.SetFrame(AValue: PtrInt);
begin
  if AValue <> FFrame then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Frame', IntToStr(AValue))
    else
      FFrame := AValue;
  end;
end;

procedure TMUIArea.SetFramePhantomHoriz(AValue: Boolean);
begin
  if FFramePhantomHoriz <> AValue then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FFramePhantomHoriz', BoolToStr(AValue, True))
    else
      FFramePhantomHoriz := AValue;
  end;
end;

procedure TMUIArea.SetFrameTitle(AValue: string);
begin
  if AValue <> FFrameTitle then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FrameTitle', AValue)
    else
      FFrameTitle := AValue;
  end;
end;

function TMUIArea.GetHeight: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_Height);
end;

function TMUIArea.GetWidth: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_Width);
end;

procedure TMUIArea.SetHorizDisappear(AValue: Integer);
begin
  if AValue <> FHorizDisappear then
  begin
    FHorizDisappear := AValue;
    if HasObj then
    begin
      SetValue(MUIA_HorizDisappear, AsTag(FHorizDisappear));
    end;
  end;
end;

procedure TMUIArea.SetHorizWeight(AValue: Integer);
begin
  if AValue <> FHorizWeight then
  begin
    FHorizWeight := AValue;
    if HasObj then
    begin
      SetValue(MUIA_HorizWeight, AsTag(FHorizWeight));
    end;
  end;
end;

procedure TMUIArea.SetVertDisappear(AValue: Integer);
begin
  if AValue <> FVertDisappear then
  begin
    FVertDisappear := AValue;
    if HasObj then
    begin
      SetValue(MUIA_VertDisappear, AsTag(FVertDisappear));
    end;
  end;
end;

procedure TMUIArea.SetVertWeight(AValue: Integer);
begin
  if AValue <> FVertWeight then
  begin
    FVertWeight := AValue;
    if HasObj then
    begin
      SetValue(MUIA_VertWeight, AsTag(FVertWeight));
    end;
  end;
end;

function TMUIArea.GetInnerLeft: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_InnerLeft);
end;

procedure TMUIArea.SetInnerLeft(AValue: Integer);
begin
  if AValue <> FInnerLeft then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_InnerLeft', IntToStr(AValue))
    else
      FInnerLeft := AValue;
  end;
end;

function TMUIArea.GetInnerTop: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_InnerTop);
end;

procedure TMUIArea.SetInnerTop(AValue: Integer);
begin
  if AValue <> FInnerTop then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_InnerTop', IntToStr(AValue))
    else
      FInnerTop := AValue;
  end;
end;

function TMUIArea.GetInnerRight: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_InnerRight);
end;

procedure TMUIArea.SetInnerRight(AValue: Integer);
begin
  if AValue <> FInnerRight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_InnerRight', IntToStr(AValue))
    else
      FInnerRight := AValue;
  end;
end;

function TMUIArea.GetInnerBottom: Integer;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_InnerBottom);
end;

procedure TMUIArea.SetInnerBottom(AValue: Integer);
begin
  if AValue <> FInnerBottom then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_InnerBottom', IntToStr(AValue))
    else
      FInnerBottom := AValue;
  end;
end;

procedure TMUIArea.SetInputMode(AValue: Integer);
begin
  if AValue <> FInputMode then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_InputMode', IntToStr(AValue))
    else
      FInputMode := AValue;
  end;
end;

procedure TMUIArea.SetMaxWidth(AValue: Integer);
begin
  if AValue <> FMaxWidth then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_MaxWidth', IntToStr(AValue))
    else
      FMaxWidth := AValue;
  end;
end;

procedure TMUIArea.SetMaxHeight(AValue: Integer);
begin
  if AValue <> FMaxHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_MaxHeight', IntToStr(AValue))
    else
      FMaxHeight := AValue;
  end;
end;

function TMUIArea.GetPressed: Boolean;
begin
  Result := False;
  if HasObj then
    Result := GetBoolValue(MUIA_Pressed);
end;

function TMUIArea.GetSelected: Boolean;
begin
  Result := FSelected;
  if HasObj then
    Result := GetBoolValue(MUIA_Selected);
end;

procedure TMUIArea.SetSelected(AValue: Boolean);
begin
  if AValue <> FSelected then
  begin
    FSelected := AValue;
    if HasObj then
    begin
      SetValue(MUIA_Selected, AsTag(FSelected));
    end;
  end;
end;

procedure TMUIArea.SetShortHelp(AValue: string);
begin
  if AValue <> FShortHelp then
  begin
    FShortHelp := AValue;
    if HasObj then
    begin
      SetValue(MUIA_ShortHelp, AsTag(PChar(FShortHelp)));
    end;
  end;
end;

procedure TMUIArea.SetShowSelState(AValue: Boolean);
begin
  if FShowSelState <> AValue then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_FShowSelState', BoolToStr(AValue, True))
    else
      FShowSelState := AValue;
  end;
end;

procedure TMUIArea.SetWeight(AValue: Integer);
begin
  if AValue <> FWeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Weight', IntToStr(AValue))
    else
      FWeight := AValue;
  end;
end;

function TMUIArea.GetWindow: PWindow;
begin
  Result := nil;
  if HasObj then
    Result := GetPointerValue(MUIA_Window);
end;

function TMUIArea.GetWindowObject: TMUIWindow;
var
  WinObj: PObject_;
begin
  Result := nil;
  if HasObj then
  begin
    WinObj := GetPointerValue(MUIA_WindowObject);
    if Assigned(WinObj) then
    begin
      Result := TMUIWindow(GetPasObject(WinObj));
    end;
  end;
end;

function TMUIArea.CreateBubble(x,y: Integer; Txt: string; Flags: Integer): PBubble;
begin
  Result := nil;
  if HasObj then
  begin
    Result := PBubble(DoMethod(MUIObj, [MUIM_CreateBubble, x, y, AsTag(PChar(Txt)), Flags]));
    if Assigned(Result) then
      FBubbleList.Add(Result);
  end;
end;

procedure TMUIArea.DeleteBubble(Bubble: PBubble);
begin
  if HasObj then
  begin
    FBubbleList.Remove(Bubble);
    DoMethod(MUIObj, [MUIM_DeleteBubble, AsTag(Bubble)]);
  end;
end;

procedure TMUIArea.DeleteAllBubbles;
var
  I: Integer;
begin
  if HasObj then
  begin
    for i := 0 to FBubbleList.Count - 1 do
      DoMethod(MUIObj, [MUIM_DeleteBubble, AsTag(FBubbleList[i])]);
  end;
  FBubbleList.Clear;
end;

{ TMUIRectangle }

constructor TMUIRectangle.Create;
begin
  inherited;
  FBarTitle := '';
  FHBar := False;
  FVBar := False;
end;

procedure TMUIRectangle.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FBarTitle <> '' then
    ATagList.AddTag(MUIA_Rectangle_BarTitle, AsTag(PChar(FBarTitle)));
  if FHBar then
    ATagList.AddTag(MUIA_Rectangle_HBar, AsTag(FHBar));
  if FVBar then
    ATagList.AddTag(MUIA_Rectangle_VBar, AsTag(FVBar));
end;

procedure TMUIRectangle.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Rectangle, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;


procedure TMUIRectangle.SetBarTitle(AValue: string);
begin
  if AValue <> FBarTitle then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Rectangle_BarTitle', AValue)
    else
      FBarTitle := AValue;
  end;
end;

procedure TMUIRectangle.SetHBar(AValue: Boolean);
begin
  if AValue <> FHBar then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Rectangle_HBar', BoolToStr(AValue, True))
    else
      FHBar := AValue;
  end;
end;


procedure TMUIRectangle.SetVBar(AValue: Boolean);
begin
  if AValue <> FVBar then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Rectangle_VBar', BoolToStr(AValue, True))
    else
      FVBar := AValue;
  end;
end;


{ TMUIBalance }

constructor TMUIBalance.Create;
begin
  inherited;
  FQuiet := False;
end;

procedure TMUIBalance.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  {$ifndef Amiga68k}
  if FQuiet then
    ATagList.AddTag(MUIA_Balance_Quiet, AsTag(FQuiet));
  {$endif}
end;

procedure TMUIBalance.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Balance, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;


procedure TMUIBalance.SetQuiet(AValue: Boolean);
begin
  if AValue <> FQuiet then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Balance_Quiet', BoolToStr(AValue, True))
    else
      FQuiet := AValue;
  end;
end;


{ TMUIGauge }

constructor TMUIGauge.Create;
begin
  inherited;
  Frame := MUIV_Frame_Gauge;
  //
  FCurrent := 0;
  FMAx := 0;
  FDivide := 0;
  FHoriz := False;
end;

procedure TMUIGauge.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FHoriz then
    ATagList.AddTag(MUIA_Gauge_Horiz, AsTag(FHoriz));
  if FCurrent > 0 then
    ATagList.AddTag(MUIA_Gauge_Current, AsTag(Min(FCurrent, FMax)));
  if FDivide > 0 then
    ATagList.AddTag(MUIA_Gauge_Divide, AsTag(FDivide));
  if FMax > 0 then
    ATagList.AddTag(MUIA_Gauge_Max, AsTag(FMax));
end;

procedure TMUIGauge.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Gauge, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIGauge.SetCurrent(AValue: Integer);
begin
  if AValue <> FCurrent then
  begin
    FCurrent := AValue;
    if HasObj then
      SetValue(MUIA_Gauge_Current, AsTag(Math.Min(FCurrent, FMax)));
  end;
end;

procedure TMUIGauge.SetDivide(AValue: LongWord);
begin
  if AValue <> FDivide then
  begin
    FDivide := AValue;
    if HasObj then
      SetValue(MUIA_Gauge_Divide, AsTag(FDivide));
  end;
end;

procedure TMUIGauge.SetHoriz(AValue: Boolean);
begin
  if AValue <> FHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Gauge_Horiz', BoolToStr(AValue, True))
    else
      FHoriz := AValue;
  end;
end;

procedure TMUIGauge.SetMax(AValue: Integer);
begin
  if AValue <> FMax then
  begin
    FMax := AValue;
    if HasObj then
      SetValue(MUIA_Gauge_Max, AsTag(FMax));
  end;
end;

{ TMUIScale }

constructor TMUIScale.Create;
begin
  inherited;
  FHoriz := False;
end;

procedure TMUIScale.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FHoriz then
    ATagList.AddTag(MUIA_Scale_Horiz, AsTag(FHoriz));
end;

procedure TMUIScale.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Scale, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIScale.SetHoriz(AValue: Boolean);
begin
  if AValue <> FHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Scale_Horiz', BoolToStr(AValue, True))
    else
      FHoriz := AValue;
  end;
end;

{ TMUIColorfield }

constructor TMUIColorfield.Create;
begin
  inherited;
  FBlue := 0;
  FBlueSet := False;
  FRed := 0;
  FRedSet := False;
  FGreen := 0;
  FGreenSet := False;
  FPen := LongWord(-1);
end;

procedure TMUIColorfield.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FBlueSet then
    ATagList.AddTag(MUIA_Colorfield_Blue, AsTag(FBlue));
  if FRedSet then
    ATagList.AddTag(MUIA_Colorfield_Red, AsTag(FRed));
  if FGreenSet then
    ATagList.AddTag(MUIA_Colorfield_Green, AsTag(FGreen));
  if FPen <> LongWord(-1) then
    ATagList.AddTag(MUIA_Colorfield_Pen, AsTag(FPen));
end;

procedure TMUIColorfield.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Colorfield, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIColorfield.SetBlue(AValue: LongWord);
begin
  if AValue <> FBlue then
  begin
    FBlue := AValue;
    FBlueSet := True;
    if HasObj then
      SetValue(MUIA_Colorfield_Blue, AValue);
  end;
end;

procedure TMUIColorfield.SetRed(AValue: LongWord);
begin
  if AValue <> FRed then
  begin
    FRed := AValue;
    FRedSet := True;
    if HasObj then
      SetValue(MUIA_Colorfield_Red, AValue);
  end;
end;

procedure TMUIColorfield.SetGreen(AValue: LongWord);
begin
  if AValue <> FGreen then
  begin
    FGreen := AValue;
    FGreenSet := True;
    if HasObj then
      SetValue(MUIA_Colorfield_Green, AValue);
  end;
end;

function TMUIColorfield.GetPen: LongWord;
begin
  Result := 0;
  if HasObj then
    Result := LongWord(GetIntValue(MUIA_ColorField_Pen));
end;

procedure TMUIColorfield.SetPen(AValue: LongWord);
begin
  if AValue <> FPen then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Colorfield_Pen', IntToHex(AValue, 8))
    else
      FPen := AValue;
  end;
end;

function TMUIColorfield.GetRGB: TMUIRGB;
begin
  Result.Red := Red;
  Result.Green := Green;
  Result.Blue := Blue;
end;


procedure TMUIColorfield.SetRGB(AValue: TMUIRGB);
begin
  Red := AValue.Red;
  Green := AValue.Green;
  Blue := AVAlue.Blue;
end;

{ TMUIText }

constructor TMUIText.Create;
begin
  inherited;
  FContents := '';
  FHiChar := #0;
  FPreParse := '';
  FSetMax := False;
  FSetMax := False;
  FSetVMax := True;
  Frame := MUIV_Frame_Text;
end;

constructor TMUIText.Create(AContents: string);
begin
  inherited Create;
  FContents := AContents;
  FHiChar := #0;
  FPreParse := '';
  FSetMax := False;
  FSetMax := False;
  FSetVMax := True;
  Frame := MUIV_Frame_Text;
end;

procedure TMUIText.GetCreateTags(var ATagList: TATagList);
begin
  inherited;

  ATagList.AddTag(MUIA_Text_Contents, AsTag(PChar(FContents)));
  if FHiChar <> #0 then
    ATagList.AddTag(MUIA_Text_HiChar, AsTag(PtrUInt(FHiChar)));
  if FPreParse <>  '' then
    ATagList.AddTag(MUIA_Text_PreParse, AsTag(PChar(FPreParse)));
  if FSetMax then
    ATagList.AddTag(MUIA_Text_SetMax, AsTag(FSetMax));
  if FSetMin then
    ATagList.AddTag(MUIA_Text_SetMin, AsTag(FSetMin));
  if not FSetVMax then
    ATagList.AddTag(MUIA_Text_SetMax, AsTag(FSetVMax));
end;


procedure TMUIText.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Text, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIText.SetContents(AContents: string);
begin
  if AContents <> FContents then
  begin
    FContents := AContents;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Text_Contents, PChar(FContents));
  end;
end;

procedure TMUIText.SetHiChar(AHiChar: char);
begin
  if AHiChar <> FHiChar then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Text_HiChar', '' + AHiChar)
    else
      FHiChar := AHiChar;
  end;
end;

procedure TMUIText.SetPreParse(AValue: string);
begin
  if FPreParse <> AValue then
  begin
    FPreParse := AValue;
    if Assigned(FMUIObj) then
      SetValue(MUIA_Text_PreParse, PChar(FPreParse));
  end;
end;

procedure TMUIText.SetSetMax(AValue: Boolean);
begin
  if FSetMax <> AValue then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Text_SetMax', BoolToStr(AValue, True))
    else
      FSetMax := AValue;
  end;
end;

procedure TMUIText.SetSetMin(AValue: Boolean);
begin
  if FSetMin <> AValue then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Text_SetMin', BoolToStr(AValue, True))
    else
      FSetMin := AValue;
  end;
end;

procedure TMUIText.SetSetVMax(AValue: Boolean);
begin
  if FSetVMax <> AValue then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Text_SetVMax', BoolToStr(AValue, True))
    else
      FSetVMax := AValue;
  end;
end;

{ TMUIPenDisplay }

constructor TMUIPenDisplay.Create;
begin
  inherited;
  FSpec := nil;
end;

procedure TMUIPenDisplay.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if Assigned(FSpec) then
    ATagList.AddTag(MUIA_PenDisplay_Spec, AsTag(FSpec));
end;

procedure TMUIPenDisplay.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_PenDisplay, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

function SpecFunc(Hook: PHook; Obj: PObject_; Msg: Pointer): PtrInt;
var
  PasObj: TMUIPenDisplay;
begin
  Result := 0;
  PasObj := TMUIPenDisplay(Hook^.h_Data);
  if Assigned(PasObj.FOnSpecChange) then
    PasObj.FOnSpecChange(PasObj);
end;

procedure TMUIPenDisplay.AfterCreateObject;
begin
  inherited;
  // Connect Events
  ConnectHook(MUIA_PenDisplay_Spec, MUIV_EveryTime, @SpecFunc);
end;

function TMUIPenDisplay.GetPen: LongWord;
begin
  Result := 0;
  if HasObj then
    Result := GetIntValue(MUIA_PenDisplay_Pen);
end;

function TMUIPenDisplay.GetSpec: PMUI_PenSpec;
begin
  Result := nil;
  if HasObj then
    Result := GetPointerValue(MUIA_Pendisplay_Spec);
end;

procedure TMUIPenDisplay.SetSpec(AValue: PMUI_PenSpec);
begin
  if AValue <> FSpec then
  begin
    FSpec := AValue;
    if HasObj then
      SetValue(MUIA_Pendisplay_Spec, AsTag(FSpec));
  end;
end;

procedure TMUIPenDisplay.SetColormap(ColorMap: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Pendisplay_SetColormap, Colormap]);
end;

procedure TMUIPenDisplay.SetMUIPen(MUIPen: Integer);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Pendisplay_SetMUIPen, MUIPen]);
end;

procedure TMUIPenDisplay.SetRGB(Red, Green, Blue: LongWord);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Pendisplay_SetRGB, Red, Green, Blue]);
end;

procedure TMUIPenDisplay.SetRGB8(Red, Green, Blue: Byte);
begin
  if HasObj then
    DoMethod(MUIObj, [MUIM_Pendisplay_SetRGB, ColComToMUI(Red), ColComToMUI(Green), ColComToMUI(Blue)]);
end;

{ TMUIPopPen }

procedure TMUIPopPen.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_PopPen, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

{ TMUIButton }

constructor TMUIButton.Create;
begin
  inherited Create;
  Frame := MUIV_Frame_Button;
  Font := MUIV_Font_Button;
  PreParse := MUIX_C;
  InputMode := MUIV_InputMode_RelVerify;
  Background.Spec := MUII_ButtonBack;
  CycleChain := 1;
end;

constructor TMUIButton.Create(ATitle: string);
begin
  inherited Create;
  Frame := MUIV_Frame_Button;
  Font := MUIV_Font_Button;
  HiChar := '_';
  PreParse := MUIX_C;
  InputMode := MUIV_InputMode_RelVerify;
  Background.Spec := MUII_ButtonBack;
  CycleChain := 1;
  Contents := ATitle;
end;

{ TMUIHSpace }

constructor TMUIHSpace.Create;
begin
  inherited Create;
  VertWeight := 0;
end;

constructor TMUIHSpace.Create(Space: Integer);
begin
  inherited Create;
  VertWeight := 0;
  FixWidth := Space;
end;

{ TMUIVSpace }

constructor TMUIVSpace.Create;
begin
  inherited Create;
  HorizWeight := 0;
end;

constructor TMUIVSpace.Create(Space: Integer);
begin
  inherited Create;
  HorizWeight := 0;
  FixHeight := Space;
end;

{ TMUIHBar }

constructor TMUIHBar.Create;
begin
  inherited Create;
  HBar := True;
end;

constructor TMUIHBar.Create(Space: Integer);
begin
  inherited Create;
  HBar := True;
  FixHeight := Space;
end;

{ TMUIVBar }

constructor TMUIVBar.Create;
begin
  inherited Create;
  VBar := True;
end;

constructor TMUIVBar.Create(Space: Integer);
begin
  inherited Create;
  VBar := True;
  FixWidth := Space;
end;

{ TMUIBarTitle }

constructor TMUIBarTitle.Create;
begin
  inherited Create;

end;

constructor TMUIBarTitle.Create(ALabel: string);
begin
  inherited Create;
  HBar := True;
  BarTitle := ALabel;
  FixHeight := 1;
end;


{ TSpecDesc }

constructor TSpecDesc.Create;
begin
  FIsSet := False;
end;

procedure TSpecDesc.SetStdPattern(APattern: Integer);        //0: MUII_
begin
  FSpec := APattern;
  FIsSet := True;
end;

procedure TSpecDesc.SetRGB(R,G,B: Byte); overload;           //2: r,g,b
begin
  SetRGB32(R shl 24 or R shl 16 or R shl 8 or R, G shl 24 or G shl 16 or G shl 8 or G, B shl 24 or B shl 16 or B shl 8 or B);
end;

procedure TSpecDesc.SetRGB32(R,G,B: LongWord); overload;     //2: r,g,b
begin
  FText := '2:' + IntToHex(R, 8) +', ' + IntToHex(G, 8) +', ' + IntToHex(B, 8);
  FSpec := PtrInt(@FText[1]);
  FIsSet := True;
end;

procedure TSpecDesc.SetBoopsiName(AName: string);            //3:
begin
  FText := '3:' + AName;
  FSpec := PtrInt(@FText[1]);
  FIsSet := True;
end;

procedure TSpecDesc.SetMUIBrush(AName: string);              //4:
begin
  FText := '4:' + AName;
  FSpec := PtrInt(@FText[1]);
  FIsSet := True;
end;

procedure TSpecDesc.SetPicture(PicPath: string);             //5:
begin
  FText := '5:' + PicPath;
  FSpec := PtrInt(@FText[1]);
  FIsSet := True;
end;

procedure TSpecDesc.SetPreConf(ABack: Integer);          //6:
begin
  FText := '6:' + IntToStr(ABack);
  FSpec := PtrInt(@FText[1]);
  FIsSet := True;
end;

procedure TSpecDesc.SetString(AText: string);
begin
  FText := AText;
  FSpec := PtrInt(@FText[1]);
  FIsSet := True;
end;

procedure TSpecDesc.SetFSpec(ASpec: PtrInt);
begin
  FSpec := ASpec;
  FIsSet := True;
end;

end.
