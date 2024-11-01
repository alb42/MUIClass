unit MUIClass.Dialog;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils,
  Exec, Utility, {$ifdef AmigaOS4}AmigaDOS,{$endif} Intuition, asl, icon, mui, muihelper,
  MUIClass.Base, MUIClass.Window;
{$M+}
type
  TBaseDialog = class
  private
    FTitleText: string;
  public
    function Execute: Boolean; virtual; abstract;
    property TitleText: string read FTitleText write FTitleText;
  end;

  { TFileDialog }

  TFileDialog = class(TBaseDialog)
  private
    FSaveMode: Boolean;
    FMultiSelect: Boolean;
    FFileName: string;
    FDirectory: string;
    FPattern: string;
    FDrawersOnly: Boolean;
    FFilenames: TStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property SaveMode: Boolean read FSaveMode write FSaveMode;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect;
    property Pattern: string read FPattern write FPattern;
    property Directory: string read FDirectory write FDirectory; // Initial and result Directory
    property FileName: string read FFileName write FFileName; // inital and result filename
    property FileNames: TStringList read FFileNames;
    property DrawersOnly: Boolean read FDrawersOnly write FDrawersOnly;
  end;

  TFontDialogOptions = set of (foFrontPen, foBackPen, foStyle, foDrawMode, foFixedOnly);

  TFontDialog = class(TBaseDialog)
  private
    FName: string;
    FSize: Integer;
    FStyle: Integer;
    FFlags: Integer;
    FFrontPen: Integer;
    FBackPen: Integer;
    FDrawMode: Integer;
    FOptions: TFontDialogOptions;
  public
    constructor Create; virtual;
    function Execute: Boolean; override;
    property Name: string read FName write FName;
    property Size: Integer read FSize write FSize;
    property Style: Integer read FStyle write FStyle;
    property Flags: Integer read FFlags write FFlags;
    property FrontPen: Integer read FFrontPen write FFrontPen;
    property BackPen: Integer read FBackPen write FBackPen;
    property DrawMode: Integer read FDrawMode write FDrawMode;
    property Options: TFontDialogOptions read FOptions write FOptions;
  end;

  TScreenDialogOptions = set of (soWidth, soHeight, soDepth, soOverscanType, soAutoScroll);

  TScreenDialog = class(TBaseDialog)
  private
    FDisplayID: Integer;
    FDisplayWidth: Integer;
    FDisplayHeight: Integer;
    FDisplayDepth: Integer;
    FOverScanType: Integer;
    FOptions: TScreenDialogOptions;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FMinHeight: Integer;
    FMaxHeight: Integer;
    FMinDepth: Integer;
    FMaxDepth: Integer;
    FAutoScroll: Boolean;
  public
    constructor Create; virtual;
    function Execute: Boolean; override;
    property DisplayID: Integer read FDisplayID write FDisplayID;
    property DisplayWidth: Integer read FDisplayWidth write FDisplayWidth;
    property DisplayHeight: Integer read FDisplayHeight write FDisplayHeight;
    property DisplayDepth: Integer read FDisplayDepth write FDisplayDepth;
    property OverScanType: Integer read FOverScanType write FOverScanType;
    property Options: TScreenDialogOptions read FOptions write FOptions;
    property MinWidth: Integer read FMinWidth write FMinWidth;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MaxHeight: Integer read FMaxHeight write FMaxHeight;
    property MinDepth: Integer read FMinDepth write FMinDepth;
    property MaxDepth: Integer read FMaxDepth write FMaxDepth;
    property AutoScroll: Boolean read FAutoScroll write FAutoScroll;
  end;

procedure ShowMessage(Text: string; HeadText: string = 'Message'; OKText: string = '_OK');
function MessageBox(HeadText, Text: string; Buttons: TStringArray): Integer;

implementation

procedure ShowMessage(Text: string; HeadText: string = 'Message'; OKText: string = '_OK');
begin
  if Assigned(MUIApp.MainWindow) then
    MUI_RequestA(MuiApp.MUIObj, MuiApp.MainWindow.MUIObj, 0, PChar(HeadText), PChar(OKText), PChar(Text), nil)
  else
    MUI_RequestA(nil, nil, 0, PChar(HeadText), PChar(OKText), PChar(Text), nil);
end;

function MessageBox(HeadText, Text: string; Buttons: TStringArray): Integer;
var
  BtnText: string;
  i: Integer;
begin
  BtnText := 'OK';
  for i := 0 to High(Buttons) do
  begin
    if i = 0 then
      BtnText := Buttons[i]
    else
      BtnText := BtnText + '|' + Buttons[i];
  end;
  if Assigned(MUIApp.MainWindow) then
    Result := MUI_RequestA(MuiApp.MUIObj, MuiApp.MainWindow.MUIObj, 0, PChar(HeadText), PChar(BtnText), PChar(Text), nil)
  else
    Result := MUI_RequestA(nil, nil, 0, PChar(HeadText), PChar(BtnText), PChar(Text), nil);
end;

{ TFileDialog }

constructor TFileDialog.Create;
begin
  FFileNames := TStringList.Create;
  FFilename := '';
  FDirectory := '';
  FPattern := '';
  FMultiSelect := False;
  FDrawersOnly := False;
  FSaveMode := False;
end;

destructor TFileDialog.Destroy;
begin
  FileNames.Free;
  inherited Destroy;
end;

function IntuiFunc(Hook: PHook; Obj: PObject_; Data: Pointer): PtrInt;
var
  IMsg: PIntuiMessage;
begin
  Unused(Hook);
  Unused(Obj);
  try
    Result := 0;
    IMsg := Data;
    if Imsg^.iClass = IDCMP_REFRESHWINDOW then
      MUIApp.CheckRefresh();
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

type
  //PWBArg = ^TWBArg;
  TWBArg = record
    wa_Lock: BPTR;         { a lock descriptor }
    wa_Name: STRPTR;       { a string relative to that lock }
  end;

  WBArgList = Array [1..100] of TWBArg; { Only 1..smNumArgs are valid }
  PWBArgList = ^WBArgList;

function TFileDialog.Execute: Boolean;
var
  TagList: TATagList;
  FR: PFileRequester;
  IntuiHook: THook;
  FName: string;
  i: Integer;
begin
  Result := False;
  FFileNames.Clear;
  //
  MH_SetHook({%H-}IntuiHook, @IntuiFunc, Self);
  TagList.AddTags([
    ASLFR_Window,  AsTag(TMUIWindow(MUIApp.MainWindow).Window),
    ASLFR_UserData, AsTag(MUIApp.MUIObj),
    ASLFR_IntuiMsgFunc, AsTag(@IntuiHook)
    ]);
  FR := AllocAslRequest(ASL_FileRequest, TagList.GetTagPointer);
  try
    //
    TagList.Clear;
    TagList.AddTags([
      ASLFR_DrawersOnly, AsTag(FDrawersOnly),
      ASLFR_DoSaveMode, AsTag(FSaveMode),
      ASLFR_TitleText, AsTag(PChar(FTitleText))
      ]);
    if (FDirectory = '') and (FFilename <> '') then
      FDirectory := ExtractFilePath(FFileName);
    FName := ExtractFileName(FFilename);
    if FName <> '' then
      TagList.AddTag(ASLFR_InitialFile, AsTag(PChar(FName)));
    if FDirectory = '' then
      FDirectory := SysUtils.GetCurrentDir;
    TagList.AddTag(ASLFR_InitialDrawer, AsTag(PChar(FDirectory)));
    if FPattern <> '' then
    begin
      TagList.AddTag(ASLFR_DoPatterns, AsTag(True));
      TagList.AddTag(ASLFR_InitialPattern, AsTag(PChar(FPattern)));
    end;
    if TitleText <> '' then
      TagList.AddTag(ASLFR_TitleText, AsTag(PChar(TitleText)));

    if not FSaveMode then
      TagList.AddTag(ASLFR_DoMultiSelect, AsTag(FMultiSelect));
    Result :=  AslRequest(FR, TagList.GetTagPointer);
    if Result then
    begin
      FFilename :=  IncludeTrailingPathDelimiter(string(FR^.fr_Drawer)) + string(FR^.fr_File);
      FDirectory := string(FR^.fr_Drawer);
      if FMultiSelect then
      begin
        for i := 1 to  FR^.fr_NumArgs do
          FileNames.Add(IncludeTrailingPathDelimiter(string(FR^.fr_Drawer)) + string(PWBArgList(FR^.fr_ArgList)^[i].wa_Name));
      end
      else
        FFilenames.Add(FFilename);
    end;
  finally
    FreeAslRequest(FR);
  end;
end;

{ TFontDialog }

constructor TFontDialog.Create;
begin
  FName := '';
  FSize := 0;
  FStyle := 0;
  FFlags := 0;
  FFrontPen := -1;
  FBackPen := -1;
  FDrawMode := -1;
  FOptions := [];
end;

function TFontDialog.Execute: Boolean;
var
  TagList: TATagList;
  FR: PFontRequester;
  IntuiHook: THook;
begin
  Result := False;
  //
  MH_SetHook({%H-}IntuiHook, @IntuiFunc, Self);
  TagList.AddTags([
    ASLFO_Window,  AsTag(TMUIWindow(MUIApp.MainWindow).Window),
    ASLFO_UserData, AsTag(MUIApp.MUIObj),
    ASLFO_IntuiMsgFunc, AsTag(@IntuiHook),
    ASLFO_TitleText, AsTag(PChar(FTitleText))
    ]);
  FR := MUI_AllocAslRequest(ASL_FontRequest, TagList.GetTagPointer);
  try
    //
    TagList.Clear;
    TagList.AddTags([
      ASLFO_TitleText, AsTag(PChar(FTitleText))
      ]);
    if FName <> '' then
      TagList.AddTag(ASLFO_InitialName, AsTag(PChar(FName)));
    if FSize <> 0 then
      TagList.AddTag(ASLFO_InitialSize, AsTag(FSize));
    if FStyle <> 0 then
      TagList.AddTag(ASLFO_InitialStyle, AsTag(FStyle));
    if FFlags <> 0 then
      TagList.AddTag(ASLFO_InitialFlags, AsTag(FFlags));
    if foFrontPen in FOptions then
      TagList.AddTag(ASLFO_DoFrontPen, AsTag(True));
    if foBackPen in FOptions then
      TagList.AddTag(ASLFO_DoBackPen, AsTag(True));
    if foStyle in FOptions then
      TagList.AddTag(ASLFO_DoStyle, AsTag(True));
    if foDrawMode in FOptions then
      TagList.AddTag(ASLFO_DoDrawMode, AsTag(True));
    if foFixedOnly in FOptions then
      TagList.AddTag(ASLFO_FixedWidthOnly, AsTag(True));
    if FFrontPen >= 0 then
      TagList.AddTag(ASLFO_InitialFrontPen, FFrontPen);
    if FBackPen >= 0 then
      TagList.AddTag(ASLFO_InitialBackPen, FBackPen);
    if FDrawMode >= 0 then
      TagList.AddTag(ASLFO_InitialDrawMode, FDrawMode);
    //
    Result :=  MUI_AslRequest(FR, TagList.GetTagPointer);
    if Result then
    begin
      FName := FR^.fo_Attr.ta_Name;
      FSize := FR^.fo_Attr.ta_YSize;
      FStyle := FR^.fo_Attr.ta_Style;
      FFlags := FR^.fo_Attr.ta_Flags;
      FFrontPen := FR^.fo_FrontPen;
      FBackPen := FR^.fo_BackPen;
      FDrawMode := FR^.fo_DrawMode;
    end;
  finally
    MUI_FreeAslRequest(FR);
  end;
end;

{ TScreenDialog }

constructor TScreenDialog.Create;
begin
  FDisplayID := -1;
  FDisplayWidth := 0;
  FDisplayHeight := 0;
  FDisplayDepth := 0;
  FOverScanType := -1;
  FOptions := [];
  FMinWidth := 0;
  FMaxWidth := 0;
  FMinHeight := 0;
  FMaxHeight := 0;
  FMinDepth := 0;
  FMaxDepth := 0;
  FAutoScroll := False;
end;

function TScreenDialog.Execute: Boolean;
var
  TagList: TATagList;
  FR: PScreenModeRequester;
  IntuiHook: THook;
begin
  Result := False;
  //
  MH_SetHook({%H-}IntuiHook, @IntuiFunc, Self);
  TagList.AddTags([
    ASLSM_Window,  AsTag(TMUIWindow(MUIApp.MainWindow).Window),
    ASLSM_UserData, AsTag(MUIApp.MUIObj),
    ASLSM_IntuiMsgFunc, AsTag(@IntuiHook),
    ASLSM_TitleText, AsTag(PChar(FTitleText))
    ]);
  FR := MUI_AllocAslRequest(ASL_ScreenModeRequest, TagList.GetTagPointer);
  try
    //
    TagList.Clear;
    TagList.AddTags([
      ASLSM_TitleText, AsTag(PChar(FTitleText))
      ]);
   if FAutoScroll then
      TagList.AddTag(ASLSM_InitialAutoScroll, AsTag(True));
   if FDisplayID >= 0 then
      TagList.AddTag(ASLSM_InitialDisplayID, AsTag(FDisplayID));
    if FDisplayWidth <> 0 then
      TagList.AddTag(ASLSM_InitialDisplayWidth, AsTag(FDisplayWidth));
    if FDisplayHeight <> 0 then
      TagList.AddTag(ASLSM_InitialDisplayHeight, AsTag(FDisplayHeight));
    if FDisplayDepth <> 0 then
      TagList.AddTag(ASLSM_InitialDisplayDepth, AsTag(FDisplayDepth));
    if FOverScanType >= 0 then
      TagList.AddTag(ASLSM_InitialOverScanType, AsTag(FOverScanType));

    if soWidth in FOptions then
      TagList.AddTag(ASLSM_DoWidth, AsTag(True));
    if soHeight in FOptions then
      TagList.AddTag(ASLSM_DoHeight, AsTag(True));
    if soDepth in FOptions then
      TagList.AddTag(ASLSM_DoDepth, AsTag(True));
    if soOverscanType in FOptions then
      TagList.AddTag(ASLSM_DoOverscanType, AsTag(True));
    if soAutoScroll in FOptions then
      TagList.AddTag(ASLSM_DoAutoScroll, AsTag(True));

    if FMinWidth <> 0 then
      TagList.AddTag(ASLSM_MinWidth, AsTag(FMinWidth));
    if FMaxWidth <> 0 then
      TagList.AddTag(ASLSM_MaxWidth, AsTag(FMaxWidth));
    if FMinHeight <> 0 then
      TagList.AddTag(ASLSM_MinHeight, AsTag(FMinHeight));
    if FMaxHeight <> 0 then
      TagList.AddTag(ASLSM_MaxHeight, AsTag(FMaxHeight));
    if FMinDepth <> 0 then
      TagList.AddTag(ASLSM_MinDepth, AsTag(FMinDepth));
    if FMaxDepth <> 0 then
      TagList.AddTag(ASLSM_MaxDepth, AsTag(FMaxDepth));
    //
    Result :=  MUI_AslRequest(FR, TagList.GetTagPointer);
    if Result then
    begin
      FDisplayID := FR^.sm_DisplayID;
      FDisplayWidth := FR^.sm_DisplayWidth;
      FDisplayHeight := FR^.sm_DisplayHeight;
      FDisplayDepth := FR^.sm_DisplayDepth;
      FOverscanType := FR^.sm_OverscanType;
      FAutoScroll := FR^.sm_AutoScroll;
    end;
  finally
    MUI_FreeAslRequest(FR);
  end;
end;

end.
