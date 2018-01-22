unit MUIClass.Image;

{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, agraphics, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area;

type
  TMUIImage = class(TMUIArea)
  private
    FFontMatch: Boolean;
    FFontMatchHeight: Boolean;
    FFontMatchWidth: Boolean;
    FFreeHoriz: Boolean;
    FFreeVert: Boolean;
    FOldImage: PImage;
    FSpec: TSpecDesc;
    FState: Integer;
    procedure SetFontMatch(AValue: Boolean);
    procedure SetFontMatchHeight(AValue: Boolean);
    procedure SetFontMatchWidth(AValue: Boolean);
    procedure SetFreeHoriz(AValue: Boolean);
    procedure SetFreeVert(AValue: Boolean);
    procedure SetOldImage(AValue: PImage);
    procedure SetState(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure CreateObject; override;
    property FontMatch: Boolean read FFontMatch write SetFontMatch;
    property FontMatchHeight: Boolean read FFontMatchHeight write SetFontMatchHeight;
    property FontMatchWidth: Boolean read FFontMatchWidth write SetFontMatchWidth;
    property FreeHoriz: Boolean read FFreeHoriz write SetFreeHoriz;
    property FreeVert: Boolean read FFreeVert write SetFreeVert;
    property OldImage: PImage read FOldImage write SetOldImage;
    property Spec: TSpecDesc read FSpec;
    property State: Integer read FState write SetState;
  end;


  TMUIBitmap = class(TMUIArea)
  private
    FUseFriend: Boolean;
    FBitmap: PBitmap;
    FHeight: Integer;
    FWidth: Integer;
    FMappingTable: PByte;
    FPrecision: Integer;
    FSourceColors: PLongWord;
    FTransparent: Integer;
    procedure SetUseFriend(AValue: Boolean);
    procedure SetBitmap(AValue: PBitmap);
    procedure SetHeight(AValue: Integer);
    procedure SetWidth(AValue: Integer);
    procedure SetMappingTable(AValue: PByte);
    procedure SetPrecision(AValue: Integer);
    function GetRemappedBitmap: PBitmap;
    procedure SetSourceColors(AValue: PLongWord);
    procedure SetTransparent(AValue: Integer);
  protected
    procedure GetCreateTags(var ATagList: TATagList); override;
  public
    constructor Create; override;
    procedure CreateObject; override;

    property Bitmap: PBitmap read FBitmap write SetBitmap;
    property UseFriend: Boolean read FUseFriend write SetUseFriend;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property MappingTable: PByte read FMappingTable write SetMappingTable;
    property Precision: Integer read FPrecision write SetPrecision;
    property RemappedBitmap: PBitmap read GetRemappedBitmap;
    property SourceColors: PLongWord read FSourceColors write SetSourceColors;
    property Transparent: Integer read FTransparent write SetTransparent;
  end;

implementation

{ TMUIImage }

constructor TMUIImage.Create;
begin
  inherited;
  FSpec := TSpecDesc.Create;
  FFontMatch := False;
  FFontMatchHeight := False;
  FFontMatchWidth := False;
  FFreeHoriz := False;
  FFreeVert := False;
  FOldImage := nil;
end;

destructor TMUIImage.Destroy;
begin
  FSpec.Free;
  inherited;
end;

procedure TMUIImage.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FFontMatch then
    ATagList.AddTag(MUIA_Image_FontMatch, AsTag(FFontMatch));
  if FFontMatchHeight then
    ATagList.AddTag(MUIA_Image_FontMatchHeight, AsTag(FFontMatchHeight));
  if FFontMatchWidth then
    ATagList.AddTag(MUIA_Image_FontMatchWidth, AsTag(FFontMatchWidth));
  if FFreeHoriz then
    ATagList.AddTag(MUIA_Image_FreeHoriz, AsTag(FFreeHoriz));
  if FFreeVert then
    ATagList.AddTag(MUIA_Image_FreeVert, AsTag(FFreeVert));
  if Assigned(FOldImage) then
    ATagList.AddTag(MUIA_Image_OldImage, AsTag(FOldImage));
  if FSpec.IsSet then
    ATagList.AddTag(MUIA_Image_OldImage, AsTag(FSpec.Spec));
end;

procedure TMUIImage.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Image, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;


procedure TMUIImage.SetFontMatch(AValue: Boolean);
begin
  if AValue <> FFontMatch then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Image_FontMatch', BoolToStr(AValue, True))
    else
      FFontMatch := AValue;
  end;
end;

procedure TMUIImage.SetFontMatchHeight(AValue: Boolean);
begin
  if AValue <> FFontMatchHeight then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Image_FontMatchHeight', BoolToStr(AValue, True))
    else
      FFontMatchHeight := AValue;
  end;
end;

procedure TMUIImage.SetFontMatchWidth(AValue: Boolean);
begin
  if AValue <> FFontMatchWidth then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Image_FontMatchWidth', BoolToStr(AValue, True))
    else
      FFontMatchWidth := AValue;
  end;
end;

procedure TMUIImage.SetFreeHoriz(AValue: Boolean);
begin
  if AValue <> FFreeHoriz then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Image_FreeHoriz', BoolToStr(AValue, True))
    else
      FFreeHoriz := AValue;
  end;
end;

procedure TMUIImage.SetFreeVert(AValue: Boolean);
begin
  if AValue <> FFreeVert then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Image_FreeVert', BoolToStr(AValue, True))
    else
      FFreeVert := AValue;
  end;
end;

procedure TMUIImage.SetOldImage(AValue: PImage);
begin
  if AValue <> FOldImage then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Image_OldImage', HexStr(AValue))
    else
      FOldImage := AValue;
  end;
end;

procedure TMUIImage.SetState(AValue: Integer);
begin
  if AValue <> FState then
  begin
    FState := AValue;
    if Assigned(FMUIObj) then
       SetValue(MUIA_Image_State, FState);
  end;
end;

{ TMUIBitmap }

constructor TMUIBitmap.Create;
begin
  inherited;
  FUseFriend := False;
  FBitmap := nil;
  FWidth := 0;
  FHeight := 0;
  FMappingTable := nil;
  FPrecision := -2; // not defined -> use default
  FSourceColors := nil;
  Transparent := -1;
end;

procedure TMUIBitmap.GetCreateTags(var ATagList: TATagList);
begin
  inherited;
  if FUseFriend then
    ATagList.AddTag(MUIA_Bitmap_UseFriend, AsTag(FUseFriend));
  if Assigned(FBitmap) then
    ATagList.AddTag(MUIA_Bitmap_Bitmap, AsTag(FBitmap));
  if FHeight > 0 then
    ATagList.AddTag(MUIA_Bitmap_Height, AsTag(FHeight));
  if FWidth > 0 then
    ATagList.AddTag(MUIA_Bitmap_Width, AsTag(FWidth));
  if Assigned(FMappingTable) then
    ATagList.AddTag(MUIA_Bitmap_MappingTable, AsTag(FMappingTable));
  if Precision <> -2 then
    ATagList.AddTag(MUIA_Bitmap_Precision, AsTag(FPrecision));
  if Assigned(FSourceColors) then
    ATagList.AddTag(MUIA_Bitmap_SourceColors, AsTag(FSourceColors));
  if FTransparent >= 0 then
    ATagList.AddTag(MUIA_Bitmap_Transparent, AsTag(FTransparent));
end;

procedure TMUIBitmap.CreateObject;
var
  TagList: TATagList;
begin
  if not Assigned(FMUIObj) then
  begin
    BeforeCreateObject;
    GetCreateTags(TagList);
    FMUIObj := MUI_NewObjectA(MUIC_Bitmap, TagList.GetTagPointer);
    AfterCreateObject
  end;
end;

procedure TMUIBitmap.SetUseFriend(AValue: Boolean);
begin
  if AValue <> FUseFriend then
  begin
    if Assigned(FMUIObj) then
      ComplainIOnly(Self, 'MUIA_Bitmap_UseFriend', BoolToStr(AValue, True))
    else
      FUseFriend := AValue;
  end;
end;

procedure TMUIBitmap.SetBitmap(AValue: PBitmap);
begin
  if AValue <> FBitmap then
  begin
    FBitmap := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_Bitmap, AsTag(FBitmap));
  end;
end;

procedure TMUIBitmap.SetHeight(AValue: Integer);
begin
  if AValue <> FHeight then
  begin
    FHeight := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_Height, AsTag(FHeight));
  end;
end;

procedure TMUIBitmap.SetWidth(AValue: Integer);
begin
  if AValue <> FWidth then
  begin
    FWidth := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_Width, AsTag(FWidth));
  end;
end;

procedure TMUIBitmap.SetMappingTable(AValue: PByte);
begin
  if AValue <> FMappingTable then
  begin
    FMappingTable := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_MappingTable, AsTag(FMappingTable));
  end;
end;

procedure TMUIBitmap.SetPrecision(AValue: Integer);
begin
  if AValue <> FPrecision then
  begin
    FPrecision := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_Precision, AsTag(FPrecision));
  end;
end;

function TMUIBitmap.GetRemappedBitmap: PBitmap;
begin
  Result := nil;
  if HasObj then
    Result := GetPointerValue(MUIA_Bitmap_RemappedBitmap);
end;

procedure TMUIBitmap.SetSourceColors(AValue: PLongWord);
begin
  if AValue <> FSourceColors then
  begin
    FSourceColors := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_SourceColors, AsTag(FSourceColors));
  end;
end;

procedure TMUIBitmap.SetTransparent(AValue: Integer);
begin
  if AValue <> FTransparent then
  begin
    FTransparent := AValue;
    if HasObj then
      SetValue(MUIA_Bitmap_Transparent, AsTag(FTransparent));
  end;
end;

end.
