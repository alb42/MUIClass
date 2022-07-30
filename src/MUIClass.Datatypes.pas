unit MUIClass.Datatypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Datatypes, AGraphics, Utility, Intuition, AmigaDos, Exec, Math,
  layers;

type

  { TBaseDataType }
  {$M+}

  TBaseDataType = class
  protected
    FDTObj: Pointer;
    function SetAttribute(Tag: LongWord; AValue: PtrUInt): PtrUInt;
    function GetAttribute(Tag: LongWord): PtrUInt;
    procedure ClearDTObject; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function LoadFile(AFilename: string): Boolean; virtual; abstract;
    function SaveFile(AFilename: string): Boolean; virtual; abstract;

    function HasDTObject: Boolean; virtual;
    property DTObj: Pointer read FDTObj;
  end;

  { TPictureDataType }


  TPictureDataType = class(TBaseDataType)
  private
    FBitmapHeader: PBitMapHeader;
    FBitmap: PBitMap;
    FDrawHandle: Pointer;

    FFilename: string;
    FRemap: Boolean;
    FPrecision: Integer;
    FScreen: PScreen;
    function GetPrecision: LongWord;
    function GetSize: TPoint;
    procedure SetPrecision(AValue: LongWord);
  protected
    procedure ClearDTObject; override;
  public
    constructor Create; override;

    function LoadFile(AFilename: string): Boolean; override;
    function SaveFile(AFilename: string): Boolean; override;

    function DrawToRastport(DestRP: PRastPort; x, y: LongInt; AWidth: LongInt = -1; AHeight: LongInt = -1): Boolean;

    property Filename: string read FFilename;
    property ImageSize: TPoint read GetSize;
    property Bitmap: PBitmap read FBitmap;
    property DrawHandle: Pointer read FDrawHandle;
    property Screen: PScreen read FScreen write FScreen;
  published
    property Remap: Boolean read FRemap write FRemap default True;
    property Precision: LongWord read GetPrecision write SetPrecision default Precision_Image;
  end;

implementation

{ TBaseDataType }

function TBaseDataType.SetAttribute(Tag: LongWord; AValue: PtrUInt): PtrUInt;
begin
  Result := SetDTAttrs(FDTObj, nil ,nil ,[Tag, AValue, TAG_END]);
end;

function TBaseDataType.GetAttribute(Tag: LongWord): PtrUInt;
begin
  GetDTAttrs(FDTObj, [Tag, AsTag(@Result), TAG_END]);
end;

procedure TBaseDataType.ClearDTObject;
begin
  if Assigned(FDTObj) then
    DisposeDTObject(FDTObj);
  FDTObj := nil;
end;

constructor TBaseDataType.Create;
begin
  FDTObj := nil;
end;

destructor TBaseDataType.Destroy;
begin
  ClearDTObject;
  inherited Destroy;
end;

function TBaseDataType.HasDTObject: Boolean;
begin
  Result := Assigned(FDTObj);
end;

{ TPictureDataType }

function TPictureDataType.GetPrecision: LongWord;
begin
  Result := FPrecision;
end;

function TPictureDataType.GetSize: TPoint;
begin
  Result.X := 0;
  Result.Y := 0;
  if Assigned(FBitmapHeader) then
  begin
    Result.X := FBitmapHeader^.bmh_Width;
    Result.Y := FBitmapHeader^.bmh_Height;
  end;
end;

procedure TPictureDataType.SetPrecision(AValue: LongWord);
begin
  FPrecision := AValue;
  if HasDTObject then
    SetAttribute(OBP_Precision, AsTag(Precision_Image));
end;

procedure TPictureDataType.ClearDTObject;
begin
  if Assigned(FDTObj) and Assigned(FDrawHandle) then
    ReleaseDTDrawInfo(FDTObj, FDrawHandle);
  FDrawHandle := nil;
  inherited ClearDTObject;
  FBitmapHeader := nil;
  FBitmap := nil;
end;

constructor TPictureDataType.Create;
begin
  inherited Create;
  FRemap := True;
  FPrecision := Precision_Image;
  FBitmapHeader := nil;
  FBitmap := nil;
  FDrawHandle := nil;
end;

function TPictureDataType.LoadFile(AFilename: string): Boolean;
var
  FScr: PScreen;
begin
  Result := False;
  ClearDTObject;
  //
  if Assigned(FScreen) then
    FScr := FScreen
  else
    FScr := IntuitionBase^.ActiveScreen;
  FFilename := AFilename;
  //
  FDTObj := NewDTObject(PChar(FFileName), [
    DTA_GroupID, GID_PICTURE,
    PDTA_Remap, AsTag(FRemap),
    PDTA_DestMode, PMODE_V43,
    PDTA_Screen, AsTag(FScr),
    OBP_Precision, Precision_Image,
    TAG_END, TAG_END]);
  if not Assigned(FDTObj) then
    Exit;
  DoMethod(FDTObj, [DTM_PROCLAYOUT, 0 , 1]);

  GetDTAttrs(FDTObj, [
    PDTA_DestBitMap, AsTag(@FBitmap),
    PDTA_BitMapHeader,AsTag(@FBitmapHeader),
  TAG_END]);
  if not Assigned(FBitmap) and not Assigned(FBitmapHeader) then
    Exit;

  FDrawHandle := ObtainDTDrawInfoA(FDTObj, nil);
  if not Assigned(DrawHandle) then
    Exit;
  Result := True;
end;

function TPictureDataType.SaveFile(AFilename: string): Boolean;
var
  Msg: TdtWrite;
  FName: string;
  Fh: BPTR;
begin
  Result := False;
  Msg.MethodID := DTM_WRITE;
  Msg.dtw_GInfo := nil;
  FName := AFilename;
  fh := DosOpen(PChar(fName), MODE_NEWFILE);
  Msg.dtw_FileHandle := fh;
  Msg.dtw_Mode := DTWM_IFF;
  Msg.dtw_AttrList := nil;
  //
  if fh = BPTR(0) then
    Exit;
  //
  DoDTMethodA(self.FDTObj, nil ,nil, @Msg);
  DOSClose(fh);
  Result := True;
end;

function TPictureDataType.DrawToRastport(DestRP: PRastPort; x, y: LongInt; AWidth: LongInt = -1; AHeight: LongInt = -1): Boolean;
begin
  Result := False;
  if Assigned(FDTObj) then
  begin
    if AWidth < 0 then
      AWidth := ImageSize.x;
    if AHeight < 0 then
      AHeight := ImageSize.y;
    Result := Boolean(DrawDTObjectA(DestRP, FDTObj, X, Y, AWidth, AHeight, 0, 0, nil));
  end;
end;




end.

