unit MUIClass.TextEdit;

{$mode ObjFPC}{$H+}

interface

uses
  intuition, TagsParamsHelper, mui, utility, muihelper, Exec,
  MUIClass.Base, MUIClass.Group, clipboard, iffparse;

type

  { TMUITextEdit }

  TMUITextEdit = class(TMUIGroup)
  private
    FTabsToSpaces: Boolean;
    FText: string;
    function GetTabsToSpaces: Boolean;
    function GetText: string;
    procedure SetTabsToSpaces(AValue: Boolean);
    procedure SetText(AValue: string);
  protected
    FScrollObj: PObject_;
    FTextObj: PObject_;
    function GetHasObj: Boolean;
    procedure GetCreateTags(var ATagList: TATagList); override;

  public
    constructor Create; override;
    procedure Clear;
    procedure Copy;
    procedure Cut;
    procedure Paste;
    property Text: string read GetText write SetText;
    property TabsToSpaces: Boolean read GetTabsToSpaces write SetTabsToSpaces;
  end;

implementation

const
  TextEditor_Dummy = $ad000000;
  MUIA_TextEditor_Contents = TextEditor_Dummy + $2;
  MUIA_TextEditor_CursorX = TextEditor_Dummy + $4;
  MUIA_TextEditor_CursorY = TextEditor_Dummy + $5;
  MUIA_TextEditor_ReadOnly = TextEditor_Dummy + $19;
  MUIA_TextEditor_Slider = TextEditor_Dummy + $1a;
  MUIM_TextEditor_ClearText = TextEditor_Dummy + $24;
  MUIM_TextEditor_ExportText = TextEditor_Dummy + $25;
  MUIM_TextEditor_InsertText = TextEditor_Dummy + $26;
  MUIA_TextEditor_ConvertTabs =TextEditor_Dummy + $3d;

  MUIV_TextEditor_InsertText_Cursor = 0;
  MUIV_TextEditor_InsertText_Top = 1;
  MUIV_TextEditor_InsertText_Bottom = 2;

  MUIC_TextEdit = 'TextEditor.mcc';

  // special function copied from AROS clipboard

const
  ID_FTXT = 1179932756;
  ID_CHRS = 1128813139;

// get text from Clipboard
function GetTextFromClip(ClipUnit: Byte): AnsiString;
var
  Iff: PIffHandle;
  Error: LongInt;
  Cn: PContextNode;
  Buf: PChar;
  Len: Integer;
  Cu: LongInt;
begin
  GetTextFromClip := '';
  Cu := ClipUnit;
  Iff := AllocIff;
  if Assigned(Iff) then
  begin
    Iff^.iff_Stream := NativeUInt(OpenClipboard(Cu));
    if Iff^.iff_Stream<>0 then
    begin
      InitIffAsClip(iff);
      if OpenIff(Iff, IFFF_READ) = 0 then
      begin
        if StopChunk(iff, ID_FTXT, ID_CHRS) = 0 then
        begin
          while True do
          begin
            Error := ParseIff(iff, IFFPARSE_SCAN);
            if (Error <> 0) and (Error <> IFFERR_EOC) then
              Break;
            Cn := CurrentChunk(Iff);
            if not Assigned(Cn) then
            begin
              Continue;
            end;
            Len := Cn^.cn_Size;
            if (Cn^.cn_Type = ID_FTXT) and (Cn^.cn_ID = ID_CHRS) and (Len > 0) then
            begin
              GetMem(Buf, Len + 1);
              FillChar(Buf^, Len + 1, #0);
              ReadChunkBytes(Iff, Buf, Len);
              GetTextFromClip := GetTextFromClip + AnsiString(Buf);
              FreeMem(Buf);
            end;
          end;
        end;
        CloseIff(Iff);
      end;
      CloseClipboard(PClipBoardHandle(iff^.iff_Stream));
    end;
    FreeIFF(Iff);
  end;
end;

function PutTextToClip(ClipUnit: Byte; Text: AnsiString): Boolean;
var
  Iff: PIffHandle;
  TText: AnsiString;
  Len: Integer;
begin
  PutTextToClip := False;
  Iff := AllocIff;
  if Assigned(Iff) then
  begin
    Iff^.iff_Stream := LongWord(OpenClipboard(ClipUnit));
    if Iff^.iff_Stream <> 0 then
    begin
      InitIffAsClip(iff);
      if OpenIff(Iff, IFFF_WRITE) = 0 then
      begin
        if PushChunk(iff, ID_FTXT, ID_FORM, IFFSIZE_UNKNOWN) = 0 then
        begin
          if PushChunk(iff, 0, ID_CHRS, IFFSIZE_UNKNOWN) = 0 then
          begin
            Len := Length(Text);
            TText := Text + #0;
            PutTextToClip := WriteChunkBytes(iff, @(TText[1]), Len) = len;
            PopChunk(iff);
          end;
          PopChunk(iff);
        end;
        CloseIff(iff);
      end;
      CloseClipboard(PClipBoardHandle(iff^.iff_Stream));
    end;
    FreeIFF(Iff);
  end;
end;

{ TMUITextEdit }

function TMUITextEdit.GetText: string;
var
  PC: PChar;
begin
  if Assigned(FTextObj) then
  begin
    PC := PChar(DoMEthod(FTextObj, [MUIM_TextEditor_ExportText]));
    if Assigned(PC) then
    begin
      FText := PC;
      FreeVec(PC);
    end;
  end;
  Result := FText;
end;

function TMUITextEdit.GetTabsToSpaces: Boolean;
begin
  if HasObj then
    FTabsToSpaces := Boolean(MH_Get(FTextObj, MUIA_TextEditor_ConvertTabs));
  Result := FTabsToSpaces;
end;

procedure TMUITextEdit.SetTabsToSpaces(AValue: Boolean);
begin
  FTabsToSpaces := AValue;
  if HasObj then
    MH_Set(FTextObj, MUIA_TextEditor_ConvertTabs, AsTag(AValue));
end;

procedure TMUITextEdit.SetText(AValue: string);
begin
  FText := AValue;
  if Assigned(FTextObj) then
    MH_Set(FTextObj, MUIA_TextEditor_Contents, AsTag(PChar(FText)));
end;

function TMUITextEdit.GetHasObj: Boolean;
begin
  Result := inherited and Assigned(FTextObj) and Assigned(FScrollObj);
end;

procedure TMUITextEdit.GetCreateTags(var ATagList: TATagList);
var
  Tags: TATagList;
begin
  Horiz := True;
  Tags.Clear;
  ATagList.AddTag(MUIA_Group_Spacing, 0);
  //
  Tags.AddTag(MUIA_TextEditor_ConvertTabs, AsTag(FTabsToSpaces));
  FTextObj := MUI_NewObjectA(MUIC_TextEdit, Tags.GetTagPointer);
  //
  Tags.Clear;
  FScrollObj := MUI_NewObjectA(MUIC_ScrollBar, Tags.GetTagPointer);
  if Assigned(FTextObj) then
    ATagList.AddTag(MUIA_Group_Child, AsTag(FTextObj));
  ATagList.AddTag(MUIA_Group_Child, AsTag(FScrollObj));
  inherited GetCreateTags(ATagList);
  if Assigned(FTextObj) and Assigned(FScrollObj) then
    MH_Set(FTextObj, MUIA_TextEditor_Slider, NativeUInt(FScrollObj));
end;

constructor TMUITextEdit.Create;
begin
  inherited Create;
end;

procedure TMUITextEdit.Clear;
begin
  FText := '';
  if HasObj then
    DoMethod(FTextObj, [NativeUInt(MUIM_TextEditor_ClearText)]);
end;

procedure TMUITextEdit.Copy;
begin
  PutTextToClip(0, Text);
end;

procedure TMUITextEdit.Cut;
begin
  PutTextToClip(0, Text);
  Clear;
end;

procedure TMUITextEdit.Paste;
begin
  Text := GetTextFromClip(0);
end;


end.

