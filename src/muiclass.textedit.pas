unit MUIClass.TextEdit;

{$mode ObjFPC}{$H+}

interface

uses
  intuition, TagsParamsHelper, mui, utility, muihelper, Exec,
  MUIClass.Base, MUIClass.Group;

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
  //
  Tags.AddTag(MUIA_TextEditor_ConvertTabs, AsTag(FTabsToSpaces));
  FTextObj := MUI_NewObjectA(MUIC_TextEdit, Tags.GetTagPointer);
  //
  Tags.Clear;
  FScrollObj := MUI_NewObjectA(MUIC_ScrollBar, Tags.GetTagPointer);

  ATagList.AddTag(MUIA_Group_Child, AsTag(FTextObj));
  ATagList.AddTag(MUIA_Group_Child, AsTag(FScrollObj));
  inherited GetCreateTags(ATagList);
  if HasObj then
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


end.

