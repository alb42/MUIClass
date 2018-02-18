unit StrArraySetUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Classes, MUI,
  MUIClass.Base, MUIClass.Area, MUIClass.Window, MUIClass.Group,
  MUIClass.Gadget, MUIClass.Image, MUIClass.List, MUIClass.Numeric,
  MainWinUnit;

type
  TStrArrayWin = class(TMUIWindow)
  private
    Edit: TMUIString;
    Text: TMUIList;
    TextView: TMUIListView;
    SL: TStringList;
    procedure SetStringArray(AValue: TStringArray);
    function GetStringArray: TStringArray;
    procedure UpdateText;
    procedure AddText(Sender: TObject);
    procedure RemoveText(Sender: TObject);
    procedure ClearText(Sender: TObject);
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
  public
    Obj: TObject;
    PropName: string;
    CurProp: TItemProp;
    constructor Create; override;
    destructor Destroy; override;
    property StrArray: TStringArray read GetStringArray write SetStringArray;
  end;
var
  StrArrayWin: TStrArrayWin;

implementation


// Create Main Window
constructor TStrArrayWin.Create;
var
  Grp: TMUIGroup;
begin
  inherited;
  SL := TStringList.Create;

  Text := TMUIList.Create;

  TextView := TMUIListView.Create;
  TextView.List := Text;
  TextView.Parent := Self;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self;
  end;

  Edit := TMUIString.Create;
  with Edit do
  begin
    OnAcknowledge := @AddText;
    Parent := Grp;
  end;

  with TMUIButton.Create('Add') do
  begin
    OnClick := @AddText;
    Parent := Grp;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self;
  end;

  with TMUIButton.Create('Remove') do
  begin
    OnClick := @RemoveText;
    Parent := Grp;
  end;

  with TMUIButton.Create('Clear') do
  begin
    OnClick := @ClearText;
    Parent := Grp;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz := True;
    Parent := Self;
  end;

  with TMUIButton.Create('OK') do
  begin
    OnClick := @OkClick;
    Parent := Grp;
  end;

  with TMUIButton.Create('Cancel') do
  begin
    OnClick := @CancelClick;
    Parent := Grp;
  end;
end;

destructor TStrArrayWin.Destroy;
begin
  SL.Free;
  inherited;
end;

procedure TStrArrayWin.SetStringArray(AValue: TStringArray);
var
  i: Integer;
begin
  SL.Clear;
  for i := 0 to High(AValue) do
  begin
    SL.Add(AValue[i]);
  end;
end;

function TStrArrayWin.GetStringArray: TStringArray;
var
  i: Integer;
begin
  SetLength(Result , SL.Count);
  for i := 0 to SL.Count - 1 do
    Result[i] := SL[i];
end;

procedure TStrArrayWin.UpdateText;
var
  i: Integer;
begin
  Text.Quiet := True;
  while Text.Entries > 0 do
    Text.Remove(MUIV_List_Remove_Last);
  for i := 0 to SL.Count - 1 do
  begin
    Text.InsertSingle(PChar(SL[i]), MUIV_List_Insert_Bottom);
  end;
  Text.Quiet := False;
end;

procedure TStrArrayWin.AddText(Sender: TObject);
begin
  if Edit.Contents <> '' then
    SL.Add(Edit.Contents);
  UpdateText;
end;

procedure TStrArrayWin.RemoveText(Sender: TObject);
begin
  if (Text.Active >= 0) and (Text.Active < Sl.Count) then
    SL.Delete(Text.Active);
  UpdateText;
end;

procedure TStrArrayWin.ClearText(Sender: TObject);
begin
  SL.Clear;
  UpdateText;
end;

procedure TStrArrayWin.OkClick(Sender: TObject);
begin
  if Assigned(Obj) then
  begin
    MainWindow.DestroyTestWin;
    if (Obj is TMUICycle) and (PropName = 'Entries') then
      TMUICycle(Obj).Entries := StrArray;
    if (Obj is TMUIRegister) and (PropName = 'Titles') then
      TMUIRegister(Obj).Titles := StrArray;
    CurProp.Value := '<Array ' + IntToStr(Length(StrArray)) + ' Entries>';
    CurProp.Active := True;
    MainWindow.PropList.List.Redraw(MUIV_List_Redraw_Active);
    MainWindow.CreateTestWin;
  end;
  Close;
end;

procedure TStrArrayWin.CancelClick(Sender: TObject);
begin
  Close
end;

end.
