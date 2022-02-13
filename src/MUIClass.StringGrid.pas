unit MUIClass.StringGrid;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, Math,
  Exec, Utility, AmigaDOS, Intuition, icon, mui, muihelper,
  tagsparamshelper, MUIClass.Base, MUIClass.Area, MUIClass.List, MUIClass.Group;
type
  {$M+}

  TGridColumn = record
    Title: string;
    Rows: array of record
      Text: string;
      Data: Pointer;
    end;
  end;
  TGridColumns = array of TGridColumn;

  TMUIStringGrid = class(TMUIListView)
  private
    FColumns: TGridColumns;
    FNumRows: Integer;
    FLines: Boolean;
    TempRows: array of string;

    function GetNumColumns: Integer;
    procedure SetNumColumms(AValue: Integer);
    procedure SetNumRows(AValue: Integer);
    function GetCells(ACol, ARow: Integer): string;
    procedure SetCells(ACol, ARow: Integer; AValue: string);
    function GetData(ACol, ARow: Integer): Pointer;
    procedure SetData(ACol, ARow: Integer; AValue: Pointer);
    procedure SetLines(AValue: Boolean);
    function GetShowTitle: Boolean;
    procedure SetShowTitle(AValue: Boolean);
    function GetTitle(ACol: Integer): string;
    procedure SetTitle(ACol: Integer; AValue: string);
    function GetRow: Integer;
    procedure SetRow(AValue: Integer);
    function GetQuiet: Boolean;
    procedure SetQuiet(AValue: Boolean);

    procedure UpdateFormat;

    procedure DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
    function ConstEvent(Sender: TObject; Pool: Pointer; Str: PChar): PChar;
    procedure DestEvent(Sender: TObject; Pool: Pointer; Entry: PChar);
  protected
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property NumColumns: Integer read GetNumColumns write SetNumColumms;
    property NumRows: Integer read FNumRows write SetNumRows;
    property ShowLines: boolean read FLines write SetLines;
    property ShowTitle: boolean read GetShowTitle write SetShowTitle;
    property Cells[ACol: Integer; ARow: Integer]: string read GetCells write SetCells;
    property Data[ACol: Integer; ARow: Integer]: Pointer read GetData write SetData;
    property Titles[ACol: Integer]: string read GetTitle write SetTitle;
    property Row: Integer read GetRow write SetRow;
    property Quiet: Boolean read GetQuiet write SetQuiet;

  end;
implementation

const
  EmptyString: PChar = ' ';

constructor TMUIStringGrid.Create;
begin
  inherited;
  FLines := False;
  List := TMUIList.Create;
  List.OnDisplay := @DisplayEvent;
  List.OnConstruct := @ConstEvent;
  List.OnDestruct := @DestEvent;
  FNumRows := 0;
end;

destructor TMUIStringGrid.Destroy;
begin
  inherited;
end;

procedure TMUIStringGrid.AfterCreateObject;
var
  i: Integer;
begin
  inherited;
  for i := 0 to FNumRows - 1 do
    List.InsertSingle(PChar(IntToStr(i)), MUIV_List_Insert_Bottom);
end;

function TMUIStringGrid.ConstEvent(Sender: TObject; Pool: Pointer; Str: PChar): PChar;
var
  s: PChar;
begin
  s := AllocPooled(Pool, 2);
  Result := s;
end;

procedure TMUIStringGrid.DestEvent(Sender: TObject; Pool: Pointer; Entry: PChar);
begin
  FreePooled(Pool, Entry, 2);
end;

procedure TMUIStringGrid.UpdateFormat;
var
  i: Integer;
  NF: string;
begin
  NF := '';
  for i := 0 to High(FColumns) do
  begin
    if i > 0 then
      NF := NF + ',';
    if FLines and (i < High(FColumns))  then
      NF := NF + 'BAR';
  end;
  List.Format := NF;
  if not Quiet then
    List.Redraw(MUIV_List_Redraw_All);
end;

function TMUIStringGrid.GetNumColumns: Integer;
begin
  Result := Length(FColumns);
end;

procedure TMUIStringGrid.SetNumColumms(AValue: Integer);
var
  i: Integer;
begin
  if Length(FColumns) = AValue then
    Exit;
  SetLength(FColumns, AValue);
  for i := 0 to High(FColumns) do
    SetLength(FColumns[i].Rows, FNumRows);
  UpdateFormat;
end;

procedure TMUIStringGrid.SetNumRows(AValue: Integer);
var
  i: Integer;
begin
  FNumRows := AValue;
  for i := 0 to High(FColumns) do
    SetLength(FColumns[i].Rows, FNumRows);
  if HasObj then
  begin
    List.Quiet := True;
    List.Clear;
    SetLength(TempRows, AValue);
    for i := 0 to AValue - 1 do
    begin
      TempRows[i] := IntToStr(i);
      List.InsertSingle(PChar(TempRows[i]), MUIV_List_Insert_Bottom);
    end;
    List.Quiet := Quiet;
  end;
end;

function TMUIStringGrid.GetCells(ACol, ARow: Integer): string;
begin
  Result := '';
  if (ACol < 0) or (ACol >= NumColumns) then
    Exit;
  if (ARow < 0) or (ARow >= FNumRows) then
    Exit;
  Result := FColumns[ACol].Rows[ARow].Text;
end;

procedure TMUIStringGrid.SetCells(ACol, ARow: Integer; AValue: string);
begin
  if (ACol < 0) or (ACol >= NumColumns) then
    Exit;
  if (ARow < 0) or (ARow >= FNumRows) then
    Exit;
  FColumns[ACol].Rows[ARow].Text := AValue;
  if not Quiet and HasObj then
    List.Redraw(ARow);
end;

function TMUIStringGrid.GetData(ACol, ARow: Integer): Pointer;
begin
  Result := nil;
  if (ACol < 0) or (ACol >= NumColumns) then
    Exit;
  if (ARow < 0) or (ARow >= FNumRows) then
    Exit;
  Result := FColumns[ACol].Rows[ARow].Data;
end;

procedure TMUIStringGrid.SetData(ACol, ARow: Integer; AValue: Pointer);
begin
  if (ACol < 0) or (ACol >= NumColumns) then
    Exit;
  if (ARow < 0) or (ARow >= FNumRows) then
    Exit;
  FColumns[ACol].Rows[ARow].Data := AValue;
end;

procedure TMUIStringGrid.DisplayEvent(Sender: TObject; ToPrint: PPChar; Entry: PChar);
var
  Idx, i: Integer;
  p: PPtrInt;
begin
  P := PPtrInt(ToPrint);
  Dec(P);
  Idx := P^;
  if Idx >= 0 then
  begin
    for i := 0 to High(FColumns) do
    begin
      ToPrint[i] := nil;
      if Idx < Length(FColumns[i].Rows) then
        ToPrint[i] := PChar(@(FColumns[i].Rows[Idx].Text[1]));
      if ToPrint[i] = nil then
        ToPrint[i] := EmptyString;
    end;
  end
  else
  begin
    for i := 0 to High(FColumns) do
    begin
      ToPrint[i] := PChar(@(FColumns[i].Title[1]));
      if ToPrint[i] = nil then
        ToPrint[i] := EmptyString;
    end
  end;
end;

procedure TMUIStringGrid.SetLines(AValue: Boolean);
begin
  if FLines = AValue then
    Exit;
  FLines := AValue;
  UpdateFormat;
end;

function TMUIStringGrid.GetShowTitle: Boolean;
begin
  Result := List.Title = #1;
end;

procedure TMUIStringGrid.SetShowTitle(AValue: Boolean);
begin
  if AValue then
    List.Title := #1
  else
    List.Title := '';
end;

function TMUIStringGrid.GetTitle(ACol: Integer): string;
begin
  if (ACol < 0) or (ACol >= NumColumns) then
    Exit;
  Result := FColumns[ACol].Title;

end;

procedure TMUIStringGrid.SetTitle(ACol: Integer; AValue: string);
begin
  if (ACol < 0) or (ACol >= NumColumns) then
    Exit;
  FColumns[ACol].Title := AValue;
end;

function TMUIStringGrid.GetRow: Integer;
begin
  Result := List.Active;
end;

procedure TMUIStringGrid.SetRow(AValue: Integer);
begin
  if (AValue < 0) or (AValue >= FNumRows) then
    Exit;
  List.Active := AValue;
end;

function TMUIStringGrid.GetQuiet: Boolean;
begin
  Result := List.Quiet;
end;

procedure TMUIStringGrid.SetQuiet(AValue: Boolean);
begin
  if not AValue then
    List.Redraw(MUIV_List_Redraw_All);
  List.Quiet := AValue;
end;

end.





