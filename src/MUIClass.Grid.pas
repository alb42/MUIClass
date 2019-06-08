unit MUIClass.Grid;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, Types, inputevent,
  Exec, Utility, AGraphics, Intuition, Layers, fgl,
  MUIClass.Base, MUIClass.Group, MUIClass.DrawPanel;

type
  TCellStatus = (csNormal, csFixed, csFocussed, csSelected);

  TRedrawList = specialize TFPGList<Integer>;
  TDrawCellEvent = procedure(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect) of object;
  TMUIGrid = class(TMUIScrollGroup)
  private
    DA: TMUIDrawPanel;
    FDefCellHeight: Integer;
    FDefCellWidth: Integer;
    FNumRows: Integer;
    FNumCols: Integer;
    FFixedRows: Integer;
    FFixedCols: Integer;
    FCellWidth: array of Integer;
    FCellHeight: array of Integer;
    FOnDrawCell: TDrawCellEvent;

    function ColRowToNum(ACol, ARow: Integer): Integer;
  protected
    BlockRecalcSize: Boolean;
    IDB: TDrawBuffer;
    ToRedraw: TRedrawList;
    AllToRedraw: Boolean;
    procedure AddToRedraw(ACol, ARow: Integer);
    procedure RecalcSize;

    procedure SetNumRows(ARows: Integer); virtual;
    procedure SetNumCols(ACols: Integer); virtual;
    procedure SetGridWidth(AWidth: Integer);
    procedure SetGridHeight(AHeight: Integer);
    function GetGridWidth: Integer;
    function GetGridHeight: Integer;
    function GetCellWidth(ACol: Integer): Integer;
    procedure SetCellWidth(ACol: Integer; AWidth: Integer);
    function GetCellHeight(ARow: Integer): Integer;
    procedure SetCellHeight(ARow: Integer; AHeight: Integer);


    property GridWidth: Integer read GetGridWidth write SetGridWidth;
    property GridHeight: Integer read GetGridHeight write SetGridHeight;

    procedure InternalDrawCells(Rp: PRastPort; DrawRect: TRect; DoAll: Boolean);

    procedure DoDrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect); virtual;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect); virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    function CoordToCell(Pt: Types.TPoint): Types.TPoint;
    function CellToRect(ACol, ARow: Integer): Types.TRect;
    function CellsToRect(ACol, ARow, BCol, BRow: Integer): Types.TRect;

    property NumRows: Integer read FNumRows write SetNumRows;
    property NumCols: Integer read FNumCols write SetNumCols;
    property FixedCols: Integer read FFixedCols write FFixedCols;
    property FixedRows: Integer read FFixedRows write FFixedRows;
    property DefCellHeight: Integer read FDefCellHeight write FDefCellHeight;
    property DefCellWidth: Integer read FDefCellWidth write FDefCellWidth;
    property CellWidth[ACol: Integer]: Integer read GetCellWidth write SetCellWidth;
    property CellHeight[ARow: Integer]: Integer read GetCellHeight write SetCellHeight;
    property OnDrawCell: TDrawCellEvent read FOnDrawCell write FOnDrawCell;
  end;

  TCellStat = record
    Text: string;
    Selected: Boolean;
  end;

  TMouseMode = (mmIdle, mmMoveCol, mmMoveRow, mmSelectCells);

  TMUIStrGrid = class(TMUIGrid)
  private
    FOnCellFocus: TNotifyEvent;
    FCol, FRow: Integer;
    MouseMode: TMouseMode;
    StartPos: Types.TPoint;
    MousePos: Types.TPoint;
    MouseDist: Types.TPoint;
    ShiftMode: Boolean;
    FUpdating: Boolean;
    FOnDblClick: TNotifyEvent;
    FStrings: array of array of TCellStat;
  protected
    procedure SetNumRows(ARows: Integer); override;
    procedure SetNumCols(ACols: Integer); override;

    procedure SetCol(ACol: Integer);
    procedure SetRow(ARow: Integer);

    function GetCell(ACol, ARow: Integer): string; virtual;
    procedure SetCell(ACol, ARow: Integer; AValue: string); virtual;
    function GetCellStatus(ACol, ARow: Integer): TCellStatus;
    procedure DoSetFocus(ACol, ARow: Integer);


    procedure DoDrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect); override;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect); override;
    procedure DoMouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure DoMouseUp(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure DoMouseMove(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
    procedure DoKeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure DoKeyUp(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure DoDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
  public
    constructor Create; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SelectAll(Select: Boolean);
    property Cells[ACol, ARow: Integer]: string read GetCell write SetCell;
    property CellStatus[ACol, ARow: Integer]: TCellStatus read GetCellStatus;

    property Row: Integer read FRow write SetRow;
    property Col: Integer read FCol write SetCol;

    property OnCellFocus: TNotifyEvent read FOnCellFocus write FOnCellFocus;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;

  end;

implementation

constructor TMUIGrid.Create;
begin
  inherited;
  IDB := nil;
  ToRedraw := TReDrawList.Create;
  DA := TMUIDrawPanel.Create;
  DA.FillArea := False;
  DA.OnDrawObject := @DoDrawObject;
  DA.Parent := Contents;
  FNumCols := 0;
  FNumRows := 0;
  SetLength(FCellWidth, 0);
  SetLength(FCellHeight, 0);

  FFixedCols := 1;
  FFixedRows := 1;
  FDefCellHeight := 20;
  FDefCellWidth := 60;
  FreeHoriz := True;
  FreeVert := True;
end;

destructor TMUIGrid.Destroy;
begin
  IDB.Free;
  ToRedraw.Free;
  inherited;
end;

procedure TMUIGrid.SetNumRows(ARows: Integer);
var
  i: Integer;
begin
  if ARows = FNumRows then
    Exit;
  SetLength(FCellHeight, ARows);
  for i := FNumRows to ARows - 1 do
    FCellHeight[i] := FDefCellHeight;
  FNumRows := ARows;
  RecalcSize;
end;

procedure TMUIGrid.SetNumCols(ACols: Integer);
var
  i: Integer;
begin
  if ACols = FNumCols then
    Exit;
  SetLength(FCellWidth, ACols);
  for i := FNumCols to ACols - 1 do
    FCellWidth[i] := FDefCellWidth;
  FNumCols := ACols;
  RecalcSize;
end;

procedure TMUIGrid.RecalcSize;
var
  w,h,i: Integer;
begin
  if BlockRecalcSize then
    Exit;
  W := 0;
  for i := 0 to High(FCellWidth) do
    W := W + FCellWidth[i];
  H := 0;
  for i := 0 to High(FCellHeight) do
    H := H + FCellHeight[i];
  Contents.InitChange;
  GridWidth := W;
  GridHeight := H;
  Contents.ExitChange;
end;

procedure TMUIGrid.SetGridWidth(AWidth: Integer);
begin
  DA.MinWidth := AWidth;
  DA.MaxWidth := AWidth;
  DA.DefWidth := AWidth;
end;

procedure TMUIGrid.SetGridHeight(AHeight: Integer);
begin
  DA.MinHeight := AHeight;
  DA.MaxHeight := AHeight;
  DA.DefHeight := AHeight;
end;

function TMUIGrid.GetGridWidth: Integer;
begin
  Result := DA.DefWidth;
end;

function TMUIGrid.GetGridHeight: Integer;
begin
  Result := DA.DefHeight;
end;

function TMUIGrid.GetCellWidth(ACol: Integer): Integer;
begin
  Result := 0;
  if InRange(ACol, 0, FNumCols - 1) then
    Result := FCellWidth[ACol]
end;

procedure TMUIGrid.SetCellWidth(ACol: Integer; AWidth: Integer);
begin
  if InRange(ACol, 0, FNumCols - 1) then
  begin
    FCellWidth[ACol] := Max(2, AWidth);
    RecalcSize;
  end;
end;

function TMUIGrid.GetCellHeight(ARow: Integer): Integer;
begin
  Result := 0;
  if InRange(ARow, 0, FNumRows - 1) then
    Result := FCellHeight[ARow]
end;

procedure TMUIGrid.SetCellHeight(ARow: Integer; AHeight: Integer);
begin
  if InRange(ARow, 0, FNumRows - 1) then
  begin
    FCellHeight[ARow] := Max(2, AHeight);
    RecalcSize;
  end;
end;

procedure TMUIGrid.DoDrawCell(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect);
begin
  try
    if Assigned(FOnDrawCell) then
      FOnDrawCell(Sender, ACol, ARow, RP, ARect);
  except
    on E: Exception do
      MUIApp.DoException(E);
  end;
end;

function TMUIGrid.ColRowToNum(ACol, ARow: Integer): Integer;
begin
  Result := ACol + ARow * FNumCols;
end;

procedure TMUIGrid.AddToRedraw(ACol, ARow: Integer);
var
  Value: Integer;
begin
  Value := ColRowToNum(ACol, ARow);
  if ToRedraw.IndexOf(Value) < 0 then
    ToRedraw.Add(Value);
end;

procedure TMUIGrid.InternalDrawCells(Rp: PRastPort; DrawRect: TRect; DoAll: Boolean);
var
  CellRect: TRect;
  l,t,x,y: Integer;
  DB: TDrawBuffer;
begin
  DB := nil;
  T := 0;
  for y := 0 to FNumRows - 1 do
  begin
    L := 0;
    for x := 0 to FNumCols - 1 do
    begin
      if DoAll or (ToRedraw.IndexOf(ColRowToNum(x,y)) >= 0) then
      begin
        //
        CellRect := Rect(0, 0, FCellWidth[x] - 1, FCellHeight[y] - 1);
        if not Assigned(DB) or (DB.Width < FCellWidth[x]) or (DB.Width < FCellHeight[x]) then
        begin
          DB.Free;
          DB := TDrawBuffer.Create(FCellWidth[x] * 2, FCellHeight[y] * 2, Rp^.Bitmap^.Depth, RP^.Bitmap);
        end;
        if (y < FFixedRows) or (x < FFixedCols) then
        begin
          DB.APen := 0;
          DB.FillRect(CellRect);
          DB.Draw3DBox(Rect(CellRect.Left + 1, CellRect.Top + 1, CellRect.Right, CellRect.Bottom));
          CellRect.Inflate(-1,-1);
        end
        else
        begin
          DB.APen := 2;
          DB.FillRect(CellRect);
          DB.APen := 0;
          DB.Line(CellRect.Left, CellRect.Bottom, CellRect.Left, CellRect.Top);
          DB.Line(CellRect.Left, CellRect.Top, CellRect.Right, CellRect.Top);
        end;

        DoDrawCell(Self, x, y, DB.RP, CellRect);
        DB.DrawToRastPort(DrawRect.Left + L, DrawRect.Top + T, FCellWidth[x], FCellHeight[y], RP);
      end;
      L := L + FCellWidth[x];
    end;
    T := T + FCellHeight[y];
  end;
  ToRedraw.Clear;
  DB.Free;
end;

procedure TMUIGrid.DoDrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  ARect: TRect;
begin
  if not Assigned(IDB) or (IDB.Width <> DrawRect.Width) or (IDB.Height <> DrawRect.Height) then
  begin
    IDB.Free;
    IDB := TDrawBuffer.Create(DrawRect.Width, DrawRect.Height, RP^.BitMap^.Depth, RP^.Bitmap);
    AllToRedraw := True;
  end;
  if AllToRedraw or (ToRedraw.Count > 0) then
  begin
    ARect := Rect(0, 0, DrawRect.Width - 1, DrawRect.Height - 1);
    InternalDrawCells(IDB.RP, ARect, AllToRedraw);
  end;
  AllToRedraw := False;
  ClipBlit(IDB.RP, 0,0, Rp, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height, $00C0);
end;

function TMUIGrid.CoordToCell(Pt: Types.TPoint): Types.TPoint;
var
  x,y,i: Integer;
begin
  Result := Point(-1, -1);
  // X position
  i := 0;
  for x := 0 to FNumCols - 1 do
  begin
    if InRange(PT.X, i, i + FCellWidth[x]) then
    begin
      Result.X := x;
      Break;
    end;
    i := i + FCellWidth[x];
  end;
  if Result.X < 0 then
    Exit;
  //
  i := 0;
  for y := 0 to FNumRows - 1 do
  begin
    if InRange(PT.Y, i, i + FCellHeight[y]) then
    begin
      Result.Y := y;
      Break;
    end;
    i := i + FCellHeight[y];
  end;
end;

function TMUIGrid.CellToRect(ACol, ARow: Integer): Types.TRect;
var
  x,y: Integer;
begin
  Result := Rect(0, 0, 0, 0);
  if not inRange(ACol, 0, FNumCols - 1) or not inRange(ARow, 0, FNumRows - 1) then
    Exit;
  // X position
  Result.Left := 0;
  Result.Top := 0;
  for x := 0 to ACol - 1 do
    Result.Left := Result.Left + FCellWidth[x];
  Result.Width := FCellWidth[ACol];
  //
  for y := 0 to ARow - 1 do
    Result.Top := Result.Top + FCellHeight[y];
  Result.Height := FCellHeight[ACol];
end;

function TMUIGrid.CellsToRect(ACol, ARow, BCol, BRow: Integer): Types.TRect;
var
  MaxRect: TRect;
begin
  Result := CellToRect(Min(ACol, BCol), Min(ARow, BRow));
  MaxRect := CellToRect(Max(ACol, BCol), Max(ARow, BRow));
  Result.Right := MaxRect.Right;
  Result.Bottom := MaxRect.Bottom;
end;

{ TMUIStrGrid }

constructor TMUIStrGrid.Create;
begin
  inherited;
  FUpdating := False;
  MouseMode := mmIdle;
  FRow := -1;
  FCol := -1;
  DA.OnMouseDown := @DoMouseDown;
  DA.OnMouseMove := @DoMouseMove;
  DA.OnMouseUp := @DoMouseUp;
  DA.OnKeyDown := @DoKeyDown;
  DA.OnKeyUp := @DoKeyUp;
  DA.OnDblClick := @DoDblClick;
end;


procedure TMUIStrGrid.SetNumRows(ARows: Integer);
begin
  SetLength(FStrings, FNumCols, ARows);
  inherited;
end;

procedure TMUIStrGrid.SetNumCols(ACols: Integer);
begin
  SetLength(FStrings, ACols, FNumRows);
  inherited;
end;

function TMUIStrGrid.GetCell(ACol, ARow: Integer): string;
begin
  Result := '';
  if InRange(ARow, 0, FNumRows - 1) and InRange(ACol, 0, FNumCols - 1) then
    Result := FStrings[ACol, ARow].Text;
end;

procedure TMUIStrGrid.SetCell(ACol, ARow: Integer; AValue: string);
begin
  if InRange(ARow, 0, FNumRows - 1) and InRange(ACol, 0, FNumCols - 1) then
  begin
    FStrings[ACol, ARow].Text := AValue;
    AllToRedraw := True;
    if not FUpdating then
      DA.RedrawObject;
  end;
end;

function TMUIStrGrid.GetCellStatus(ACol, ARow: Integer): TCellStatus;
begin
  Result := csNormal;
  if (ACol < FixedCols) or (ARow < FixedRows) then
    Result := csFixed
  else
  begin
    if InRange(ARow, 0, FNumRows - 1) and InRange(ACol, 0, FNumCols - 1) then
    begin
      if FStrings[ACol, ARow].Selected then
        Result := csSelected
      else
        Result := csNormal;
    end;
    if (Row = ARow) and (Col = ACol) then
      Result := csFocussed;
  end;
end;

procedure TMUIStrGrid.DoDrawCell(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect);
var
  s: string;
  CS: TCellStatus;
begin
  s := Cells[ACol, ARow];
  CS := CellStatus[ACol, ARow];

  SetDrMd(RP, JAM1);
  if (CS = csSelected) then
  begin
    SetAPen(RP, 3);
    RectFill(RP, ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
    SetAPen(RP, 0);
    GfxMove(RP, ARect.Left, ARect.Bottom);
    AGraphics.Draw(RP, ARect.Left, ARect.Top);
    AGraphics.Draw(RP, ARect.Right, ARect.Top);
    SetAPen(RP, 2);
  end
  else
  begin
    SetAPen(RP, 1);
  end;
  GfxMove(RP, ARect.Left + 1, ARect.Top + ARect.Height div 2 + RP^.Font^.tf_Baseline div 2);
  GfxText(RP, PChar(s), Length(s));

  if CS = csFocussed then
  begin
    SetAPen(RP, 1);
    GfxMove(RP, ARect.Left + 1, ARect.Top + 1);
    AGraphics.Draw(RP, ARect.Right, ARect.Top + 1);
    AGraphics.Draw(RP, ARect.Right, ARect.Bottom);
    AGraphics.Draw(RP, ARect.Left + 1, ARect.Bottom);
    AGraphics.Draw(RP, ARect.Left + 1, ARect.Top + 1);
  end;
end;

procedure TMUIStrGrid.DoDrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
begin
  inherited;
  if MouseMode = mmMoveCol then
  begin
    SetAPen(RP, 1);
    GFXMove(RP, DrawRect.Left +MousePos.X, DrawRect.Top);
    AGraphics.Draw(RP, DrawRect.Left + MousePos.X, DrawRect.Bottom);
  end;
  if MouseMode = mmMoveRow then
  begin
    SetAPen(RP, 1);
    GFXMove(RP, DrawRect.Left, DrawRect.Top + MousePos.Y);
    AGraphics.Draw(RP, DrawRect.Right, DrawRect.Top + MousePos.Y);
  end;
end;

procedure TMUIStrGrid.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TMUIStrGrid.EndUpdate;
begin
  FUpdating := False;
  DA.RedrawObject;
end;

procedure TMUIStrGrid.DoMouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
var
  CC: Types.TPoint;
  CR: Types.TRect;
begin
  if MouseBtn = mmbLeft then
  begin
    EatEvent := True;
    CC := CoordToCell(Point(X,Y));
    CR := CellToRect(CC.X, CC.Y);
    // Click to one of the border on the top
    if (CC.Y = 0) and (Abs(CR.Left - x) < 5)then
    begin
      MouseDist.X := Min(CR.Left - x, CR.Right - x);
      MouseDist.Y := 0;
      if (Abs(CR.Left - x) < 5) and (CC.X > 0) then
        CC.X := CC.X - 1;
      StartPos := CC;
      CR := CellToRect(CC.X, CC.Y);
      StartPos.Y := CR.Left; // Minimum
      MousePos := Point(x + MouseDist.X,y);
      MouseMode := mmMoveCol;
      DA.RedrawObject;
      Exit;
    end;
    if (CC.X = 0) and (Abs(CR.Top - y) < 5) then
    begin
      MouseDist.Y := Min(CR.Top - y, CR.Bottom - y);
      MouseDist.X := 0;
      if (Abs(CR.Top - y) < 5) and (CC.Y > 0) then
        CC.Y := CC.Y - 1;
      StartPos := CC;
      CR := CellToRect(CC.X, CC.Y);
      StartPos.X := CR.Top; // Minimum
      MousePos := Point(x,y + MouseDist.Y);
      MouseMode := mmMoveRow;
      DA.RedrawObject;
      Exit;
    end;
    if (CC.X >= FixedCols) and (CC.Y >= FixedRows) then
    begin
      if InRange(CC.Y, 0, FNumRows - 1) and InRange(CC.X, 0, FNumCols - 1) then
      begin
        SelectAll(False);
        AddToRedraw(CC.Y, CC.Y);
        FStrings[CC.X, CC.Y].Selected := True;
        StartPos := CC;
        DoSetFocus(CC.X, CC.Y);
        DA.RedrawObject;
        MouseMode := mmSelectCells;
        Exit;
      end;
    end;
  end;
end;

procedure TMUIStrGrid.DoMouseMove(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
var
  CC: Types.TPoint;
  St,En: Types.TPoint;
begin
  case MouseMode of
    mmSelectCells:
    begin
      CC := CoordToCell(Point(X,Y));
      if (CC.X >= FixedCols) and (CC.Y >= FixedRows) then
      begin
        SelectAll(False);
        St.X := Min(CC.X, StartPos.X);
        ST.Y := Min(CC.Y, StartPos.Y);
        En.X := Max(CC.X, StartPos.X);
        En.Y := Max(CC.Y, StartPos.Y);
        for x := St.X to En.X do
        begin
          for y := St.y to En.Y do
          begin
            AddToRedraw(X, Y);
            FStrings[X, Y].Selected := True;
          end;
        end;
        DA.RedrawObject;
      end;
    end;
    mmMoveCol:
    begin
      MousePos := Point(Max(X, StartPos.Y) + MouseDist.X, Y);
      DA.RedrawObject;
    end;
    mmMoveRow:
    begin
      MousePos := Point(X, Max(Y, StartPos.X)  + MouseDist.Y);
      DA.RedrawObject;
    end;
  end;
end;

procedure TMUIStrGrid.DoMouseUp(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
var
  CC: Types.TPoint;
  NVal: Integer;
begin
  if (MouseBtn = mmbLeft) then
  begin
    case MouseMode of
      mmSelectCells:
      begin
        MouseMode := mmIdle;
        EatEvent := True;
        {CC := CoordToCell(Point(X,Y));
        if (CC.X >= FixedCols) and (CC.Y >= FixedRows) then
        begin
          DoSetFocus(CC.X, CC.Y);
        end
        else
        begin
          FRow := -1;
          FCol := -1;
        end;}
        DA.RedrawObject;
      end;
      mmMoveCol:
      begin
        MouseMode := mmIdle;
        MousePos := Point(Max(X, StartPos.Y) + MouseDist.X, Y);
        NVal := MousePos.X - StartPos.Y;
        if Abs(NVal - CellWidth[StartPos.X]) > 2 then
          CellWidth[StartPos.X] := NVal
        else
          DA.RedrawObject;
        EatEvent := True;
      end;
      mmMoveRow:
      begin
        MouseMode := mmIdle;
        MousePos := Point(X, Max(Y, StartPos.Y) + MouseDist.Y);
        NVal := MousePos.Y - StartPos.X;
        if Abs(NVal - CellHeight[StartPos.Y]) > 2 then
          CellHeight[StartPos.Y] := NVal
        else
          DA.RedrawObject;
        EatEvent := True;
      end;
    end;
  end;
end;

procedure TMUIStrGrid.DoDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
var
  CC: Types.TPoint;
begin
  if (MouseBtn = mmbLeft) then
  begin
    EatEvent := True;
    CC := CoordToCell(Point(X,Y));
    if (CC.X >= FixedCols) and (CC.Y >= FixedRows) then
    begin
      DoSetFocus(CC.X, CC.Y);
    end
    else
    begin
      FRow := -1;
      FCol := -1;
    end;
    DA.RedrawObject;
    if Assigned(FOnDblClick) then
      FOnDblClick(Sender);
  end;
end;

procedure TMUIStrGrid.SelectAll(Select: Boolean);
var
  x,y: Integer;
begin
  for x := 0 to FNumCols - 1 do
  begin
    for y := 0 to FNumRows - 1 do
    begin
      if FStrings[x,y].Selected <> Select then
        AddToRedraw(x, y);
      FStrings[x,y].Selected := Select;
    end;
  end;
end;

procedure TMUIStrGrid.SetCol(ACol: Integer);
begin
  DoSetFocus(ACol, FRow);
end;

procedure TMUIStrGrid.SetRow(ARow: Integer);
begin
  DoSetFocus(FCol, ARow);
end;



procedure TMUIStrGrid.DoKeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
  procedure DeleteSelection;
  var
    y,x: Integer;
  begin
    BeginUpdate;
    for y := 0 to FNumRows - 1 do
    begin
      for x := 0 to FNumCols - 1 do
      begin
        if CellStatus[x,y] in [csSelected, csFocussed] then
        begin
          Cells[x,y] := '';
        end;
      end;
    end;
    EndUpdate;
  end;

var
  x,y: Integer;
  CC, ST, En: Types.TPoint;
begin
  EatEvent := True;
  if (Row < 0) or (Col < 0) then
    Exit;
  if ShiftMode and (Code in [CursorUp, CursorDown, CursorLeft, CursorRight]) then
  begin
    case Code of
      CursorDown: CC := Point(FCol, FRow + 1);
      CursorUp: CC := Point(FCol, FRow - 1);
      CursorRight: CC := Point(FCol + 1, FRow);
      CursorLeft: CC := Point(FCol - 1, FRow);
    end;
    SelectAll(False);
    St.X := Min(CC.X, StartPos.X);
    ST.Y := Min(CC.Y, StartPos.Y);
    En.X := Max(CC.X, StartPos.X);
    En.Y := Max(CC.Y, StartPos.Y);
    for x := St.X to En.X do
    begin
      for y := St.y to En.Y do
      begin
        AddToRedraw(X, Y);
        FStrings[X, Y].Selected := True;
      end;
    end;
  end;
  case Code of
    CursorDown: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol, FRow + 1); end;
    CursorUp: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol, FRow - 1); end;
    CursorRight: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol + 1, FRow); end;
    CursorLeft: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol - 1, FRow); end;
    70: DeleteSelection; // Delete
    96, 97: begin
      StartPos.X := FCol;
      StartPos.Y := FRow;
      ShiftMode := True; //Shift
    end;
  end;
  EatEvent := True;
  if InRange(Col, 0, FNumCols - 1) and InRange(Row, 0, FNumRows - 1) then
    FStrings[Col,Row].Selected := True;
end;

procedure TMUIStrGrid.DoKeyUp(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
begin
  case Code of
    96, 97: begin
      StartPos := Point(-1, -1);
      ShiftMode := False;
    end;
  end;
end;

procedure TMUIStrGrid.DoSetFocus(ACol, ARow: Integer);
begin
  if (ACol < FNumCols) and (ACol >= FFixedCols) and (ARow < FNumRows) and (ARow >= FFixedRows) and ((ACol <> FCol) or (ARow <> FRow))then
  begin
    AddToRedraw(FCol, FRow);
    FRow := ARow;
    FCol := ACol;
    AddToRedraw(FCol, FRow);
    if Assigned(FOnCellFocus) then
      FOnCellFocus(Self);
    DA.RedrawObject;
  end;
end;

end.
