unit MUIClass.Grid;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Math, Types, inputevent,
  Exec, Utility, AGraphics, Intuition, Layers, fgl,
  MUIClass.Base, MUIClass.Group, MUIClass.DrawPanel;

const
  GRABDISTANCE = 5;

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

  protected
    FCol, FRow: Integer; // currently selected cell
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
    function GetCellWidth(ACol: Integer): Integer; virtual;
    procedure SetCellWidth(ACol: Integer; AWidth: Integer); virtual;
    function GetCellHeight(ARow: Integer): Integer; virtual;
    procedure SetCellHeight(ARow: Integer; AHeight: Integer); virtual;


    property GridWidth: Integer read GetGridWidth write SetGridWidth;
    property GridHeight: Integer read GetGridHeight write SetGridHeight;

    procedure InternalDrawCells(Rp: PRastPort; DrawRect: TRect; DoAll: Boolean);

    procedure DoDrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect); virtual;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect); virtual;

    function ColRowToNum(ACol, ARow: Integer): Integer;
    function NumToColRow(Num: Integer): Types.TPoint;
  public
    constructor Create; override;
    destructor Destroy; override;

    function CoordToCell(Pt: Types.TPoint): Types.TPoint;
    function CellToRect(ACol, ARow: Integer): Types.TRect;
    function CellsToRect(ACol, ARow, BCol, BRow: Integer): Types.TRect;

    procedure RedrawCell(ACol, ARow: Integer);
    procedure SetSize(ACols, ARows: Integer);

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

  TSelectionList = specialize TFPGList<LongWord>;

  TMouseMode = (mmIdle, mmMoveCol, mmMoveRow, mmSelectCells);

  TMUIStrGrid = class(TMUIGrid)
  private
    FOnCellFocus: TNotifyEvent;
    MouseMode: TMouseMode;
    StartPos: Types.TPoint;
    MousePos: Types.TPoint;
    MouseDist: Types.TPoint;
    ShiftMode: Boolean;
    FUpdating: Boolean;
    FOnDblClick: TNotifyEvent;
    FStrings: array of array of TCellStat;
    FEditMode: Boolean;
    FEditText: string;
    FSelectionList: TSelectionList;
  protected
    procedure SetNumRows(ARows: Integer); override;
    procedure SetNumCols(ACols: Integer); override;

    procedure SetCol(ACol: Integer);
    procedure SetRow(ARow: Integer);

    function GetCell(ACol, ARow: Integer): string; virtual;
    procedure SetCell(ACol, ARow: Integer; AValue: string); virtual;
    function GetCellStatus(ACol, ARow: Integer): TCellStatus;
    procedure DoSetFocus(ACol, ARow: Integer);
    procedure DoDeleteCell(ACol, ARow: Integer); virtual;

    function GetSelCount: Integer; virtual;
    function GetSelection(AIdx: Integer): Types.TPoint; virtual;

    procedure CopyToClip; virtual;
    procedure CutToClip; virtual;
    procedure PasteFromClip; virtual;

    procedure DoDrawObject(Sender: TObject; Rp: PRastPort; DrawRect: TRect); override;
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; RP: PRastPort; ARect: TRect); override;
    procedure DoMouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure DoMouseUp(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure DoMouseMove(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
    procedure DoKeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure DoKeyUp(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure DoDblClick(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);

    property EditMode: boolean read FEditMode;
    property EditText: string read FEditText;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure SelectAll(Select: Boolean);
    property Cells[ACol, ARow: Integer]: string read GetCell write SetCell;
    property CellStatus[ACol, ARow: Integer]: TCellStatus read GetCellStatus;

    property SelectionCount: Integer read GetSelCount;
    property Selection[AIdx: Integer]: Types.TPoint read GetSelection;

    procedure DeleteSelectedCells;

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

  FCol := -1;
  FRow := -1;
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
  if (FNumCols = 0) and (ACols > 0) then
    FCellWidth[0] := 50;
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
  for i := 0 to FNumCols - 1 do
    W := W + CellWidth[i];
  H := 0;
  for i := 0 to FNumRows - 1 do
    H := H + CellHeight[i];
  Contents.InitChange;
  GridWidth := W;
  GridHeight := H;
  Contents.ExitChange;
  DA.RedrawObject;
end;

procedure TMUIGrid.SetSize(ACols, ARows: Integer);
begin
  BlockRecalcSize := True;
  try
    NumCols := ACols;
    NumRows := ARows;
  finally
    BlockRecalcSize := False;
  end;
  RecalcSize;
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

function TMUIGrid.NumToColRow(Num: Integer): Types.TPoint;
begin
  Result.Y := Num div FNumCols;
  Result.X := Num mod FNumCols;
end;

procedure TMUIGrid.AddToRedraw(ACol, ARow: Integer);
var
  Value: Integer;
begin
  Value := ColRowToNum(ACol, ARow);
  if ToRedraw.IndexOf(Value) < 0 then
    ToRedraw.Add(Value);
end;

procedure TMUIGrid.RedrawCell(ACol, ARow: Integer);
begin
  AddToRedraw(ACol, ARow);
  DA.RedrawObject;
end;

procedure TMUIGrid.InternalDrawCells(Rp: PRastPort; DrawRect: TRect; DoAll: Boolean);
var
  CellRect: TRect;
  l,t,x,y: Integer;
  DB: TDrawBuffer;
  cw, ch: Integer;
begin
  DB := nil;
  T := 0;
  for y := 0 to FNumRows - 1 do
  begin
    L := 0;
    ch := CellHeight[y];
    if ch = 0 then
      Continue;
    for x := 0 to FNumCols - 1 do
    begin
      cw := CellWidth[x];
      if cw = 0 then
        Continue;
      if DoAll or (ToRedraw.IndexOf(ColRowToNum(x,y)) >= 0) then
      begin
        CellRect := Rect(0, 0, cw - 1, ch - 1);
        if not Assigned(DB) or (DB.Width < cw) or (DB.Height < ch) then
        begin
          DB.Free;
          DB := TDrawBuffer.Create(cw * 2, ch * 2, Rp^.Bitmap^.Depth, RP^.Bitmap);
        end;
        if (y < FFixedRows) or (x < FFixedCols) then
        begin
          if ((y < FFixedRows) and (x = FCol)) or ((x < FFixedCols) and (y = FRow)) then
          begin
            DB.APen := 3;
            DB.FillRect(CellRect);
            DB.Draw3DBox(Rect(CellRect.Left + 1, CellRect.Top + 1, CellRect.Right, CellRect.Bottom), False);
          end
          else
          begin
            DB.APen := 0;
            DB.FillRect(CellRect);
            DB.Draw3DBox(Rect(CellRect.Left + 1, CellRect.Top + 1, CellRect.Right, CellRect.Bottom), True);
          end;
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
        DB.DrawToRastPort(DrawRect.Left + L, DrawRect.Top + T, cw, ch, RP);
      end;
      L := L + cw;
    end;
    T := T + ch;
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
    if InRange(PT.X, i, i + CellWidth[x]) then
    begin
      Result.X := x;
      Break;
    end;
    i := i + CellWidth[x];
  end;
  if Result.X < 0 then
    Exit;
  //
  i := 0;
  for y := 0 to FNumRows - 1 do
  begin
    if InRange(PT.Y, i, i + CellHeight[y]) then
    begin
      Result.Y := y;
      Break;
    end;
    i := i + CellHeight[y];
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
    Result.Left := Result.Left + CellWidth[x];
  Result.Width := CellWidth[ACol];
  //
  for y := 0 to ARow - 1 do
    Result.Top := Result.Top + CellHeight[y];
  Result.Height := CellHeight[ARow];
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
  FSelectionList := TSelectionList.Create;
  DA.OnMouseDown := @DoMouseDown;
  DA.OnMouseMove := @DoMouseMove;
  DA.OnMouseUp := @DoMouseUp;
  DA.OnKeyDown := @DoKeyDown;
  DA.OnKeyUp := @DoKeyUp;
  DA.OnDblClick := @DoDblClick;
  FEditMode := False;
end;

destructor TMUIStrGrid.Destroy;
begin
  FSelectionList.Free;
  inherited;
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
  TE: TTextExtent;
begin

  CS := CellStatus[ACol, ARow];
  if EditMode and (CS = csFocussed) then
    s := EditText
  else
    s := Cells[ACol, ARow];

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
  if EditMode then
  begin
    TextExtent(RP, PChar(s), Length(s), @TE);
    GfxMove(RP, ARect.Right - 3 - TE.te_Width, ARect.Top + ARect.Height div 2 + RP^.Font^.tf_Baseline div 2);
  end
  else
    GfxMove(RP, ARect.Left + 1, ARect.Top + ARect.Height div 2 + RP^.Font^.tf_Baseline div 2);
  GfxText(RP, PChar(s), Length(s));

  if CS = csFocussed then
  begin
    SetAPen(RP, 1);
    // Draw cursor line
    if EditMode then
    begin
      GfxMove(RP, ARect.Right - 3, ARect.Top + ARect.Height div 2 + RP^.Font^.tf_Baseline div 2 - TE.te_Height);
      AGraphics.Draw(RP, ARect.Right - 3, ARect.Top + ARect.Height div 2 + RP^.Font^.tf_Baseline div 2 + TE.te_Height div 2);
    end;
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
  i: Integer;
begin
  if MouseBtn = mmbLeft then
  begin
    EatEvent := True;
    CC := CoordToCell(Point(X,Y));
    CR := CellToRect(CC.X, CC.Y);
    // Click to one of the border on the top
    if (CC.Y = 0) and ((x - CR.Left < GRABDISTANCE) or (CR.Right - x < GRABDISTANCE)) then
    begin
      if (x - CR.Left < GRABDISTANCE) and (CC.X > 0) then
        CC.X := CC.X - 1;
      StartPos := CC;
      CR := CellToRect(CC.X, CC.Y);
      //
      MouseDist.X := CR.Right - x;
      MouseDist.Y := 0;
      //
      StartPos.Y := CR.Left; // Minimum
      MousePos := Point(x + MouseDist.X,y);
      MouseMode := mmMoveCol;
      DA.RedrawObject;
      Exit;
    end;
    if (CC.X = 0) and ((y - CR.Top < GRABDISTANCE) or (CR.Bottom - y < GRABDISTANCE)) then
    begin
      if (y - CR.Top < GRABDISTANCE) and (CC.Y > 0) then
        CC.Y := CC.Y - 1;
      StartPos := CC;
      CR := CellToRect(CC.X, CC.Y);
      //
      MouseDist.X := 0;
      MouseDist.Y := CR.Bottom - y;
      //
      StartPos.X := CR.Top; // Minimum
      MousePos := Point(x, y + MouseDist.Y);
      MouseMode := mmMoveRow;
      DA.RedrawObject;
      Exit;
    end;
    if (CC.X >= FixedCols) and (CC.Y >= FixedRows) then
    begin
      if InRange(CC.Y, 0, FNumRows - 1) and InRange(CC.X, 0, FNumCols - 1) then
      begin
        SelectAll(False);
        FStrings[CC.X, CC.Y].Selected := True;
        FSelectionList.Add(ColRowToNum(CC.x,CC.y));
        StartPos := CC;
        DoSetFocus(CC.X, CC.Y);
        DA.RedrawObject;
        MouseMode := mmSelectCells;
        Exit;
      end;
    end
    else
    begin
      if (CC.X >= FixedCols) and (CC.Y < FixedRows) then
      begin
        SelectAll(False);
        for i := FixedRows to FNumRows - 1 do
        begin
          FStrings[CC.X, i].Selected := True;
          FSelectionList.Add(ColRowToNum(CC.X, i));
          AddToRedraw(CC.X, i);
        end;
        DoSetFocus(CC.X, FixedRows);
        DA.RedrawObject;
        Exit;
      end
      else
      if (CC.Y >= FixedRows) and (CC.X < FixedCols) then
      begin
        SelectAll(False);
        for i := FixedCols to FNumCols - 1 do
        begin
          FStrings[i, CC.Y].Selected := True;
          FSelectionList.Add(ColRowToNum(i, CC.Y));
          AddToRedraw(i, CC.Y);
        end;
        DoSetFocus(FixedCols, CC.Y);
        DA.RedrawObject;
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
            FSelectionList.Add(ColRowToNum(x,y));
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
  NVal: Integer;
begin
  if (MouseBtn = mmbLeft) then
  begin
    case MouseMode of
      mmSelectCells:
      begin
        MouseMode := mmIdle;
        EatEvent := True;
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
  if not Select then
    FSelectionList.Clear;
  for x := 0 to FNumCols - 1 do
  begin
    for y := 0 to FNumRows - 1 do
    begin
      if FStrings[x,y].Selected <> Select then
        AddToRedraw(x, y);
      FStrings[x,y].Selected := Select;
      if Select then
        FSelectionList.Add(ColRowToNum(x,y));
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

procedure TMUIStrGrid.DoDeleteCell(ACol, ARow: Integer);
begin
  if InRange(ACol, 0, NumCols - 1) and InRange(ARow, 0, NumRows - 1) then
    Cells[ACol, ARow] := '';
end;

procedure TMUIStrGrid.DeleteSelectedCells;
var
  i: Integer;
begin
  BeginUpdate;
  for i := 0 to SelectionCount - 1 do
    DoDeleteCell(Selection[i].X, Selection[i].y);
  EndUpdate;
end;

procedure TMUIStrGrid.DoKeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
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
        FSelectionList.Add(ColRowToNum(x,y));
      end;
    end;
  end;
  //
  if ((Trim(key) <> '') or (Key = ' ')) and (Code <> 70) and  (not (mssCtrl in Shift)) and (not (mssLAmiga in Shift)) and (not (mssRAmiga in Shift)) and (not (mssLAlt in Shift))  then
  begin
    if not EditMode then
    begin
      FEditMode := True;
      FEditText := '';
    end;
    FEditText := FEditText + Key;
    AddToRedraw(Col, Row);
    DA.RedrawObject;
  end;
  case Code of
    CursorDown: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol, FRow + 1); end;
    CursorUp: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol, FRow - 1); end;
    CursorRight: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol + 1, FRow); end;
    CursorLeft: begin if not (mssShift in Shift) then SelectAll(False); DoSetFocus(FCol - 1, FRow); end;
    65: if EditMode then
      begin // Backspace
        Delete(FEditText, Length(FEditText), 1);
        AddToRedraw(Col, Row);
        DA.RedrawObject;
      end
      else
        DeleteSelectedCells;
    66, 67, 68:begin
      if (mssCtrl in Shift) and EditMode and (Code in [67,68]) then
      begin
        FEditText := FEditText + #10;
        AddToRedraw(Col, Row);
        DA.RedrawObject;
      end
      else
      begin
        if EditMode then // tab, return, enter
        begin // enter
          FEditMode := False;
          Cells[FCol, FRow] := FEditText;
        end;
        SelectAll(False);
        if Code = 66 then // tab
        begin
          if mssShift in Shift then
            DoSetFocus(FCol - 1, FRow)
          else
            DoSetFocus(FCol + 1, FRow)
        end
        else
          if mssShift in Shift then
            DoSetFocus(FCol, FRow - 1)
          else
            DoSetFocus(FCol, FRow + 1);
      end;
    end;
    70: begin
      if not EditMode then
        DeleteSelectedCells; // Delete
    end;
    96, 97: begin
      StartPos.X := FCol;
      StartPos.Y := FRow;
      ShiftMode := True; //Shift
    end;
  end;
  if (mssRAmiga in Shift) or (mssCtrl in Shift) then
  begin
    case Key of
      'c': CopyToClip;
      'x': CutToClip;
      'v': PasteFromClip;
    end;
  end;
  EatEvent := True;
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

procedure TMUIStrGrid.CopyToClip;
var
  s: string;
  i,x,y: Integer;
  t, MinP, MaxP: Types.TPoint;
begin
  s := '';
  MinP := Point(MaxInt, MaxInt);
  MaxP := Point(-1, -1);
  for i := 0 to SelectionCount - 1 do
  begin
    t := Selection[i];
    MinP.X := Min(MinP.X, t.X);
    MinP.Y := Min(MinP.Y, t.y);
    MaxP.X := Max(MaxP.X, t.x);
    MaxP.Y := Max(MaxP.Y, t.y);
  end;
  s := '';
  for y := MinP.Y to MaxP.Y do
  begin
    for x := MinP.X to MaxP.Y do
    begin
      if x = MinP.X then
        s := s + Cells[x,y]
      else
        s := s + #9 + Cells[x,y];
    end;
    s := s + #13#10;
  end;
  //PutTextToClip(0, s);
end;

procedure TMUIStrGrid.CutToClip;
begin
end;

procedure TMUIStrGrid.PasteFromClip;
begin
end;


procedure TMUIStrGrid.DoSetFocus(ACol, ARow: Integer);
begin
  if (ACol < FNumCols) and (ACol >= FFixedCols) and (ARow < FNumRows) and (ARow >= FFixedRows) and ((ACol <> FCol) or (ARow <> FRow))then
  begin
    if FEditMode then
    begin
      FEditMode := False;
      Cells[FCol, FRow] := FEditText;
    end;
    AddToRedraw(FCol, FRow);
    if FFixedCols > 0 then
      AddToRedraw(0, FRow);
    if FFixedRows > 0 then
      AddToRedraw(FCol, 0);
    FRow := ARow;
    FCol := ACol;
    AddToRedraw(FCol, FRow);
    if FFixedCols > 0 then
      AddToRedraw(0, FRow);
    if FFixedRows > 0 then
      AddToRedraw(FCol, 0);
    FStrings[FCol, FRow].Selected := True;
    if FSelectionList.IndexOf(ColRowToNum(FCol, FRow)) < 0 then
      FSelectionList.Add(ColRowToNum(FCol, FRow));
    if Assigned(FOnCellFocus) then
      FOnCellFocus(Self);
    DA.RedrawObject;
  end;
end;

function TMUIStrGrid.GetSelCount: Integer;
begin
  Result := FSelectionList.Count;
end;

function TMUIStrGrid.GetSelection(AIdx: Integer): Types.TPoint;
begin
  Result := Point(-1,-1);
  if InRange(AIdx, 0, FSelectionList.Count - 1) then
    Result := NumToColRow(FSelectionList[AIdx]);
end;

end.
