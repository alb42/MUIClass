unit MUIClass.Tree;
{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, fgl, mui, AGraphics, Math,
  MUIClass.Group, MUIClass.Gadget, MUIClass.DrawPanel;

type
  TMUITreeNode = class;

  TMUITreeNodeList = specialize TFPGObjectList<TMUITreeNode>;

  { TMUITreeNode }

  TMUITreeNode = class
  private
    FExpanded: Boolean;
    TextRect: TRect;
    ImgRect: TRect;
    function GetHasChilds: Boolean;
    procedure SetExpanded(AValue: Boolean);
  public
    Name: string;
    Data: Pointer;
    Childs: TMUITreeNodeList;
    constructor Create; virtual;
    destructor Destroy; override;

    property HasChilds: Boolean read GetHasChilds;
    property Expanded: Boolean read FExpanded write SetExpanded;
  end;



  { TMUICustomTree }

  TMUICustomTree = class(TMUIGroup)
  private
    FSelectedNode: TMUITreeNode;
    TH: LongWord;
    FDrawPanel: TMUIDrawPanel;
    FScroller: TMUIScrollbar;
    procedure DrawMe(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure FirstChange(Sender: TObject);
    procedure KeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
    procedure SetSelectedNode(AValue: TMUITreeNode);
  public
    AllNodes: TMUITreeNodeList;
    Nodes: TMUITreeNodeList;

    function AddNode(ParentNode: TMUITreeNode; AName: string; Data: Pointer = nil): TMUITreeNode;

    constructor Create; override;
    destructor Destroy; override;

    property SelectedNode: TMUITreeNode read FSelectedNode write SetSelectedNode;
  end;


implementation

{ TMUICustomTree }

procedure TMUICustomTree.DrawMe(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  TE: TTextExtent;
  y, YStart: Integer;

  procedure DrawChilds(Ident: Integer; NodeList: TMUITreeNodeList);
  var
    i: Integer;
    LastY: Integer;
    Node: TMUITreeNode;
  begin
    LastY := (y - TH) + 2;
    for i := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[i];
      GFXMove(RP, DrawRect.Left + Ident - 15, DrawRect.Top + LastY);
      LastY := y - TH div 4;
      Draw(RP,DrawRect.Left + Ident - 15, DrawRect.Top + LastY);
      Draw(RP,DrawRect.Left + Ident - 5, DrawRect.Top + LastY);
      GFXMove(RP, DrawRect.Left + Ident, DrawRect.Top + y);
      if Node = FSelectedNode then
        SetABPenDrMd(rp, 2, 3, Jam2)
      else
        SetABPenDrMd(rp, 1, 3, Jam1);
      GfxText(rp, PChar(Node.Name), Length(Node.Name));
      SetABPenDrMd(rp, 1, 3, Jam1);
      Node.TextRect := Rect(Ident, y - TH, Ident + TextLength(rp, PChar(Node.Name), Length(Node.Name)), y);
      y := y + TH;
      Node.ImgRect := TRect.Empty;
      if Node.HasChilds then
      begin
        Node.ImgRect := Node.TextRect;
        Node.ImgRect := Rect(Ident - 19, Node.TextRect.CenterPoint.Y - 2, Ident - 11, Node.TextRect.CenterPoint.Y + 6);
        SetAPen(RP, 1);
        RectFill(RP, DrawRect.Left + Node.ImgRect.Left, DrawRect.Top + Node.ImgRect.Top, DrawRect.Left + Node.ImgRect.Right, DrawRect.Top + Node.ImgRect.Bottom);
        SetAPen(RP, 0);
        RectFill(RP, DrawRect.Left + Node.ImgRect.Left + 1, DrawRect.Top + Node.ImgRect.Top + 1, DrawRect.Left + Node.ImgRect.Right - 1, DrawRect.Top + Node.ImgRect.Bottom - 1);
        SetAPen(RP, 1);
        GfxMove(RP, DrawRect.Left + Node.ImgRect.Left + 2, DrawRect.Top + Node.ImgRect.CenterPoint.Y);
        Draw(RP, DrawRect.Left + Node.ImgRect.Right - 2, DrawRect.Top + Node.ImgRect.CenterPoint.Y);
        if Node.Expanded then
        begin
          DrawChilds(Ident + 20, Node.Childs);
        end
        else
        begin
          GfxMove(RP, DrawRect.Left + Node.ImgRect.CenterPoint.X, DrawRect.Top + Node.ImgRect.Top + 2);
          Draw(RP, DrawRect.Left + Node.ImgRect.CenterPoint.X, DrawRect.Top + Node.ImgRect.Bottom - 2);
        end;
      end;
    end;
  end;

begin
  SetABPenDrMd(rp, 1, 3, Jam1);
  TextExtent(RP, 'Wp', 2, @TE);
  TH := Round(TE.te_Height * 1.2);
  YStart := FScroller.First;
  Y := TH - YStart;
  DrawChilds(20, Nodes);
  FScroller.Entries := y + YStart;
  FScroller.Visible := DrawRect.Height;

end;

procedure TMUICustomTree.FirstChange(Sender: TObject);
begin
  Self.FDrawPanel.RedrawObject;
end;

procedure TMUICustomTree.KeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
var
  Idx: Integer;
begin
  EatEvent := True;
  //writeln(' Code =  ', Code);
  case code of
    76: begin // up
      Idx := AllNodes.IndexOf(FSelectedNode);
      if Idx > 0 then
        SelectedNode := AllNodes[Idx - 1];
    end;
    77: begin // down
      Idx := AllNodes.IndexOf(FSelectedNode);
      if (Idx >= 0) and (Idx < AllNodes.Count - 1) then
        SelectedNode := AllNodes[Idx + 1];
    end;
    78: begin // Right
      if Assigned(FSelectedNode) and FSelectedNode.HasChilds and not FSelectedNode.Expanded then
      begin
        FSelectedNode.Expanded := True;
        Self.FDrawPanel.RedrawObject;
      end;
    end;
    79: begin // left
      if Assigned(FSelectedNode) and FSelectedNode.HasChilds and FSelectedNode.Expanded then
      begin
        FSelectedNode.Expanded := False;
        Self.FDrawPanel.RedrawObject;
      end;
    end;
  end;
  //if Code =
end;

procedure TMUICustomTree.MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
var
  Found: Boolean;

  procedure CheckClick(NodeList: TMUITreeNodeList);
  var
    i: Integer;
    Node: TMUITreeNode;
  begin
    for i := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[i];
      if Node.HasChilds and Node.ImgRect.Contains(Point(x,y)) then
      begin
        Node.Expanded := not Node.Expanded;
        Found := True;
        Exit;
      end;
      //
      if Node.TextRect.Contains(Point(x,y)) then
      begin
        SelectedNode := Node;
        Found := True;
        Exit;
      end;
      //
      if Node.Expanded then
        CheckClick(Node.Childs);
      if Found then
        Exit;

    end;
  end;
begin
  Found := False;
  CheckClick(Nodes);
  Self.FDrawPanel.RedrawObject;
end;

procedure TMUICustomTree.SetSelectedNode(AValue: TMUITreeNode);
begin
  if FSelectedNode = AValue then Exit;
  FSelectedNode := AValue;

  if Assigned(FSelectedNode) then
  begin
    if FSelectedNode.TextRect.Top < 0 then
      FScroller.First := FScroller.First + FSelectedNode.TextRect.Top
    else
    if FSelectedNode.TextRect.Bottom > FDrawPanel.Height then
    begin
      FScroller.First := FScroller.First + FSelectedNode.TextRect.Top - (FDrawPanel.Height - 2 * TH)
    end;

  end;

  Self.FDrawPanel.RedrawObject;
end;

function TMUICustomTree.AddNode(ParentNode: TMUITreeNode; AName: string; Data: Pointer): TMUITreeNode;
var
  NNode: TMUITreeNode;
begin
  NNode := TMUITreeNode.Create;
  NNode.Name := AName;
  NNode.Data := Data;
  if Assigned(ParentNode) then
  begin
    ParentNode.Childs.Add(NNode);
  end
  else
  begin
    Nodes.Add(NNode);
  end;
  AllNodes.Add(NNode);
  Result := NNode;
end;

constructor TMUICustomTree.Create;
begin
  inherited Create;
  FSelectedNode := nil;
  Nodes := TMUITreeNodeList.Create(False);
  AllNodes := TMUITreeNodeList.Create(True);
  Horiz := True;
  Frame := MUIV_Frame_None;
  FDrawPanel := TMUIDrawPanel.Create;
  with FDrawPanel do
  begin
    Frame := MUIV_Frame_None;
    MinHeight := 100;
    MinWidth := 100;
    DefHeight := 200;
    DefWidth := 200;
    MaxHeight := MUI_MAXMAX;
    MaxWidth := MUI_MAXMAX;
    FillArea := True;
    OnDrawObject  := @DrawMe;
    OnMouseDown  := @MouseDownEvent;
    OnKeyDown  := @KeyDown;
    Parent := Self;
  end;
  FScroller := TMUIScrollbar.Create;
  with FScroller do
  begin
    Frame := MUIV_Frame_None;
    Horiz := False;
    Parent := Self;
    OnFirstChange  := @FirstChange;
  end;

end;

destructor TMUICustomTree.Destroy;
begin
  Nodes.Free;
  AllNodes.Free;
  inherited Destroy;
end;


{ TMUITreeNode }

function TMUITreeNode.GetHasChilds: Boolean;
begin
  Result := Childs.Count > 0;
end;

procedure TMUITreeNode.SetExpanded(AValue: Boolean);
begin
  if FExpanded = AValue then
    Exit;
  FExpanded := AValue;
end;

constructor TMUITreeNode.Create;
begin
  Childs := TMUITreeNodeList.Create(False);
  FExpanded := True;
end;

destructor TMUITreeNode.Destroy;
begin
  Childs.Free;
  inherited Destroy;
end;

end.

