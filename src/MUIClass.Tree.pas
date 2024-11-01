unit MUIClass.Tree;
{$mode ObjFPC}{$H+}
interface

uses
  Classes, SysUtils, fgl, mui, AGraphics, Utility, Intuition, Math,
  MUIClass.Area,
  MUIClass.Base, MUIClass.Group, MUIClass.Gadget, MUIClass.DrawPanel;

type
  TMUITreeNode = class;
  TMUITreeView = class;

  TMUITreeNodeList = specialize TFPGObjectList<TMUITreeNode>;

  { TMUITreeNode }

  TMUITreeNode = class
  private
    FExpanded: Boolean;
    FParentNode: TMUITreeNode;
    FTextRect: TRect;
    FImgRect: TRect;
    FChilds: TMUITreeNodeList;
    FData: Pointer;
    FName: string;
    FLevel: Integer;
    FTree: TMUITreeView;
    function GetChildCount: Integer;
    function GetChilds(Idx: Integer): TMUITreeNode;
    function GetHasChilds: Boolean;
    procedure SetExpanded(AValue: Boolean);
    procedure SetName(AValue: string);
  public
    constructor Create(AParentNode: TMUITreeNode; ATree: TMUITreeView); virtual;
    destructor Destroy; override;

    property ChildCount: Integer read GetChildCount;
    property Childs[Idx: Integer]: TMUITreeNode read GetChilds;

    property Name: string read FName write SetName;
    property Data: Pointer read FData write FData;
    property Level: Integer read FLevel;
    property Tree: TMUITreeView read FTree;
    property HasChilds: Boolean read GetHasChilds;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property ParentNode: TMUITreeNode read FParentNode;
  end;

  { TMUITreeView }

  TMUITreeView = class(TMUIGroup)
  private
    FNormFont: PTextFont;
    FDontUseNorm: Boolean;
    FOnNodeClick: TNotifyEvent;
    FOnNodeDblClick: TNotifyEvent;
    FOnNodeRightClick: TNotifyEvent;
    FOnOpenContextMenu: TMUIAllowedToOpenEvent;
    FOnSelectedNode: TNotifyEvent;
    FSelectedNode: TMUITreeNode;
    FTextHeight: Integer;
    FDrawPanel: TMUIDrawPanel;
    FVScroller: TMUIScrollbar;
    FHScroller: TMUIScrollbar;
    FUpdating: Boolean;

    FAllNodes: TMUITreeNodeList;
    FNodes: TMUITreeNodeList;

    procedure DrawMe(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
    procedure DrawOpenContextMenu(Sender: TObject; x, y: Integer; var AllowedToOpen: Boolean);
    procedure FirstChange(Sender: TObject);
    function GetAllNode(Idx: Integer): TMUITreeNode;
    function GetAllNodeCount: Integer;
    function GetNode(Idx: Integer): TMUITreeNode;
    function GetNodeCount: Integer;
    procedure KeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
    procedure MouseDblEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
    procedure MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
    procedure SetSelectedNode(AValue: TMUITreeNode);
    procedure WheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
    procedure RemoveNode(ANode: TMUITreeNode);
  protected
    procedure BeforeCreateObject; override;
    procedure AfterCreateObject; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function AddNode(ParentNode: TMUITreeNode; AName: string; Data: Pointer = nil): TMUITreeNode;
    function DeleteNode(ANode: TMUITreeNode): Boolean;

    function GetNodeAtPosition(x, y: Integer): TMUITreeNode;

    procedure Redraw;

    procedure BeginUpdate;
    procedure EndUpdate;

    property SelectedNode: TMUITreeNode read FSelectedNode write SetSelectedNode;
    property NodeCount: Integer read GetNodeCount;
    property Nodes[Idx: Integer]: TMUITreeNode read GetNode;
    property AllNodeCount: Integer read GetAllNodeCount;
    property AllNodes[Idx: Integer]: TMUITreeNode read GetAllNode;

    property OnSelectedNode: TNotifyEvent read FOnSelectedNode write FOnSelectedNode;
    property OnNodeClick: TNotifyEvent read FOnNodeClick write FOnNodeClick;
    property OnNodeRightClick: TNotifyEvent read FOnNodeRightClick write FOnNodeRightClick;
    property OnNodeDblClick: TNotifyEvent read FOnNodeDblClick write FOnNodeDblClick;

    property OnOpenContextMenu: TMUIAllowedToOpenEvent read FOnOpenContextMenu write FOnOpenContextMenu;
  end;

implementation

{ TMUITreeView }

procedure TMUITreeView.DrawMe(Sender: TObject; Rp: PRastPort; DrawRect: TRect);
var
  TE: TTextExtent;
  XStart, y, YStart: Integer;
  OldFont: pTextFont;
  LocalRP: PRastPort;
  LocalRect: TRect;
  DB: TDrawBuffer;
  TextAttr: TTextAttr;
  MaxX: Integer;

  procedure DrawChilds(LocalIndent: Integer; NodeList: TMUITreeNodeList);
  var
    i: Integer;
    LastY: Integer;
    Node: TMUITreeNode;
    LastHadChild: Boolean;
    TxtLen: Integer;
  begin
    LastHadChild := False;
    LastY := (y - FTextHeight) + 2;
    for i := 0 to NodeList.Count - 1 do
    begin
      Node := NodeList[i];
      if LastHadChild then
        GFXMove(LocalRP, XStart + LocalRect.Left + LocalIndent - 15, LocalRect.Top + LastY + 3)
      else
        GFXMove(LocalRP, XStart + LocalRect.Left + LocalIndent - 15, LocalRect.Top + LastY);
      LastY := y - FTextHeight div 4 - 1;
      Draw(LocalRP,XStart + LocalRect.Left + LocalIndent - 15, LocalRect.Top + LastY);
      Draw(LocalRP,XStart + LocalRect.Left + LocalIndent - 5, LocalRect.Top + LastY);
      GFXMove(LocalRP, XStart + LocalRect.Left + LocalIndent, LocalRect.Top + y);
      if Node = FSelectedNode then
        SetABPenDrMd(Localrp, 2, 3, Jam2)
      else
        SetABPenDrMd(Localrp, 1, 3, Jam1);
      GfxText(Localrp, PChar(Node.Name), Length(Node.Name));
      SetABPenDrMd(Localrp, 1, 3, Jam1);
      TxtLen := TextLength(Localrp, PChar(Node.Name), Length(Node.Name));
      Node.FTextRect := Rect(XStart + LocalIndent, y - FTextHeight, XStart + LocalIndent + TxtLen, y);
      MaxX := Max(MaxX, LocalIndent + TxtLen);
      y := y + FTextHeight;
      Node.FImgRect := TRect.Empty;
      LastHadChild := Node.HasChilds;
      if Node.HasChilds then
      begin
        Node.FImgRect := Node.FTextRect;
        Node.FImgRect := Rect(XStart + LocalIndent - 19, Node.FTextRect.CenterPoint.Y - 2, XStart + LocalIndent - 11, Node.FTextRect.CenterPoint.Y + 6);
        SetAPen(LocalRP, 1);
        RectFill(LocalRP, LocalRect.Left + Node.FImgRect.Left, LocalRect.Top + Node.FImgRect.Top, LocalRect.Left + Node.FImgRect.Right, LocalRect.Top + Node.FImgRect.Bottom);
        SetAPen(LocalRP, 0);
        RectFill(LocalRP, LocalRect.Left + Node.FImgRect.Left + 1, LocalRect.Top + Node.FImgRect.Top + 1, LocalRect.Left + Node.FImgRect.Right - 1, LocalRect.Top + Node.FImgRect.Bottom - 1);
        SetAPen(LocalRP, 1);
        GfxMove(LocalRP, LocalRect.Left + Node.FImgRect.Left + 2, LocalRect.Top + Node.FImgRect.CenterPoint.Y);
        Draw(LocalRP, LocalRect.Left + Node.FImgRect.Right - 2, LocalRect.Top + Node.FImgRect.CenterPoint.Y);
        if Node.Expanded then
        begin
          DrawChilds(LocalIndent + 20, Node.FChilds);
        end
        else
        begin
          GfxMove(LocalRP, LocalRect.Left + Node.FImgRect.CenterPoint.X, LocalRect.Top + Node.FImgRect.Top + 2);
          Draw(LocalRP, LocalRect.Left + Node.FImgRect.CenterPoint.X, LocalRect.Top + Node.FImgRect.Bottom - 2);
        end;
      end;
    end;
  end;

begin
  DB := TDrawBuffer.Create(DrawRect.Width, DrawRect.Height, RP^.Bitmap^.Depth);
  try
    LocalRP := DB.RP;
    DB.Clear(0);
    LocalRect := DrawRect;
    LocalRect.Left := 0;
    LocalRect.Top := 0;
    if not FDontUseNorm and not Assigned(FNormFont) then
    begin
      FNormFont := OpenMUIFont(fkNormal);
      if not Assigned(FNormFont) then
      begin
        if Assigned(RP^.Font) then
        begin
          TextAttr.ta_YSize := RP^.Font^.tf_YSize;
          TextAttr.ta_Name := RP^.Font^.tf_Message.mn_Node.ln_Name;
          TextAttr.ta_Flags := FPF_ROMFONT or FPF_DISKFONT;
          TextAttr.ta_Style := FS_NORMAL;
          FNormFont := OpenFont(@TextAttr);
        end;
        FDontUseNorm := not Assigned(FNormFont);
      end;
    end;
    OldFont := LocalRP^.Font;
    if Assigned(FNormFont) then
      LocalRP^.Font := FNormFont
    else
      LocalRP^.Font := RP^.Font;
    SetABPenDrMd(Localrp, 1, 3, Jam1);
    TextExtent(LocalRP, 'Wp', 2, @TE);
    FTextHeight := Round(TE.te_Height * 1.2);
    YStart := FVScroller.First;
    Y := FTextHeight - YStart;
    XStart := -FHScroller.First;
    MaxX := 20;
    DrawChilds(20, FNodes);
    FVScroller.Entries := y + YStart + FTextHeight;
    FVScroller.Visible := LocalRect.Height;
    //
    FHScroller.Entries := MaxX + 20;
    FHScroller.Visible := LocalRect.Width;
    LocalRP^.Font := OldFont;
    DB.DrawToRastPort(DrawRect.Left, DrawRect.Top,RP)
  finally
    DB.Free;
  end;
end;

procedure TMUITreeView.DrawOpenContextMenu(Sender: TObject; x, y: Integer; var AllowedToOpen: Boolean);
begin
  if Assigned(FOnOpenContextMenu) then
    FOnOpenContextMenu(Self, x - FDrawPanel.LeftEdge, y - FDrawPanel.TopEdge, AllowedToOpen);
end;

procedure TMUITreeView.FirstChange(Sender: TObject);
begin
  Redraw;
end;

function TMUITreeView.GetAllNode(Idx: Integer): TMUITreeNode;
begin
  Result := nil;
  if InRange(Idx, 0, FAllNodes.Count - 1) then
    Result := FAllNodes[Idx];
end;

function TMUITreeView.GetAllNodeCount: Integer;
begin
  Result := FAllNodes.Count;
end;

function TMUITreeView.GetNode(Idx: Integer): TMUITreeNode;
begin
  Result := nil;
  if InRange(Idx, 0, FNodes.Count - 1) then
    Result := FNodes[Idx];
end;

function TMUITreeView.GetNodeCount: Integer;
begin
  Result := FNodes.Count;
end;

procedure TMUITreeView.KeyDown(Sender: TObject; Shift: TMUIShiftState; Code: Word; Key: Char; var EatEvent: Boolean);
var
  Idx: Integer;
begin
  Unused(Key);
  EatEvent := True;
  case code of
    76: begin // up
      if mssShift in Shift then
      begin
        FVScroller.First := Max(0 , FVScroller.First - 1);
      end
      else
      begin
        Idx := FAllNodes.IndexOf(FSelectedNode);
        if Idx > 0 then
          SelectedNode := FAllNodes[Idx - 1];
      end;
    end;
    77: begin // down
      if mssShift in Shift then
      begin
        FVScroller.First := FVScroller.First + 1;
      end
      else
      begin
        Idx := FAllNodes.IndexOf(FSelectedNode);
        if (Idx >= 0) and (Idx < FAllNodes.Count - 1) then
          SelectedNode := FAllNodes[Idx + 1];
      end;
    end;
    78: begin // Right
      if mssShift in Shift then
      begin
        FHScroller.First := FHScroller.First + 1;
      end
      else
        if Assigned(FSelectedNode) and FSelectedNode.HasChilds and not FSelectedNode.Expanded then
        begin
          FSelectedNode.Expanded := True;
          Redraw;
        end;
    end;
    79: begin // left
      if mssShift in Shift then
      begin
        FHScroller.First := Max(0 , FHScroller.First - 1);
      end
      else
        if Assigned(FSelectedNode) and FSelectedNode.HasChilds and FSelectedNode.Expanded then
        begin
          FSelectedNode.Expanded := False;
          Redraw;
        end;
    end;
  end;
end;

procedure TMUITreeView.WheelEvent(Sender: TObject; ScrollUp: Boolean; var EatEvent: Boolean);
begin
  EatEvent := True;
  if ScrollUp then
    FVScroller.First := FVScroller.First - FTextHeight
  else
    FVScroller.First := FVScroller.First + FTextHeight;
end;

procedure TMUITreeView.RemoveNode(ANode: TMUITreeNode);
begin
  FAllNodes.Remove(ANode);
  if FSelectedNode = ANode then
    SelectedNode := nil;
end;

procedure TMUITreeView.BeforeCreateObject;
begin
  inherited BeforeCreateObject;
  Self.FDrawPanel.ContextMenu := ContextMenu;
end;

procedure TMUITreeView.AfterCreateObject;
begin
  inherited AfterCreateObject;
end;

procedure TMUITreeView.MouseDblEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
begin

  if Assigned(FSelectedNode) and Assigned(FOnNodeDblClick) and (MouseBtn = mmbLeft) then
  begin
    if FSelectedNode.FTextRect.Contains(Point(x,y)) then
    begin
      FOnNodeDblClick(Self);
      EatEvent := True;
    end;
  end;
end;

procedure TMUITreeView.MouseDownEvent(Sender: TObject; MouseBtn: TMUIMouseBtn; X, Y: Integer; var EatEvent: Boolean);
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
      if Node.HasChilds and Node.FImgRect.Contains(Point(x,y)) then
      begin
        Node.Expanded := not Node.Expanded;
        Found := True;
        Exit;
      end;
      //
      if Node.FTextRect.Contains(Point(x,y)) then
      begin
        SelectedNode := Node;
        if MouseBtn = mmbLeft then
        begin
          if Assigned(FOnNodeClick) then
            FOnNodeClick(Self);
        end;
        if MouseBtn = mmbRight then
        begin
          if Assigned(FOnNodeRightClick) then
            FOnNodeRightClick(Self);
        end;
        Found := True;
        Exit;
      end;
      //
      if Node.Expanded then
        CheckClick(Node.FChilds);
      if Found then
        Exit;
    end;
  end;
begin
  Found := False;
  CheckClick(FNodes);
  EatEvent := True;
  Redraw;
end;

procedure TMUITreeView.SetSelectedNode(AValue: TMUITreeNode);
begin
  if FSelectedNode = AValue then Exit;
  FSelectedNode := AValue;

  if Assigned(FSelectedNode) then
  begin
    if FSelectedNode.FTextRect.Top < 0 then
      FVScroller.First := FVScroller.First + FSelectedNode.FTextRect.Top
    else
    if FSelectedNode.FTextRect.Bottom > FDrawPanel.Height then
      FVScroller.First := FVScroller.First + FSelectedNode.FTextRect.Top - (FDrawPanel.Height - 2 * FTextHeight)
  end;
  if Assigned(FOnSelectedNode) then
      FOnSelectedNode(Self);
  Redraw;
end;

function TMUITreeView.AddNode(ParentNode: TMUITreeNode; AName: string; Data: Pointer): TMUITreeNode;
var
  NNode: TMUITreeNode;
begin
  NNode := TMUITreeNode.Create(ParentNode, Self);
  NNode.Name := AName;
  NNode.Data := Data;
  if Assigned(ParentNode) then
  begin
    ParentNode.FChilds.Add(NNode);
    NNode.FLevel := ParentNode.Level + 1;
  end
  else
    FNodes.Add(NNode);
  FAllNodes.Add(NNode);
  Result := NNode;
  Redraw;
end;

function TMUITreeView.DeleteNode(ANode: TMUITreeNode): Boolean;
begin
  Result := False;
  if not Assigned(ANode) then
    Exit;
  if Assigned(ANode.FParentNode) then
    ANode.FParentNode.FChilds.Remove(ANode)
  else
    FNodes.Remove(ANode);
  Result := True;
  Redraw;
end;

function TMUITreeView.GetNodeAtPosition(x, y: Integer): TMUITreeNode;
var
  i: Integer;
  Node: TMUITreeNode;
begin
  Result := nil;
  for i := 0 to FAllNodes.Count - 1 do
  begin
    Node := FAllNodes[i];
    if Node.FTextRect.Contains(Point(x,y)) then
    begin
      Result := Node;
      Exit;
    end;
  end;
end;

constructor TMUITreeView.Create;
var
  Grp: TMUIGroup;
begin
  inherited Create;
  FDontUseNorm := False;
  FUpdating := False;
  FNormFont := nil;
  FSelectedNode := nil;
  FNodes := TMUITreeNodeList.Create(True);
  FAllNodes := TMUITreeNodeList.Create(False);
  HorizSpacing := 0;
  VertSpacing := 0;
  Horiz := False;
  Frame := MUIV_Frame_None;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Frame := MUIV_Frame_None;
    HorizSpacing := 0;
    VertSpacing := 0;
    Horiz := True;
    Parent := Self;
  end;

  FDrawPanel := TMUIDrawPanel.Create;
  with FDrawPanel do
  begin
    Frame := MUIV_Frame_None;
    MinHeight := 50;
    MinWidth := 50;
    DefHeight := 200;
    DefWidth := 200;
    MaxHeight := MUI_MAXMAX;
    MaxWidth := MUI_MAXMAX;
    FillArea := False;
    OnDrawObject  := @DrawMe;
    OnMouseDown  := @MouseDownEvent;
    OnDblClick  := @MouseDblEvent;
    OnKeyDown  := @KeyDown;
    OnMouseWheel := @WheelEvent;
    OnOpenContextMenu  := @DrawOpenContextMenu;
    Parent := Grp;
  end;
  FVScroller := TMUIScrollbar.Create;
  with FVScroller do
  begin
    Frame := MUIV_Frame_None;
    Horiz := False;
    Parent := Grp;
    OnFirstChange  := @FirstChange;
  end;

  FHScroller := TMUIScrollbar.Create;
  with FHScroller do
  begin
    Frame := MUIV_Frame_None;
    Horiz := True;
    Parent := Self;
    OnFirstChange  := @FirstChange;
  end;
end;

destructor TMUITreeView.Destroy;
begin
  FDrawPanel.ContextMenu := nil;
  CloseMUIFont(FNormFont);
  FNodes.Clear;
  FNodes.Free;
  FAllNodes.Free;
  inherited Destroy;
end;

procedure TMUITreeView.Redraw;
begin
  if not FUpdating then
    FDrawPanel.RedrawObject;
end;

procedure TMUITreeView.BeginUpdate;
begin
  FUpdating := True;
end;

procedure TMUITreeView.EndUpdate;
begin
  FUpdating := False;
  Redraw;
end;

{ TMUITreeNode }

function TMUITreeNode.GetHasChilds: Boolean;
begin
  Result := FChilds.Count > 0;
end;

function TMUITreeNode.GetChildCount: Integer;
begin
  Result := FChilds.Count;
end;

function TMUITreeNode.GetChilds(Idx: Integer): TMUITreeNode;
begin
  Result := nil;
  if InRange(Idx, 0, FChilds.Count - 1) then
    Result := FChilds[Idx];
end;

procedure TMUITreeNode.SetExpanded(AValue: Boolean);
begin
  if FExpanded = AValue then
    Exit;
  FExpanded := AValue;
  FTree.Redraw;
end;

procedure TMUITreeNode.SetName(AValue: string);
begin
  if FName = AValue then
    Exit;
  FName := AValue;
  FTree.Redraw;
end;

constructor TMUITreeNode.Create(AParentNode: TMUITreeNode; ATree: TMUITreeView);
begin
  FTree := ATree;
  FChilds := TMUITreeNodeList.Create(True);
  FExpanded := True;
  FParentNode := AParentNode;
  FLevel := 0;
end;

destructor TMUITreeNode.Destroy;
begin
  FTree.RemoveNode(Self);
  FChilds.Free;
  inherited Destroy;
end;

end.

