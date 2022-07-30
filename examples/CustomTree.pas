program CustomTree;
{$mode objfpc}{$H+}
uses
  SysUtils,
  MUIClass.Base, MUIClass.Window, MUIClass.Tree,
  MUIClass.Group, MUIClass.Area, MUIClass.Gadget;


type

  { TTreeTestWin }

  TTreeTestWin = class(TMUIWindow)
    Tree: TMUICustomTree;
    SelText: TMUIText;
    NewText: TMUIString;
    constructor Create; override;
  private
    procedure AddNode(Sender: TObject);
    procedure RemoveNode(Sender: TObject);
    procedure SelectedNodeChanged(Sender: TObject);
  end;

{ TTreeTestWin }

constructor TTreeTestWin.Create;
var
  Grp, Grp2: TMUIGroup;
  Node: TMUITreeNode;
  i: Integer;
begin
  inherited Create;
  Horizontal := True;

  Grp := TMUIGroup.Create;
  Grp.Horiz := True;
  Grp.Parent := Self;
  //
  Tree := TMUICustomTree.Create;
  Tree.MaxWidth := 1000;
  Tree.MaxHeight := 10000;
  Tree.OnSelectedNode  := @SelectedNodeChanged;
  Tree.Parent := Grp;

  Grp2 := TMUIGroup.Create;
  Grp2.Parent := Grp;

  SelText :=  TMUIText.Create;
  SelText.Parent := Grp2;

  NewText := TMUIString.Create;
  NewText.Parent := Grp2;

  with TMUIButton.Create('Add') do
  begin
    OnClick  := @AddNode;
    Parent := Grp2;
  end;
  with TMUIButton.Create('Remove') do
  begin
    OnClick   := @RemoveNode;
    Parent := Grp2;
  end;

  TMUIRectangle.Create.Parent := Grp2;


  Tree.BeginUpdate;
  Node := Tree.AddNode(nil, 'testnode');
  Tree.AddNode(Node, 'child1');
  Node := Tree.AddNode(Node, 'child2');
  Tree.AddNode(Node, 'subchild1');
  Tree.SelectedNode := Tree.AddNode(Node, 'subchild2');
  Tree.AddNode(Node, 'subchild3');
  for i := 0 to 20 do
    Tree.AddNode(Node, 'subchild' + IntToStr(4+i));
  Tree.EndUpdate;
end;

procedure TTreeTestWin.AddNode(Sender: TObject);
begin
  Tree.AddNode(Tree.SelectedNode, NewText.Contents);
end;

procedure TTreeTestWin.RemoveNode(Sender: TObject);
begin
  if Assigned(Tree.SelectedNode) then
    Tree.DeleteNode(Tree.SelectedNode);
end;

procedure TTreeTestWin.SelectedNodeChanged(Sender: TObject);
begin
  if Assigned(Tree.SelectedNode) then
    SelText.Contents := Tree.SelectedNode.Name
  else
    SelText.Contents := 'nil';
end;

begin
  TTreeTestWin.Create;
  //
  MUIApp.Run;
end.

