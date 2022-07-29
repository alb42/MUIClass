program CustomTree;

uses
  SysUtils,
  MUIClass.Base, MUIClass.Window, MUIClass.Tree,
  MUIClass.Group, MUIClass.Area;


type

  { TTreeTestWin }

  TTreeTestWin = class(TMUIWindow)
    Tree: TMUICustomTree;
    constructor Create; override;
  end;

{ TTreeTestWin }

constructor TTreeTestWin.Create;
var
  Grp: TMUIGroup;
  Node: TMUITreeNode;
  i: Integer;
begin
  inherited Create;
  Horizontal := True;

  //Grp := TMUIGroup.Create;
  //Grp.Parent := Self;
  //
  Tree := TMUICustomTree.Create;
  Tree.MaxWidth := 1000;
  Tree.MaxHeight := 10000;
  Tree.Parent := Self;

  Node := Tree.AddNode(nil, 'testnode');
  Tree.AddNode(Node, 'child1');
  Node := Tree.AddNode(Node, 'child2');
  Tree.AddNode(Node, 'subchild1');
  Tree.SelectedNode := Tree.AddNode(Node, 'subchild2');
  Tree.AddNode(Node, 'subchild3');
  for i := 0 to 20 do
    Tree.AddNode(Node, 'subchild' + IntToStr(4+i));
  //TMUIRectangle.Create.Parent := Grp;

  //TMUIButton.Create('test').Parent := Grp;
  //TMUIRectangle.Create.Parent := Self;
end;

begin
  TTreeTestWin.Create;
  //
  MUIApp.Run;
end.

