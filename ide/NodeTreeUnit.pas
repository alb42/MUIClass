unit NodeTreeUnit;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL;

type
  TItemNode = class;
  TItemTree = class;

  TItemNodes = specialize TFPGObjectList<TItemNode>;

  TItemNode = class
  private
    FName: string;
    Childs: TItemNodes;
    TopNode: TItemTree;
    FParent: TItemNode;
    FData: TObject;
    FGlobalIdx: Integer;
    function GetCount: Integer;
    function GetChild(Idx: Integer): TItemNode;
  protected
    procedure SetNodesText(Indent: string; SL: TStringList; LastEntry: Boolean); virtual;
  public
    constructor Create(ATopNode: TItemTree); virtual;
    destructor Destroy; override;
    //
    function NewChild(AName: string; AData: TObject = nil): TItemNode;
    function ChildByName(AName: string): Integer; virtual;
    property Name: string read FName write FName;
    property Parent: TItemNode read FParent;
    property Data: TObject read FData write FData;
    property Count: Integer read GetCount;
    property Child[Idx: Integer]: TItemNode read GetChild; default;
    property GlobalIdx: Integer read FGlobalIdx;
  end;

  TItemTree = class(TItemNode)
  private
    AllChilds: TItemNodes;
    function GetAllCount: Integer;
    function GetAllChild(Idx: Integer): TItemNode;

  public
    NodesText: TStringList;
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    procedure UpdateNodesText;
    function AllChildByName(AName: string): Integer;
    property AllCount: Integer read GetAllCount;
    property AllChild[Idx: Integer]: TItemNode read GetAllChild;
  end;


implementation

constructor TItemNode.Create(ATopNode: TItemTree);
begin
  TopNode := ATopNode;
  Childs := TItemNodes.Create(False);
end;

destructor TItemNode.Destroy;
var
  Item: TItemNode;
begin
  if Assigned(Parent) then
    Parent.Childs.Remove(Self);
  while Childs.Count > 0 do
  begin
    Item := Childs[0];
    Childs.Delete(0);
    Item.Free;
  end;
  Childs.Clear;
  Childs.Free;
  if Assigned(TopNode) and Assigned(TopNode.AllChilds) then
  begin
    TopNode.AllChilds.Remove(Self);
    TopNode.UpdateNodesText;
  end;
  inherited;
end;

function TItemNode.NewChild(AName: string; AData: TObject = nil): TItemNode;
begin
  Result := TItemNode.Create(TopNode);
  Result.Name := AName;
  Result.Data := AData;
  Result.FParent := Self;
  Childs.Add(Result);
  if Assigned(TopNode) then
  begin
    TopNode.AllChilds.Add(Result);
    TopNode.UpdateNodesText;
  end;
end;

function TItemNode.ChildByName(AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for i := 0 to Childs.Count - 1 do
  begin
    if Childs[i].Name = AName then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function TItemNode.GetCount: Integer;
begin
  Result := Childs.Count;
end;

function TItemNode.GetChild(Idx: Integer): TItemNode;
begin
  Result := nil;
  if (Idx >= 0) and (Idx < Childs.Count) then
    Result := Childs[Idx];
end;

procedure TItemNode.SetNodesText(Indent: string; SL: TStringList; LastEntry: Boolean);
var
  I: Integer;
begin
  FGlobalIdx := SL.Count;
  if Assigned(FData) then
    SL.Add(Indent + FName + ' (' + FData.ClassName + ')')
  else
    SL.Add(Indent + FName + ' ');
  if LastEntry then
    Indent := stringreplace(Indent, '  +-', '    ', [rfReplaceAll])
  else
    Indent := stringreplace(Indent, '  +-', '  | ', [rfReplaceAll]);
  for i := 0 to Childs.Count - 1 do
  begin
    Childs[i].SetNodesText(Indent + '  +-', SL, i = Childs.Count - 1 )
  end;
end;

{ TItemTree }

constructor TItemTree.Create;
begin
  inherited Create(Self);
  AllChilds := TItemNodes.Create(False);
  AllChilds.Add(Self);
  NodesText := TStringList.Create;
end;

destructor TItemTree.Destroy;
begin
  AllChilds.Clear;
  AllChilds.Free;
  AllChilds := nil;
  NodesText.Free;
  NodesText := nil;
  inherited;
end;

function TItemTree.GetAllCount: Integer;
begin
  Result := AllChilds.Count;
end;

function TItemTree.GetAllChild(Idx: Integer): TItemNode;
begin
  Result := nil;
  if (Idx >= 0) and (Idx < AllChilds.Count) then
    Result := AllChilds[Idx];
end;

function ChildCompare(const Item1, Item2: TItemNode): Integer;
begin
  Result := Item1.GlobalIdx - Item2.GlobalIdx;
end;

procedure TItemTree.UpdateNodesText;
begin
  if not Assigned(NodesText) then
    Exit;
  NodesText.Clear;
  SetNodesText('', NodesText, True);
  AllChilds.Sort(@ChildCompare);
end;

function TItemTree.AllChildByName(AName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for i := 0 to AllChilds.Count - 1 do
  begin
    if LowerCase(AllChilds[i].Name) = LowerCase(AName) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

end.
