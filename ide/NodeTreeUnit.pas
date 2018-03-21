unit NodeTreeUnit;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL, typinfo,
  MUIClass.Group;

const
  ProjectExtension = '.miprj';

type
    // If an Eventhandler is specified we save the contents
  // because we cant connect it to the object itself like other properties
  TEventHandler = class
    Name: string;    // name of Event On...
    Event: string;   // Name of the EventHandler
    PlainHeader: string; // Plain Header
    Header: string;  // Full Header of the EventHandler
    Text: string;    // Ful Text of the Eventhandler as given by user
  end;
  TEventHandlers = specialize TFPGObjectList<TEventHandler>;

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
    FProps: TStringList;
    FGlobalIdx: Integer;
    FEvents: TEventHandlers;
    FParentIdent: string;
    function GetCount: Integer;
    function GetChild(Idx: Integer): TItemNode;
  protected
    procedure SetNodesText(Indent: string; SL: TStringList; LastEntry: Boolean); virtual;
  public
    constructor Create(ATopNode: TItemTree); virtual;
    destructor Destroy; override;
    //
    function NewChild(AName: string; AData: TObject = nil): TItemNode;
    function NewOtherChild(AIdent: string; AName: string; AData: TObject = nil): TItemNode;
    function ChildByName(AName: string): Integer; virtual;
    property Name: string read FName write FName;
    property Parent: TItemNode read FParent;
    property Data: TObject read FData write FData;
    property Properties: TStringList read FProps;
    property Events: TEventHandlers read FEvents;
    property Count: Integer read GetCount;
    property Child[Idx: Integer]: TItemNode read GetChild; default;
    property GlobalIdx: Integer read FGlobalIdx;
    property ParentIdent: string read FParentIdent;
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

    function SaveToFile(AFileName: string): Boolean;
    function LoadFromFile(AFileName: string): Boolean;
  end;


implementation

uses
  DOM, XMLWrite, XMLRead, MUICompUnit;

constructor TItemNode.Create(ATopNode: TItemTree);
begin
  TopNode := ATopNode;
  Childs := TItemNodes.Create(False);
  FProps := TStringList.Create;
  FEvents := TEventHandlers.Create(True);
  FParentIdent := '';
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
  FProps.Free;
  FEvents.Free;
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

function TItemNode.NewOtherChild(AIdent: string; AName: string; AData: TObject = nil): TItemNode;
begin
  Result := TItemNode.Create(TopNode);
  Result.FParentIdent := AIdent;
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
  if ParentIdent <> '' then
    Indent := Indent + #27 + 'i';
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

function TItemTree.SaveToFile(AFileName: string): Boolean;
var
  Doc: TXMLDocument;
  Root: TDOMNode;

  procedure PutProperties(ParentNode: TDOMNode; ItemNode: TItemNode);
  var
    Obj: TObject;
    PP : PPropList;
    PT : PTypeData;
    PI : PTypeInfo;
    i, j: Integer;
    Node, SaNode: TDOMNode;
    UseSa: Boolean;
    Sa: TStringArray;
  begin
    Obj := ItemNode.Data; // Object to inspect
    PI := Obj.ClassInfo;
    PT := GetTypeData(PI);
    GetMem (PP, PT^.PropCount * SizeOf(Pointer));
    J := GetPropList(PI, tkAny, PP);  // List of published properties
    for I := 0 to J - 1 do
    begin
      with PP^[i]^ do
      begin
        if ItemNode.Properties.IndexOf(Name) < 0 then
          Continue;
        case PropType^.Kind of
          // ####################### Method
          tkMethod: begin

          end;
          // ######################## Integer
          tkInteger: begin
            Node := Doc.CreateElement('property');
            ParentNode.AppendChild(Node);
            TDOMElement(Node).SetAttribute('name', WideString(Name));
            TDOMElement(Node).SetAttribute('type', WideString(IntToStr(Ord(PropType^.Kind))));
            TDOMElement(Node).TextContent := WideString(IntToStr(GetOrdProp(Obj, PP^[i])));
          end;
          // ####################### Boolean
          tkBool: begin
            Node := Doc.CreateElement('property');
            ParentNode.AppendChild(Node);
            TDOMElement(Node).SetAttribute('name', WideString(Name));
            TDOMElement(Node).SetAttribute('type', WideString(IntToStr(Ord(PropType^.Kind))));
            TDOMElement(Node).TextContent := WideString(IntToStr(GetOrdProp(Obj, PP^[i])));
          end;
          // ####################### String
          tkString, tkAString: begin
            Node := Doc.CreateElement('property');
            ParentNode.AppendChild(Node);
            TDOMElement(Node).SetAttribute('name', WideString(Name));
            TDOMElement(Node).SetAttribute('type', WideString(IntToStr(Ord(PropType^.Kind))));
            TDOMElement(Node).TextContent := WideString(GetStrProp(Obj, PP^[i]));
          end;
          // ##################### DynArray (TStringArray)
          tkDynArray: begin
            UseSa := False;
            if (Obj is TMUICycle) and (Name = 'Entries') then
            begin
              sa := TMUICycle(Obj).Entries;
              UseSa := True;
            end;
            if (Obj is TMUIRegister) and (Name = 'Titles') then
            begin
              sa := TMUIRegister(Obj).Titles;
              UseSa := True;
            end;
            if UseSa then
            begin
              Node := Doc.CreateElement('property');
              ParentNode.AppendChild(Node);
              TDOMElement(Node).SetAttribute('name', WideString(Name));
              TDOMElement(Node).SetAttribute('type', WideString(IntToStr(Ord(PropType^.Kind))));
              for j := 0 to High(Sa) do
              begin
                SaNode := Doc.CreateElement('entry');
                Node.AppendChild(SaNode);
                SaNode.TextContent := WideString(Sa[j]);
              end;
            end;
          end;
          else
            writeln(name, 'Not handled Type: ', PropType^.Kind); // still unknown Types needs Handler
        end;
      end;
    end;
    FreeMem(PP);
  end;

  procedure PutEvents(ParentNode: TDOMNode; ItemNode: TItemNode);
  var
    i: Integer;
    ANode, Node: TDOMNode;
    EV: TEVentHandler;
  begin
    for i := 0 to ItemNode.Events.Count - 1 do
    begin
      EV := ItemNode.Events[i];
      //
      Node := Doc.CreateElement('event');
      ParentNode.AppendChild(Node);
      TDOMElement(Node).SetAttribute('name', WideString(EV.Name));
      TDOMElement(Node).SetAttribute('eventname', WideString(EV.Event));

      ANode := Doc.CreateElement('plainheader');
      Node.AppendChild(ANode);
      ANode.TextContent := WideString(Ev.PlainHeader);

      ANode := Doc.CreateElement('header');
      Node.AppendChild(ANode);
      ANode.TextContent := WideString(Ev.Header);

      ANode := Doc.CreateElement('text');
      Node.AppendChild(ANode);
      ANode.TextContent := WideString(Ev.Text);

    end;
  end;

  procedure PutInNode(ParentNode: TDOMNode; ItemNode: TItemNode);
  var
    Node: TDOMNode;
    i: Integer;
  begin
    Node := Doc.CreateElement('element');
    ParentNode.AppendChild(Node);
    TDOMElement(Node).SetAttribute('name', WideString(ItemNode.Name));
    TDOMElement(Node).SetAttribute('parentident', WideString(ItemNode.ParentIdent));
    TDOMElement(Node).SetAttribute('class', WideString(ItemNode.Data.ClassName));
    PutProperties(Node, ItemNode);
    PutEvents(Node, ItemNode);
    for i := 0 to ItemNode.Childs.Count - 1 do
      PutInNode(Node, ItemNode.Childs[i]);
  end;
begin
  Result := False;
  Doc := TXMLDocument.Create;
  try
    Root := Doc.CreateElement('application');
    Doc.AppendChild(Root);
    Root := Doc.DocumentElement;
    PutInNode(Root, Self);
    //
    WriteXMLFile(Doc, AFileName);
    Result := True;
  finally
    Doc.Free;
  end;
end;

function TItemTree.LoadFromFile(AFileName: string): Boolean;
var
  Doc: TXMLDocument;
  App, Win: TDOMNode;

  procedure GetProperties(Node: TDOMNode; ItemNode: TItemNode);
  var
    i, j : Integer;
    Prop, ANode: TDOMNode;
    Field: string;
    Temp: Integer;
    Obj: TObject;
    sa: TStringArray;
    EV: TEventHandler;
  begin
    Obj := ItemNode.Data;
    for i := 0 to Node.ChildNodes.Count - 1 do
    begin
      Prop := Node.ChildNodes[i];
      if Prop.NodeName = 'property' then
      begin
        Field := AnsiString(TDOMElement(Prop).GetAttribute('name'));
        ItemNode.Properties.Add(Field);
        Temp := StrToIntDef(AnsiString(TDOMElement(Prop).GetAttribute('type')), -1);
        if Temp < 0 then
          Continue;
        case TTypeKind(Temp) of
          // ####################### Method
          tkMethod: begin
            // see further down
          end;
          // ####################### Boolean
          // ######################## Integer
          tkBool,
          tkInteger: begin
            if TryStrToInt(AnsiString(Prop.TextContent), Temp) then
              SetOrdProp(Obj, Field, Temp);
          end;
          // ####################### String
          tkString, tkAString: begin
            SetStrProp(Obj, Field, AnsiString(Prop.TextContent));
          end;
          // ##################### DynArray (TStringArray)
          tkDynArray: begin
            SetLength(Sa, Prop.ChildNodes.Count);
            for j := 0 to Prop.ChildNodes.Count - 1 do
            begin
              Sa[j] := AnsiString(Prop.ChildNodes[j].TextContent);
            end;
            if (Obj is TMUICycle) and (Field = 'Entries') then
            begin
              TMUICycle(Obj).Entries := sa;
            end;
            if (Obj is TMUIRegister) and (Field = 'Titles') then
            begin
              TMUIRegister(Obj).Titles := sa;
            end;
          end;
          else
            writeln(name, 'Not handled Type: ', TTypeKind(Temp)); // still unknown Types needs Handler
        end;
      end
      else
      if Prop.NodeName = 'event' then
      begin
        EV := TEventHandler.Create;
        EV.Name := AnsiString(TDOMElement(Prop).GetAttribute('name'));
        EV.Event := AnsiString(TDOMElement(Prop).GetAttribute('eventname'));
        for j := 0 to Prop.ChildNodes.Count - 1 do
        begin
          ANode := Prop.ChildNodes[j];
          if ANode.NodeName = 'plainheader' then
            EV.PlainHeader := AnsiString(ANode.TextContent)
          else
          if ANode.NodeName = 'header' then
            EV.Header := AnsiString(ANode.TextContent)
          else
          if ANode.NodeName = 'text' then
            EV.Text := AnsiString(ANode.TextContent)
        end;
        ItemNode.Events.Add(Ev);
      end;
    end;
  end;

  procedure GetChilds(ParentNode: TDOMNode; ItemNode: TItemNode);
  var
    Elem: TDOMNode;
    i: Integer;
    AName: string;
    PIdent: string;
    NObj: TMUIClass;
    NItemNode: TItemNode;
  begin
    for i := 0 to ParentNode.ChildNodes.Count - 1 do
    begin
      Elem := ParentNode.ChildNodes[i];
      if Elem.NodeName = 'element' then
      begin
        AName :=  AnsiString(TDOMElement(Elem).GetAttribute('name'));
        try
        PIdent :=  AnsiString(TDOMElement(Elem).GetAttribute('parentident'));
        except
          PIdent := '';
        end;
        NObj := GetClassByClassName(AnsiString(TDOMElement(Elem).GetAttribute('class')));
        if Assigned(NObj) then
        begin
          NItemNode := ItemNode.NewChild(AName, NObj.Create);
          NItemNode.FParentIdent := PIdent;
          GetProperties(Elem, NItemNode);
          GetChilds(Elem, NItemNode);
        end;
      end;
    end;
  end;


begin
  Result := False;
  Doc := nil;
  try
    ReadXMLFile(Doc, AFileName);
    App := Doc.FindNode('application');
    if Assigned(App) then
    begin
      Win := App.FindNode('element');
      if Assigned(Win) then
      begin
        Self.Name := AnsiString(TDOMElement(Win).GetAttribute('name'));
        GetProperties(Win, Self);
        GetChilds(Win, Self);
      end;
    end;
  finally
    Doc.Free;
  end;
end;

end.
