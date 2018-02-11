{
 *****************************************************************************
 *                             TagsParamsHelper.pas                          *
 *                              --------------                               *
 *            Taglist handling for Amiga-style systems                       *
 *                                                                           *
 *****************************************************************************
}
unit TagsParamsHelper;

{$mode objfpc}{$H+}

interface

uses
  Exec, Utility,
  Classes, SysUtils, Math;

const
  TagTrue = 1;
  TagFalse = 0;

type

  { TATagList }

  TATagList = object
  private
    List: array of TTagItem;
    procedure TagDbgOut(txt: string);
  public
    procedure Clear;
    function GetTagPointer: PTagItem;
    procedure AddTag(Tag: LongWord; Data: NativeUInt);
    procedure AddTags(const AList: array of NativeUInt);
    procedure DebugPrint;
  end;

  operator := (AList: TATagList): PTagItem;

implementation

{ TATagList }

procedure TATagList.Clear;
begin
  SetLength(List, 1);
  List[0].ti_Tag := TAG_DONE;
  List[0].ti_Data := 0;
end;

function TATagList.GetTagPointer: PTagItem;
begin
  Result := @(List[0]);
end;

procedure TATagList.AddTag(Tag: LongWord; Data: NativeUInt);
var
  CurIdx: Integer;
begin
  if Tag = TAG_DONE then
    Exit;
  CurIdx := Max(0, High(List));
  SetLength(List, CurIdx + 2);
  List[CurIdx].ti_Tag := Tag;
  List[CurIdx].ti_Data := Data;
  List[CurIdx + 1].ti_Tag := TAG_DONE;
  List[CurIdx + 1].ti_Data := TAG_DONE;
end;

procedure TATagList.AddTags(const AList: array of NativeUInt);
var
  Tag: LongWord;
  Data: NativeUInt;
  i: Integer;
begin
  i := 0;
  while i <= High(AList) do
  begin
    Tag := AList[i];
    Inc(i);
    if i <= High(AList) then
    begin
      Data := AList[i];
      Self.AddTag(Tag, Data);
      Inc(i);
    end else
    begin
      if Tag <> TAG_DONE then
      {$ifdef HASAMIGA}
        SysDebugln('AddTags called with odd number of Parameter (' + IntToStr(Length(AList)) + ')');
      {$else}
        Writeln('AddTags called with odd number of Parameter (' + IntToStr(Length(AList)) + ')');
      {$endif}
    end;
  end;
end;

procedure TATagList.TagDbgOut(txt: string);
begin
  {$ifdef HASAMIGA}
  SysDebugln('TagList('+HexStr(@List[0]) + '):' + txt);
  {$else}
  Writeln('TagList('+HexStr(@List[0]) + '):' + txt);
  {$endif}
end;

procedure TATagList.DebugPrint;
var
  i: Integer;
begin
  TagDbgOut('List with ' + IntToStr(Length(List)) + ' Entries.');
  for i := 0 to High(List) do
  begin
    //TagDbgOut('+ ' + IntToStr(i) + '. ' + HexStr(@List[i]));
    TagDbgOut('  ' + IntToStr(i) + '. Tag: ' + HexStr(Pointer(List[i].ti_Tag)) + ' Data: ' + HexStr(Pointer(List[i].ti_Data)));
    //TagDbgOut('- ' + IntToStr(i) + '. ' + HexStr(@List[i]));
  end;
  TagDbgOut('End Of List');
end;


operator := (AList: TATagList): PTagItem;
begin
  Result := AList.GetTagPointer;
end;

end.

