unit PrefsUnit;
{$mode objfpc}{$H+}
interface
uses
  SysUtils, inifiles;

// Default Editor to use for editing Eventhandlers
const
  {$ifdef AROS}
  DEFEDITOR = 'sys:EdiSyn/EdiSyn';
  EDITORMSG = 0;
  {$else}
  {$ifdef Amiga68k}
  DEFEDITOR = 'sys:Tools/EditPad';
  EDITORMSG = 0;
  {$else}
  {$ifdef MorphOS}
  DEFEDITOR = 'sys:Applications/Scribble/Scribble';
  EDITORMSG = 1;
  {$else}  
  DEFEDITOR = 'c:ed';
  EDITORMSG = 0;
  {$endif}
  {$endif}
  {$endif}

type
  TPrefsType = (ptString, ptInteger, ptBoolean);
  TPrefsInit = record
    Name: string;
    ValType: TPrefsType;
    DefaultStr: string;
    ValueStr: string;
    DefaultInt: Integer;
    Value: Integer;
  end;

const
  NumSettings= 2;

var
  PrefsInit: array[0..NumSettings - 1] of TPrefsInit =
    (
    // 0
     (Name: 'Editor';
      ValType: ptString;
      DefaultStr: DEFEDITOR;
      ValueStr: DEFEDITOR;
      DefaultInt: 0;
      Value: 0),
      
    // 1 
     (Name: 'EditorMessage';
      ValType: ptBoolean;
      DefaultStr: '';
      ValueStr: '';
      DefaultInt: EDITORMSG;
      Value: EDITORMSG)
    );


type
  TIDEPrefs = class
  private
    PrefsName: string;
    IniFile: TIniFile;
    function GetBoolItem(Idx: Integer): Boolean;
    procedure SetBoolItem(Idx: Integer; AValue: Boolean);
    function GetStrItem(Idx: Integer): string;
    procedure SetStrItem(Idx: Integer; AValue: string); 
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Editor: string index 0 read GetStrItem write SetStrItem;
    property EditorMsg: Boolean index 1 read GetBoolItem write SetBoolItem;
  end;

var
  IDEPrefs: TIDEPrefs;

implementation
  

constructor TIDEPrefs.Create;
var
  i: Integer;
begin
  PrefsName := ChangeFileExt(ParamStr(0), '.ini');
  IniFile := TIniFile.Create(PrefsName);
  for i := 0 to High(PrefsInit) do
  begin
    case PrefsInit[i].ValType of
      ptString: PrefsInit[i].ValueStr := IniFile.ReadString('General', PrefsInit[i].Name, PrefsInit[i].DefaultStr); 
      ptInteger, ptBoolean: PrefsInit[i].Value := IniFile.ReadInteger('General', PrefsInit[i].Name, PrefsInit[i].DefaultInt); 
    end;
  end;
end;

destructor TIDEPrefs.Destroy;
var
  i: Integer;
begin
  for i := 0 to High(PrefsInit) do
  begin
    case PrefsInit[i].ValType of
      ptString: IniFile.WriteString('General', PrefsInit[i].Name, PrefsInit[i].ValueStr); 
      ptInteger, ptBoolean: IniFile.WriteInteger('General', PrefsInit[i].Name, PrefsInit[i].Value); 
    end;
  end;
  IniFile.UpdateFile;
  IniFile.Free;
  inherited;
end;

function TIDEPrefs.GetBoolItem(Idx: Integer): Boolean;
begin
  Result := False;
  writeln('get index ', Idx);
  if (Idx >= 0) and (Idx <= High(PrefsInit)) then
    Result := PrefsInit[Idx].Value <> 0; 
  writeln('Idx is ', Result);
end;

procedure TIDEPrefs.SetBoolItem(Idx: Integer; AValue: Boolean);
begin
  if (Idx >= 0) and (Idx <= High(PrefsInit)) then
    PrefsInit[Idx].Value := Integer(AValue); 
end;
    

function TIDEPrefs.GetStrItem(Idx: Integer): string;
begin
  Result := '';
  if (Idx >= 0) and (Idx <= High(PrefsInit)) then
    Result := PrefsInit[Idx].ValueStr; 
end;

procedure TIDEPrefs.SetStrItem(Idx: Integer; AValue: string);
begin
  if (Idx >= 0) and (Idx <= High(PrefsInit)) then
    PrefsInit[Idx].ValueStr := AValue; 
end;

initialization
  IDEPrefs := TIDEPrefs.Create;
finalization
  IDEPrefs.Free;
end.