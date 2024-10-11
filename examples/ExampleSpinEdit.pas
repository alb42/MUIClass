program ExampleSpinEdit;
{$mode objfpc}{$H+}
uses
  MUI, Math, Classes, SysUtils,
  MUIClass.Base, MUIClass.Window, MUIClass.Gadget, MUIClass.Image, MUIClass.Group, MUIClass.Area;

type

  { TMUISpinEdit }

  TMUISpinEdit = class(TMUIGroup)
  private
    FEdit: TMUIString;
    FIncrement: Integer;
    FIncrementAsFactor: Boolean;
    FMaxValue: Integer;
    FMinValue: Integer;
    FOnAckEvent: TNotifyEvent;
    FOnChangeValue: TNotifyEvent;
    FUpBtn: TMUIImage;
    FDownBtn: TMUIImage;
    procedure AckEvent(Sender: TObject);
    procedure ChangeEvent(Sender: TObject);
    procedure DownClick(Sender: TObject);
    function GetValue: Integer;
    procedure SetMaxValue(AValue: Integer);
    procedure SetMinValue(AValue: Integer);
    procedure SetValue(AValue: Integer);
    procedure UpClick(Sender: TObject);
  private
    procedure DoOnChange;
    procedure DoOnAck;
  public
    constructor Create; override;

    property Value: Integer read GetValue write SetValue;
    property MinValue: Integer read FMinValue write SetMinValue;
    property MaxValue: Integer read FMaxValue write SetMaxValue;

    property IncrementAsFactor: Boolean read FIncrementAsFactor write FIncrementAsFactor;
    property Increment: Integer read FIncrement write FIncrement;

    property OnAckValue: TNotifyEvent read FOnAckEvent write FOnAckEvent;
    property OnChangeValue: TNotifyEvent read FOnChangeValue write FOnChangeValue;
  end;


type

  { TMyWindow }

  TMyWindow= class(TMUIWindow)
  private
    procedure AckValueEvent(Sender: TObject);
    procedure ValueChangeEvent(Sender: TObject);
  public
    Status: TMUIText;
    Status2: TMUIText;
    constructor Create; override;
  end;

{ TMUISpinEdit }

function TMUISpinEdit.GetValue: Integer;
begin
  Result := EnsureRange(FEdit.IntegerValue, FMinValue, FMaxValue);
end;

procedure TMUISpinEdit.DownClick(Sender: TObject);
var
  NVal: Int64;
begin
  if FIncrementAsFactor then
  begin
    if FIncrement <> 0 then
      Value := Value div FIncrement
  end
  else
  begin
    NVal := Int64(Value) - Int64(FIncrement);
    if NVal < FMinValue then
      Value := FMinValue
    else
      Value := NVal;
  end;
  DoOnAck;
end;

procedure TMUISpinEdit.AckEvent(Sender: TObject);
begin
  Value := Value;
  DoOnAck;
end;

procedure TMUISpinEdit.ChangeEvent(Sender: TObject);
begin
  DoOnChange;
end;

procedure TMUISpinEdit.SetMaxValue(AValue: Integer);
begin
  if FMaxValue = AValue then Exit;
  FMaxValue := AValue;
  Value := Value;
end;

procedure TMUISpinEdit.SetMinValue(AValue: Integer);
begin
  if FMinValue = AValue then Exit;
  FMinValue := AValue;
  Value := Value;
end;

procedure TMUISpinEdit.SetValue(AValue: Integer);
begin
  FEdit.Contents := IntToStr(EnsureRange(AValue, MinValue, MaxValue));
end;

procedure TMUISpinEdit.UpClick(Sender: TObject);
var
  NVal: Int64;
begin
  if FIncrementAsFactor then
    NVal := Int64(Value) * Int64(FIncrement)
  else
    NVal := Int64(Value) + Int64(FIncrement);
  if NVal >= FMaxValue then
    Value := FMaxValue
  else
    Value := NVal;
  if FIncrementAsFactor and (Value = 0) then
    Value := 1;
  DoOnAck;
end;

procedure TMUISpinEdit.DoOnChange;
begin
  if Assigned(FOnChangeValue) then
    FOnChangeValue(Self);
end;

procedure TMUISpinEdit.DoOnAck;
begin
  Value := Value;
  if Assigned(FOnAckEvent) then
    FOnAckEvent(Self);
end;

constructor TMUISpinEdit.Create;
begin
  inherited Create;
  FIncrement := 1;
  FIncrementAsFactor := False;
  FMinValue := -MaxInt;
  FMaxValue := MaxInt;
  Horiz := True;
  FEdit := TMUIString.Create;
  Frame := MUIV_Frame_String;
  HorizSpacing := 0;
  VertSpacing := 0;
  with FEdit do
  begin
    Format := MUIV_String_Format_Right;
    Frame := MUIV_Frame_None;
    Accept := '-1234567890'#8;
    Parent := Self;
    IntegerValue := 0;
    MaxLen := 10;
    OnAcknowledge  := @AckEvent;
    OnContentsChange := @ChangeEvent;
  end;

  FDownBtn := TMUIImage.Create;
  with FDownBtn do
  begin
    Frame := MUIV_Frame_None;
    InputMode := MUIV_InputMode_RelVerify;
    Spec.SetStdPattern(MUII_ArrowDown);
    OnClick  := @DownClick;
    Parent := Self;
  end;

  FUpBtn := TMUIImage.Create;
  with FUpBtn do
  begin
    Frame := MUIV_Frame_None;
    InputMode := MUIV_InputMode_RelVerify;
    Spec.SetStdPattern(MUII_ArrowUp);
    OnClick := @UpClick;
    Parent := Self;
  end;
end;

{ TMyWindow }

procedure TMyWindow.AckValueEvent(Sender: TObject);
begin
  if Sender is TMUISpinEdit then
    Status2.Contents := 'Spin ' + IntToStr(TMUISpinEdit(Sender).Tag) + ' acknowledged ' + IntToStr(TMUISpinEdit(Sender).Value);
end;

procedure TMyWindow.ValueChangeEvent(Sender: TObject);
begin
  if Sender is TMUISpinEdit then
    Status.Contents := 'Spin ' + IntToStr(TMUISpinEdit(Sender).Tag) + ' changed to ' + IntToStr(TMUISpinEdit(Sender).Value);
end;

constructor TMyWindow.Create;
var
  Grp: TMUIGroup;
begin
  inherited Create;

  Horizontal := False;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz:= True;
    Frame := MUIV_Frame_None;
    Parent := Self
  end;

  with TMUIText.Create('2x on Spin:') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  With TMUISpinEdit.Create do
  begin
    IncrementAsFactor := True;
    Increment := 2;
    MinValue := 16;
    MaxValue := 1024;
    Tag := 1;
    OnChangeValue := @ValueChangeEvent;
    OnAckValue  := @AckValueEvent;
    Parent := Grp;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz:= True;
    Frame := MUIV_Frame_None;
    Parent := Self
  end;

  with TMUIText.Create('10x on Spin:') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  With TMUISpinEdit.Create do
  begin
    IncrementAsFactor := True;
    Increment := 10;
    MinValue := 10;
    MaxValue := 1000000000;
    Tag := 2;
    OnChangeValue := @ValueChangeEvent;
    OnAckValue  := @AckValueEvent;
    Parent := Grp;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz:= True;
    Frame := MUIV_Frame_None;
    Parent := Self
  end;

  with TMUIText.Create('Linear Spin:') do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  With TMUISpinEdit.Create do
  begin
    Increment := 1;
    MinValue := -42;
    MaxValue := 42;
    Value := 17;
    Tag := 3;
    OnChangeValue := @ValueChangeEvent;
    OnAckValue  := @AckValueEvent;
    Parent := Grp;
  end;

  Grp := TMUIGroup.Create;
  with Grp do
  begin
    Horiz:= False;
    Frame := MUIV_Frame_String;
    Parent := Self
  end;

  Status := TMUIText.Create('');
  with Status do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;

  Status2 := TMUIText.Create('');
  with Status2 do
  begin
    Frame := MUIV_Frame_None;
    Parent := Grp;
  end;


end;

begin
  TMyWindow.Create;

  MUIApp.Run
end.

