program ModalWindow;
{$mode objfpc}{$H+}

uses
  SysUtils, MUIClass.Base, MUIClass.Area, MUIClass.Window,
  MUIClass.Dialog;

type

  { TWin1 }

  TWin1 = class(TMUIWindow)
  public
    constructor Create; override;
    procedure ButtonClick(Sender: TObject);
  end;

  { TWin2 }

  TWin2 = class(TMUIWindow)
  private
    Res: Boolean;
  public
    constructor Create; override;
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    function Execute: Boolean;
  end;

var
  Win2: TWin2;

{ TWin2 }

constructor TWin2.Create;
begin
  inherited Create;
  Horizontal := True;
  with TMUIButton.Create('  OK  ') do
  begin
    OnClick := @OKButtonClick;
    Parent := Self;
  end;
  with TMUIButton.Create('  Cancel  ') do
  begin
    OnClick := @CancelButtonClick;
    Parent := Self;
  end;
end;

procedure TWin2.OKButtonClick(Sender: TObject);
begin
  Res := True;
  Self.Close;
end;

procedure TWin2.CancelButtonClick(Sender: TObject);
begin
  Res := False;
  Self.Close;
end;

function TWin2.Execute: Boolean;
begin
  Res := False;
  Show;
  repeat
    MUIApp.ProcessMessages;
    SysUtils.Sleep(25);
  until not Open;
  Result := Res;
end;

{ TWin1 }

constructor TWin1.Create;
begin
  inherited Create;

  with TMUIButton.Create('      Start Modal Window     ') do
  begin
    OnClick := @ButtonClick;
    Parent := Self;
  end;
end;

procedure TWin1.ButtonClick(Sender: TObject);
begin
  if Win2.Execute then
    ShowMessage('OK pressed')
  else
    ShowMessage('Cancel pressed');
end;



begin
  TWin1.Create;
  Win2 := TWin2.Create;
  MUIApp.Run;
end.

