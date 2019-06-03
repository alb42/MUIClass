program GridTest;
{$mode objfpc}{$H+}
uses
  SysUtils,
  MUIClass.Base,
  MUIClass.Window,
  MUIClass.Grid,
  MUIClass.Gadget,
  MUIClass.Group,
  MUIClass.Area;

type
  // My Window (mainly because we need the Event attached to an Object)
  TMyWindow = class(TMUIWindow)
    procedure ButtonClick(Sender: TObject);
    procedure ListClick(Sender: TObject);
    procedure SetSizeClick(Sender: TObject);
    procedure SetClick(Sender: TObject);
  end;

var
  Win: TMyWindow;     // Window
  HeadGroup, FootGroup: TMUIGroup;
  WColRow: TMUIText;
  SetCols, SetRows, WText: TMUIString;
  SG: TMUIStrGrid;  // A StringGrid

// Event called, when Button is pressed


procedure TMyWindow.SetSizeClick(Sender: TObject);
begin
  SG.BeginUpdate;
  SG.NumCols := SetCols.IntegerValue;
  SG.NumRows := SetRows.IntegerValue;
  SG.EndUpdate;
end;

procedure TMyWindow.ButtonClick(Sender: TObject);
var
  x,y: Integer;
begin
  SG.BeginUpdate;
  for Y := 0 to SG.NumRows - 1 do
  begin
    for X := 0 to SG.NumCols - 1 do
    begin
      SG.Cells[x, y] := IntToStr(x) +' ; ' + IntToStr(y);
    end;
  end;
  SG.EndUpdate;
end;

procedure TMyWindow.SetClick(Sender: TObject);
begin
  SG.Cells[SG.Col, SG.Row] := WText.Contents;
end;

procedure TMyWindow.ListClick(Sender: TObject);
begin
  WColRow.Contents := IntToStr(SG.Col) + ';' + IntToStr(SG.Row);
  WText.Contents := SG.Cells[SG.Col, SG.Row]
end;

begin
  // Create a Window, with a title bar text
  Win := TMyWindow.Create;
  Win.Title := 'Test Window';

  HeadGroup := TMUIGroup.Create;
  HeadGroup.Horiz := True;
  HeadGroup.Parent := Win;

  SetCols := TMUIString.Create;
  SetCols.Accept := '0123456789';
  SetCols.IntegerValue := 5;
  SetCols.Parent := HeadGroup;

  SetRows := TMUIString.Create;
  SetRows.Accept := '0123456789';
  SetRows.IntegerValue := 7;
  SetRows.Parent := HeadGroup;

  With TMUIButton.Create('Set Grid Size') do
  begin
    OnClick := @Win.SetSizeClick;
    Parent := HeadGroup;
  end;

  SG := TMUIStrGrid.Create;
  SG.DefCellWidth := 100;
  SG.NumCols := 5;
  SG.NumRows := 7;

  SG.Cells[0,2] := 'Test 0,2';
  SG.Cells[1,4] := 'Test 1,4';
  SG.Cells[2,0] := 'Test 2,0';
  SG.OnCellFocus := @Win.ListClick;
  SG.Parent := Win;

  FootGroup := TMUIGroup.Create;
  FootGroup.Horiz := True;
  FootGroup.Parent := Win;

  WColRow := TMUIText.Create;
  WColRow.Parent := FootGroup;

  WText := TMUIString.Create;
  WText.Parent := FootGroup;

  with TMUIButton.Create('Set') do
  begin
    OnClick := @Win.SetClick;
    Parent := FootGroup;
  end;

  // Create a Button with some text
  with TMUIButton.Create('Fill all') do
  begin
    OnClick := @Win.ButtonClick;  // Connect the Click Event
    Parent := FootGroup;                // Insert Button in the Window
  end;

  // will actually start everything,
  // Create MUI object, connect them together
  // from the Parent relation we builded with Pascal Classes
  // Open the Window and run the Message Loop
  // destroy everything after the Window is closed again.
  MUIApp.Run;
end.
