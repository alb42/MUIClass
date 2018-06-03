program StringGridTest;
{$mode objfpc}{$H+}
uses
  SysUtils,
  MUIClass.Base,
  MUIClass.Window,
  MUIClass.StringGrid,
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
  SetCols, SetRows, WCols, WRows, WText: TMUIString;
  SG: TMUIStringGrid;  // A StringGrid

// Event called, when Button is pressed


procedure TMyWindow.SetSizeClick(Sender: TObject);
begin
  SG.Quiet := True;
  SG.NumColumns := SetCols.IntegerValue;
  SG.NumRows := SetRows.IntegerValue;
  SG.Quiet := False;
end;

procedure TMyWindow.ButtonClick(Sender: TObject);
var
  x,y: Integer;
begin
  SG.Quiet := True;
  for y := 0 to SG.NumRows - 1 do
  begin
    for x := 0 to SG.NumColumns - 1 do
      SG.Cells[x, y] := IntToStr(x) +' ; ' + IntToStr(y);
  end;
  SG.Quiet := False;
end;

procedure TMyWindow.SetClick(Sender: TObject);
begin
  SG.Cells[WCols.IntegerValue, WRows.IntegerValue] := WText.Contents;
end;

procedure TMyWindow.ListClick(Sender: TObject);
begin
  WCols.IntegerValue := SG.ClickColumn;
  WRows.IntegerValue := SG.Row;
  WText.Contents := SG.Cells[SG.ClickColumn, SG.Row];
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
  SetCols.IntegerValue := 3;
  SetCols.Parent := HeadGroup;

  SetRows := TMUIString.Create;
  SetRows.Accept := '0123456789';
  SetRows.IntegerValue := 5;
  SetRows.Parent := HeadGroup;

  With TMUIButton.Create('Set Grid Size') do
  begin
    OnClick := @Win.SetSizeClick;
    Parent := HeadGroup;
  end;

  SG := TMUIStringGrid.Create;
  SG.NumColumns := 3;
  SG.NumRows := 5;
  SG.ShowLines := True;
  SG.ShowTitle := True;
  SG.Cells[0,2] := 'Test 0,2';
  SG.Cells[1,4] := 'Test 1,4';
  SG.Cells[2,0] := 'Test 2,0';
  SG.Titles[0] := 'Title 1';
  SG.Titles[1] := 'Title a';
  SG.Titles[2] := 'Title %';
  {$ifdef AROS}
  SG.OnDoubleClick := @Win.ListClick;
  {$else}
  SG.OnClick := @Win.ListClick;
  {$endif}
  SG.Parent := Win;

  FootGroup := TMUIGroup.Create;
  FootGroup.Horiz := True;
  FootGroup.Parent := Win;

  WCols := TMUIString.Create;
  WCols.Accept := '0123456789';
  WCols.IntegerValue := 0;
  WCols.Parent := FootGroup;

  WRows := TMUIString.Create;
  WRows.Accept := '0123456789';
  WRows.IntegerValue := 0;
  WRows.Parent := FootGroup;

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
