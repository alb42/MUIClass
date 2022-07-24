program DatatypeTest;
{$mode objfpc}{$H+}
uses
  Types, MUIClass.Base, MUIClass.Window, MUIClass.DrawPanel, AGraphics, MUIClass.Datatypes, Intuition;
type
  TMyWindow = class(TMUIWindow)
  public
    procedure PaintMe(Sender: TObject; RP: PRastPort; DrawRect: TRect);
    procedure MouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
  end;
var
  Win: TMyWindow;
  DP: TMUIDrawPanel;
  DB: TDrawBuffer;
  DT: array[boolean] of TPictureDataType;
  Current: Boolean = True;

procedure TMyWindow.PaintMe(Sender: TObject; RP: PRastPort; DrawRect: TRect); // Paint Event
begin
  if not Assigned(DB) then // Create a Image Buffer to paint on (create here to have the friend Bitmap)
  begin
    DB := TDrawBuffer.Create(Dp.Width, DP.Height, RP^.Bitmap^.Depth, RP^.Bitmap);
    SetAPen(DB.RP, 2);  // fill it with white color
    RectFill(DB.RP, 0, 0, DP.Width, DP.Height);
  end
  else
    if (DrawRect.Width <> DB.Width) or (DrawRect.Height <> DB.Height) then
      DB.Resize(DrawRect.Width, DrawRect.Height, True);
  ClipBlit(DB.Rp, 0, 0, RP, DrawRect.Left, DrawRect.Top, DrawRect.Width, DrawRect.Height, $00C0); // Just draw the picture
end;

procedure TMyWindow.MouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  if MouseBtn = mmbLeft then // when left button is pressed
  begin
    DT[Current].DrawToRastport(DB.RP, x, y);
    Current := not Current;
    TMUIDrawPanel(Sender).RedrawObject;
  end;
  EatEvent := True;
end;


begin
  Win := TMyWindow.Create;      // create the window
  DP := TMUIDrawPanel.Create;   // create the Draw Buffer
  DP.MinWidth := 100;
  DP.MinHeight := 100;
  DP.DefWidth := 256;
  Dp.DefHeight := 256;
  DP.MaxWidth := IntuitionBase^.ActiveScreen^.Width;
  DP.MaxHeight := IntuitionBase^.ActiveScreen^.Height;
  DP.FillArea := False; //we paint the whole window no flickering
  DP.OnMouseDown := @Win.MouseDown; // Connect the events for mouse and draw
  DP.OnDrawObject := @Win.PaintMe;
  DP.Parent := Win;  // place the Drawbuffer to Window
  DB := nil;         // Draw Buffer will be created inside first DrawEvent

  DT[True] := TPictureDataType.Create;
  DT[True].LoadFile('test.png');

  DT[False] := TPictureDataType.Create;
  DT[False].LoadFile('test2.png');

  DT[true].SaveFile('Ram:test');
  DT[False].SaveFile('Ram:test2');


  MUIApp.Run;        // lets go

  DT[True].Free;
  DT[False].Free;
  DB.Free;           // we need to destroy the drawbuffer, its not connected
end.



