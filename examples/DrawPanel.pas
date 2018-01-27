program Drawing;
{$mode objfpc}{$H+}
uses
  Types, MUIClass.Base, MUIClass.Window, MUIClass.DrawPanel, AGraphics;
type
  TMyWindow = class(TMUIWindow)
    Down: Boolean;
    procedure PaintMe(Sender: TObject; RP: PRastPort; DrawRect: TRect);
    procedure MouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseUp(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
    procedure MouseMove(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
  end;
var
  Win: TMyWindow;
  DP: TMUIDrawPanel;
  DB: TDrawBuffer;

procedure TMyWindow.PaintMe(Sender: TObject; RP: PRastPort; DrawRect: TRect); // Paint Event
begin
  if not Assigned(DB) then // Create a Image Buffer to paint on (create here to have the friend Bitmap)
  begin
    DB := TDrawBuffer.Create(256, 256, RP^.Bitmap^.Depth, RP^.Bitmap);
    SetAPen(DB.RP, 2);  // fill it with white color
    RectFill(DB.RP, 0, 0, 256, 256);
  end;
  ClipBlit(DB.Rp, 0, 0, RP, DrawRect.Left, DrawRect.Top, DB.Width, DB.Height, $00C0); // Just draw the picture
end;

procedure TMyWindow.MouseDown(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  if MouseBtn = mmbLeft then // when left button is pressed
  begin
    SetAPen(DB.RP, 1);     // set the color to black
    GFXMove(DB.RP, x, y);  // Move to down position (drawing will always draw lines from last point)
    Down := True;
  end;
end;

procedure TMyWindow.MouseMove(Sender: TObject; X,Y: Integer; var EatEvent: Boolean);
begin
  if Down then // only when mouse is pressed (see mouse down)
  begin
    Draw(DB.RP, x, y); // draw a line in black
    TMUIDrawPanel(Sender).RedrawObject; // let the Panel redraw
  end;
end;

procedure TMyWindow.MouseUp(Sender: TObject; MouseBtn: TMUIMouseBtn; X,Y: Integer; var EatEvent: Boolean);
begin
  if MouseBtn = mmbLeft then Down := False; // mouse release, do not draw anymore
end;

begin
  Win := TMyWindow.Create;      // create the window
  DP := TMUIDrawPanel.Create;   // create the Draw Buffer
  DP.MinWidth := 256;           // Not smaller than the DrawBuffer (not needed but looks nicer)
  DP.MinHeight := 256;
  DP.OnMouseDown := @Win.MouseDown; // Connect the events for mouse and draw
  DP.OnMouseUp := @Win.MouseUp;
  DP.OnMouseMove := @Win.MouseMove;
  DP.OnDrawObject := @Win.PaintMe;
  DP.Parent := Win;  // place the Drawbuffer to Window
  DB := nil;         // Draw Buffer will be created inside first DrawEvent
  MUIApp.Run;        // lets go
  DB.Free;           // we need to destroy the drawbuffer, its not connected
end.

