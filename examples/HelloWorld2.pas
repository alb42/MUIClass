program HelloWorld2;
{$mode objfpc}{$H+}
uses
  MUIClass.Base,    //needed for MUIApp
  MUIClass.Window,  //needed for TMUIWindow
  MUIClass.Area;    //needed for TMUIButton and TMUIText

type
  // My Window (mainly because we need the Event attached to an Object)
  TMyWindow = class(TMUIWindow)
    procedure ButtonClick(Sender: TObject);
  end;

var
  Win: TMyWindow;     // Window
  Text: TMUIText;     // Text field
  Button: TMUIButton; // Button

// Event called, when Button is pressed
procedure TMyWindow.ButtonClick(Sender: TObject);
begin
  Text.Contents := 'Clicked'; // Change string in Text Object
end;

begin
  // Create a Window, with a title bar text
  Win := TMyWindow.Create;
  Win.Title := 'Test Window';

  // Create a Text field with text
  Text := TMUIText.Create;
  Text.Contents := 'not Clicked';
  Text.Parent := Win;               // Insert text field in the Window

  // Create a Button with some text
  Button := TMUIButton.Create;
  Button.Contents := 'Click Me';
  Button.OnClick := @Win.ButtonClick;  // Connect the Click Event
  Button.Parent := Win;                // Insert Button in the Window

  // will actually start everything,
  // Create MUI object, connect them together
  // from the Parent relation we builded with Pascal Classes
  // Open the Window and run the Message Loop
  // destroy everything after the Window is closed again.
  MUIApp.Run;
end.
