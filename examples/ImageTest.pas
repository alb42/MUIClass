program ImageTest;
{$mode objfpc}{$H+}
uses
  MUIClass.Base, MUIClass.Window,  //needed for TMUIWindow
  MUIClass.Group,   //needed for TMUIGroup
  MUIClass.Area,    //needed for TMUIButton and TMUIText
  MUIClass.Image;   //needed for TMUIImage

type
  // My Window (mainly because we need the Event attached to an Object)
  TMyWindow = class(TMUIWindow)
    Text: TMUIText;     // Text field
    Button1: TMUIButton; // Button
    Button2: TMuiButton;
    Image: TMUIImage;   // Image
    Grp: TMUIGroup;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);

    constructor Create; override;
  end;

// Event called, when Button is pressed
procedure TMyWindow.Button1Click(Sender: TObject);
begin
  InitChange; // Tell the application that we are making updates to it, or it might not update the GUI correctly

  // Delete the old image ;)
  Image.Free;

  // create a new image with different picture
  // because the picture you only can change on creation
  Image := TMUIImage.Create;
  Image.FixWidth := 174; // This matches test.png width
  Image.FixHeight := 129; // This matches test.png height
  Image.FreeHoriz := True;
  Image.FreeVert := True;
  Image.Spec.SetPicture('test.png');
  Image.Parent := Grp; // now the element will be created and linked to application

  ExitChange; // We are done with the updates, refresh the application
end;

// Event called, when Button is pressed
procedure TMyWindow.Button2Click(Sender: TObject);
begin
  InitChange; // Tell the application that we are making updates to it, or it might not update the GUI correctly

  // Delete the old image ;)
  Image.Free;

  // create a new image with different picture
  // because the picture you only can change on creation
  Image := TMUIImage.Create;
  Image.FixWidth := 200; // This matches test2.png width
  Image.FixHeight := 123; // This matches test2.png Height
  Image.FreeHoriz := True;
  Image.FreeVert := True;
  Image.Spec.SetPicture('test2.png');
  Image.Parent := Grp; // now the element will be created an linked to application

  ExitChange; // We are done with the updates, refresh the application
end;

constructor TMyWindow.Create;
var
  Grp2: TMUIGroup;
begin
  inherited Create;

  Title := 'Image Window';
  // a group for holding the image
  Grp := TMUIGroup.Create;
  Grp.Parent := Self;

  // Create a image object
  Image := TMUIImage.Create;
  Image.FixWidth := 174;   // image size, it should be read automatically, but often does not work, so we fix it here
  Image.FixHeight := 129;
  Image.FreeHoriz := True; // do not scale the image
  Image.FreeVert := True;
  Image.Spec.SetPicture('test.png');
  Image.Parent := Grp;

  // separate group or the recreation will change the position of the image
  Grp2 := TMUIGroup.Create;
  Grp2.Parent := Self;

  // Create Button1 with some text
  Button1 := TMUIButton.Create;
  Button1.Contents := 'Click Me';
  Button1.OnClick := @Button1Click;       // Connect the Click Event
  Button1.Parent := Grp2;                // Insert Button in the Window

  // Create Button1 with some text
  Button2 := TMUIButton.Create;
  Button2.Contents := 'Click Me';
  Button2.OnClick := @Button2Click;       // Connect the Click Event
  Button2.Parent := Grp2;                // Insert Button in the Window
end;

begin
  // Create a Window, with a title bar text
  TMyWindow.Create;


  // will actually start everything,
  // Create MUI object, connect them together
  // from the Parent relation we builded with Pascal Classes
  // Open the Window and run the Message Loop
  // destroy everything after the Window is closed again.
  MUIApp.Run;
end.

