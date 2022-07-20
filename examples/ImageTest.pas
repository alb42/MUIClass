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
    Button: TMUIButton; // Button
    Image: TMUIImage;   // Image
    Grp: TMUIGroup;
    procedure ButtonClick(Sender: TObject);

    constructor Create; override;
  end;

//var
  //Win: TMyWindow;     // Window

// Event called, when Button is pressed
procedure TMyWindow.ButtonClick(Sender: TObject);
begin
  // Delete the old image ;)
  Image.Free;
  //
  // create a new image with different picture
  // because the picture you only can change on creation
  Image := TMUIImage.Create;
  Image.FixWidth := 200;
  Image.FixHeight := 123;
  Image.FreeHoriz := True;
  Image.FreeVert := True;
  Image.Spec.SetPicture('test2.png');
  Image.Parent := Grp; // now the element will be created an linked to application
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

  // Create a Button with some text
  Button := TMUIButton.Create;
  Button.Contents := 'Click Me';
  Button.OnClick := @ButtonClick;       // Connect the Click Event
  Button.Parent := Grp2;                // Insert Button in the Window
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

