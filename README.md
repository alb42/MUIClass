# MUIClass
Wrapper to use Amiga MUI Objects with standard Pascal objects

## Units
<table border=1>
<tr><td>Unit Name</td><td>MUI Objects</td></tr>
<tr><td>MUIClass.Base</td><td>RootClass, Notify, Application, Family, Semaphore, Dataspace</td></tr>
<tr><td>MUIClass.Area</td><td>Area, Rectangle, Balance, Gauge, Scale, Colorfield, Text, PenDisplay, PopPen, Button</td></tr>
<tr><td>MUIClass.Group</td><td>Group, ListView, Register, Virtgroup, Scrollgroup, Radio, Cycle, Coloradjust, Palette</td></tr>
<tr><td>MUIClass.Window</td><td>Window, AboutMUI</td></tr>
<tr><td>MUIClass.Gadget</td><td>Gadget, String, Prop, Scrollbar</td></tr>
<tr><td>MUIClass.Image</td><td>Image, Bitmap</td></tr>
<tr><td>MUIClass.List</td><td>List, FloatText, VolumeList, DirList</td></tr>
<tr><td>MUIClass.Popstring</td><td>Popstring, PopObject, PopList, PopASL</td></tr>
<tr><td>MUIClass.Menu</td><td>Menustrip, Menu, Menuitem</td></tr>
<tr><td>MUIClass.Numeric</td><td>Numeric, Knop, Levelmeter, NumericButton, Slider</td></tr>
<tr><td>MUIClass.DrawPanel</td><td>DrawPanel (not an actual MUI Object, just an Area with more Events like Paint, Mouse,  Key)</td></tr>
</table>
## Basic usage

Make a new class for your Window.
<pre>
type
  TMyWindow = class(TMUIWindow)
  end;
var
  Win: TMyWindow;
</pre>
In your main routine, create your window class.
<pre>
Win := TMUIWindow.Create;
</pre>
Set some properties, for example the window title text.
<pre>
Win.Title := 'Test Window';
</pre>
Create some contents for the Window, for example a Button and a Text with some properties.
<pre>
Text := TMUIText.Create;
Text.Contents := 'Not Clicked';
Button := TMUIButton.Create;
Button.Contents := 'Click me';
</pre>
Now connect both to the Window by assigning the Parents to the window.
The order of Parent assignment defines the order in the Window later. By default the window aligns the Items vertically, one over the other (By setting  <code>Win.Horizontal := True;</code> you get them side by side).
<pre>
Text.Parent := Win;
Button.Parent := Win;
</pre>
We want also some Action in the Window, create an Event Function inside your Window Class:
<pre>
type
  TMyWindow = class(TMUIWindow)
    procedure ButtonClick(Sender: TObject);
  end;
  
procedure TMyWindow.ButtonClick(Sender: TObject);
begin
  Text.Contents := 'Clicked';
end;
</pre>
The Event should be called when the Button is clicked. Connect it to the OnClick Event.
<pre>
Button.OnClick := @Win.ButtonClick;
</pre>
Now everythis is ready to run. We start our created application. (There is no need to create a application object or connect the Windows to the application object, this is done automatically). The application is started with
<pre>
MUIApp.Run;
</pre>

MUIApp is a global Application object in MUIClass.Base. It has the usual fields you would expect, like application description, events for iconify and so on.
If the main Window is closed the application will terminate. (you can also call <code>MUIApp.Terminate</code> to end the application).
When the Application quits it will destroy everything which is connected to MUIApp (also nested through other classes or objects) it will free all MUI objects and Pascal Classes.

MUI objects have several fields which can only be changed before the actual object is created, make sure to set them before you start MUIApp.Run. If you try to set such a field in runtime you will get a warning in the debug log and the value will be ignored.

You can find that complete Source of this example in examples/HelloWorld2.pas

How to Handle other problems like dynamic creation/destruction of Windows other event types an Drawing to a Panel check the examples/TestApp.pas which contains many objects and Event connections.
