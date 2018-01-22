program HelloWorld;

uses
  MUIClass.Window, MUIClass.Area, MUIClass.Base;

begin
  with TMUIText.Create do
  begin
    Contents := 'Hello World';
    Parent := TMUIWindow.Create;
  end;
  MUIApp.Run;
end.
