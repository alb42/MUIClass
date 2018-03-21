program MUIIDE;
{$mode objfpc}{$H+}
uses
  MUIClass.Base,
  MainWinUnit,
  StrArraySetUnit,
  MenuEditorUnit;

begin
  MainWindow := TMainWindow.Create;
  StrArrayWin := TStrArrayWin.Create;
  MenuEditor := TMenuEditor.Create;
  MUIApp.Run;
end.
