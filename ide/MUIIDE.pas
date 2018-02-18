program MUIIDE;
{$mode objfpc}{$H+}
uses
  MUIClass.Base,
  MainWinUnit,
  StrArraySetUnit;

begin
  MainWindow := TMainWindow.Create;
  StrArrayWin := TStrArrayWin.Create;
  MUIApp.Run;
end.
