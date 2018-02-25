unit MUICompUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils, Fgl, TypInfo,
  MUIClass.Base, MUIClass.Area, MUIClass.Window, MUIClass.Group,
  MUIClass.Gadget, MUIClass.Image, MUIClass.List, MUIClass.Numeric;

type
  TMUIClass = class of TMUINotify;
  TMUIComp = class
  public
    MUIClass: TMUIClass;
    HasChild: Boolean;
    Name: string;
    AUnit: string
  end;
  TMUIComps = specialize TFPGObjectList<TMUIComp>;

procedure RegisterMUIClass(MUIClass: TMUIClass; HasChilds: Boolean = False; Name: string = '');

var
  MUIComps: TMUIComps;
implementation


procedure RegisterBasics;
begin
  // Area classes
  RegisterMUIClass(TMUIBalance);
  RegisterMUIClass(TMUIButton);
  RegisterMUIClass(TMUIGauge);
  RegisterMUIClass(TMUIText);
  RegisterMUIClass(TMUIRectangle);
  RegisterMUIClass(TMUIScale);
  // Group Classes
  RegisterMUIClass(TMUIGroup, True);
  RegisterMUIClass(TMUIRegister, True);
  RegisterMUIClass(TMUICycle);
  // Gaget Classes
  RegisterMUIClass(TMUIString);
  RegisterMUIClass(TMUIProp);
  RegisterMUIClass(TMUIScrollBar);
  // Image classes
  RegisterMUIClass(TMUIImage);
  RegisterMUIClass(TMUICheckmark);
  RegisterMUIClass(TMUIPopButton);
  // other classes
  RegisterMUIClass(TMUIKnob);
  RegisterMUIClass(TMUILevelMeter);
  RegisterMUIClass(TMUINumericButton);
  RegisterMUIClass(TMUISlider);

end;

procedure RegisterMUIClass(MUIClass: TMUIClass; HasChilds: Boolean = False; Name: string = '');
var
  MUIComp: TMUIComp;
begin
  MUIComp := TMUIComp.Create;
  MUIComp.MUIClass := MUIClass;
  MUIComp.HasChild := HasChilds;
  if Name = '' then
  begin
    MUIComp.Name := MUIClass.ClassName;
    if UpperCase(Copy(MUIComp.Name, 1, 1)) = 'T' then
      Delete(MUIComp.Name, 1, 1);
    if UpperCase(Copy(MUIComp.Name, 1, 3)) = 'MUI' then
      Delete(MUIComp.Name, 1, 3);
  end
  else
    MUIComp.Name := Name;
  MUIComp.AUnit := MUIClass.UnitName;
  MUIComps.Add(MUIComp);
end;

initialization

  MUIComps := TMUIComps.Create(True);
  RegisterBasics;

finalization
  MUIComps.Free;
end.
