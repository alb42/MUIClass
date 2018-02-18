unit MUICompUnit;
{$mode objfpc}{$H+}
interface

uses
  SysUtils,
  MUIClass.Base, MUIClass.Area, MUIClass.Window, MUIClass.Group,
  MUIClass.Gadget, MUIClass.Image, MUIClass.List, MUIClass.Numeric;

type
  TMUIClass = class of TMUINotify;
  TMUIComponent = record
    MUIClass: TMUIClass;
    HasChild: Boolean;
    Name: string;
    AUnit: string
  end;

const
  MUIComponents: array[0..19] of TMUIComponent =
    ((MUIClass: TMUIBalance;       HasChild: False; Name: 'Balance';       AUnit: 'MUIClass.Area'),
     (MUIClass: TMUIButton;        HasChild: False; Name: 'Button';        AUnit: 'MUIClass.Area'),
     (MUIClass: TMUIGauge;         HasChild: False; Name: 'Gauge';         AUnit: 'MUIClass.Area'),
     (MUIClass: TMUIText;          HasChild: False; Name: 'Text';          AUnit: 'MUIClass.Area'),
     (MUIClass: TMUIRectangle;     HasChild: False; Name: 'Rectangle';     AUnit: 'MUIClass.Area'),
     (MUIClass: TMUIScale;         HasChild: False; Name: 'Scale';         AUnit: 'MUIClass.Area'),
 
     (MUIClass: TMUIGroup;         HasChild: True;  Name: 'Group';         AUnit: 'MUIClass.Group'),
     (MUIClass: TMUIRegister;      HasChild: True;  Name: 'Register';      AUnit: 'MUIClass.Group'),
     (MUIClass: TMUICycle;         HasChild: False; Name: 'Cycle';         AUnit: 'MUIClass.Group'),

     (MUIClass: TMUIString;        HasChild: False; Name: 'String';        AUnit: 'MUIClass.Gadget'),

     (MUIClass: TMUIProp;          HasChild: False; Name: 'Prop';          AUnit: 'MUIClass.Prop'),
     (MUIClass: TMUIScrollBar;     HasChild: False; Name: 'ScrollBar';     AUnit: 'MUIClass.Gadget'),
     
     (MUIClass: TMUIImage;         HasChild: False; Name: 'Image';         AUnit: 'MUIClass.Image'),
     (MUIClass: TMUICheckmark;     HasChild: False; Name: 'Checkmark';     AUnit: 'MUIClass.Image'),
     (MUIClass: TMUIPopButton;     HasChild: False; Name: 'PopButton';     AUnit: 'MUIClass.Image'),

     (MUIClass: TMUIFloatText;     HasChild: False; Name: 'FloatText';     AUnit: 'MUIClass.List'),

     (MUIClass: TMUIKnob;          HasChild: False; Name: 'Knob';          AUnit: 'MUIClass.Numeric'),
     (MUIClass: TMUILevelMeter;    HasChild: False; Name: 'Levelmeter';    AUnit: 'MUIClass.Numeric'),
     (MUIClass: TMUINumericButton; HasChild: False; Name: 'NumericButton'; AUnit: 'MUIClass.Numeric'),
     (MUIClass: TMUISlider;        HasChild: False; Name: 'Slider';        AUnit: 'MUIClass.Numeric')

    );

implementation

end.
