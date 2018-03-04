unit IDETypes;
{$mode objfpc}{$H+}
interface

uses
  fgl, NodeTreeUnit, typinfo;

type
  TItemProp = class
  private
    FActive: Boolean;
    procedure SetActive(AValue: Boolean);
  public
    Name: string;
    Value: string;
    DisplayName: string;
    DisplayValue: string;
    Additional: string;
    IsSpecial: Boolean;
    property Active: boolean read FActive write SetActive;
  end;

  TItemProps = specialize TFPGObjectList<TItemProp>;

  // If an Eventhandler is specified we save the contents
  // because we cant connect it to the object itself like other properties
  TEventHandler = class
    Name: string;    // name of Event On...
    Obj: TItemNode;  // Item Node which the Event belongs to
    Event: string;   // Name of the EventHandler
    Header: string;  // Full Header of the EventHandler
    Text: string;    // Ful Text of the Eventhandler as given by user
  end;
  TEventHandlers = specialize TFPGObjectList<TEventHandler>;

  // Types for creation of Event Footprint
  TEventParam = record
    Style: TParamFlags;
    Name: string;
    Typ: string;
  end;
  TEventParams = array of TEventParam;
  PParamFlags = ^TParamFlags;

implementation


// if an Property is active (means changed by user and should be included to source)
// it will be printed bold
procedure TItemProp.SetActive(AValue: Boolean);
begin
  FActive := AValue;
  if Active then
  begin
    DisplayName := #27'b' + Name;
    DisplayValue := #27'b' + Value;
  end
  else
  begin
    DisplayName := Name;
    DisplayValue := Value;
  end;
end;

end.
