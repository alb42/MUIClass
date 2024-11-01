unit IDETypes;
{$mode objfpc}{$H+}
interface

uses
  fgl, typinfo;

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
