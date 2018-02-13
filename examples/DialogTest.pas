program DialogTest;
{$mode objfpc}{$H+}

uses
  SysUtils,
  MUIClass.Base, MUIClass.Window, MUIClass.Group, MUIClass.Dialog,
  MUIClass.Area, MUIClass.Image, MUIClass.Gadget, MUIClass.List;

type
  TMyWindow = class(TMUIWindow)
  private
    Pages: TMUIRegister;
    SaveMode, Multi, Style, DrawMode,Overscan, Autoscroll,
    FrontPen, BackPen, FixedOnly,
    WidthEdit, HeightEdit, DepthEdit: TMUICheckMark;
    FileTitle, FontTitle, ScTitle, Pattern: TMUIString;
    Memo: TMUIFloatText;

    procedure FileStart(Sender: TObject);
    procedure FontStart(Sender: TObject);
    procedure ScreenStart(Sender: TObject);
  public
    constructor Create; override;

  end;

constructor TMyWindow.Create;
var
  FG, SG: TMUIGroup;
begin
  inherited;

  Pages := TMUIRegister.Create;
  with Pages do
  begin
    Titles := ['File Dialog', 'Font Dialog', 'Screen Dialog'];
    Parent := Self;
  end;
  //###################### FileDialog
  FG := TMUIGroup.Create;
  with FG do
  begin
    Parent := Pages;
  end;

  SG := TMUIGroup.Create;
  with SG do
  begin
    Columns := 2;
    Parent := FG;
  end;

  with TMUIText.Create do
  begin
    Contents := 'Title Text';
    Parent := SG;
  end;
  FileTitle := TMUIString.Create;
  FileTitle.Contents := 'Select File';
  FileTitle.FixWidth := 150;
  FileTitle.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Pattern';
    Parent := SG;
  end;
  Pattern := TMUIString.Create;
  Pattern.Contents := '';
  Pattern.FixWidth := 150;
  Pattern.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'MultiSelect';
    Parent := SG;
  end;
  Multi := TMUICheckmark.Create;
  Multi.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'SaveMode';
    Parent := SG;
  end;
  SaveMode := TMUICheckmark.Create;
  SaveMode.Parent := SG;

  with TMUIButton.create do
  begin
    Contents := 'Execute';
    OnClick := @FileStart;
    Parent := FG;
  end;


  // ################## FontDialog
  FG := TMUIGroup.Create;
  with FG do
  begin
    Parent := Pages;
  end;

  SG := TMUIGroup.Create;
  with SG do
  begin
    Columns := 2;
    Parent := FG;
  end;

  with TMUIText.Create do
  begin
    Contents := 'Title Text';
    Parent := SG;
  end;
  FontTitle := TMUIString.Create;
  FontTitle.Contents := 'Select Font';
  FontTitle.FixWidth := 150;
  FontTitle.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Style';
    Parent := SG;
  end;
  Style := TMUICheckmark.Create;
  Style.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Draw Mode';
    Parent := SG;
  end;
  DrawMode := TMUICheckmark.Create;
  DrawMode.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Front Pen';
    Parent := SG;
  end;
  FrontPen := TMUICheckmark.Create;
  FrontPen.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Back Pen';
    Parent := SG;
  end;
  BackPen := TMUICheckmark.Create;
  BackPen.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Fixed Fonts only';
    Parent := SG;
  end;
  FixedOnly := TMUICheckmark.Create;
  FixedOnly.Parent := SG;

  with TMUIButton.create do
  begin
    Contents := 'Execute';
    OnClick := @FontStart;
    Parent := FG;
  end;

  //####################### FScreenDialog
  FG := TMUIGroup.Create;
  with FG do
  begin
    Parent := Pages;
  end;

  SG := TMUIGroup.Create;
  with SG do
  begin
    Columns := 2;
    Parent := FG;
  end;

  with TMUIText.Create do
  begin
    Contents := 'Title Text';
    Parent := SG;
  end;
  ScTitle := TMUIString.Create;
  ScTitle.Contents := 'Select ScreenMode';
  ScTitle.FixWidth := 150;
  ScTitle.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Width Editable';
    Parent := SG;
  end;
  WidthEdit := TMUICheckmark.Create;
  WidthEdit.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Height Editable';
    Parent := SG;
  end;
  HeightEdit := TMUICheckmark.Create;
  HeightEdit.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Depth Editable';
    Parent := SG;
  end;
  DepthEdit := TMUICheckmark.Create;
  DepthEdit.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Overscan Type';
    Parent := SG;
  end;
  Overscan := TMUICheckmark.Create;
  Overscan.Parent := SG;

  with TMUIText.Create do
  begin
    Contents := 'Set AutoScroll';
    Parent := SG;
  end;
  AutoScroll := TMUICheckmark.Create;
  AutoScroll.Parent := SG;


  with TMUIButton.create do
  begin
    Contents := 'Execute';
    OnClick := @ScreenStart;
    Parent := FG;
  end;

  Memo := TMUIFloatText.Create;
  with Memo do
  begin
    Parent := Self;
  end;

end;


procedure TMyWindow.FileStart(Sender: TObject);
var
  FD: TFileDialog;
begin
  FD := TFileDialog.Create;
  FD.MultiSelect := Multi.Selected;
  FD.SaveMode := SaveMode.Selected;
  FD.TitleText := FileTitle.Contents;
  FD.Pattern := Pattern.Contents;
  if FD.Execute then
  begin
    if FD.SaveMode then
      Memo.Text := 'File selected to save: '#10 + FD.Filename
    else
      Memo.Text := 'File(s) selected to load: '#10 + FD.Filenames.Text;
  end;
  FD.Free;
end;

procedure TMyWindow.FontStart(Sender: TObject);
var
  FO: TFontDialog;
  s: string;
begin
  FO := TFontDialog.Create;
  Fo.TitleText := FontTitle.Contents;
  FO.Options := [];
  if FrontPen.Selected then
    FO.Options := FO.Options + [foFrontPen];
  if BackPen.Selected then
    FO.Options := FO.Options + [foBackPen];
  if DrawMode.Selected then
    FO.Options := FO.Options + [foDrawMode];
  if Style.Selected then
    FO.Options := FO.Options + [foStyle];
  if FixedOnly.Selected then
    FO.Options := FO.Options + [foFixedOnly];
  if FO.Execute then
  begin
    s := 'Font Selected: '#10 + FO.Name + ': ' +IntToStr(FO.Size) + #10;
    if FrontPen.Selected then
      S := S + '  FrontPen: ' + IntToStr(FO.FrontPen) + #10;
    if BackPen.Selected then
      S := S + '  BackPen: ' + IntToStr(FO.BackPen) + #10;
    if DrawMode.Selected then
      S := S + '  DrawMode: ' + IntToStr(FO.DrawMode) + #10;
    if Style.Selected then
      S := S + '  Style: ' + IntToStr(FO.Style) + #10;
    Memo.Text := s;
  end;
  FO.Free;
end;

procedure TMyWindow.ScreenStart(Sender: TObject);
var
  SD: TScreenDialog;
  s: string;
begin
  SD := TScreenDialog.Create;
  SD.TitleText := ScTitle.Contents;
  SD.Options := [];
  if WidthEdit.Selected then
    SD.Options := SD.Options + [soWidth];
  if HeightEdit.Selected then
    SD.Options := SD.Options + [soHeight];
  if DepthEdit.Selected then
    SD.Options := SD.Options + [soDepth];
  if Overscan.Selected then
    SD.Options := SD.Options + [soOverscanType];
  if AutoScroll.Selected then
    SD.Options := SD.Options + [soAutoscroll];
  if SD.Execute then
  begin
    s:= 'Screen Selected: ' + IntToStr(SD.DisplayID) + ' - ' + IntToStr(SD.DisplayWidth) + 'x' + IntToStr(SD.DisplayHeight) + 'x' + IntToStr(SD.DisplayDepth)+#10;
    if Overscan.Selected then
      s := s + '  Overscan: ' + IntToStr(SD.OverscanType) + #10;
    if Autoscroll.Selected then
      s := s + '  AutoScroll: ' + BoolToStr(SD.Autoscroll, True) + #10;
    Memo.Text := s;
  end;
end;

begin
  TMyWindow.Create;
  MUIApp.Run;
end.
