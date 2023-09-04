unit uXPlotFrame;

// uses TPath to plot the charts. Wanted to remove TeeChart dependency so
// compiles on a  Vanilla version of Delphi

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, Generics.Collections,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Memo.Types, FMX.ScrollBox, FMX.Memo, FMX.Layouts,
  FMX.Controls.Presentation;

type
  unixdate = int64;
  cPrice = double;

type
  TPlotData = record
    fPrice: cPrice;
    fVolume: double;
    constructor Create(const P: cPrice; Volume: double);
  end;

type
  TPlotdata2 = TList<TPair<unixdate, TPlotData>>;

type
  TXPlotFrame = class(TFrame)
    BkgRectangle: TRectangle;
    VolLabel: TLabel;
    PricePath: TPath;
    VolPath: TPath;
    PriceLayout: TLayout;
    PriceLabel: TLabel;
    VolLayout: TLayout;
    Splitter1: TSplitter;
    Ellipse1: TEllipse;
    DetailMemo: TMemo;
    Line1: TLine;
    Ellipse2: TEllipse;
    procedure PricePathResized(Sender: TObject);
    procedure PricePathMouseEnter(Sender: TObject);
    procedure PricePathMouseLeave(Sender: TObject);
    procedure PricePathMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure PricePathPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure VolPathPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure FrameResized(Sender: TObject);
    procedure VolPathMouseEnter(Sender: TObject);
    procedure VolPathMouseLeave(Sender: TObject);
    procedure VolPathMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure VolLabelMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Single);
    procedure VolLabelMouseLeave(Sender: TObject);
    procedure VolLabelMouseEnter(Sender: TObject);
    procedure BkgRectanglePaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    { Private declarations }
    fPathData: TPathData;
    function Json2Plotdata(const Str: String): TPlotdata2;
    procedure AfterConstruction; override;
  public
    { Public declarations }

    procedure _PlotData(const JSonStr: String);
    procedure RefreshGraphs();
    destructor Destroy; override;
  end;

implementation

uses System.DateUtils, JSON, System.Generics.Collections, System.Math.Vectors,
  System.Math, uCryptoData;

{$R *.fmx}

type
  TRoundedValues = record
    UpperValue: integer;
    LowerValue: integer;
    factor: double;
  end;

type
  TMappedRecord = record
    Date: TDate;
    MinFromStartDT: int64; // used to plot along the y axis
    oPrice: cPrice; // original Price
    mPrice: cPrice; // mapped Price  to Graph
    oVolume: double;
    mVolume: double;
  end;

type
  TMappedRecords = array of TMappedRecord;

  TMappedRecordsHelper = record helper for TMappedRecords
    function ToString: String;
    procedure SaveToFile(const FName: String);
    function MaxPrice: cPrice;
    function MinPrice: cPrice;
    function MaxVolume: double;
    function MinVolume: double;
    function PriceIncreased: Boolean;
    function VolumeIncreased: Boolean;
  end;

var
  MappedRecords: TMappedRecords;
  PlotGraph: Boolean = False;
  PriceLowerValue, PriceUpperValue: Extended;
  VolLowerValue, VolUpperValue: Extended;
  XFrequency: Single;
  StrFormat: String; // Date formater
  DTMarkers: TList<TPair<int64, TDateTime>>;

function xlceil(const value: double; const roundto: integer): integer;
begin
  Result := roundto * ceil(value / roundto);
end;

function xlfloor(const value: double; const roundto: integer): integer;
begin
  Result := roundto * floor(value / roundto);
end;

function xlRound(const value: double; const roundto: integer): TRoundedValues;
const
  iLen = 4; // 123.456 naming convention mantissa.characteristic
var
  oExp, fExp: integer;
  factor: Extended;
begin
  var
    Str: String := '-08';
  var
  eStrMax := FloatToStrF(value, ffExponent, 4, 2);
  var
  eStrMin := FloatToStrF(value, ffExponent, 4, 2);
  // ie for 1234.567 Using 4,4 = 1.235E+0003

  var
  List := TStringList.Create;
  try
    ExtractStrings(['E'], [], PChar(eStrMax), List);
    Str := List[1];
    oExp := Str.ToInteger();
    // V := List[0].ToInteger();
  finally
    List.Free;
  end;

  fExp := iLen - oExp;
  factor := power(10, fExp);
  Result.UpperValue := xlceil(value * factor, roundto);
  Result.LowerValue := xlfloor(value * factor, roundto);
  Result.factor := factor;
end;

function mapRange(const output_end, output_start, input_end,
  input_start: Single; const input: Single): Single;
begin
  var
    slope: Single := ((output_end - output_start) / (input_end - input_start));
  Result := output_start + slope * (input - input_start)
end;

{ TXPlotFrame }

procedure TXPlotFrame.AfterConstruction;
begin
  inherited;
  DetailMemo.Visible := False;
  fPathData := TPathData.Create;
  DTMarkers := TList < TPair < int64, TDateTime >>.Create();

  Line1.Visible := False;
  Line1.Enabled := False;
  DetailMemo.Enabled := False;
  DetailMemo.Visible := False;
  Ellipse1.Visible := False;
  Ellipse2.Visible := False;
end;

destructor TXPlotFrame.Destroy;
begin
  DTMarkers.Free;
  fPathData.Free;
  inherited;
end;

procedure TXPlotFrame.FrameResized(Sender: TObject);
begin
  RefreshGraphs;
end;

function TXPlotFrame.Json2Plotdata(const Str: String): TPlotdata2;
var
  JsonValue: TJSONObject;
  jPItem, jVItem, jPArray, jVArray: TJSONArray;
  Dunixdate1, DUnixdate2: int64;
  Volume, price: double;
  I: integer;

begin
  Result := nil;
  JsonValue := TJSONObject.ParseJSONValue(Str) as TJSONObject;
  if Assigned(JsonValue) then
  begin
    jPArray := JsonValue.GetValue('stats') as TJSONArray;
    jVArray := JsonValue.GetValue('total_volumes') as TJSONArray;
    if jPArray.Count <> jVArray.Count then
    begin
      Log.d('Error');
    end;
    if Assigned(jPArray) then // and Assigned(jVArray) then
    begin
      Result := TPlotdata2.Create();
      // Ensure we get the last element
      // then reverse it at the end of the loop

      // First pass get the actual price n volume
      // We work map to canvas coordinates in PricePathResized
      for I := jPArray.Count - 1 DownTo 0 do
      begin
        jPItem := jPArray.Items[I] as TJSONArray;
        jPItem.Items[0].TryGetValue(Dunixdate1); // milliseconds
        jPItem.Items[1].TryGetValue(price);
        if Assigned(jVArray) then
        begin
          if I < jVArray.Count then
          begin
            jVItem := jVArray.Items[I] as TJSONArray;
            jVItem.Items[0].TryGetValue(DUnixdate2);
            // Assert(Dunixdate1 = DUnixdate2);
            jVItem.Items[1].TryGetValue(Volume);
          end;
        end;
        Dunixdate1 := round(Dunixdate1 / 1000);
        Result.Add(TPair<unixdate, TPlotData>.Create(Dunixdate1,
          TPlotData.Create(price, Volume)));
      end;
      Result.Reverse;
    end;
    JsonValue.Free;
  end;
end;

procedure TXPlotFrame.PricePathMouseEnter(Sender: TObject);
begin
  if not PlotGraph then
    exit;
  Line1.Enabled := True;
  Line1.Visible := True;
  Line1.Opacity := 0.5;
  DetailMemo.Enabled := True;
  DetailMemo.Visible := True;
  Ellipse1.Visible := True;
  Ellipse2.Visible := True;
end;

procedure TXPlotFrame.PricePathMouseLeave(Sender: TObject);
begin
  if not PlotGraph then
    exit;
  Line1.Visible := False;
  Line1.Enabled := False;
  DetailMemo.Enabled := False;
  DetailMemo.Visible := False;
  Ellipse1.Visible := False;
  Ellipse2.Visible := False;
end;

procedure TXPlotFrame.PricePathMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
var
  P1, P2: TPointF;
  n, I: integer;

begin
  if not PlotGraph then
    exit;

  P1 := PricePath.LocalToAbsolute(PointF(X, Y));
  P2 := PricePath.LocalToAbsolute(PointF(X, PricePath.Height));

  Line1.Position.X := X;
  Line1.Position.Y := 0;
  Line1.Height := PricePath.Height + Splitter1.Height + VolLayout.Height;
  if Line1.Position.X + DetailMemo.Width > PricePath.Width then
    DetailMemo.Position.X := X - 175
  else
    DetailMemo.Position.X := X + 15;

  DetailMemo.Position.Y := Y; // P1.Y;
  var
  MinFromTheStart := round(X / XFrequency);
  I := -1;
  repeat
    inc(I);
  until (MappedRecords[I].MinFromStartDT >= MinFromTheStart);

  n := I;
  Ellipse1.Position.X := X - Ellipse1.Width / 2;
  Ellipse1.Position.Y := PricePath.Height - MappedRecords[I].mPrice -
    Ellipse1.Height / 2; // - Path1.Height;

  Ellipse2.Position.X := X - Ellipse2.Width / 2;
  Ellipse2.Position.Y := VolPath.Height - MappedRecords[I].mVolume -
    Ellipse2.Height / 2; // - Path1.Height;

  try
    DetailMemo.text := '';
    DetailMemo.Lines.Add(FormatDateTime('dd/mmm/yy t', MappedRecords[n].Date));
    DetailMemo.Lines.Add(Format('%m', [MappedRecords[n].oPrice]));
    DetailMemo.Lines.Add(FloatToStrF(MappedRecords[n].oVolume,
      ffExponent, 4, 2));
  except
    on E: EIntOverflow do
    begin
      Log.d('EIntOverflow');
    end;
  end;
end;

procedure TXPlotFrame.PricePathPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  NoofPoints, I: integer;
  strdata, Str: String;
  FString, mString: TMRStringList;
  M1, M2, M3, m4: TMatrix;

begin
  if not PlotGraph then
    exit;

  var
  Save := Canvas.SaveState;
  try
    M1 := Canvas.Matrix.CreateScaling(1, -1);
    M2 := Canvas.Matrix.CreateTranslation(0, ARect.Height);
    M3 := Canvas.Matrix;
    m4 := M1 * M2 * M3;
    Canvas.SetMatrix(m4);
    NoofPoints := High(MappedRecords);
    strdata := 'M0.0,0.0';
    FString.List.Add(strdata);
    for I := 0 to NoofPoints do
    begin
      Str := Format('L%.4f,%.4f ',
        [MappedRecords[I].MinFromStartDT * XFrequency,
        MappedRecords[I].mPrice]);
      FString.List.Add(Str);
      strdata := strdata + Str;
    end;
    Str := Format('L%.4f,%.4f ', [MappedRecords[I - 1].MinFromStartDT *
      XFrequency, 0.0]);
    FString.List.Add(Str);
    strdata := strdata + Str;
{$IFDEF DEBUG}
    FString.DumpToFile('strdata.txt');
{$ENDIF}
    fPathData.Data := strdata;

    Canvas.Stroke.Join := TStrokeJoin.round;
    Canvas.Stroke.Thickness := 2;

    if MappedRecords.PriceIncreased then
    begin
      Canvas.Stroke.Color := TAlphaColorRec.Darkgreen;
      Canvas.Fill.Color := TAlphaColorRec.Darkgrey;
    end
    else
    begin
      Canvas.Stroke.Color := TAlphaColorRec.Firebrick;
      Canvas.Fill.Color := TAlphaColor($FFF4CCCC);
    end;
    Canvas.DrawPath(fPathData, 0.75);
    Canvas.FillPath(fPathData, 0.5);
  finally
    Canvas.RestoreState(Save);
  end;
end;

procedure TXPlotFrame.PricePathResized(Sender: TObject);
var
  PriceMin, PriceMax: double;
  VolMin, VolMax: double;
  I: integer;
  FString, mString: TMRStringList;
  RV: TRoundedValues;

begin
  if not PlotGraph then
    exit;

  PriceMin := MappedRecords.MinPrice;
  PriceMax := MappedRecords.MaxPrice;
  VolMin := MappedRecords.MinVolume;
  VolMax := MappedRecords.MaxVolume;

  RV := xlRound(PriceMax, 50);
  PriceUpperValue := RV.UpperValue / RV.factor;
  RV := xlRound(PriceMin, 50);
  PriceLowerValue := RV.LowerValue / RV.factor;
  RV := xlRound(VolMax, 50);
  VolUpperValue := RV.UpperValue / RV.factor;
  RV := xlRound(VolMin, 50);
  VolLowerValue := RV.LowerValue / RV.factor;

  XFrequency := PricePath.Width / MappedRecords[High(MappedRecords)
    ].MinFromStartDT;
  for I := Low(MappedRecords) to High(MappedRecords) do // - 1 do
  begin
    MappedRecords[I].mPrice := mapRange(PricePath.Height, 0, PriceUpperValue,
      PriceLowerValue, MappedRecords[I].oPrice);
    MappedRecords[I].mVolume := mapRange(VolPath.Height, 0, VolUpperValue,
      VolLowerValue, MappedRecords[I].oVolume);
    mString.List.Add(Format('%s,Mins:%d,Xpos(Mins(x)Xpos):%.5f,[%.5f:%.5f]',
      [FormatDateTime('dd/mm/yy t', MappedRecords[I].Date),
      MappedRecords[I].MinFromStartDT, MappedRecords[I].MinFromStartDT *
      XFrequency, MappedRecords[I].oPrice, MappedRecords[I].mPrice]));
  end;
  mString.List.Add(Format('==== XFrequency : %.8f', [XFrequency]));
{$IFDEF DEBUG}
  mString.DumpToFile('MappedRecArray.txt');
  MappedRecords.SaveToFile('MappedRecords.txt');
{$ENDIF}
end;

procedure TXPlotFrame.BkgRectanglePaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
const
  SizeofMarkers = 10;
  YAxisTxtMargin = 10;
  OpacityValue = 100;
  StrokeThickness = 2;
  TextRectOffset = 8;

var
  I: integer;
  s, XDiv: Single;
  RectT: TRectF;
  LogStr: TMRStringList;
  pos: Single;
  PricePlotPt, VolPlotPt: TPointF;
  DI: TPair<int64, TDateTime>;

begin
  if not PlotGraph then
    exit;

  XDiv := PricePath.BoundsRect.Height / 10;
  PricePlotPt := TPointF.Create(PriceLayout.BoundsRect.TopLeft.X,
    PriceLayout.BoundsRect.TopLeft.Y + PriceLabel.Height);

  // Y-Axis
  var
  Save := Canvas.SaveState;
  try
    Canvas.Stroke.Color := TAlphaColorRec.Blue;
    for I in [0, 5, 10] do
    begin
      s := PricePlotPt.Y + (XDiv * I);
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      Canvas.DrawLine(PointF(PricePlotPt.X - SizeofMarkers, s),
        PointF(PricePlotPt.X, s), AbsoluteOpacity, Canvas.Stroke);
      // extended the X as a dotted line across the Graph,
      Canvas.Stroke.Dash := TStrokeDash.Dash;
      Canvas.DrawLine(PointF(PricePlotPt.X, s),
        PointF(PricePlotPt.X + PricePath.Width, s), 0.15, Canvas.Stroke);
      RectT := RectF(10, s - TextRectOffset, 100, s - TextRectOffset + 15);
      if I = 0 then
        Canvas.FillText(RectT, FormatFloat('0.00', PriceUpperValue), False, 100,
          [], TTextAlign.Center)
      else if I = 5 then
        Canvas.FillText(RectT, FormatFloat('0.00',
          (PriceLowerValue + PriceUpperValue) / 2), False, 100, [],
          TTextAlign.Center)
      else if I = 10 then
        Canvas.FillText(RectT, FormatFloat('0.00', PriceLowerValue), False, 100,
          [], TTextAlign.Center);
    end;

    XDiv := VolPath.BoundsRect.Height / 10;
    VolPlotPt := TPointF.Create(VolLayout.BoundsRect.TopLeft.X,
      VolLayout.BoundsRect.TopLeft.Y + VolLabel.Height);

    for I in [0, 5, 10] do
    begin
      s := VolPlotPt.Y + (XDiv * I);
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      Canvas.DrawLine(PointF(VolPlotPt.X - SizeofMarkers, s),
        PointF(VolPlotPt.X, s), AbsoluteOpacity, Canvas.Stroke);
      // extended the X as a dotted line across the Graph,
      Canvas.Stroke.Dash := TStrokeDash.Dash;
      Canvas.DrawLine(PointF(VolPlotPt.X, s),
        PointF(VolPlotPt.X + PricePath.Width, s), 0.15, Canvas.Stroke);
      RectT := RectF(10, s - TextRectOffset, 100, s - TextRectOffset + 15);
      if I = 0 then
        Canvas.FillText(RectT, FloatToStrF(VolUpperValue, ffExponent, 4, 2),
          False, 100, [], TTextAlign.Center)
      else if I = 5 then
        Canvas.FillText(RectT,
          FloatToStrF(((VolUpperValue + VolLowerValue) / 2), ffExponent, 4, 2),
          False, 100, [], TTextAlign.Center)
      else if I = 10 then
        Canvas.FillText(RectT, FloatToStrF(VolLowerValue, ffExponent, 4, 2),
          False, 100, [], TTextAlign.Center)
    end;
    Canvas.Stroke.Dash := TStrokeDash.Solid;

    // X-Axis
    if DTMarkers.Count > 0 then
    begin
      Canvas.Stroke.Thickness := 2; // StrokeThickness;
      Canvas.Stroke.Color := TAlphaColorRec.Blue;
      Canvas.Stroke.Kind := TBrushKind.Solid;
      for DI in DTMarkers do
      begin
        pos := DI.Key * XFrequency;
        LogStr.List.Add(Format('%s : MinBetween:%d,XFreq: %.8f, XPos: %.8f ',
          [FormatDateTime('dd-mm-yy t', DI.value), DI.Key, XFrequency, pos]));
        Canvas.DrawLine(PointF(VolPlotPt.X + pos - StrokeThickness,
          VolLayout.BoundsRect.Bottom),
          PointF(VolPlotPt.X + pos - StrokeThickness,
          VolLayout.BoundsRect.Bottom + SizeofMarkers), 1, Canvas.Stroke);
        RectT := RectF(VolLayout.BoundsRect.left + pos - 100,
          VolLayout.BoundsRect.Bottom + 4, VolPlotPt.X + pos - YAxisTxtMargin,
          VolLayout.BoundsRect.Bottom + 18);
        Canvas.FillText(RectT, FormatDateTime(StrFormat, DI.value), False,
          OpacityValue, [], TTextAlign.Trailing);
      end;
      Canvas.Stroke.Thickness := 1;
    end;
  finally
    Canvas.RestoreState(Save);
  end;
end;

procedure TXPlotFrame.RefreshGraphs;
begin
  PricePathResized(nil);
  Repaint();
end;

procedure TXPlotFrame.VolLabelMouseEnter(Sender: TObject);
begin
  PricePathMouseEnter(Sender);
end;

procedure TXPlotFrame.VolLabelMouseLeave(Sender: TObject);
begin
  PricePathMouseLeave(Sender);
end;

procedure TXPlotFrame.VolLabelMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  PricePathMouseMove(Sender, Shift, X, Y);
end;

procedure TXPlotFrame.VolPathMouseEnter(Sender: TObject);
begin
  PricePathMouseEnter(Sender);
end;

procedure TXPlotFrame.VolPathMouseLeave(Sender: TObject);
begin
  PricePathMouseLeave(Sender);
end;

procedure TXPlotFrame.VolPathMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  PricePathMouseMove(Sender, Shift, X, Y);
end;

procedure TXPlotFrame.VolPathPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  NoofPoints: integer;
  strdata, Str: String;
  FString, mString: TMRStringList;
  I: integer;
  M1, M2, M3, m4: TMatrix;

begin
  if not PlotGraph then
    exit;

  var
  Save := Canvas.SaveState;
  try

    Canvas.Fill.Color := TAlphaColorRec.Black;
    M1 := Canvas.Matrix.CreateScaling(1, -1);
    M2 := Canvas.Matrix.CreateTranslation(0, ARect.Height);
    M3 := Canvas.Matrix;
    m4 := M1 * M2 * M3;
    Canvas.SetMatrix(m4);
    NoofPoints := High(MappedRecords);
    strdata := 'M0.0,0.0';
    FString.List.Add(strdata);
    for I := 0 to NoofPoints do
    begin
      Str := Format('L%.4f,%.4f ',
        [MappedRecords[I].MinFromStartDT * XFrequency,
        MappedRecords[I].mVolume]);
      FString.List.Add(Str);
      strdata := strdata + Str;
    end;
    Str := Format('L%.4f,%.4f ', [MappedRecords[I - 1].MinFromStartDT *
      XFrequency, 0.0]);
    FString.List.Add(Str);
    strdata := strdata + Str;
{$IFDEF DEBUG}
    FString.DumpToFile('PlotData.txt');
{$ENDIF}
    fPathData.Data := strdata;

    Canvas.Stroke.Join := TStrokeJoin.round;
    Canvas.Stroke.Thickness := 1;

    if MappedRecords.VolumeIncreased then
    begin
      Canvas.Stroke.Color := TAlphaColorRec.Darkgreen;
      Canvas.Fill.Color := TAlphaColorRec.Lightgreen;
    end
    else
    begin
      Canvas.Stroke.Color := TAlphaColorRec.Firebrick;
      Canvas.Fill.Color := TAlphaColor($FFF4CCCC);
    end;
    Canvas.DrawPath(fPathData, 0.75);
    Canvas.FillPath(fPathData, 0.10);
  finally
    Canvas.RestoreState(Save);
  end;
end;

procedure TXPlotFrame._PlotData(const JSonStr: String);
var
  I, DaySpan: integer;
  Item: TPair<unixdate, TPlotData>;
  FString: TMRStringList;
  StartDT, EndDT, d: TDateTime;
  PlotData: TPlotdata2;

begin
  PlotGraph := False;
  if JSonStr.Trim.IsEmpty then
  begin
    exit;
  end;

  PlotData := Json2Plotdata(JSonStr);
  FString.List.text := PlotData.ToString();
{$IFDEF DEBUG}
  FString.DumpToFile('PlotData.txt');
{$ENDIF}
  setLength(MappedRecords, PlotData.Count);

  I := 0;
  for Item in PlotData do
  begin
    MappedRecords[I].oPrice := Item.value.fPrice;
    MappedRecords[I].mPrice := 0; // This will be done in Path1Paint
    MappedRecords[I].oVolume := Item.value.fVolume;
    MappedRecords[I].mVolume := 0;
    MappedRecords[I].Date := UnixToDateTime(Item.Key);
    if I = 0 then
      StartDT := MappedRecords[I].Date;
    MappedRecords[I].MinFromStartDT :=
      MinutesBetween(MappedRecords[I].Date, StartDT);
    inc(I);
  end;
  // Calculate time span and corresponding X axis markers
  DTMarkers.Clear;
  EndDT := MappedRecords[High(MappedRecords)].Date;
  DaySpan := DaysBetween(EndDT, StartDT);
  d := Dateof(StartDT);
  StrFormat := 'dd-mm-yy';
  if DaySpan = 0 then // 1 days result
  begin
    StrFormat := 't mmm dd';
    d := inchour(d, HourOf(StartDT));
    d := inchour(d, 3);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := inchour(d, 3)
    until d > EndDT;
  end
  else if DaySpan in [6, 7, 13, 14] then // 12 wk 1 month
  begin
    d := IncDay(d, 1);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := IncDay(d, 1)
    until d > EndDT;
  end
  else if DaySpan in [29, 30] then // 12 wk 1 month
  begin
    d := IncDay(d, 3);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := IncDay(d, 3)
    until d > EndDT;
  end
  else if DaySpan in [89, 90] then // 1 week result
  begin
    d := IncDay(d, 7);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := IncDay(d, 7)
    until d > EndDT;
  end
  else if DaySpan in [179, 180] then // 1 week result
  begin
    d := IncDay(d, 14);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := IncDay(d, 14)
    until d > EndDT;
  end
  else if (DaySpan = 364) or (DaySpan = 365) then
  begin // 1 year
    d := IncDay(d, 28);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := IncDay(d, 28)
    until d > EndDT;
  end
  else
  begin // 1 year
    d := IncDay(d, 240);
    repeat
      DTMarkers.Add(TPair<int64, TDateTime>.Create(MinutesBetween(d,
        StartDT), d));
      d := IncDay(d, 240)
    until d > EndDT;
  end;
  PlotData.Free;
  PlotGraph := True;
  PricePathResized(nil);
  Repaint();
end;

{ TMappedRecordsHelper }

function TMappedRecordsHelper.MaxPrice: cPrice;
var
  I: integer;
begin
  Result := MappedRecords[Low(MappedRecords)].oPrice;
  for I := Low(MappedRecords) + 1 to High(MappedRecords) do
    if Result < MappedRecords[I].oPrice then
      Result := MappedRecords[I].oPrice;
end;

function TMappedRecordsHelper.MaxVolume: double;
var
  I: integer;
begin
  Result := MappedRecords[Low(MappedRecords)].oVolume;
  for I := Low(MappedRecords) to High(MappedRecords) do
    if Result < MappedRecords[I].oVolume then
      Result := MappedRecords[I].oVolume;
end;

function TMappedRecordsHelper.MinPrice: cPrice;
var
  I: integer;
begin
  Result := MappedRecords[Low(MappedRecords)].oPrice;
  for I := Low(MappedRecords) + 1 to High(MappedRecords) do
    if Result > MappedRecords[I].oPrice then
      Result := MappedRecords[I].oPrice;
end;

function TMappedRecordsHelper.MinVolume: double;
var
  I: integer;
  d: double;
begin
  d := MappedRecords[Low(MappedRecords)].oVolume;
  for I := Low(MappedRecords) + 1 to High(MappedRecords) do
    if d > MappedRecords[I].oVolume then
      d := MappedRecords[I].oVolume;

  Result := d;
end;

function TMappedRecordsHelper.PriceIncreased: Boolean;
var
  a, b: double;
begin
  a := MappedRecords[Low(MappedRecords) + 1].oPrice;
  b := MappedRecords[High(MappedRecords)].oPrice;
  Result := b > a;
end;

function TMappedRecordsHelper.VolumeIncreased: Boolean;
var
  a, b: double;
begin
  a := MappedRecords[Low(MappedRecords) + 1].oVolume;
  b := MappedRecords[High(MappedRecords)].oVolume;
  Result := b > a;
end;

procedure TMappedRecordsHelper.SaveToFile(const FName: String);
var
  t: TMRStringList;
begin
  t.List.text := Self.ToString();
  t.List.SaveToFile(FName);
end;

function TMappedRecordsHelper.ToString: String;
var
  I: integer;
  Str: String;
begin
  // Firstline Header.
  Str := 'MinFromStart,oPrice,mPrice,oVol,mVol' + sLineBreak;
  for I := Low(MappedRecords) to High(MappedRecords) do
  begin
    Str := Str + Format('%d,%.8f,%.8f,%.8f,%.8f%s',
      [MappedRecords[I].MinFromStartDT, MappedRecords[I].oPrice,
      MappedRecords[I].mPrice, MappedRecords[I].oVolume,
      MappedRecords[I].mVolume, sLineBreak]);
  end;
  Result := Str;
end;

{ TPlotData }

constructor TPlotData.Create(const P: cPrice; Volume: double);
begin
  fPrice := P;
  fVolume := Volume;
end;

end.
