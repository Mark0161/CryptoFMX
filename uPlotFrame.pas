unit uPlotFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMXTee.Engine, FMXTee.Series, FMXTee.Procs, FMXTee.Chart, uDataStruct,
  FMX.TabControl;

type
  TPlotFrame = class(TFrame)
    TabControl: TTabControl;
    PriceTab: TTabItem;
    VolTab: TTabItem;
    PriceChart: TChart;
    LineSeries1: TLineSeries;
    VolChart: TChart;
    LineSeries2: TLineSeries;
  private
    { Private declarations }

  public
    { Public declarations }
    procedure PlotData(const JSonStr: String);
    function Json2Plotdata(const Str: String): TPlotData2;
  end;

implementation

{$R *.fmx}

uses System.DateUtils, JSON, REST.JSON, System.Generics.Collections;

{ TPlotFrame }

function TPlotFrame.Json2Plotdata(const Str: String): TPlotData2;
var
  JsonValue: TJSONObject;
  jPItem, jVItem, jPArray, jVArray: TJSONArray;
  Dunixdate1, DUnixdate2: Int64;
  Volume, price: Double;
  I: Integer;

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
    if Assigned(jPArray) then
    begin
      Result := TPlotData2.Create();
      // Ensure we get the last element
      // then reverse it at the end of the loop

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
        Result.Add(TPair<unixdate, TPlotRec>.Create(Dunixdate1,
          TPlotRec.Create(price, Volume)));
      end;
      Result.Reverse;
    end;
    JsonValue.free;
  end;
end;

procedure TPlotFrame.PlotData(const JSonStr: String);
var
  JsonValue: TJSONObject;
  PlotData: TPlotData2;
  I: Integer;
  Item: TPair<unixdate, TPlotRec>;
  // http://www.teechart.net/docs/teechart/vclfmx/tutorials/UserGuide/html/manu423y.htm#creating_series_at_runtime
begin
  PlotData := Json2Plotdata(JSonStr);
  I := 0;
  PriceChart.Series[0].Clear;
  VolChart.Series[0].Clear;
  for Item in PlotData do
  begin
    // PriceChart.Series[0].Add(Item.value.fPrice);
    // PriceChart.Series[0].Add(Item.value.fPrice,UnixToDateTime(Item.Key));
    PriceChart.Series[0].AddXY(UnixToDateTime(Item.Key), Item.value.fPrice);
    VolChart.Series[0].AddXY(UnixToDateTime(Item.Key), Item.value.fVolume);
   // VolChart.Series[0].Add(Item.value.fVolume);
  end;
  PlotData.free;

end;

end.
