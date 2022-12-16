unit uPlotFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo;

type
  TPlotFrame = class(TFrame)
    Memo1: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
    procedure PlotData(const JSonStr: String);
  end;

implementation

{$R *.fmx}

uses JSON, REST.JSON;

{ TPlotFrame }

procedure TPlotFrame.PlotData(const JSonStr: String);
var
  JsonValue: TJSONObject;
begin
  JsonValue := TJSONObject.ParseJSONValue(JSonStr) as TJSONObject;
  Memo1.text := TJson.Format(JsonValue);
  FreeAndNil(JsonValue);
end;

end.
