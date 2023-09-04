unit uViewModel;

// All the async/ui coordination takes place here
// only one Async operation takes place at a time see fTaskActive

interface

uses System.SysUtils, System.Classes, System.Threading, Generics.Collections,
  FMX.Graphics, FMX.ListView, FMX.ListView.Appearances, uCryptoData;

type
  TVDMethod = Procedure(const Active: Boolean) of Object;
  TVDPlotDataMethod = Procedure(Data: String) of Object;
  TVDisplayCryptoDetails = Procedure of Object;

type
  TViewModel = class
  private
    class var fListView: TListView;
    class var fAsyncIndicator: TVDMethod;
    class var fAsyncPlotGraph: TVDPlotDataMethod;
    class var fDisplayCryptoDetails: TVDisplayCryptoDetails;
    class var fSelectedItem: Integer;
    class var fSelectedPlot: Integer; // 24hour
    class var fTaskActive: Integer;

    class function AsyncFn_UpdateListView(): IFuture<Boolean>; static;
    class function AsyncFn_GetPriceChart(datetimeurl: String)
      : IFuture<String>; static;

    class function GetCryptoName: String; static;
    class function GetLast_updated: String; static;
    class function GetCryptoID: String; static;
    class function GetCryptoCurrent_price: Currency; static;
    class function GetCryptoMarket_cap: Currency; static;
    class function GetCryptoTotal_supply: double; static;
    class function GetCryptoMax_supply: double; static;
    class function GetCryptoPrice_change_percentage_24h: double; static;
    class function GetCryptoTotal_volume: UINT64; static;
  public
    class property TaskActive: Integer read fTaskActive;
    class property SelectedItem: Integer read fSelectedItem write fSelectedItem;
    class property SelectedPlot: Integer read fSelectedPlot write fSelectedPlot;
    class property CryptoName: String read GetCryptoName;
    class property Last_updated: String read GetLast_updated;
    class property CryptoID: String read GetCryptoID;
    class property CryptoCurrent_price: Currency read GetCryptoCurrent_price;
    class property CryptoMarket_cap: Currency read GetCryptoMarket_cap;
    class property CryptoTotal_supply: double read GetCryptoTotal_supply;
    class property CryptoMax_supply: double read GetCryptoMax_supply;
    class property CryptoPrice_change_percentage_24h: double
      read GetCryptoPrice_change_percentage_24h;
    class property CryptoTotal_volume: UINT64 read GetCryptoTotal_volume;
    // Used to Synchronise UI stuff with PPL
    class procedure UIRegisterProcListView(ListView: TListView); static;
    class procedure UIRegisterProcAsyncIndicator(IndicatorProc
      : TVDMethod); static;
    class procedure UIRegisterProcDisplayCryptoDetails(DisplayCryptoDetails
      : TVDisplayCryptoDetails); static;
    class procedure UIRegisterProcPlotGraph(const PlotGraph
      : TVDPlotDataMethod); static;

    class procedure DisplayData(); static;
    class procedure DisplayPlot(); static;
    class function GetBitmap: TBitmap; static;

    class constructor Create;
    class destructor Destroy;
  end;

implementation

{ TViewModel }

uses System.UITypes, FMX.ListView.Types, FMX.Dialogs, uDM;

const
  PlotDataValues: TArray<String> = ['24_hours.json', '7_days.json',
    '14_days.json', '30_days.json', '90_days.json', '180_days.json',
    '365_days.json', 'max.json'];

var
  CryptoList: TCryptoList;
  CryptoImages: TCryptoImages;
  // or place in class constructor/destructor

  { TViewModel }

  // Register UI procs
class procedure TViewModel.UIRegisterProcAsyncIndicator(IndicatorProc
  : TVDMethod);
begin
  fAsyncIndicator := IndicatorProc;
end;

class procedure TViewModel.UIRegisterProcDisplayCryptoDetails
  (DisplayCryptoDetails: TVDisplayCryptoDetails);
begin
  fDisplayCryptoDetails := DisplayCryptoDetails;
end;

class procedure TViewModel.UIRegisterProcListView(ListView: TListView);
begin
  fListView := ListView;
end;

class procedure TViewModel.UIRegisterProcPlotGraph(const PlotGraph
  : TVDPlotDataMethod);
begin
  fAsyncPlotGraph := PlotGraph;
end;

class procedure TViewModel.DisplayData;

begin
  if fTaskActive > 0 then
    exit;

  fAsyncIndicator(true);
  AtomicIncrement(fTaskActive);
  TTask.Run(
    procedure
    var
      PlotData: String;

    begin
      AsyncFn_UpdateListView().Value;
      PlotData := AsyncFn_GetPriceChart(PlotDataValues
        [fSelectedPlot]).Value;

      TThread.Synchronize(nil,
        procedure
        begin
          fAsyncIndicator(false);
          if assigned(fAsyncPlotGraph) then
            fAsyncPlotGraph(PlotData);
          AtomicDecrement(fTaskActive);
        end);
    end);
end;

class procedure TViewModel.DisplayPlot;
begin
  if fTaskActive > 0 then
    exit;

  fAsyncIndicator(true);
  AtomicIncrement(fTaskActive);
  TTask.Run(
    procedure
    var
      PlotData: String;
    begin
      PlotData := AsyncFn_GetPriceChart(PlotDataValues
        [fSelectedPlot]).Value;

      TThread.Synchronize(nil,
        procedure
        begin
          fAsyncIndicator(false);
          if assigned(fAsyncPlotGraph) then
            fAsyncPlotGraph(PlotData);

          AtomicDecrement(fTaskActive);
        end);
    end);
end;

class function TViewModel.GetBitmap: TBitmap;
begin
  result := CryptoImages.GetBitmap(GetCryptoID);
end;

class function TViewModel.GetCryptoCurrent_price: Currency;
begin
  result := CryptoList[SelectedItem].Current_price;
end;

class function TViewModel.GetCryptoID: String;
begin
  result := CryptoList[SelectedItem].id;
end;

class function TViewModel.GetCryptoMarket_cap: Currency;
begin
  result := CryptoList[SelectedItem].Market_cap;
end;

class function TViewModel.GetCryptoMax_supply: double;
begin
  result := CryptoList[SelectedItem].Max_supply;
end;

class function TViewModel.GetCryptoName: String;
begin
  result := CryptoList[SelectedItem].Name;
end;

class function TViewModel.GetCryptoPrice_change_percentage_24h: double;
begin
  result := CryptoList[SelectedItem].Price_change_percentage_24h;
end;

class function TViewModel.GetCryptoTotal_supply: double;
begin
  result := CryptoList[SelectedItem].Total_supply;
end;

class function TViewModel.GetCryptoTotal_volume: UINT64;
begin
  result := CryptoList[SelectedItem].Total_volume;
end;

class function TViewModel.GetLast_updated: String;
begin
  result := CryptoList[SelectedItem].Last_updated;
end;

class function TViewModel.AsyncFn_GetPriceChart(datetimeurl: String)
  : IFuture<String>;
begin
  result := TTask.Future<String>(
    function: String
    begin
      var
      Str := CryptoID;
      result := DM.GetPriceChart(Str, datetimeurl);
    end);
end;

class function TViewModel.AsyncFn_UpdateListView: IFuture<Boolean>;
begin
  result := TTask.Future<Boolean>(
    function: Boolean
    begin
      DM.GetCryptoList(CryptoList);
      if not assigned(CryptoImages) then
      begin
        CryptoImages := TCryptoImages.Create();
        DM.GetImageList(CryptoList, CryptoImages);
      end;

      TThread.Synchronize(nil,
        procedure
        var
          bmp: TBitmap;
          Index: Integer;
          CryptoObj: TCryptoStruct;
        begin
          Index := 0;
          fListView.Items.Clear;
          try
            for CryptoObj in CryptoList._CryptoList do
            begin
              var
                LItem: TListViewItem := fListView.Items.Add;
              LItem.Tag := Index;
              inc(Index);
              LItem.Data['Text1'] := CryptoObj.Name;
              LItem.Data['Text3'] := UpperCase(CryptoObj.symbol);
              if CryptoObj.Current_price < CryptoObj.prev_price then
              begin
                TListItemText(LItem.Objects.FindDrawable('Text4')).TextColor :=
                  TAlphaColorRec.Red;
              end
              else if CryptoObj.Current_price > CryptoObj.prev_price then
              begin
                TListItemText(LItem.Objects.FindDrawable('Text4')).TextColor :=
                  TAlphaColorRec.Lightgreen;
              end;
              LItem.Data['Text4'] := CurrToStrF(CryptoObj.Current_price,
                ffCurrency, 2);
              LItem.Data['Text5'] := UpperCase(Format('%f',
                [CryptoObj.Price_change_percentage_24h]));
              if CryptoObj.Price_change_percentage_24h < 0 then
                TListItemText(LItem.Objects.FindDrawable('Text5')).TextColor :=
                  TAlphaColorRec.Red
              else
                TListItemText(LItem.Objects.FindDrawable('Text5')).TextColor :=
                  TAlphaColorRec.Lightblue;

              bmp := CryptoImages.GetBitmap(CryptoObj.id);
              if assigned(bmp) then
              begin
                LItem.Data['Image2'] := bmp;
              end;
            end;
            if assigned(fDisplayCryptoDetails) then
            begin
              fDisplayCryptoDetails;
            end;
          finally
            fListView.EndUpdate;
          end;
        end);
      result := true;
    end);
end;

class constructor TViewModel.Create;
begin
  CryptoList := TCryptoList.Create();
  fTaskActive := 0;
end;

class destructor TViewModel.Destroy;
begin
  if assigned(CryptoList) then
    FreeAndNil(CryptoList);
  if assigned(CryptoImages) then
    FreeAndNil(CryptoImages);
end;

end.
