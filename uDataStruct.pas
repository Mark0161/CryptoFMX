unit uDataStruct;

interface

uses System.Classes, FMX.Graphics, System.Generics.Collections;

type
  TCryptoStruct = class
  type
    Pts = array of Single;
  private
    fID: String;
    fSymbol: String;
    fName: String;
    fImage: String;
    fCurrent_price: Currency;
    fPrev_price: Currency;
    fMarket_cap: Currency; // could be too big for integer
    fMarket_cap_rank: integer;
    fFully_diluted_valuation: String;
    fTotal_volume: UINT64;
    fHigh_24h: Currency;
    fLow_24h: Currency;
    fPrice_change_24h: double;
    fPrice_change_percentage_24h: double;
    fMarket_cap_change_24h: double;
    fMarket_cap_change_percentage_24h: double;
    fCirculating_supply: double;
    fTotal_supply: double;
    fMax_supply: double;
    fAth: double;
    fAth_change_percentage: double;
    fAth_date: string;
    fAtl: Single;
    fAtl_change_percentage: double;
    fAtl_date: TDateTime;
    fROI: string;
    fLast_updated: string;
    fSparkline_in_7d: String;
  public
    property ID: String read fID write fID;
    property Symbol: String read fSymbol write fSymbol;
    property Name: String read fName write fName;
    property Image: String read fImage write fImage;
    property Current_price: Currency read fCurrent_price write fCurrent_price;
    property Prev_price: Currency read fPrev_price write fPrev_price;
    property Market_cap: Currency read fMarket_cap write fMarket_cap;
    property Market_cap_rank: integer read fMarket_cap_rank
      write fMarket_cap_rank;
    property Fully_diluted_valuation: String read fFully_diluted_valuation
      write fFully_diluted_valuation;
    property Total_volume: UINT64 read fTotal_volume write fTotal_volume;
    property High_24h: Currency read fHigh_24h write fHigh_24h;
    property Low_24h: Currency read fLow_24h write fLow_24h;
    property Price_change_24h: double read fPrice_change_24h
      write fPrice_change_24h;
    property Price_change_percentage_24h: double
      read fPrice_change_percentage_24h write fPrice_change_percentage_24h;
    property Market_cap_change_24h: double read fMarket_cap_change_24h
      write fMarket_cap_change_24h;
    property Market_cap_change_percentage_24h: double
      read fMarket_cap_change_percentage_24h
      write fMarket_cap_change_percentage_24h;
    property Circulating_supply: double read fCirculating_supply
      write fCirculating_supply;
    property Total_supply: double read fTotal_supply write fTotal_supply;
    property Max_supply: double read fMax_supply write fMax_supply;
    property Ath: double read fAth write fAth;
    property Ath_change_percentage: double read fAth_change_percentage
      write fAth_change_percentage;
    property Ath_date: string read fAth_date write fAth_date;
    property Atl: Single read fAtl write fAtl;
    property Atl_change_percentage: double read fAtl_change_percentage
      write fAtl_change_percentage;
    property Atl_date: TDateTime read fAtl_date write fAtl_date;
    property ROI: string read fROI write fROI;
    property Last_updated: string read fLast_updated write fLast_updated;
    property Sparkline_in_7d: String read fSparkline_in_7d
      write fSparkline_in_7d;

    // property SparkPoints: Pts read fPts write fPts;
  var
    ndx: integer;

  end;

  TRCryptoList = record
    fCryptoList: TObjectList<TCryptoStruct>;
    class operator Initialize(out Dest: TRCryptoList);
    class operator Finalize(var Dest: TRCryptoList);
    class operator Assign(var Dest: TRCryptoList; const [ref] Src: TRCryptoList);
    // [ref] used here to prevent compiler doing a value copy
  end;

  PCryptoList = ^TRCryptoList;

type
  TRCryptoImages = record
    fCryptoImages: TObjectDictionary<String, TBitMap>;
    class operator Initialize(out Dest: TRCryptoImages);
    class operator Finalize(var Dest: TRCryptoImages);
    class operator Assign(var Dest: TRCryptoImages;
      const [ref] Src: TRCryptoImages);
  end;

type
  unixdate = int64;
  cPrice = double;

  TPlotRec = record
    fPrice: cPrice;
    fVolume: double;
    constructor Create(const P: cPrice; Volume: double);
  end;

  TPlotdata2 = TList<TPair<unixdate, TPlotRec>>;

type
  TRoundedValues = record
    UpperValue: integer;
    LowerValue: integer;
    factor: double;
  end;

type
  TMRStringList = record
    List: TStringlist;
    class operator Initialize(out Dest: TMRStringList);
    class operator Finalize(var Dest: TMRStringList);
    procedure DumpToFile(const fName: String);
  end;

implementation

uses System.SysUtils, math;

{ TCryptoList }

class operator TRCryptoList.Finalize(var Dest: TRCryptoList);
begin
  Dest.fCryptoList.free;
end;

class operator TRCryptoList.Initialize(out Dest: TRCryptoList);
begin
  Dest.fCryptoList := TObjectList<TCryptoStruct>.Create();
end;

class operator TRCryptoList.Assign(var Dest: TRCryptoList;
  const [ref] Src: TRCryptoList);
var
  srcCrypto: TCryptoStruct;
begin
  TMonitor.Enter(Dest.fCryptoList);
  try
    Dest.fCryptoList.Clear;
    for srcCrypto in Src.fCryptoList do
    begin
      var
        destCrypto: TCryptoStruct := TCryptoStruct.Create();

      destCrypto.ID := srcCrypto.ID;
      destCrypto.ndx := srcCrypto.ndx;
      destCrypto.Symbol := srcCrypto.Symbol;
      destCrypto.Name := srcCrypto.Name;
      destCrypto.Image := srcCrypto.Image;
      destCrypto.Current_price := srcCrypto.Current_price;
      destCrypto.Prev_price := srcCrypto.Prev_price;
      destCrypto.Market_cap := srcCrypto.Market_cap;
      destCrypto.Market_cap_rank := srcCrypto.Market_cap_rank;
      destCrypto.Fully_diluted_valuation := srcCrypto.Fully_diluted_valuation;
      destCrypto.Total_volume := srcCrypto.Total_volume;
      destCrypto.High_24h := srcCrypto.High_24h;
      destCrypto.Low_24h := srcCrypto.Low_24h;
      destCrypto.Price_change_24h := srcCrypto.Price_change_24h;
      destCrypto.Price_change_percentage_24h :=
        srcCrypto.Price_change_percentage_24h;
      destCrypto.Market_cap_change_24h := srcCrypto.Market_cap_change_24h;
      destCrypto.Market_cap_change_percentage_24h :=
        srcCrypto.Market_cap_change_percentage_24h;
      destCrypto.Circulating_supply := srcCrypto.Circulating_supply;
      destCrypto.Total_supply := srcCrypto.Total_supply;
      destCrypto.Max_supply := srcCrypto.Max_supply;
      destCrypto.Ath := srcCrypto.Ath;
      destCrypto.Ath_change_percentage := srcCrypto.Ath_change_percentage;
      destCrypto.Ath_date := srcCrypto.Ath_date;
      destCrypto.Atl := srcCrypto.Atl;
      destCrypto.Atl_change_percentage := srcCrypto.Atl_change_percentage;
      destCrypto.Atl_date := srcCrypto.Atl_date;
      destCrypto.ROI := srcCrypto.ROI;
      destCrypto.Last_updated := srcCrypto.Last_updated;
      destCrypto.Sparkline_in_7d := srcCrypto.Sparkline_in_7d;
      // destCrypto.SparkPoints := srcCrypto.SparkPoints;
      Dest.fCryptoList.Add(destCrypto);
    end;
  finally
    TMonitor.Exit(Dest.fCryptoList);
  end;
end;

{ TCryptoImages }

class operator TRCryptoImages.Assign(var Dest: TRCryptoImages;
  const [ref] Src: TRCryptoImages);

var
  Item: TPair<String, TBitMap>;
begin
  Dest.fCryptoImages.Clear;
  for Item in Src.fCryptoImages do
  begin
    var
      bmp: TBitMap := TBitMap.Create();
    bmp.Assign(Item.value);
    Dest.fCryptoImages.Add(Item.key, bmp);
  end;
end;

class operator TRCryptoImages.Finalize(var Dest: TRCryptoImages);
begin;
  Dest.fCryptoImages.free;
end;

class operator TRCryptoImages.Initialize(out Dest: TRCryptoImages);
begin;
  Dest.fCryptoImages := TObjectDictionary<String, TBitMap>.Create
    ([doOwnsValues]);
end;

{ TMRStringList }

procedure TMRStringList.DumpToFile(const fName: String);
begin
  List.SaveToFile(fName);
end;

class operator TMRStringList.Finalize(var Dest: TMRStringList);
begin
  if Assigned(Dest.List) then
    Dest.List.free;
end;

class operator TMRStringList.Initialize(out Dest: TMRStringList);
begin
  Dest.List := TStringlist.Create();
end;

{ TPlotRec }

constructor TPlotRec.Create(const P: cPrice; Volume: double);
begin
  fPrice := P;
  fVolume := Volume;
end;

end.
