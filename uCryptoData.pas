unit uCryptoData;

interface

uses System.Classes, Generics.Collections, FMX.Graphics;

type
  Pts = array of Single;

type
  TCryptoStruct = class
    // as TJSON.JsonToObject only works on classes not records
  private
    fID: String;
    fImgNdx: integer; // Image reference
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

    procedure SetID(const Value: string);
    procedure SetNdx(const Value: integer);
    procedure SetSymbol(const Value: string);
    procedure SetName(const Value: string);
    procedure SetImage(const Value: string);
    procedure SetCurrent_price(const Value: Currency);
    procedure SetPrev_price(const Value: Currency);
    procedure SetMarket_cap(const Value: Currency);
    procedure SetMarket_cap_rank(const Value: integer);
    procedure SetFully_diluted_valuation(const Value: string);
    procedure SetTotal_volume(const Value: UINT64);
    procedure SetHigh_24h(const Value: Currency);
    procedure SetLow_24h(const Value: Currency);
    procedure SetPrice_change_24h(const Value: double);
    procedure SetPrice_change_percentage_24h(const Value: double);
    procedure SetMarket_cap_change_24h(const Value: double);
    procedure SetMarket_cap_change_percentage_24h(const Value: double);
    procedure SetCirculating_supply(const Value: double);
    procedure SetTotal_supply(const Value: double);
    procedure SetMax_supply(const Value: double);
    procedure SetAth(const Value: double);
    procedure SetAth_change_percentage(const Value: double);
    procedure SetAth_date(const Value: String);
    procedure SetAtl(const Value: Single);
    procedure SetAtl_change_percentage(const Value: double);
    procedure SetAtl_date(const Value: TDateTime);
    procedure SetROI(const Value: string);
    procedure SetLast_updated(const Value: string);
  public
    property ID: String read fID write SetID; // writefID;
    property ImgNdx: integer read fImgNdx write SetNdx; // fSymbol;
    property Symbol: String read fSymbol write SetSymbol; // fSymbol;
    property Name: String read fName write SetName; // fName;
    property Image: String read fImage write SetImage; // fImage;
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
    function ToString(): String;
  end;

  TCryptoList = class
  private
    FLock: TObject;
    fCryptoList: TObjectList<TCryptoStruct>;
    function GetCryptoList: TEnumerable<TCryptoStruct>;
    function LockList: TObjectList<TCryptoStruct>;
    procedure UnlockList; inline;
    function GetItem(Index: integer): TCryptoStruct; inline;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear();
    function ToString(): String;
    function ToStringListTxt(): String;
    function LoadJSON(const JSONStr: String): Boolean;
    property _CryptoList: TEnumerable<TCryptoStruct> read GetCryptoList;
    property Items[Index: integer]: TCryptoStruct read GetItem; default;
  end;

type
  TCryptoImages = class(TObject)
  private
    FLock: TObject;
    fCryptoImages: TObjectDictionary<String, TBitmap>;
    function GetCryptoImages: TEnumerable<TPair<String, TBitmap>>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function LockDictionary: TObjectDictionary<String, TBitmap>;
    procedure UnlockDictionary; inline;
    procedure Add(const ID: String; Bmp: TBitmap);
    function GetBitmap(const ID: String): TBitmap;
  end;

type   //Used for dumping files for debugging
  TMRStringList = record
    List: TStringlist;
    class operator Initialize(out Dest: TMRStringList);
    class operator Finalize(var Dest: TMRStringList);
    procedure DumpToFile(const fName: String);
  end;

implementation

uses SysUtils, JSON, Rest.JSON;

procedure TCryptoList.Clear;
begin
  fCryptoList.Clear;
end;

constructor TCryptoList.Create;
begin
  FLock := TObject.Create;
  fCryptoList := TObjectList<TCryptoStruct>.Create(True);
end;

destructor TCryptoList.Destroy;
begin
  LockList; // Make sure nobody else is inside the list.
  try
    fCryptoList.Free;
    inherited Destroy;
  finally
    UnlockList;
    FLock.Free;
  end;
end;

function TCryptoList.LockList: TObjectList<TCryptoStruct>;
begin
  TMonitor.Enter(FLock);
  result := fCryptoList;
end;

procedure TCryptoList.UnlockList;
begin
  TMonitor.exit(FLock);
end;

function TCryptoList.GetCryptoList: TEnumerable<TCryptoStruct>;
begin
  result := fCryptoList;
end;

function TCryptoList.GetItem(Index: integer): TCryptoStruct;
begin
  if Cardinal(Index) >= Cardinal(fCryptoList.Count) then
    ErrorArgumentOutOfRange;
  result := fCryptoList[Index];
end;

function TCryptoList.LoadJSON(const JSONStr: String): Boolean;
const
  PosofIndex = 5;
var
  LJson, jvalue: TJSONValue;
  jarray: TJSONArray;
begin
  LockList;
  try
    result := False;
    if JSONStr.Trim.IsEmpty then
      exit;

    LJson := TJSONObject.ParseJSONValue(JSONStr);
    try
      if not Assigned(LJson) then
        exit(False);
      jarray := TJSONObject.ParseJSONValue(LJson.ToString()) as TJSONArray;
      if not Assigned(jarray) then
        exit(False);

      try
        for jvalue in jarray do
        begin
          var
            i: integer := fCryptoList.Add
              (TJSON.JsonToObject<TCryptoStruct>(jvalue as TJSONObject));
          var
          splitString := TStringlist.Create;
          try
            splitString.Delimiter := '/';
            splitString.DelimitedText := fCryptoList[i].Image;
            fCryptoList[i].ImgNdx := StrToInt(splitString[PosofIndex]);
          finally
            splitString.Free;
          end;
        end;
        var
          str: String := ToString();
      except
        FreeAndNil(fCryptoList);
        raise;
      end;
    finally;
      jarray.Free;
      LJson.Free;
    end;
    result := True;
  finally
    UnlockList
  end;
end;

function TCryptoList.ToString: String;
begin
  LockList;
  try
    var
    builder := TStringBuilder.Create;
    try
      for var CryptoRec in fCryptoList do
      begin
        builder.Append(CryptoRec.ToString());
      end;
    finally
      result := builder.ToString();
      builder.Free;
    end;
  finally
    UnlockList;
  end;
end;

function TCryptoList.ToStringListTxt: String;
begin
  LockList;
  try
    var
    builder := TStringBuilder.Create;
    try
      for var CryptoRec in fCryptoList do
      begin
        var
        CryptoStr := CryptoRec.ToString();
        CryptoStr := Copy(CryptoStr, 1, Length(CryptoStr) - 1) + sLineBreak;
        builder.Append(CryptoStr);
      end;
    finally
      result := builder.ToString();
      builder.Free;
    end;
  finally
    UnlockList;
  end;
end;

{ TCryptoRec }

procedure TCryptoStruct.SetAth(const Value: double);
begin
  fAth := Value;
end;

procedure TCryptoStruct.SetAth_change_percentage(const Value: double);
begin
  fAth_change_percentage := Value;
end;

procedure TCryptoStruct.SetAth_date(const Value: String);
begin
  fAth_date := Value;
end;

procedure TCryptoStruct.SetAtl(const Value: Single);
begin
  fAtl := Value;
end;

procedure TCryptoStruct.SetAtl_change_percentage(const Value: double);
begin
  fAtl_change_percentage := Value;
end;

procedure TCryptoStruct.SetAtl_date(const Value: TDateTime);
begin
  fAtl_date := Value;
end;

procedure TCryptoStruct.SetCirculating_supply(const Value: double);
begin
  fCirculating_supply := Value;
end;

procedure TCryptoStruct.SetCurrent_price(const Value: Currency);
begin
  fCurrent_price := Value;
end;

procedure TCryptoStruct.SetFully_diluted_valuation(const Value: string);
begin
  fFully_diluted_valuation := Value;
end;

procedure TCryptoStruct.SetHigh_24h(const Value: Currency);
begin
  fHigh_24h := Value;
end;

procedure TCryptoStruct.SetID(const Value: string);
begin
  fID := Value;
end;

procedure TCryptoStruct.SetImage(const Value: string);
begin
  fImage := Value;
end;

procedure TCryptoStruct.SetLast_updated(const Value: string);
begin
  fLast_updated := Value;
end;

procedure TCryptoStruct.SetLow_24h(const Value: Currency);
begin
  fLow_24h := Value;
end;

procedure TCryptoStruct.SetMarket_cap(const Value: Currency);
begin
  fMarket_cap := Value;
end;

procedure TCryptoStruct.SetMarket_cap_change_24h(const Value: double);
begin
  fMarket_cap_change_24h := Value;
end;

procedure TCryptoStruct.SetMarket_cap_change_percentage_24h
  (const Value: double);
begin
  fMarket_cap_change_percentage_24h := Value;
end;

procedure TCryptoStruct.SetMarket_cap_rank(const Value: integer);
begin
  fMarket_cap_rank := Value;
end;

procedure TCryptoStruct.SetMax_supply(const Value: double);
begin
  fMax_supply := Value;
end;

procedure TCryptoStruct.SetName(const Value: string);
begin
  fName := Value;
end;

procedure TCryptoStruct.SetNdx(const Value: integer);
begin
  fImgNdx := Value;
end;

procedure TCryptoStruct.SetPrev_price(const Value: Currency);
begin
  fPrev_price := Value;
end;

procedure TCryptoStruct.SetPrice_change_24h(const Value: double);
begin
  fPrice_change_24h := Value;
end;

procedure TCryptoStruct.SetPrice_change_percentage_24h(const Value: double);
begin
  fPrice_change_percentage_24h := Value;
end;

procedure TCryptoStruct.SetROI(const Value: string);
begin
  fROI := Value;
end;

procedure TCryptoStruct.SetSymbol(const Value: string);
begin
  fSymbol := Value;
end;

procedure TCryptoStruct.SetTotal_supply(const Value: double);
begin
  fTotal_supply := Value;
end;

procedure TCryptoStruct.SetTotal_volume(const Value: UINT64);
begin
  fTotal_volume := Value;
end;

function TCryptoStruct.ToString: String;
begin
  var
  builder := TStringBuilder.Create;
  try
    builder.AppendFormat('ID: %s,', [ID]);
    builder.AppendFormat('Symbol: %s,', [Symbol]);
    builder.AppendFormat('Name: %s,', [Name]);
    builder.AppendFormat('Image: %s,', [Image]);
    builder.AppendFormat('Current_price: %m,', [Current_price]);
    builder.AppendFormat('Market_cap: %m,', [Market_cap]);
    builder.AppendFormat('Market_cap_rank: %d,', [Market_cap_rank]);
    builder.AppendFormat('Fully_diluted_valuation: %s,',
      [Fully_diluted_valuation]);
    builder.AppendFormat('High_24h %m,', [High_24h]);
    builder.AppendFormat('Low_24h %m,', [Low_24h]);
    builder.AppendFormat('Price_change_24h: %s,', [Price_change_24h.ToString]);
    builder.AppendFormat('Price_change_%_24h: %s,',
      [Price_change_percentage_24h.ToString()]);
    builder.AppendFormat('Market_cap_change_24h: %s,',
      [Market_cap_change_24h.ToString()]);
    builder.AppendFormat('Circulating_supply: %s,',
      [Circulating_supply.ToString()]);
    builder.AppendFormat('Total_supply: %s,', [Total_supply.ToString()]);
    builder.AppendFormat('Max_supply: %s,', [Max_supply.ToString()]);
    builder.AppendFormat('Ath: %s,', [Ath.ToString()]);
    builder.AppendFormat('Ath_change_percentage: %s,',
      [Ath_change_percentage.ToString()]);
    builder.AppendFormat('Ath_date: %s', [Ath_date]);
  finally
    result := builder.ToString();
    builder.Free;
  end;

end;

{ TCryptoImages }

constructor TCryptoImages.Create;
begin
  FLock := TObject.Create;
  fCryptoImages := TObjectDictionary<String, TBitmap>.Create([doOwnsValues]);
end;

destructor TCryptoImages.Destroy;
begin
  TMonitor.Enter(FLock); // Make sure nobody else is inside the list.
  try
    fCryptoImages.Free;
    inherited Destroy;
  finally
    FLock.Free;
  end;
end;

function TCryptoImages.LockDictionary: TObjectDictionary<String, TBitmap>;
begin
  TMonitor.Enter(FLock);
  result := fCryptoImages;
end;

procedure TCryptoImages.UnlockDictionary;
begin
  TMonitor.exit(FLock);
end;

procedure TCryptoImages.Add(const ID: String; Bmp: TBitmap);
// use within unlock
begin
  fCryptoImages.Add(ID, Bmp);
end;

procedure TCryptoImages.Clear;
// use within unlock
begin
  TMonitor.Enter(FLock);
  try
    fCryptoImages.Clear;
  finally
    FLock.Free;;
  end;
end;

function TCryptoImages.GetBitmap(const ID: String): TBitmap;
begin
  fCryptoImages.TryGetValue(ID, result);
end;

function TCryptoImages.GetCryptoImages: TEnumerable<TPair<String, TBitmap>>;
begin
  result := fCryptoImages;
end;

{ TMRStringList }

procedure TMRStringList.DumpToFile(const fName: String);
begin
  List.SaveToFile(fName);
end;

class operator TMRStringList.Finalize(var Dest: TMRStringList);
begin
  if Assigned(Dest.List) then
    Dest.List.Free;
end;

class operator TMRStringList.Initialize(out Dest: TMRStringList);
begin
  Dest.List := TStringlist.Create();
end;

end.
