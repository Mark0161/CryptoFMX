unit uDataStructHelpers;

interface

uses uDataStruct;

type
  TRCryptoListHelper = record helper for TRCryptoList
    function LoadJSON(JSONStr: String): Boolean;
    function ToString(): String;
  end;

  // function xlRound(const value: double; const roundto: integer): TRoundedValues;

implementation

uses System.Threading, JSON, Classes, FMX.Types, System.SysUtils,
  System.Generics.Collections,
  Data.DBXJSON, Data.DBXJSONReflect, Rest.JSON, Math;

{ TCryptoListHelper }

function OutputToString(CryptoList: TRCryptoList): String;
var
  str: String;
  item: TCryptoStruct;
begin
  for item in CryptoList.fCryptoList do
  begin
    str := str + 'ID: ' + item.ID + ',';
    str := str + 'Symbol: ' + item.Symbol + ',';
    str := str + 'Name: ' + item.Name + ',';
    str := str + 'Image: ' + item.Image + ',';
    str := str + 'Current_price: ' + format('%m', [item.Current_price]) + ',';
    str := str + 'Market_cap: ' + format('%m', [item.Market_cap]) + ',';
    str := str + 'Market_cap_rank: ' + item.Market_cap_rank.ToString + ',';
    str := str + 'Fully_diluted_valuation: ' +
      item.Fully_diluted_valuation + ',';
    str := str + 'High_24h: ' + format('%m', [item.High_24h]) + ',';
    str := str + 'Low_24h: ' + format('%m', [item.Low_24h]) + ',';
    str := str + 'Price_change_24h: ' + item.Price_change_24h.ToString + ',';
    str := str + 'Price_change_%_24h: ' + item.Price_change_percentage_24h.
      ToString() + ',';
    str := str + 'Market_cap_change_24h: ' +
      item.Market_cap_change_24h.ToString() + ',';
    str := str + 'Circulating_supply: ' +
      item.Circulating_supply.ToString() + ',';
    str := str + 'Total_supply: ' + item.Total_supply.ToString() + ',';
    str := str + 'Max_supply: ' + item.Max_supply.ToString() + ',';
    str := str + 'Ath: ' + item.Ath.ToString() + ',';
    str := str + 'Ath_change_percentage: ' +
      item.Ath_change_percentage.ToString() + ',';
    str := str + 'Ath_date: ' + item.Ath_date + sLineBreak;
  end;
  result := str;
end;

function TRCryptoListHelper.LoadJSON(JSONStr: String): Boolean;
const
  PosofIndex = 5;
var
  LJson, jvalue: TJSONValue;
  jarray: TJSONArray;
begin
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
          // "image": "https://assets.coingecko.com/coins/images/9576/large/BUSD.png?1568947766",
          // we need to extract the >>> 9576 <<< ie the NDX. This is used in
          // PlotGraph, maybe it appears elsewhere but for the timebeing
        var
        splitString := TStringList.Create;
        try
          splitString.Delimiter := '/';

          splitString.DelimitedText := fCryptoList[i].Image;
          fCryptoList[i].ndx := StrToInt(splitString[PosofIndex]);
        finally
          splitString.free;
        end;
      end;
      var
        str: String := ToString();
    except
      FreeAndNil(fCryptoList);
      raise;
    end;
  finally;
    jarray.free;
    LJson.free;
  end;
  result := True;
end;

function TRCryptoListHelper.ToString: String;
var
  str: String;
  item: TCryptoStruct;
begin
  for item in fCryptoList do
  begin
    str := str + 'ID: ' + item.ID + ',';
    str := str + 'ndx: ' + item.ndx.ToString + ',';
    str := str + 'Symbol: ' + item.Symbol + ',';
    str := str + 'Name: ' + item.Name + ',';
    str := str + 'Image: ' + item.Image + ',';
    str := str + 'Current_price: ' + format('%m', [item.Current_price]) + ',';
    str := str + 'Market_cap: ' + format('%m', [item.Market_cap]) + ',';
    str := str + 'Market_cap_rank: ' + item.Market_cap_rank.ToString + ',';
    str := str + 'Fully_diluted_valuation: ' +
      item.Fully_diluted_valuation + ',';
    str := str + 'High_24h: ' + format('%m', [item.High_24h]) + ',';
    str := str + 'Low_24h: ' + format('%m', [item.Low_24h]) + ',';
    str := str + 'Price_change_24h: ' + item.Price_change_24h.ToString + ',';
    str := str + 'Price_change_%_24h: ' + item.Price_change_percentage_24h.
      ToString() + ',';
    str := str + 'Market_cap_change_24h: ' +
      item.Market_cap_change_24h.ToString() + ',';
    str := str + 'Circulating_supply: ' +
      item.Circulating_supply.ToString() + ',';
    str := str + 'Total_supply: ' + item.Total_supply.ToString() + ',';
    str := str + 'Max_supply: ' + item.Max_supply.ToString() + ',';
    str := str + 'Ath: ' + item.Ath.ToString() + ',';
    str := str + 'Ath_change_percentage: ' +
      item.Ath_change_percentage.ToString() + ',';
    str := str + 'Ath_date: ' + item.Ath_date + sLineBreak;
  end;
  result := str;
end;

{ TPlotdataHelper }

end.
