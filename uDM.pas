unit uDM;

interface

uses
  System.SysUtils, System.Classes, System.Threading, REST.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  System.Generics.Collections, REST.Client, Data.Bind.Components,
  Data.Bind.ObjectScope, uCryptoData;

type
  TDM = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    NetHTTPClient1: TNetHTTPClient;
    NetHTTPRequest1: TNetHTTPRequest;
    RESTResponse1: TRESTResponse;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    fCurrency: String;
  public
    CryptoImages: TCryptoImages;
    { Public declarations }
    procedure GetCryptoList(var CryptoList: TCryptoList);
    procedure GetImageList(CryptoList: TCryptoList;
      var CryptoImages: TCryptoImages);
    function GetPriceChart(const id: String;
      const datetimeurl: String): String;
  end;

const
  defaultCurrency = 'gbp';

var
  DM: TDM;

implementation

uses REST.HttpClient, FMX.Graphics, FMX.Types,
  FMX.ListView, DateUtils;

{%CLASSGROUP 'FMX.Controls.TControl'}
{$R *.dfm}
{ TDM }

const
  baseurl = 'https://api.coingecko.com/api/v3/';
  price_charts = 'https://www.coingecko.com/price_charts/%s/%s/';
  getFullList =
    'coins/markets?vs_currency=%s&order=market_cap_desc&per_page=100&page=1&sparkline=true';
  getDataFromDateRange =
    'coins/%s/market_chart/range?vs_currency=%s&from=%d&to=%d';

procedure TDM.DataModuleCreate(Sender: TObject);
begin
  fCurrency := 'gbp'; // set default curency
end;

procedure TDM.DataModuleDestroy(Sender: TObject);
begin
  CryptoImages.Free;
end;

function TDM.GetPriceChart(const id: String;
const datetimeurl: String): String;
var
  urlstr: String;
begin
  result := '';
  RESTRequest1.Params.Clear;
  urlstr := format(price_charts, [id, fCurrency]) + datetimeurl;
  RESTClient1.baseurl := urlstr;
  RESTRequest1.Method := rmGET;
  RESTRequest1.Execute;
  if RESTResponse1.StatusCode = 200 then
  begin
    result := RESTResponse1.JSONText;
  end;
end;

procedure TDM.GetCryptoList(var CryptoList: TCryptoList);
begin
  CryptoList.Clear;
  RESTRequest1.Params.Clear;
  RESTClient1.baseurl := baseurl + format(getFullList, [fCurrency]);
  try
    RESTRequest1.Execute;
    if RESTResponse1.StatusCode = 200 then
    begin
      var
      Rst := RESTResponse1.Content;
      CryptoList.LoadJSON(Rst);
    end;
  except
    on E: EHTTPProtocolException do
    begin
      log.d('EHTTPProtocolException');
    end;
  end;
end;

procedure TDM.GetImageList(CryptoList: TCryptoList;
var CryptoImages: TCryptoImages);
var
  MS: TMemoryStream;
  url: String;
  CryptoStruct: TCryptoStruct;

begin
  var
  Dict := CryptoImages.LockDictionary;
  try
    Dict.Clear;
    for CryptoStruct in CryptoList._CryptoList do
    begin
      url := CryptoStruct.Image.Trim();
      if not url.IsEmpty then
      begin
        var
          thumburl: string := url.Replace('large', 'thumb');
{$IFDEF DEBUG}
        log.d(thumburl);
{$ENDIF}
        try
          MS := TMemoryStream.Create();
          var
            LResponse: IHTTPResponse := NetHTTPRequest1.Get(url, MS);
          if LResponse.StatusCode = 200 then
          begin
            MS.Position := 0;
            Dict.Add(CryptoStruct.id, TBitMap.CreateFromStream(MS));
          end;
        finally
          MS.DisposeOf;
          MS := nil;
        end;
      end;
    end;
  finally
    CryptoImages.UnlockDictionary;
  end;
end;

end.
