unit uDM;

interface

uses
  System.SysUtils, System.Classes, System.Threading, REST.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  uDataStruct;

type
  TDM = class(TDataModule)
    RESTClient1: TRESTClient;
    RESTRequest1: TRESTRequest;
    NetHTTPClient1: TNetHTTPClient;
    NetHTTPRequest1: TNetHTTPRequest;
    RESTResponse1: TRESTResponse;
    procedure DataModuleCreate(Sender: TObject);

  protected
    fCurrency: String;

    // GetCryptoImagesPPL not currently used but left in for
    function GetCryptoImagesPPL(CryptoList: TRCryptoList): Boolean;
    function GetCryptoImages(CryptoList: TRCryptoList): Boolean;

  public
    CryptoImages: TRCryptoImages;
    property Currency: String read fCurrency write fCurrency;
    {
      Note the Record is passed as pointer, otherwise using any other option
      delphi complains that the cannot capture the record in the anonymous method
    }

    // Blocking function
    function GetfulllistofCryptoCurrencies(const Dest: PCryptoList;
      const GetImages: Boolean): String;
    // equivalent Async Futures functions
    function CreateAsyncFn_GetfulllistofCryptoCurrencies
      (const Dest: PCryptoList; const GetImages: Boolean): iFuture<String>;
  end;

var
  DM: TDM;

implementation

uses System.Generics.Collections, FMX.Graphics, FMX.Types, DateUtils,
  uDataStructHelpers;

{$R *.dfm}

const
  baseurl = 'https://api.coingecko.com/api/v3/';
  price_charts = 'https://www.coingecko.com/price_charts/%d/%s/';
  getFullList =
    'coins/markets?vs_currency=%s&order=market_cap_desc&per_page=100&page=1&sparkline=true';
  getDataFromDateRange =
    'coins/%s/market_chart/range?vs_currency=%s&from=%d&to=%d';

  { TDM }
procedure TDM.DataModuleCreate(Sender: TObject);
begin
  fCurrency := 'gbp'; // set default curency
end;

function TDM.CreateAsyncFn_GetfulllistofCryptoCurrencies
  (const Dest: PCryptoList; const GetImages: Boolean): iFuture<String>;
begin
  result := TTask.Future<String>(
    function: String
    begin
      result := GetfulllistofCryptoCurrencies(Dest, GetImages);
    end);
end;

function TDM.GetCryptoImages(CryptoList: TRCryptoList): Boolean;
var
  i: integer;
  MS: TMemoryStream;
  url: String;

begin
  result := false;
  CryptoImages.fCryptoImages.Clear;
  for i := 0 to CryptoList.fCryptoList.Count - 1 do
  begin
    var
      CryptoObj: TCryptoStruct := CryptoList.fCryptoList.Items[i];
    url := CryptoObj.Image.Trim();
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
          CryptoImages.fCryptoImages.Add(CryptoObj.id,
            TBitMap.CreateFromStream(MS));
        end;
      finally
        MS.DisposeOf;
        MS := nil;
      end;
    end;
  end;
  result := true;
end;

function TDM.GetCryptoImagesPPL(CryptoList: TRCryptoList): Boolean;
begin
  result := false;
  CryptoImages.fCryptoImages.Clear;
  TParallel.For(0, CryptoList.fCryptoList.Count - 1,
    procedure(i: integer)
    begin
      var
        CryptoObj: TCryptoStruct := CryptoList.fCryptoList.Items[i];
      var
      url := CryptoObj.Image.Trim();
      if not url.IsEmpty then
      begin
        var
          thumburl: string := url.Replace('large', 'thumb');
{$IFDEF DEBUG}
        log.d(thumburl);
{$ENDIF}
        var
          MS: TMemoryStream := TMemoryStream.Create();
        var
          NetHTTPRequest: TNetHTTPRequest := TNetHTTPRequest.Create(nil);
        NetHTTPRequest.Client := NetHTTPClient1;
        try
          var
            LResponse: IHTTPResponse := NetHTTPRequest.Get(url, MS);
          if LResponse.StatusCode = 200 then
          begin
            MS.Position := 0;
            CryptoImages.fCryptoImages.Add(CryptoObj.id,
              TBitMap.CreateFromStream(MS));
          end;
        finally
          MS.DisposeOf;
          MS := nil;
          NetHTTPRequest.Free;
        end;
      end;
    end);
  result := true;
end;

function TDM.GetfulllistofCryptoCurrencies(const Dest: PCryptoList;
const GetImages: Boolean): String;
var
  CryptoList: TRCryptoList;
  Rst: String;
  MR: TMRStringList;
begin
  result := 'ERROR';
  RESTRequest1.Params.Clear;
  var
    urlstr: String := baseurl + format(getFullList, [fCurrency]);
  RESTClient1.baseurl := urlstr;
  RESTRequest1.Execute;
  if RESTResponse1.StatusCode = 200 then
  begin
    Rst := RESTResponse1.Content;
    if CryptoList.LoadJSON(Rst) then
      Dest^ := CryptoList; // Record Assign kicks in
    if GetImages then
      GetCryptoImagesPPL(CryptoList);
    //  GetCryptoImages(CryptoList);

    result := 'OK';
  end;
end;

end.
