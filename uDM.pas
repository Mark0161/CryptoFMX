unit uDM;

// Delphi: Wait for threads to finish
// https://stackoverflow.com/questions/33345396/delphi-wait-for-threads-to-finish

// This branch explores reducing the DM to absolute core synchrous blocking http request
// incopoatring async further up the tree.

// https://api.coingecko.com/api/v3/coins/markets?vs_currency=gbp&order=market_cap_desc&per_page=100&page=1&sparkline=false

interface

uses
  System.SysUtils, System.Classes, System.Threading, REST.Types,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope;

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

  public
  end;

var
  DM: TDM;

implementation

uses System.Generics.Collections, FMX.Graphics, FMX.Types;

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

end.
