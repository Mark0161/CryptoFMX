unit uModel;

interface

uses System.Generics.Collections, uDataStruct, uDM;

// EventBus Dataload Interfaces BEGIN
type
  IUIUpdate = interface
    ['{C5D96305-4663-442D-9BF2-C0DD67177000}']
  end;

  ICryptoList = interface
    ['{7E0162C6-0434-4AB8-B88F-84E1D469503B}']
    function hasImages: Boolean;
    function Getdata(): TRCryptoList;
    function GetImages(): TRCryptoImages;
  end;

  ICryptoListUpdated = interface(ICryptoList)
    ['{5F05B399-065F-4A20-B2CE-51847150E488}']
  end;

  ICryptoPlotData = interface
    ['{DFF722D0-6637-43A2-BA74-84A99415EED8}']
    function GetId(): String;
    function Getdata(): String;
    function GetdateRange(): String;
  end;

  ICryptoPlotData2 = interface
    ['{B59DC663-A6F6-40DD-9D1D-4CE9368C89D0}']
    function GetId(): String;
    function Getdata(): String;
    function GetdateRange(): String;
  end;
  // On Async operation completion, signals the calling thread with EventBus Event/Dataload
  TUIRequestClass = class
  public
    class procedure Async_GetInitialCryptoListFields(localCryptoList
      : pCryptoList; const GetImages: Boolean = false); static;
    class procedure Async_GetCryptoListFields(localCryptoList: pCryptoList;
      const GetImages: Boolean = false); static;
    class function GetPlotData24h_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotData7d_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotData14d_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotData30d_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotData90d_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotData180d_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotData365d_Async(const widgetid: integer)
      : Boolean; static;
    class function GetPlotDataMax_Async(const widgetid: integer)
      : Boolean; static;
  end;

  // An Event/Action is a command that can be dispatched (with an optional data payload) and listened to.

  // Create Dataloads that will be passed with EventBus to calling thread
function GetUIEvent(): IUIUpdate;
function GetICryptoListUpdatedEvent(data: TRCryptoList): ICryptoListUpdated;
function GetICryptoListEvent(data: TRCryptoList): ICryptoList;

implementation

uses System.Classes, System.Threading, System.SysUtils, FMX.Types, EventBus,
  uDataStructHelpers;

type
  TUIUpdate = class(TInterfacedObject, IUIUpdate)
  end;

  TICryptoListUpdated = class(TInterfacedObject, ICryptoListUpdated)
  private
    fdata: TRCryptoList;
  public
    function Getdata(): TRCryptoList;
    function hasImages: Boolean;
    function GetImages(): TRCryptoImages;
    function GetRefCount(): integer;
    constructor Create(data: TRCryptoList);
    Destructor Destroy; override;
  end;

  TICryptoList = class(TICryptoListUpdated, ICryptoList);

  TCryptoPlotData2 = class(TInterfacedObject, ICryptoPlotData2)
  private
    fJSONStr: String;
    fid: string;
    fRange: String;
  public
    function GetId(): String;
    function Getdata(): String;
    function GetdateRange(): String;
    constructor Create(const JSONStr: String; id, dateRange: String);
    Destructor Destroy; override;
  end;

function GetUIEvent(): IUIUpdate;
begin
  result := TUIUpdate.Create;
end;

function GetICryptoListEvent(data: TRCryptoList): ICryptoList;
begin
 // result := (TICryptoListUpdated.Create(data) as ICryptoList);
  result := TICryptoList.Create(data);
end;

function GetICryptoListUpdatedEvent(data: TRCryptoList): ICryptoListUpdated;
begin
  result := TICryptoListUpdated.Create(data);
end;

function GetICryptoPlotData2(data: String; const id, dateRange: String)
  : ICryptoPlotData2;
begin
  result := TCryptoPlotData2.Create(data, id, dateRange);
end;

{ TUIRequestClass }

class procedure TUIRequestClass.Async_GetInitialCryptoListFields(localCryptoList
  : pCryptoList; const GetImages: Boolean);
begin
  TTask.Run(
    procedure
    begin
      Dm.CreateAsyncFn_GetfulllistofCryptoCurrencies(localCryptoList,
        GetImages).Wait();
      var
        CryptoList: ICryptoList :=
          GetICryptoListEvent(localCryptoList^);
      GlobalEventBus.post(CryptoList);
    end);
end;

class procedure TUIRequestClass.Async_GetCryptoListFields(localCryptoList
  : pCryptoList; const GetImages: Boolean = false);

begin
  TTask.Run(
    procedure
    begin
      Dm.CreateAsyncFn_GetfulllistofCryptoCurrencies(localCryptoList,
        GetImages).Wait();
      var
        CryptoListUpdated: ICryptoListUpdated :=
          GetICryptoListUpdatedEvent(localCryptoList^);
      GlobalEventBus.post(CryptoListUpdated);
    end);
end;

class function TUIRequestClass.GetPlotData24h_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin;
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '24_hours.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData, '',
          '24_hours');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotData30d_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin;
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '30_days.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData, '',
          '30_day');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotData7d_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '7_days.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData,
          '', '7_day');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotData14d_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '14_days.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData, '',
          '14_day');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotData90d_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '90_days.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData, '',
          '90_day');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotData180d_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '180_days.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData, '',
          '180_day');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotData365d_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        '365_days.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData, '',
          '365_day');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

class function TUIRequestClass.GetPlotDataMax_Async(const widgetid
  : integer): Boolean;
var
  PlotData: String;
begin
  TTask.Run(
    procedure
    begin
      PlotData := Dm.CreateAsyncFn_GetPlotDatafromURL(widgetid,
        'max.json').Value;
      var
        uCryptoPlotData: ICryptoPlotData2 := GetICryptoPlotData2(PlotData,
          '', 'Max');
      GlobalEventBus.post(uCryptoPlotData);
    end);
  result := true;
end;

constructor TICryptoListUpdated.Create(data: TRCryptoList);
begin
  inherited Create;
  fdata := data;
end;

destructor TICryptoListUpdated.Destroy;
begin
  inherited;
end;

function TICryptoListUpdated.Getdata: TRCryptoList;
begin
  result := fdata;
end;

function TICryptoListUpdated.GetImages: TRCryptoImages;
begin
  result := Dm.CryptoImages;
end;

function TICryptoListUpdated.GetRefCount: integer;
begin
  result := RefCount;
end;

function TICryptoListUpdated.hasImages: Boolean;
begin
  result := Dm.CryptoImages.fCryptoImages.Count <> 0;
end;

{ TCryptoPlotData2 }

constructor TCryptoPlotData2.Create(const JSONStr: String;
id, dateRange: String);
begin
  inherited Create;
  fJSONStr := JSONStr;
  fid := id;
  fRange := dateRange;
end;

destructor TCryptoPlotData2.Destroy;
begin
  inherited;
end;

function TCryptoPlotData2.Getdata: String;
begin
  result := fJSONStr;
end;

function TCryptoPlotData2.GetdateRange: String;
begin
  result := fRange;
end;

function TCryptoPlotData2.GetId: String;
begin
  result := fid;
end;

end.
