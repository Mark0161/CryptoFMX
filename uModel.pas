unit uModel;

interface

uses System.Generics.Collections, uDataStruct, uDM;

// EventBus Dataload Interfaces BEGIN
type
  IUIUpdate = interface
    ['{C5D96305-4663-442D-9BF2-C0DD67177000}']
  end;

  ICryptoListUpdated = interface
    ['{5F05B399-065F-4A20-B2CE-51847150E488}']
    function hasImages: Boolean;
    function Getdata(): TRCryptoList;
    function GetImages(): TRCryptoImages;
    function GetRefCount(): integer;
  end;

  // EventBus Dataload Interfaces BEGIN

  // On Async operation completion, signals calling thread with EventBus Event/Dataload
  TUIRequestClass = class
  public
    class procedure Async_GetCryptoListFields(localCryptoList: pCryptoList;
      const GetImages: Boolean = false); static;

  end;

  // An Event/Action is a command that can be dispatched (with an optional data payload) and listened to.

  // Create Dataloads that will be passed with EventBus to calling thread
function GetUIEvent(): IUIUpdate;
function GetICryptoListUpdatedEvent(data: TRCryptoList): ICryptoListUpdated;

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

function GetUIEvent(): IUIUpdate;
begin
  result := TUIUpdate.Create;
end;

function GetICryptoListUpdatedEvent(data: TRCryptoList): ICryptoListUpdated;
begin
  result := TICryptoListUpdated.Create(data);
end;

{ TUIRequestClass }

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

end.
