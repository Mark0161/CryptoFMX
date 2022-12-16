unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.ListView, FMX.MultiView, FMX.Edit, FMX.Objects, FMX.Layouts,
  FMX.Controls.Presentation, EventBus, FMX.Ani, uModel, uPlotFrame;

type
  TMainForm = class(TForm)
    Panel1: TPanel;
    GridPanelLayout2: TGridPanelLayout;
    Rectangle1: TRectangle;
    Label2: TLabel;
    PriceLbl: TLabel;
    Rectangle2: TRectangle;
    Label3: TLabel;
    MarketCapLbl: TLabel;
    Rectangle3: TRectangle;
    Supply: TLabel;
    SupplyLbl: TLabel;
    Rectangle4: TRectangle;
    Label7: TLabel;
    MaxSupplyLbl: TLabel;
    Rectangle6: TRectangle;
    Label11: TLabel;
    dayChangeLbl: TLabel;
    Rectangle5: TRectangle;
    Label9: TLabel;
    dayVolumeLbl: TLabel;
    Panel2: TPanel;
    imgContact: TImage;
    Layout3: TLayout;
    lblName: TLabel;
    lblLastUpdated: TLabel;
    ToolBar1: TToolBar;
    Text1: TText;
    MasterButton: TSpeedButton;
    GridPanelLayout3: TGridPanelLayout;
    SpeedButton24h: TSpeedButton;
    SpeedButton7d: TSpeedButton;
    SpeedButton14d: TSpeedButton;
    SpeedButton30d: TSpeedButton;
    SpeedButton90d: TSpeedButton;
    SpeedButton180d: TSpeedButton;
    SpeedButton365d: TSpeedButton;
    SpeedButton1Y: TSpeedButton;
    MultiView1: TMultiView;
    ToolBar2: TToolBar;
    Label1: TLabel;
    ListView1: TListView;
    PlotPanel: TPanel;
    Layout1: TLayout;
    PleaseWaitRectangle: TRectangle;
    AniIndicator1: TAniIndicator;
    StatusBar1: TStatusBar;
    StatusLabel: TLabel;
    ScrollBar1: TScrollBar;
    FloatAnimation1: TFloatAnimation;
    ActiveTaskLabel: TLabel;
    ElapsedTimeLabel: TLabel;
    PollForCryptoData: TTimer;
    PlotFrame1: TPlotFrame;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure PollForCryptoDataTimer(Sender: TObject);
    procedure GetPlotDataClick(Sender: TObject);
  private
    { Private declarations }
    procedure updateListview(CryptoList: ICryptoListUpdated;
      var firstShow: Boolean);
    procedure LockScreen(const Active: Boolean = true);
    procedure AsyncActive(const Active: Boolean = true);
  public
    { Public declarations }
    [Subscribe(TThreadMode.Main)]
    procedure onAsyncCompleteEvent(ACryptoListUpdated
      : ICryptoListUpdated); overload;
    [Subscribe(TThreadMode.Main)]
    procedure onAsyncCompleteEvent(ACryptoPlotData: ICryptoPlotData2); overload;
  end;

var
  MainForm: TMainForm;

implementation

uses System.Threading, System.SyncObjs, System.Diagnostics, uDataStruct;

{$R *.fmx}
{ TMainForm }

var
  firstShow: Boolean = true;
  CryptoImages: TRCryptoImages;
  localCryptoList: TRCryptoList;
  SelectedCrypto: TCryptoStruct;
  SelectedListViewItem: integer = 0;
  Stopwatch: TStopwatch;
  nActiveTasks: integer = 0;

procedure TMainForm.LockScreen(const Active: Boolean);
begin
  Layout1.Enabled := Active;
  AniIndicator1.Enabled := Active;
  PleaseWaitRectangle.Enabled := Active;
  Layout1.Enabled := Active;
  AniIndicator1.Enabled := Active;
  Layout1.Visible := Active;
  AniIndicator1.Visible := Active;
  AsyncActive(Active);
end;

procedure TMainForm.AsyncActive(const Active: Boolean);
begin
  if Active = true then
  begin
    Stopwatch := TStopwatch.StartNew;
    StatusLabel.text := 'Please Wait...  ';
    AtomicIncrement(nActiveTasks);
  end
  else
  begin
    Stopwatch.Stop;
    StatusLabel.text := 'Ready';
    ElapsedTimeLabel.text := Format('Async time %d ms',
      [Stopwatch.ElapsedMilliseconds]);
    AtomicDecrement(nActiveTasks);
  end;
  ActiveTaskLabel.text := 'Async count: ' + nActiveTasks.ToString();
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if nActiveTasks <> 0 then
  begin
    StatusLabel.text := 'Please wait for Task to complete';
    CanClose := False;
    exit;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if firstShow then
  begin
    GlobalEventBus.RegisterSubscriberForEvents(Self);
    LockScreen(true);
    PollForCryptoData.Enabled := true;
    TUIRequestClass.Async_GetCryptoListFields(@localCryptoList, true);
  end;
end;

procedure TMainForm.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
var
  bmp: TBitmap;
begin
  // MultiView1.HideMaster;
  SelectedListViewItem := AItem.Tag; // Index;
  SelectedCrypto := localCryptoList.fCryptoList[SelectedListViewItem];
  lblName.text := SelectedCrypto.Name;

  var
    ts: TFormatSettings;
  ts := TFormatSettings.Create;
  ts.ShortDateFormat := 'yyyy-MM-dd';
  ts.DateSeparator := '-';
  ts.TimeSeparator := ':';
  var
  dt := StrToDateTime(SelectedCrypto.Last_updated, ts);

  lblLastUpdated.text := Format('Last updated: %s %s',
    [FormatDateTime('dd-mmm-yyyy', dt), FormatDateTime('hh:nn:ss', dt)]);
  CryptoImages.fCryptoImages.TryGetValue(SelectedCrypto.ID, bmp);
  imgContact.bitmap := bmp;
  PriceLbl.text := Format('%m', [SelectedCrypto.current_price]);
  MarketCapLbl.text := Format('%m', [SelectedCrypto.market_cap]);
  SupplyLbl.text := Format('%m', [SelectedCrypto.total_supply]);
  // FloatToStrF(CryptoObj.total_supply, ffCurrency,10,0);
  MaxSupplyLbl.text := Format('%.0n', [SelectedCrypto.max_supply + 0.0]);
  dayChangeLbl.text := Format('%.2f %%',
    [SelectedCrypto.price_change_percentage_24h]);
  dayVolumeLbl.text := Format('%.0n', [SelectedCrypto.total_volume + 0.0]);
  // Set initial plotdata

  // called by PollForCryptoData as well as SpeedButtonS
  if (Sender is TSpeedButton) then
  begin
    SpeedButton24h.SetFocus;
    SpeedButton24h.IsPressed := true;
    SpeedButton24h.Onclick(nil);
  end;

  MultiView1.HideMaster;
end;

procedure TMainForm.onAsyncCompleteEvent(ACryptoPlotData: ICryptoPlotData2);
var
  data: String;
begin
  AsyncActive(False);
  data := ACryptoPlotData.Getdata;
  PlotFrame1.PlotData(data);
end;

procedure TMainForm.onAsyncCompleteEvent(ACryptoListUpdated
  : ICryptoListUpdated);
begin
  { if ACryptoListUpdated.hasImages then
    begin
    CryptoImages := ACryptoListUpdated.GetImages;
    end; }
  updateListview(ACryptoListUpdated, firstShow);
  if firstShow then
  begin
    LockScreen(False);
    firstShow := False;
    SpeedButton24h.SetFocus;
    SpeedButton24h.IsPressed := true;
    SpeedButton24h.Onclick(nil);
  end
  else
    AsyncActive(False);

end;

procedure TMainForm.PollForCryptoDataTimer(Sender: TObject);
begin
  if (nActiveTasks) <> 0 then
    // if async operation active then back off
    exit;
  try
    AsyncActive(true);
    TUIRequestClass.Async_GetCryptoListFields(@localCryptoList, False);
  except
    AsyncActive(False);
  end;
end;

procedure TMainForm.GetPlotDataClick(Sender: TObject);
var
  ndx: integer;
begin
  try
    AsyncActive(true);
    ndx := localCryptoList.fCryptoList[SelectedListViewItem].ndx;
    if Sender = nil then
      TUIRequestClass.GetPlotData24h_Async(ndx)
    else
      case (Sender as TComponent).Tag of
        1: // day
          TUIRequestClass.GetPlotData24h_Async(ndx);
        7: // days
          TUIRequestClass.GetPlotData7d_Async(ndx);
        14: // days
          TUIRequestClass.GetPlotData14d_Async(ndx);
        30: // days
          TUIRequestClass.GetPlotData30d_Async(ndx);
        90: // days
          TUIRequestClass.GetPlotData90d_Async(ndx);
        180: // days
          TUIRequestClass.GetPlotData180d_Async(ndx);
        365: // 1 year
          TUIRequestClass.GetPlotData365d_Async(ndx);
        9999: // Max
          TUIRequestClass.GetPlotDataMax_Async(ndx);
      end;
  except
    AsyncActive(False);
  end;
end;

procedure TMainForm.updateListview(CryptoList: ICryptoListUpdated;
  var firstShow: Boolean);
var
  bmp: TBitmap;
  CryptoObj: TCryptoStruct;
begin
  ListView1.BeginUpdate;
  ListView1.Items.Clear;
  try
    var
      Index: integer := 0;
    ListView1.Items.Clear;
    for CryptoObj in CryptoList.Getdata().fCryptoList do
    begin
      var
        LItem: TListViewItem := ListView1.Items.Add;
      LItem.Tag := Index;
      inc(Index);
      LItem.data['Text1'] := CryptoObj.Name;
      LItem.data['Text3'] := UpperCase(CryptoObj.symbol);
      if CryptoObj.current_price < CryptoObj.prev_price then
      begin
        TListItemText(LItem.Objects.FindDrawable('Text4')).TextColor :=
          TAlphaColorRec.Red;
      end
      else if CryptoObj.current_price > CryptoObj.prev_price then
      begin
        TListItemText(LItem.Objects.FindDrawable('Text4')).TextColor :=
          TAlphaColorRec.Lightgreen;
      end;
      LItem.data['Text4'] := CurrToStrF(CryptoObj.current_price, ffCurrency, 2);
      LItem.data['Text5'] :=
        UpperCase(Format('%f', [CryptoObj.price_change_percentage_24h]));
      if CryptoObj.price_change_percentage_24h < 0 then
        TListItemText(LItem.Objects.FindDrawable('Text5')).TextColor :=
          TAlphaColorRec.Red
      else
        TListItemText(LItem.Objects.FindDrawable('Text5')).TextColor :=
          TAlphaColorRec.Lightblue;
      if CryptoImages.fCryptoImages.TryGetValue(CryptoObj.ID, bmp) then
      begin
        LItem.data['Image2'] := bmp;
{$IFDEF DEBUG}
        // log.d(Format('bmp %s', [CryptoObj.ID]))
{$ENDIF}
      end;
    end;
  finally
    ListView1.EndUpdate;
    {
      if firstShow then
      begin
      SelectedListViewItem := 0;
      firstShow := False;
      end;
    }
    ListView1ItemClick(nil, ListView1.Items[SelectedListViewItem]);
  end;
end;

end.
