unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  FMX.MultiView, uXPlotFrame, FMX.Objects, FMX.Layouts;

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
    Layout2: TLayout;
    XPlotFrame1: TXPlotFrame;
    Layout1: TLayout;
    PleaseWaitRectangle: TRectangle;
    AniIndicator1: TAniIndicator;
    StatusBar1: TStatusBar;
    StatusLabel: TLabel;
    ElapsedTimeLabel: TLabel;
    StyleBook1: TStyleBook;
    PollForData_Timer: TTimer;
    MultiView1: TMultiView;
    ToolBar2: TToolBar;
    Label1: TLabel;
    ListView1: TListView;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure SpeedButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PollForData_TimerTimer(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    procedure AsyncActiveIndicator(const Active: Boolean = true);
    procedure PlotGraph(Data: String);
    procedure DisplayCryptoDetails;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses System.Diagnostics, Math, uViewModel;

var
  Stopwatch: TStopwatch;  //used to display how long an async operation takes
  firstShow: Boolean = true;
  PollForData_Counter: Integer = 0;  // running total of nnumber of polls

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if TViewModel.TaskActive <> 0 then
  begin
    StatusLabel.text := 'Please wait for Task to complete';
    CanClose := False;
    exit;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  PollForData_Timer.Interval := 60000 * 5; { Once every 5 minutes }
  PollForData_Timer.Enabled := False;
  TViewModel.SelectedItem := 0;
  TViewModel.SelectedPlot := 0;

  // Register the following procedure
  // Will be called within a TTask in uViewModel
  TViewModel.UIRegisterProcListView(ListView1);
  TViewModel.UIRegisterProcAsyncIndicator(AsyncActiveIndicator);
  TViewModel.UIRegisterProcPlotGraph(PlotGraph);
  TViewModel.UIRegisterProcDisplayCryptoDetails(DisplayCryptoDetails);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if firstShow then
    try
      TViewModel.DisplayData();
      SpeedButton24h.IsPressed := true;
    finally
      firstShow := False;
    end;
end;

procedure TMainForm.PollForData_TimerTimer(Sender: TObject);
begin
  TViewModel.DisplayData();
  inc(PollForData_Counter, 1);
  Label4.text := PollForData_Counter.ToString();
end;

// Displays TAniIndicator when async in operation
procedure TMainForm.AsyncActiveIndicator(const Active: Boolean);
begin
  Layout1.Enabled := Active;
  AniIndicator1.Enabled := Active;
  PleaseWaitRectangle.Enabled := Active;
  Layout1.Enabled := Active;
  AniIndicator1.Enabled := Active;
  Layout1.Visible := Active;
  AniIndicator1.Visible := Active;
  if Active = true then
  begin
    Stopwatch := TStopwatch.StartNew;
    StatusLabel.text := 'Please Wait...  ';
  end
  else
  begin
    Stopwatch.Stop;
    StatusLabel.text := 'Ready';
    ElapsedTimeLabel.text := Stopwatch.ElapsedMilliseconds.ToString() + ' ms';
  end;
end;

procedure TMainForm.DisplayCryptoDetails;
var
  ts: TFormatSettings;
  dt: TDateTime;
begin
  lblName.text := TViewModel.CryptoName;
  ts := TFormatSettings.Create;
  ts.ShortDateFormat := 'yyyy-MM-dd';
  ts.DateSeparator := '-';
  ts.TimeSeparator := ':';
  dt := StrToDateTime(TViewModel.Last_updated, ts);

  lblLastUpdated.text := Format('Last updated: %s %s',
    [FormatDateTime('dd-mmm-yyyy', dt), FormatDateTime('hh:nn:ss', dt)]);
  imgContact.bitmap := TViewModel.GetBitmap;
  PriceLbl.text := Format('%m', [TViewModel.CryptoCurrent_price]);
  MarketCapLbl.text := Format('%m', [TViewModel.CryptoMarket_cap]);
  SupplyLbl.text := Format('%m', [TViewModel.CryptoTotal_supply]);
  MaxSupplyLbl.text := Format('%.0n', [TViewModel.CryptoMax_supply + 0.0]);
  dayChangeLbl.text := Format('%.2f %%',
    [TViewModel.CryptoPrice_change_percentage_24h]);
  dayVolumeLbl.text := Format('%.0n', [TViewModel.CryptoTotal_volume + 0.0]);

  if PollForData_Timer.Enabled = False then
  begin
    PollForData_Timer.Enabled := true;
  end;
end;

procedure TMainForm.PlotGraph(Data: String);
begin
  XPlotFrame1._PlotData(Data);
end;

procedure TMainForm.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);

begin
  TViewModel.SelectedItem := AItem.Index;
  TViewModel.DisplayData;
  MultiView1.HideMaster;
end;

procedure TMainForm.SpeedButtonClick(Sender: TObject);
begin;
  TViewModel.SelectedPlot := (Sender as TSpeedButton).Tag;
  TViewModel.DisplayPlot;
end;

end.
