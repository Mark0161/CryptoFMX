unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.StdCtrls, FMX.ListView, FMX.MultiView, FMX.Edit, FMX.Objects, FMX.Layouts,
  FMX.Controls.Presentation, EventBus,
  FMX.Ani;

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
    lblTitle: TLabel;
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
    procedure FormShow(Sender: TObject);
    procedure AniIndicator1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses System.Threading,System.SyncObjs,System.Diagnostics;

{$R *.fmx}
{ TMainForm }

var
  FirstShow: Boolean = True;
  fTaskActive: integer = 0;

procedure TMainForm.AniIndicator1Click(Sender: TObject);
begin
  ShowMessage('Hello');
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if fTaskActive <> 0 then
  begin
    StatusLabel.text := 'Please wait for Task to complete';
    CanClose := False;
    exit;
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  if FirstShow then
  begin
    StatusLabel.Text := 'Please Wait';
    Layout1.Enabled := True;
    AniIndicator1.Enabled := True;
    PleaseWaitRectangle.Enabled := True;
    Layout1.Visible := True;
    AniIndicator1.Visible := True;
    PleaseWaitRectangle.Visible := True;
    TTask.Run(
      procedure
      var // aTask:ITask;
        tasks: array of ITask;
        Stopwatch: TStopwatch;
      begin
        AtomicIncrement(fTaskActive);
        Stopwatch := TStopwatch.StartNew;
        Setlength(tasks, 1);
        tasks[0] := TTask.Run(
          procedure
          begin
            Sleep(10000);
          end);
        tasks[0].Start;
        TTask.WaitforAll(tasks);
        TThread.Synchronize(TThread.Current,
          procedure
          begin
            Layout1.Enabled := False;
            AniIndicator1.Enabled := False;
            PleaseWaitRectangle.Enabled := False;
            Layout1.Enabled := False;
            AniIndicator1.Enabled := False;
            PleaseWaitRectangle.Enabled := False;
            Layout1.Visible := False;
            AniIndicator1.Visible := False;
            Stopwatch.Stop;
            StatusLabel.Text := 'Time elapsed: ' + Stopwatch.ElapsedMilliseconds.ToString()+ 'ms';
            AtomicDecrement(fTaskActive);
          end);
      end);
  end;
end;

end.
