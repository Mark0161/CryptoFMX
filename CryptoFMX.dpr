program CryptoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {MainForm},
  uCryptoData in 'uCryptoData.pas',
  uDM in 'uDM.pas' {DM: TDataModule},
  uViewModel in 'uViewModel.pas',
  uXPlotFrame in 'uXPlotFrame.pas' {XPlotFrame: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDM, DM);
  Application.Run;
end.
