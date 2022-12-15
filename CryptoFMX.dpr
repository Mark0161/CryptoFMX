program CryptoFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form1} ,
  uDM in 'uDM.pas' {DM: TDataModule} ,
  uDataStruct in 'uDataStruct.pas',
  uDataStructHelpers in 'uDataStructHelpers.pas',
  uModel in 'uModel.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TDM, DM);
  Application.Run;

end.
