program HSSESplash;
//{$AppType CONSOLE}
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  SysUtils,
  FrmMain,
  SplashUtils,
  FrmSplash
  { you can add units after this };

{$R *.res}


// TODO
// - package into NSIS (incl config-only installation)

begin
  Application.Initialize;
  if FileExists(Application.Title + '.ico') then
  try
    Application.Icon.LoadFromFile(Application.Title + '.ico');
  finally
  end;
  {$IFDEF WINDOWS}
  Application.MainFormOnTaskBar:=True; // hide for ShowInTaskbar=stNever
  {$ENDIF}
  Application.CreateForm(TFrmMain, formMain);
  Application.Run;

  //WriteLn('Press enter to exit');
end.

