unit FrmSplash;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ActiveX, uWVWindowParent, uWVBrowser, uWVWinControl, URIParser,
  uWVTypeLibrary, uWVLoader, uWVTypes, uWVBrowserBase;

const
 IID_VDM: TGUID ='{A5CD92FF-29BE-454C-8D04-D82879FB3F1B}';
 CLSID_VDM: TGUID ='{AA509086-5CA9-4C25-8F95-589D3C07B48A}';

type

  { TFrmSplash }

  TFrmSplash = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    tmrVDesktop: TTimer;
    tmrWb: TTimer;
    wbFlash: TWVBrowser;
    wbContainer: TWVWindowParent;

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure SetKeyFocus();
    procedure FormResize(Sender: TObject);
    procedure initialize(monitorNum: integer; formSize:integer);
    procedure setUrl(url: String);
    procedure resizeForm;
    procedure FormCreate(Sender: TObject);
    procedure tmrVDesktopTimer(Sender: TObject);
    procedure tmrWbTimer(Sender: TObject);
    procedure wbContainerEnter(Sender: TObject);
    procedure wbContainerExit(Sender: TObject);
    procedure wbContainerSizeConstraintsChange(Sender: TObject);
    procedure wbFlashAcceleratorKeyPressed(Sender: TObject;
      const aController: ICoreWebView2Controller;
      const aArgs: ICoreWebView2AcceleratorKeyPressedEventArgs);
    procedure wbFlashAfterCreated(Sender: TObject);
    procedure wbFlashDOMContentLoaded(Sender: TObject;
      const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2DOMContentLoadedEventArgs);
  private
    pMonitorNum: integer;
  public
    formCloseRequested: boolean;
  end;

  {$EXTERNALSYM IVirtualDesktopManager}
  IVirtualDesktopManager = interface(IUnknown)
    ['{A5CD92FF-29BE-454C-8D04-D82879FB3F1B}']
    function IsWindowOnCurrentVirtualDesktop(Wnd:cardinal; var IsTrue: bool): HResult; stdcall;
    function GetWindowDesktopId(Wnd:cardinal; pDesktopID: PGUID): HResult; stdcall;
    function MoveWindowToDesktop(Wnd:cardinal; DesktopID: PGUID): HResult; stdcall;
  end;

var
  formSplash: TFrmSplash;
  cacheDir : string;
  MouseIsDown: boolean;
  PX, PY: integer;
implementation

uses FrmMain;
{$R *.lfm}

{ TFrmSplash }

procedure TFrmSplash.initialize(monitorNum: integer; formSize:integer);
begin
  self.icon :=formMain.icon;
  self.Caption:=formMain.Caption;
  self.pMonitorNum:=monitorNum;
  self.visible:=true;
  self.WindowState:=wsnormal;
  //self.Left := Screen.Monitors[self.pMonitorNum].Left;
  //self.Top := Screen.Monitors[self.pMonitorNum].top;
  //self.BoundsRect := Screen.Monitors[self.pMonitorNum].BoundsRect;
  self.Width:=round(Screen.Monitors[self.pMonitorNum].Width * formSize/100);
  self.Height:=round(Screen.Monitors[self.pMonitorNum].Height * formSize/100);
  self.Left:=Screen.Monitors[self.pMonitorNum].left
    + round(Screen.Monitors[self.pMonitorNum].Width/2 * (1 - formSize/100));
  self.Top:=Screen.Monitors[self.pMonitorNum].top
    + round(Screen.Monitors[self.pMonitorNum].Height/2 * (1 - formSize/100));
  if formSize = 100 then
  begin
    self.WindowState:=wsMaximized;
  end
  else
  begin
    if formmain.pShowCloseButton then self.BorderStyle:=bsDialog;
  end;
  wbContainer.enabled:=formmain.pDisablePageBlock;
  self.FormStyle:= fsSystemStayOnTop;
  self.ShowInTaskBar:=stNever;
  label1.Caption:=formmain.usage();
  resizeForm;

end;


procedure TFrmSplash.setUrl(url: String);
var
  navurl: String;
begin
  if Trim(url) = '' then exit;
  navurl := url;
  if not IsAbsoluteURI(url) then
  begin
       if FileExists(url) then
          navurl:= FilenameToURI(ExpandFileName(url))
       else
          navurl:= FilenameToURI(url);
  end;
  if (navurl<>'') then self.wbFlash.DefaultURL:=navurl;
end;

procedure TFrmSplash.resizeForm;
begin
  self.wbContainer.top := 0;
  self.wbContainer.left := 0;
  self.Edit1.top := self.ClientHeight - self.Edit1.Height;;
  self.Edit1.left := 0;
  self.wbContainer.Width := self.ClientWidth;
  self.wbContainer.Height := self.ClientHeight;
  self.Edit1.Width := self.ClientWidth;

  wbContainer.Visible:= not(GlobalWebView2Loader.InitializationError
    or (wbFlash.DefaultURL=''));
  if GlobalWebView2Loader.InitializationError then
  begin
    self.Edit1.Text:=GlobalWebView2Loader.ErrorMessage;
    self.Edit1.visible :=true;
  end;

end;

procedure TFrmSplash.FormCreate(Sender: TObject);
begin
  SetKeyFocus;
end;


function GetVDM: IVirtualDesktopManager;
begin
  Result := nil;
  CoCreateInstance(CLSID_VDM, nil, CLSCTX_INPROC_SERVER, IVirtualDesktopManager, Result);
end;

function IsOnCurrentDesktop(wnd: HWND): Boolean;
var
  value: Bool = false;
begin
  GetVDM.IsWindowOnCurrentVirtualDesktop(wnd, value);
  Result := value;
end;

procedure TFrmSplash.tmrVDesktopTimer(Sender: TObject);
var
  onCurrentDesktop: bool = true;
begin
  // old windows (prior Win10) don't have this feature, so we ignore any exception occur
  try
    //onCurrentDesktop := IsOnCurrentDesktop(application.Handle);
    onCurrentDesktop := IsOnCurrentDesktop(self.Handle);
  except
  end;
  if not onCurrentDesktop then
  begin
    //showmessage('get over here');
    application.Minimize;
    application.restore;
    self.Hide;
    self.show;
  end;
end;

procedure TFrmSplash.tmrWbTimer(Sender: TObject);
begin
  tmrWb.Enabled := False;
    if GlobalWebView2Loader.Initialized then
    begin
      wbFlash.CreateBrowser(wbContainer.Handle);
      wbFlash.navigate(wbFlash.DefaultURL);
      Edit1.Caption:=wbFlash.DefaultURL;
    end;
end;

procedure TFrmSplash.wbContainerEnter(Sender: TObject);
begin
  SetKeyFocus;
end;

procedure TFrmSplash.wbContainerExit(Sender: TObject);
begin
  SetKeyFocus;
end;

procedure TFrmSplash.wbContainerSizeConstraintsChange(Sender: TObject);
begin
  SetKeyFocus;
end;

procedure TFrmSplash.wbFlashAcceleratorKeyPressed(Sender: TObject;
  const aController: ICoreWebView2Controller;
  const aArgs: ICoreWebView2AcceleratorKeyPressedEventArgs);
begin
  SetKeyFocus;
end;

procedure TFrmSplash.wbFlashAfterCreated(Sender: TObject);
begin
  wbcontainer.UpdateSize;
  SetKeyFocus;
end;

procedure TFrmSplash.wbFlashDOMContentLoaded(Sender: TObject;
  const aWebView: ICoreWebView2;
  const aArgs: ICoreWebView2DOMContentLoadedEventArgs);
begin
  self.Edit1.visible:=false;
end;

procedure TFrmSplash.FormResize(Sender: TObject);
begin
  resizeForm;
end;

procedure TFrmSplash.SetKeyFocus();
begin
  if formmain.pDisablePageBlock then exit;
  formmain.BringToFront;
  try
    if formmain.visible then formmain.setfocus;
  except
  end;
  try
    if formmain.visible then formmain.editFocus.SetFocus;
  except
  end;
end;

procedure TFrmSplash.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  //CanClose:=false;
  if formCloseRequested then exit;
  if not(formmain.pDisableEscape) then
  begin
    CanClose:=false;
    formmain.cancelSplash('OFF');
  end;
end;

procedure TFrmSplash.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if formmain.pDisablePageBlock then exit;
  if Button = mbLeft then begin
    // only able to move not in fullscreen
    MouseIsDown := (not formmain.pDisableDragMove) and (pSize<100) and True;
    PX := X;
    PY := Y;
  end;
end;

procedure TFrmSplash.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if formmain.pDisablePageBlock then exit;
  if MouseIsDown then begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;
end;

procedure TFrmSplash.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if formmain.pDisablePageBlock then exit;
  MouseIsDown:=False;
  SetKeyFocus;
end;

procedure TFrmSplash.FormShow(Sender: TObject);
begin
  self.BringToFront;
end;

procedure TFrmSplash.FormWindowStateChange(Sender: TObject);
begin
  self.BringToFront;
end;


initialization
  cacheDir:= GetEnvironmentVariable('LOCALAPPDATA') + DirectorySeparator
    + Application.Title;
  If Not DirectoryExists(cacheDir) then CreateDir (cacheDir);
  GlobalWebView2Loader                 := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder  := cacheDir;
  GlobalWebView2Loader.AutoplayPolicy  := TWVAutoplayPolicy.appNoUserGestureRequired;
  GlobalWebView2Loader.KioskPrinting   := True; // This property enables silent priting
  GlobalWebView2Loader.DisableFeatures := 'msWebOOUI,msPdfOOUI';  // Disable the text selection context menu
  GlobalWebView2Loader.StartWebView2;

end.

