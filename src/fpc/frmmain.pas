unit FrmMain;

{$mode ObjFPC}{$H+}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, IniFiles,
  eventlog, process, advancedsingleinstance, singleinstance, registry, lclintf,
  ExtCtrls, Graphics, SplashUtils, FrmSplash, DisableWinKeys;

type

  { TFrmMain }

  TFrmMain = class(TForm)
    editFocus: TEdit;
    evLog: TEventLog;
    imgLogo: TImage;
    tmrInstanceServer: TTimer;
    tmrCounter: TTimer;
    tmrNotify: TTimer;
    tmrPlay: TTimer;
    tmrSplash: TTimer;
    pSingleInstance: TAdvancedSingleInstance;
    trayNotify: TTrayIcon;
    procedure editFocusExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure imgLogoClick(Sender: TObject);
    procedure SetFormFocus(Sender: TObject);
    procedure SetKeyFocus();
    procedure ServerReceivedParams(Sender: TBaseSingleInstance;
      MsgData: TStringList);
    procedure ServerReceivedCustomRequest(Sender: TBaseSingleInstance;
      MsgID: Integer; MsgType: Integer; MsgData: TStream);
    procedure tmrCounterTimer(Sender: TObject);
    procedure tmrInstanceServerTimer(Sender: TObject);
    procedure tmrPlayTimer(Sender: TObject);
    procedure tmrSplashTimer(Sender: TObject);
    procedure tmrNotifyTimer(Sender: TObject);
    procedure editFocusKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure destroyClones();
    procedure showSplash();
    procedure hideSplash(restartIntervalTimer: boolean = true);
    procedure cancelSplash(logType: string);
    procedure trayNotifyClick(Sender: TObject);
    procedure trayNotifyBalloonClick(Sender: TObject);
    function usage(): string;

  private
    pCloned: array of TFrmSplash;
    pCounter: integer;
    procedure initialize(readIni: bool = true; paramList: TStringList = nil);
    procedure initComponent();
    procedure logMe(appname:string; urlLog:string; logtype: string);
    procedure SessionWndProc(var Message: TMessage);

  public
    pDisablePageBlock:  boolean;
    pShowCloseButton:   boolean;
    pDisableDragMove:   boolean;
  end;

var                                                                 
  SessionWnd: HWND;
  formMain: TFrmMain;
  pUrl:               string;
  pUrlLog:            string;
  pAppName:           string = 'SPLASH';
  pMessageNotify:     string = 'Splash will be shown.'#13
    + 'Click here to show immediately or press Esc to cancel.';
  pDuration:          integer = 10;
  pSize:              integer = 50;
  pInterval:          real = 0;
  pDisableLogging:    boolean = false;
  pDisableKeyLock:    boolean = false;
  pDisableEscape:     boolean = false;
  pDisableTrayIcon:   boolean = false;
  pMaxCancel:         integer = 0;
  pCancelCounter:     integer = 0;
  pDisableMultiScreen:boolean = false;
  pCaption:           string = 'Splash';
  pWindowState        : TWindowState;
  pUsage:             string = '';

  pIniFile:            string;
  pIniFileTimestamp:   TDateTime;
  pIniCounter:         integer;
  pIsInitialized:       boolean=false;
  pIsScreenLocked:       boolean=false;

implementation

const
  LWA_COLORKEY = 1;
  LWA_ALPHA = 2;
  LWA_BOTH = 3;
  WS_EX_LAYERED = $80000;
  GWL_EXSTYLE = -20;
  NOTIFY_FOR_THIS_SESSION = $0;
  NOTIFY_FOR_ALL_SESSIONS = $1;
  WTS_CONSOLE_CONNECT = $1;
  WTS_CONSOLE_DISCONNECT = $2;
  WTS_REMOTE_CONNECT = $3;
  WTS_REMOTE_DISCONNECT = $4;
  WTS_SESSION_LOGON = $5;
  WTS_SESSION_LOGOFF = $6;
  WTS_SESSION_LOCK = $7;
  WTS_SESSION_UNLOCK = $8;
  WTS_SESSION_REMOTE_CONTROL = $9;
  WTS_SESSION_CREATE = $A;
  WTS_SESSION_TERMINATE = $B;

{$R *.lfm}

{ TFrmMain }
function TFrmMain.usage(): string;
var
  buff: string;
begin
  if pUsage <> '' then
  begin
    usage:=pUsage;
    exit;
  end;
  buff:= 'Usage: '#13+ ExtractFileName(Application.ExeName)
    + ' -u url [-t time] [-i interval] [-i size] [-mc maxcnt] '
    + '[-a appname] [-c caption] [-m msg] [-l urlLog] [-f filename] '
    + '[-db] [-dl] [-de] [-dk] [-dm] [-dd] [-dt] [-mi] [-x] [-w] [-r|-dr]'#13;
  buff:= buff + ' -u url     : url of web page to load.'
    + ' Using https://, http://, or file:/// to load file in local computer'#13;
  buff:= buff + ' -t time    : duration splash will show in second (default = '
    + inttostr(pDuration) + ' seconds; 0 = forever)'#13;
  buff:= buff + ' -i interval: interval hours the splash will be shown, '
    + 'if omitted splash will be shown once'#13;
  buff:= buff + ' -s size    : display size within screen in percentage '
    + '(default = ' + inttostr(pSize) + ')'#13;
  buff:= buff + ' -c caption : application/notification caption (default = '
    + pCaption + ')'#13;
  buff:= buff + ' -m msg     : notification message prior splash appears'#13;
  buff:= buff + ' -a appname : log identifier name (default = '
    + pAppName + ')'#13;
  buff:= buff + ' -l urlLog  : url server to log usage of splash by user'#13;
  buff:= buff + ' -dl        : disable logging'#13;
  buff:= buff + ' -x         : show close button, not working with -de option'#13;
  buff:= buff + ' -db        : disable web page block / enable interactive mode'
    + ', implies -x'#13;
  buff:= buff + ' -dd        : disable drag/move splash, '
    + 'not working in interactive mode'#13;
  buff:= buff + ' -de        : disable Esc key'#13;
  buff:= buff + ' -dk        : disable system key lock'#13;
  buff:= buff + ' -dm        : disable show into multiple screen'#13;
  buff:= buff + ' -dt        : disable tray icon'#13;
  buff:= buff + ' -mc maxcnt : max cancel splash count before splash is forced '
    + 'to be shown (default = 0, it means never force splash to be shown)'#13;
  buff:= buff + ' -mi        : enable multiple instance running application'#13;
  buff:= buff + ' -stop      : stop existing single instance running'#13;
  buff:= buff + ' -f filename: .ini configuration file (default = '
    + Application.title + '.ini). Any changes to this file will take effect '
    + 'immediately to current running application'#13;
  buff:= buff + ' -w         : write to file config as defined in -f option, '
    + 'then exit'#13;
  buff:= buff + ' -r, -dr    : add/remove config to automatically run '
    + 'application when computer start'#13;
  buff:= buff + 'all parameters in bracket are optional'#13;
  usage:=buff;
end;

function GetFileModTime(const AFileName: String; var FileTime: TDateTime): Boolean;
var
  SR: TSearchRec;
  LocalFileTime: TFileTime;
  SystemTime: TSystemTime;
begin
  Result := False;
  if FindFirst(AFileName, faAnyFile, SR) = 0 then
  begin
    if FileTimeToLocalFileTime(SR.FindData.ftLastWriteTime, LocalFileTime) then
    begin
      if FileTimeToSystemTime(LocalFileTime, SystemTime) then
       begin
         FileTime := SystemTimeToDateTime(SystemTime);
         Result := True;
       end;
    end;
    SysUtils.FindClose(SR);
  end;
end;

procedure TFrmMain.ServerReceivedParams (Sender: TBaseSingleInstance;
  MsgData: TStringList);
begin
  if getParamAsBool(MsgData, '-stop', false) then
  begin
    //showmessage('param server received:'#13 + MsgData.Text);
    self.Close;
    //Application.Terminate;
  end
  else
  begin
    initialize(false, MsgData);
    initComponent();
  end;
end;

procedure TFrmMain.ServerReceivedCustomRequest(Sender: TBaseSingleInstance;
  MsgID: Integer; MsgType: Integer; MsgData: TStream);
begin
  //showmessage('other new instance ServerReceivedCustomRequest');
end;

procedure TFrmMain.initialize(readIni: bool = true; paramList: TStringList = nil);
const
  C_INI: string = 'HSSESplash';
var
  buff: string;
  pMemIni : TMemIniFile = nil;
  pWriteIni : bool = false;
  pMultipleInstance: bool = false;
  params: TSTringList;
  pRegistry: TRegistry;
  pOpenKeyResult: bool = false;
  pAutoRun: bool=false;
begin
  params := paramList;
  if params = nil then params:= TSTringList.Create;


  // first, check if we sould get config from ini
  buff:= getParam(params, '-f');
  if buff <> '' then pIniFile:= buff
  else
  begin
    if not pIsInitialized then
      pIniFile:= GetEnvironmentVariable('LOCALAPPDATA') + DirectorySeparator
        + Application.Title + DirectorySeparator + Application.Title + '.ini';
  end;
  if not FileExists(pIniFile) then pIniFile:= Application.Title + '.ini';
  pWriteIni:= getParamAsBool(params, '-w', false);// and FileExists(pIniFile);

  // check for multiple instance
  pMultipleInstance:= getParamAsBool(params, '-mi', false);
  if not(pMultipleInstance) and not assigned(pSingleInstance) then
  begin
     pSingleInstance := TAdvancedSingleInstance.Create(Self);
     pSingleInstance.Global:= false;
     pSingleInstance.OnServerReceivedParams:= @ServerReceivedParams;
     pSingleInstance.OnServerReceivedCustomRequest:= @ServerReceivedCustomRequest;

     if pSingleInstance.Start = siServer then
       tmrInstanceServer.enabled:=true
     else
     begin
       tmrInstanceServer.enabled:=false;
       pSingleInstance.ClientPostParams;
       Application.Terminate;
     end;
  end;

  // here we go, set all variable
  if not pIsInitialized then pAppName:= Application.Title;
  if not pIsInitialized then pCaption:= Application.Title;

  //if not assigned(pMemIni) then
  //begin
    pMemIni:= TMemIniFile.Create(pIniFile);
    pMemIni.CacheUpdates:= true;
    pMemIni.Options := pMemIni.Options + [ifoWriteStringBoolean];
    pMemIni.BoolTrueStrings := ['true','yes','1'];
    pMemIni.BoolFalseStrings := ['false','no','0'];
  //end;

  self.usage();

  if not pIsInitialized then pUrl:= '';
  if readIni then pUrl:= pMemIni.ReadString(C_INI,'url',pUrl);
  buff:= getParam(params, '-u');
  //assume the first parameter is url
  if buff='' then
  begin
    if pUrl='' then pUrl:=ParamStr(1);
    if pUrl.StartsWith('-') then pUrl:='';
  end
  else
    pUrl:=buff;
  if pWriteIni then pMemIni.WriteString(C_INI,'url',pUrl);

  if not pIsInitialized then pUrlLog:= '';
  if readIni then pUrlLog:= pMemIni.ReadString(C_INI,'logServer',pUrlLog);
  buff:= getParam(params, '-l');
  if not (buff = '') then pUrlLog:=buff;
  if pWriteIni then pMemIni.WriteString(C_INI,'logServer',pUrlLog);

  if readIni then pAppName:= pMemIni.ReadString(C_INI,'appName',pAppName);
  buff:= getParam(params, '-a');
  if not (buff = '') then pAppName:= buff;
  if pWriteIni then pMemIni.WriteString(C_INI,'appName',pAppName);

  if readIni then pDisableKeyLock:= pMemIni.ReadBool(C_INI,'disableKeyLock', pDisableKeyLock);
  if getParamAsBool(params, '-dk', false) then pDisableKeyLock:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disableKeyLock', pDisableKeyLock);

  if readIni then pDisableLogging:= pMemIni.ReadBool(C_INI,'disableLogging', pDisableLogging);
  if getParamAsBool(params, '-dl', false) then pDisableLogging:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disableLogging', pDisableLogging);

  if not pIsInitialized then self.pShowCloseButton:= false;
  if readIni then self.pShowCloseButton:= pMemIni.ReadBool(C_INI,'showCloseButton', self.pShowCloseButton);
  if getParamAsBool(params, '-x', false) then self.pShowCloseButton:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'showCloseButton', self.pShowCloseButton);

  if not pIsInitialized then self.pDisablePageBlock:= false;
  if readIni then self.pDisablePageBlock:= pMemIni.ReadBool(C_INI,'disablePageBlock', self.pDisablePageBlock);
  if getParamAsBool(params, '-db', false) then self.pDisablePageBlock:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disablePageBlock', self.pDisablePageBlock);
  if self.pDisablePageBlock then self.pShowCloseButton:=true;

  if readIni then pDisableMultiScreen:= pMemIni.ReadBool(C_INI,'disableMultiScreen', pDisableMultiScreen);
  if getParamAsBool(params, '-dm', false) then pDisableMultiScreen:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disableMultiScreen', pDisableMultiScreen);

  if readIni then pDuration:= pMemIni.ReadInteger(C_INI,'duration', pDuration);
  pDuration:= getParamAsInt(params, '-t', pDuration);
  if pWriteIni then pMemIni.WriteInteger(C_INI,'duration', pDuration);

  if readIni then pDisableEscape:= pMemIni.ReadBool(C_INI,'disableEscape', pDisableEscape);
  if getParamAsBool(params, '-de', false) and (pDuration>0) then
     pDisableEscape:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disableEscape', pDisableEscape);

  if readIni then pDisableTrayIcon:= pMemIni.ReadBool(C_INI,'disableTrayIcon', pDisableTrayIcon);
  if getParamAsBool(params, '-dt', false) then pDisableTrayIcon:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disableTrayIcon', pDisableTrayIcon);

  if readIni then pMaxCancel:= pMemIni.ReadInteger(C_INI,'maxCancel', pMaxCancel);
  pMaxCancel:= getParamAsInt(params, '-mc', pMaxCancel);
  if pMaxCancel<=0 then pMaxCancel := 0;
  if pWriteIni then pMemIni.WriteInteger(C_INI,'maxCancel', pMaxCancel);

  if readIni then pSize:= pMemIni.ReadInteger(C_INI,'size', pSize);
  pSize:= getParamAsInt(params, '-s', pSize);
  if pSize<=0 then pSize := 10;
  if pWriteIni then pMemIni.WriteInteger(C_INI,'size', pSize);

  if not pIsInitialized then pDisableDragMove:= false;
  if readIni then pDisableDragMove:= pMemIni.ReadBool(C_INI,'disableDragMove', pDisableDragMove);
  if getParamAsBool(params, '-dd', false) then pDisableDragMove:= true;
  if pWriteIni then pMemIni.WriteBool(C_INI,'disableDragMove', pDisableDragMove);
  // only able to move not in fullscreen
  pDisableDragMove:= (pSize>=100) or pDisableDragMove;

  if readIni then pInterval:= pMemIni.ReadFloat(C_INI,'interval', pInterval);
  pInterval:= getParamAsFloat(params, '-i', pInterval);
  if pInterval < 0 then pInterval:= 0;
  if pWriteIni then pMemIni.WriteFloat(C_INI,'interval', pInterval);

  if readIni then pCaption:= pMemIni.ReadString(C_INI,'caption', pCaption);
  buff:= getParam(params, '-c');
  if buff <> '' then pCaption:= buff;
  if pWriteIni then pMemIni.WriteString(C_INI,'caption',pCaption);

  if readIni then pMessageNotify:= pMemIni.ReadString(C_INI,'messageNotify', pMessageNotify);
  buff:= getParam(params, '-m');
  if buff <> '' then pMessageNotify:= buff;
  if pWriteIni then pMemIni.WriteString(C_INI,'messageNotify',pMessageNotify);

  if readIni then pAutoRun:= pMemIni.ReadBool(C_INI, 'autoRun', pAutoRun);
  pAutoRun:= pAutoRun or getParamAsBool(params, '-r', false);
  if getParamAsBool(params, '-dr', false) then pAutoRun:= false;
  if pWriteIni then pMemIni.WriteBool(C_INI, 'autoRun', pAutoRun);

  // write ini as necessary
  if pWriteIni then
  try
    pMemIni.UpdateFile();
    pMemIni.Free;
  finally
    Application.Terminate;
  end
  else pMemIni.Free;

  if pinifile <> '' then
     GetFileModTime(pIniFile, pIniFileTimestamp);

  try
    pRegistry := TRegistry.Create;
    // Set for current user only
    pRegistry.RootKey := HKEY_CURRENT_USER;
    // Ensure registry can be written to
    pRegistry.Access:= KEY_WRITE;
    // OpenKey creates the new key, if it does not already exist. Returns true if successful.
    buff:= '\Software\Microsoft\Windows\CurrentVersion\Run\';
    pOpenKeyResult := pRegistry.OpenKey(buff, true);
    if pOpenKeyResult then
    begin
      if pAutoRun then
        pRegistry.WriteString(application.title,
        ExtractFilePath(application.ExeName)
        + ExtractFileName(application.ExeName))
      else
        pRegistry.DeleteValue(application.title);
    end;
  finally
    pRegistry.Free;
  end;

  if getParamAsBool(params, '-stop', false) then
     application.terminate; // just stop.

  pIsInitialized:= true;
end;


procedure TFrmMain.logMe(appname:string; urlLog:string; logtype: string);
var
  msg: string;
begin
  if pDisableLogging then exit;

  log(appname, urlLog, logType, pCounter);

  if not (logtype = 'ON') then
  begin
    msg := inttostr((pCounter mod (60))) + ' second(s)';
    if pCounter >= 60 then
      msg := inttostr((pCounter div (60))) + ' minute(s) ' + msg;
    if pCounter >= 60 * 60 then
      msg := inttostr((pCounter div (60*60))) + ' hour(s) ' + msg;
    msg:=appname + ' : '
          + uppercase(GetEnvVarValue('USERDOMAIN'))
          + '\'
          + uppercase(GetEnvVarValue('USERNAME'))
          + ' - ' + logType
          + #13' Total session: ' + msg;
    evLog.log(msg);
  end;
end;

procedure TFrmMain.initComponent();
begin

  // turn it off first
  tmrNotify.enabled := false;
  tmrPlay.enabled := false;

  // start log
  self.evLog.Identification:=pAppName;
  self.evLog.Active:=(not pDisableLogging);

  self.caption:= pCaption;
  application.Title:= pCaption;
  self.editFocus.text:='';
  self.editFocus.BorderStyle:=bsnone;
  self.BorderStyle:=bsnone;
  self.Top:=0;
  self.Left:=0;
  self.clientwidth:=self.imgLogo.ClientWidth;
  self.ClientHeight:=self.imgLogo.ClientHeight;
  //pWindowState:= self.WindowState;
  Self.Color := clWhite;
  //Transparency := Self.Color;
  if not(pDisableEscape) then SetTranslucent(Self.Handle, Self.Color, 128);

  //imgLogo.Picture.LoadFromResourceName(self.ParentWindow, 'LOGO-32X32');
  //popupAlert.Icon.LoadFromResourceName(self.ParentWindow, 'LOGO-32X32');
  self.Icon:=Application.Icon;
  imgLogo.Picture.Icon:= Application.Icon; // .LoadFromResourceName(self.ParentWindow, 'MAINICON');
  //popupAlert.Icon.Icon:= Application.Icon; // .LoadFromResourceName(self.ParentWindow, 'MAINICON');
  trayNotify.Icon:= Application.Icon;
  //popupAlert.StyleOptions.DisplayDuration:=tmrNotify.Interval;
  //popupAlert.Title:=pCaption;
  //popupAlert.Text:=pMessageNotify;
  trayNotify.BalloonTitle:= pCaption;
  trayNotify.BalloonHint:= pMessageNotify;
  trayNotify.ShowIcon:=true;
  trayNotify.Visible:=not(pDisableTrayIcon);
  trayNotify.BalloonTimeout:=tmrNotify.Interval;

  //if pUrl='' then pUrl:='file:///' + createTempFile(usage);
  //pUrl:='https://www.google.com/';

  tmrSplash.Interval := pDuration * 1000;
  tmrNotify.enabled:=true;

  tmrPlay.Interval := round(pInterval * 1000) * 60 * 60;
  tmrPlay.enabled:=(pInterval>0);

  logMe(pAppname, pUrlLog, 'ON');
  self.WindowState:=wsNormal;
  self.BringToFront();
  //popupAlert.ShowAtPos(Screen.Width - 2,
  //  Screen.WorkAreaHeight - popupAlert.vNotifierForm.Height - 2);
  trayNotify.ShowBalloonHint;

  SetKeyFocus;
end;

function WTSRegisterSessionNotification(hWnd: HWND; dwFlags: DWORD): Boolean; stdcall;
  external 'wtsapi32.dll' name 'WTSRegisterSessionNotification';
function WTSUnRegisterSessionNotification(hWnd: HWND): Boolean; stdcall;
  external 'wtsapi32.dll' name 'WTSUnRegisterSessionNotification';

procedure TFrmMain.FormCreate(Sender: TObject);
var
  i: integer;
  params: TSTringList;
begin

  SessionWnd := LCLIntf.AllocateHWnd(@SessionWndProc);
  if not WTSRegisterSessionNotification(SessionWnd, NOTIFY_FOR_THIS_SESSION) then
  begin
    // uh oh, cannot get session notification
    // RaiseLastOSError;
  end;

  params:= TSTringList.Create;
  for i := 1 to ParamCount do
      params.Add(paramStr(i));

  self.initialize(true, params);

  Application.OnDeactivate:=@SetFormFocus;
  Application.OnActivate:=@SetFormFocus;
  trayNotify.OnBalloonClick :=@trayNotifyBalloonClick;

  initComponent();
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  if SessionWnd <> 0 then
  begin
    WTSUnRegisterSessionNotification(SessionWnd);
    DeallocateHWnd(SessionWnd);
  end;
end;

procedure TFrmMain.SessionWndProc(var Message: TMessage);
begin
  if Message.Msg = WM_WTSSESSION_CHANGE then
  begin
    case Message.wParam of
      WTS_SESSION_LOCK: begin
        //Inc(LockedCount);
        pIsScreenLocked:= true;
      end;
      WTS_SESSION_UNLOCK: begin
        //lbl2.Caption := Format('Session was locked %d times.', [LockedCount]);
        pIsScreenLocked:= false;
      end;
    end;
  end;

  Message.Result := DefWindowProc(SessionWnd, Message.Msg, Message.WParam, Message.LParam);
end;

procedure TFrmMain.FormWindowStateChange(Sender: TObject);
begin
  Application.BringToFront;
  //self.WindowState:=pWindowState;
  self.BringToFront;
end;

procedure TFrmMain.SetFormFocus(Sender: TObject);
begin
  Application.BringToFront;
  SetKeyFocus();
end;

procedure TFrmMain.SetKeyFocus();
begin
  if not tmrSplash.Enabled then exit;
  if pDisablePageBlock then exit;
  try
    if formmain.visible then formmain.setfocus;
  except
  end;
  try
    if formmain.visible then formmain.editFocus.SetFocus;
  except
  end;
end;

procedure TFrmMain.tmrCounterTimer(Sender: TObject);
begin
  pCounter:=pCounter+1;       
  //if not formmain.pDisablePageBlock then
  //  Application.BringToFront;
end;

procedure TFrmMain.tmrInstanceServerTimer(Sender: TObject);
var
  filetimestamp: TDateTime;
begin
  if Assigned(pSingleInstance) then
    if pSingleInstance.IsServer then pSingleInstance.ServerCheckMessages;

  // TODO make sure we do not want to reinitialized twice at the same time
  // with pSingleInstance.ServerCheckMessages
  if pinifile <> '' then
   begin
     pIniCounter:=pIniCounter+1;
     if pIniCounter mod 10 = 0 then
     begin
       pIniCounter:=0;
       // check change on ini file
       if GetFileModTime(pIniFile, filetimestamp) then
         if filetimestamp <> pIniFileTimestamp then
           begin
             // do something when ini file is changed
             initialize();
             initComponent();
           end;
     end;
   end;
end;

procedure TFrmMain.destroyClones();
var
  i: integer;
  frm:TFrmSplash;
begin

  for i:=0 to high(pcloned) do
  begin
    try
    frm:=pcloned[i];
    if assigned(frm) then
    begin
      frm.hide;
      frm.Destroy;
      pCloned[i]:=nil;
    end;
    except
    end;
  end;
end;

procedure TFrmMain.tmrSplashTimer(Sender: TObject);
begin
  if not pDisableKeyLock then
    DisableWinKeys.EnableWindowsUI;
  hideSplash();
  pCancelCounter:=0;
  trayNotify.Icon:= application.Icon;
  trayNotify.Hint:= pCaption;
  tmrSplash.Enabled:=false;
  tmrCounter.Enabled:=false;
  pCounter:=pCounter+1;
  logMe(pappname, purlLog, 'END');
  if pInterval<=0 then
  begin
    self.close;
    application.Terminate;
  end;
end;

procedure TFrmMain.tmrNotifyTimer(Sender: TObject);
begin
  self.tmrNotify.Enabled:=false;  
  //self.popupAlert.Hide;
  if not pIsScreenLocked then // not showing splash when screen is locked
    showSplash
  else
  begin
    hideSplash();
    tmrSplash.Enabled:=false;
    tmrCounter.Enabled:=false;
    logMe(pappname, purlLog, 'LOCK');
    if pInterval<=0 then
    begin
      self.close;
      application.Terminate;
    end;
  end;
end;

procedure TFrmMain.showSplash();
var
  i:integer;
  frmcl: TFrmSplash;
begin
  self.Show;
  self.visible:=true;
  //application.Restore;

  if not pDisableKeyLock then
    DisableWinKeys.DisableWindowsUI;

  if pDuration>=0 then
  begin
    tmrSplash.Interval:= pDuration * 1000;
    tmrSplash.Enabled:=true;
    tmrCounter.Enabled:=true;
    pCounter:=0;
  end;

  SetKeyFocus;

  if pDisableMultiScreen then
    SetLength(pCloned,1)
  else
    SetLength(pCloned,screen.MonitorCount);

  Application.BringToFront;
  self.BringToFront;
  for i := 0 to high(pCloned) do
  begin
    frmcl:=TFrmSplash.create(self);
    frmcl.Icon:=self.Icon;
    frmcl.initialize(i, pSize);
    pCloned[i]:=frmcl;
  end;
  for i := 0 to high(pCloned) do
  begin
    pCloned[i].wbFlash.DefaultURL:=pUrl;
    pCloned[i].BringToFront;
  end;
  SetKeyFocus;
end;

procedure TFrmMain.editFocusKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key=27) and (pDisableEscape = false) then
    cancelSplash('OFF');
end;

procedure TFrmMain.hideSplash(restartIntervalTimer: boolean = true);
begin
  if not pDisableKeyLock then
      DisableWinKeys.EnableWindowsUI;
  // re-enable play interval after splash are closed
  if (restartIntervalTimer) then
  begin
    tmrPlay.enabled:=false;
    tmrPlay.enabled:=(pInterval>0);
  end;
  application.Minimize;
  self.Hide;
  destroyClones();
  ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TFrmMain.cancelSplash(logType: string);
begin
  if (pCancelCounter >= pMaxCancel) and (pMaxCancel > 0) then
  begin
     exit;
  end;
  pCancelCounter := pCancelCounter + 1;
  hideSplash(false);
  trayNotify.Hint:= pCaption
    + #13'Cancel: ' + inttostr(pCancelCounter) + ' time';
  If pCancelCounter>1 then trayNotify.Hint := trayNotify.Hint+'s';
  case pCancelCounter of
    0: trayNotify.Icon:= application.Icon;
    1,2,3,4,5,6,7,8,9:
      trayNotify.Icon.LoadFromResourceName(self.ParentWindow,
        'NUM-' + inttostr(pCancelCounter)+ '-32X32');
    else
      trayNotify.Icon.LoadFromResourceName(self.ParentWindow, 'NUM-X-32X32');
  end;

  logMe(pappname, purlLog, logType);
  tmrSplash.enabled:=false;
  tmrNotify.enabled:=false;
  if pInterval<=0 then
    application.Terminate;
end;

procedure TFrmMain.trayNotifyClick(Sender: TObject);
begin
  // to prevent multiple splash at the same time
  if not(tmrSplash.Enabled) and not(tmrNotify.Enabled) then tmrNotifyTimer(nil);
  SetKeyFocus();
end;

procedure TFrmMain.trayNotifyBalloonClick(Sender: TObject);
begin
  // to prevent multiple splash at the same time
  if not (tmrSplash.Enabled) then tmrNotifyTimer(nil);
  SetKeyFocus();
end;

(*
procedure TFrmMain.popupAlertClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  self.popupAlert.Hide; // no-show-splash when alert is closed immediately
  if not pDisableEscape then cancelSplash('OFF');
end;
*)

procedure TFrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  cancelSplash('OFF');
end;

procedure TFrmMain.imgLogoClick(Sender: TObject);
begin
  showmessage(usage + #13#13#$C2#$A9' @ablehunder :)'); // what's wrong with #169?
end;

procedure TFrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:= not(pDisableEscape);
end;

procedure TFrmMain.tmrPlayTimer(Sender: TObject);
begin
    // to avoid new splash while existing still played
    if not (tmrSplash.Enabled) and not(tmrNotify.Enabled)
      then
    begin
      logMe(pAppname, pUrlLog, 'ON');
      tmrNotify.Enabled:=true;
      self.WindowState:=wsNormal;
      self.BringToFront();
      //popupAlert.Title:=pCaption;
      //popupAlert.Text:=pMessageNotify;
      //popupAlert.ShowAtPos(Screen.Width - 2,
      //  Screen.WorkAreaHeight - popupAlert.vNotifierForm.Height - 2);
      trayNotify.BalloonTitle:= pCaption;
      trayNotify.BalloonHint:=pMessageNotify;
      trayNotify.ShowBalloonHint ;
    end;
end;

procedure TFrmMain.editFocusExit(Sender: TObject);
begin
    //SetKeyFocus;
end;

end.

