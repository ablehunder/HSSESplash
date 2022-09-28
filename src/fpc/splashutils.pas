unit SplashUtils;

{$mode ObjFPC}{$H+}

interface

uses Windows, Classes, Sysutils, Winsock, wininet, JwaTlHelp32;

function GetEnvVarValue(const VarName: string): string;
Function GetIPAddress():String;
function createTempFile(content:string): string;
function getParam(paramList: TStringList; paramName: string; hasValue:boolean = true):string;
function getParamAsBool(paramList: TStringList; paramName: string; hasValue:bool = true):bool;
function getParamAsInt(paramList: TStringList; paramName: string; defaultValue: integer = 0):integer;
function getParamAsFloat(paramList: TStringList; paramName: string; defaultValue: real = 0):real;
Function getServerName(const URL : String) : String;
Function getServerPort(const URL : String; defaultPort : Integer = 80) : Integer;
Function getServerPage(const URL : String) :String;
procedure log(appname:string; UrlLog:string; logtype: string; duration: integer=0);
function requestHttp(const AUrl, AData: AnsiString; blnSSL: Boolean = True): AnsiString;
procedure SetTranslucent(ThehWnd: longint; Color: longint; nTrans: integer);

implementation

function GetEnvVarValue(const VarName: string): string;
var
  BufSize: Integer;  // buffer size required for value
begin
  // Get required buffer size (inc. terminal #0)

  BufSize := GetEnvironmentVariableA(PChar(VarName), nil, 0);
  if BufSize > 0 then
  begin
    // Read env var value into result string
    SetLength(Result, BufSize - 1);
    GetEnvironmentVariableA(PChar(VarName),
      PChar(Result), BufSize);
  end

  else
    // No such environment variable
    Result := '';

end;

Function GetIPAddress():String;
type
  pu_long = ^u_long;
var
  varTWSAData : TWSAData;
  varPHostEnt : PHostEnt;
  varTInAddr : TInAddr;
  namebuf : Array[0..255] of char;
begin
  If WSAStartup($101,varTWSAData) <> 0 Then
  Result := ''
  Else Begin
    gethostname(namebuf,sizeof(namebuf));
    varPHostEnt := gethostbyname(namebuf);
    varTInAddr.S_addr := u_long(pu_long(varPHostEnt^.h_addr_list^)^);
    Result :=  inet_ntoa(varTInAddr);
  End;
  WSACleanup;
end;

function createTempFile(content:string): string;
var
lpTempFileName: array[0..MAX_PATH] of Char;
lpTempPath: array[0..MAX_PATH] of Char;
myFile: TextFile ;
begin

  Windows.GetTempPath(MAX_PATH, lpTempPath);
  Windows.GetTempFileName(lpTempPath, nil,0, lpTempFileName);

  AssignFile(myFile,strpas(lpTempFileName));
  Rewrite(myFile);

  try
      WriteLn(myFile, content);
 finally
   CloseFile(myFile);
 end;

Result:=strpas(lpTempFileName);
end;

function Mid_mc(s:string; StartPos: integer; EndPos: integer = -1):string;
// NumOfCharacters = -1 means "to the end of the string"
var i, j: integer;
begin
  i := StartPos;
  if (i < 1) then i := 1;
  if (i > length(s)) then
  begin
    Mid_mc := '';
    exit;
  end;
  if EndPos < 0 then
    j := length(s)
  else
    j := EndPos;
  Mid_mc := copy(s, i, j-i+1);
end; // Mid_mc integer, integer

function InStr_mc(S, Pattern: string; Start: integer=1; CaseSensitive:boolean=false):integer;
var
  tempStr: string;
  i, j: integer;
begin
  i := Start;
  if (i < 1) then i := 1;
  if (i > length(s)) then
  begin
    InStr_mc := 0; exit;
  end;
  tempStr := copy(s, i, length(s));
  if CaseSensitive then
    j := pos(Pattern, tempStr)
  else
    j := pos(upperCase(Pattern), upperCase(tempStr));
  if J<>0 then j := i+j-1 ;
  InStr_mc := j;
end; // InStr_mc

Function getServerName(const URL : String) : String;
var
vResult:string;
begin
vResult := URL;
If InStr_mc(URL, '//') > 0 Then
    vResult := Mid_mc(URL, InStr_mc(URL, '//') + 2);
If InStr_mc(vResult, '/') > 0 Then
    vResult := Mid_mc(vResult, 1, InStr_mc(vResult, '/') - 1);
If InStr_mc(vResult, ':') > 0 Then
    vResult := Mid_mc(vResult, 1, InStr_mc(vResult, ':') - 1);
Result:=vResult;
end;


Function getServerPort(const URL : String; defaultPort : Integer = 80) : Integer;
var
vResult:Integer;
buff:string;
begin

  If InStr_mc(URL, '//') > 0 Then
    buff:= Mid_mc(URL, InStr_mc(URL, '//') + 2);
  If InStr_mc(buff, '/') > 0 Then
    buff:= Mid_mc(buff, 1, InStr_mc(buff, '/') - 1);
  If InStr_mc(buff, ':') > 0 Then
    buff:= Mid_mc(buff, InStr_mc(buff, ':') + 1);

  try
    vResult:= strtoInt(buff);
  except
    vResult:= defaultPort;
  end;

Result:=vResult;
end;

Function getServerPage(const URL : String) :String;
var
vResult:string;
begin
vResult:= URL;
If InStr_mc(URL, '//') > 0 Then
    vResult:= Mid_mc(URL, InStr_mc(URL, '//') + 2);
If InStr_mc(vResult, '/') > 0 Then
    vResult:= Mid_mc(vResult, InStr_mc(vResult, '/'));
Result:=vResult;
end;

function getParam(paramList: TStringList; paramName: string; hasValue:boolean = true):string;
var
  i: integer;
  buff: string;
begin      
  buff:='';
  for i := 0 to paramList.Count-1 do
  begin
    if paramList.Strings[i] = paramName then
    begin
      if hasValue then buff:=paramList.Strings[i+1]
      else buff:=paramList.Strings[i];
      break;
    end;
  end;
  result:=buff;
end;

function getParamAsBool(paramList: TStringList; paramName: string; hasValue:bool = true):bool;
var
  buff: string;
begin
  buff:= getParam(paramList, paramName, hasValue);
  if (buff = 'true') or ((hasValue=false) and (buff=paramName)) then
     result:= true
  else
     result:= false;
end;

function getParamAsInt(paramList: TStringList; paramName: string; defaultValue:integer=0):integer;
var
  buff: integer = 0;
begin
  try
    buff:=strtoInt(getParam(paramList, paramName));
  except
    on e: EConvertError do
      buff:=defaultValue;
  end;
  result:=buff;
end;

function getParamAsFloat(paramList: TStringList; paramName: string; defaultValue:real=0): real;
var
  buff: real = 0;
begin
 try
   buff:=StrToFloat(getParam(paramList, paramName));
 except
   on e: EConvertError  do
     buff:=defaultValue;
 end;
 result:=buff;
end;

function requestHttp(const AUrl, AData: AnsiString; blnSSL: Boolean = True): AnsiString;
var
  aBuffer     : Array[0..4096] of Char;
  Header      : TStringStream;
  BufStream   : TMemoryStream;
  sMethod     : AnsiString;
  BytesRead   : Cardinal;
  pSession    : HINTERNET;
  pConnection : HINTERNET;
  pRequest    : HINTERNET;
  port        : Integer;
  flags       : DWord;
  serverName : string;
  scriptName : string;
begin
  Result := '';
  try
    serverName:=getServerName(AUrl);
    port:=getserverport(AUrl);
    scriptName := getServerPage(AUrl);
    except
    exit;
  end;

  pSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(pSession) then
  try
    if port=80 then
    begin
      if blnSSL then
        Port := INTERNET_DEFAULT_HTTPS_PORT
      else
        Port := INTERNET_DEFAULT_HTTP_PORT;
    end;
    pConnection := InternetConnect(pSession, PChar(serverName), port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);

    if Assigned(pConnection) then
    try
      if (AData = '') then
        sMethod := 'GET'
      else
        sMethod := 'POST';

      if blnSSL then
        flags := INTERNET_FLAG_SECURE or INTERNET_FLAG_KEEP_CONNECTION
      else
        flags := INTERNET_SERVICE_HTTP;

      pRequest := HTTPOpenRequest(pConnection, PChar(sMethod), PChar(scriptName), nil, nil, nil, flags, 0);

      if Assigned(pRequest) then
      try
        Header := TStringStream.Create('');
        try
          with Header do
          begin
            WriteString('Host: ' + serverName + sLineBreak);
            WriteString('User-Agent: HSSESplash' +SLineBreak);
            WriteString('Accept: text/plain,text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8'+SLineBreak);
            WriteString('Accept-Language: en-us,en;q=0.5' + SLineBreak);
            WriteString('Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7'+SLineBreak);
            WriteString('Keep-Alive: 300'+ SLineBreak);
            WriteString('Content-Type: application/x-www-form-urlencoded'+ SLineBreak);
            WriteString('Connection: Keep-Alive'+ SlineBreak);
            //WriteString('Connection: Close'+ SlineBreak);
            WriteString(SLineBreak);
          end;

          HttpAddRequestHeaders(pRequest, PChar(Header.DataString), Length(Header.DataString), HTTP_ADDREQ_FLAG_ADD);

          if HTTPSendRequest(pRequest, nil, 0, Pointer(AData), Length(AData)) then
          begin
            BufStream := TMemoryStream.Create;
            try
              while InternetReadFile(pRequest, @aBuffer, SizeOf(aBuffer), BytesRead) do
              begin
                if (BytesRead = 0) then Break;
                BufStream.Write(aBuffer, BytesRead);
              end;

              aBuffer[0] := #0;
              BufStream.Write(aBuffer, 1);
              Result := PChar(BufStream.Memory);
            finally
              BufStream.Free;
            end;
          end;
        finally
          Header.Free;
        end;
      finally
        InternetCloseHandle(pRequest);
      end;
    finally
      InternetCloseHandle(pConnection);
    end;
  finally
    InternetCloseHandle(pSession);
  end;
end;

procedure log(appname:string; urlLog:string; logtype: string; duration: integer=0);
var
pParamPost: string;
pDate:tdatetime;
begin
  if urlLog = '' then exit;
  pdate:=now;

  if appname='' then appname:='SPLASH';

  pParamPost:='a=' + appname
    + '&h=' + uppercase(GetEnvVarValue('COMPUTERNAME'))
    + '&d=' + uppercase(GetEnvVarValue('USERDOMAIN'))
    + '&u=' + uppercase(GetEnvVarValue('USERNAME'))
    + '&i=' + GetIPAddress
    + '&t=' + FormatDateTime('yyyyMMddHHmmss', pdate) + '000'
    + '&s=' + logtype
    + '&l=' + inttostr(duration);


  requestHttp(urlLog, pParamPost, false);

end;

// these function to enable form transparent
{Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal Color As Long, ByVal X As Byte, ByVal alpha As Long) As Boolean }
function SetLayeredWindowAttributes(hWnd: longint; Color: longint;
  X: byte; alpha: longint): bool stdcall; external 'USER32';

{not sure how to alias these functions here ????   alias setwindowlonga!!}
{Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long }
function SetWindowLongA(hWnd: longint; nIndex: longint;
  dwNewLong: longint): longint stdcall; external 'USER32';

{Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long }
function GetWindowLongA(hWnd: longint; nIndex: longint): longint stdcall;
  external 'user32';

procedure SetTranslucent(ThehWnd: longint; Color: longint; nTrans: integer);
var
  Attrib: longint;
begin
  {SetWindowLong and SetLayeredWindowAttributes are API functions, see MSDN for details }
  Attrib := GetWindowLongA(ThehWnd, GWL_EXSTYLE);
  SetWindowLongA(ThehWnd, GWL_EXSTYLE, attrib or WS_EX_LAYERED);
  {anything with color value color will completely disappear if flag = 1 or flag = 3  }
  SetLayeredWindowAttributes(ThehWnd, Color, nTrans, 1);
end;


end.

