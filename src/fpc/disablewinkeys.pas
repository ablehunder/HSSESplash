(************************************************************
this unit migrated from dll project(by Kambiz@delphiarea.com)

downloaded at:
http://forum.delphiarea.com/viewtopic.php?f=5&t=1744#p6504

Originally created by:
Andrea Belli@delphiarea.com
March 5th, 2010, 12:51 am
enhanced: CTRL+ESC,ALT+TAB,ALT+ESC

modified by;
rehmoe@yahoo.com
11:23 29/11/2010 more key added,
enhanced: CTRL+ESC,
ALT+TAB,
ALT+ESC,
CTRL+ENTER,
WINKEY (Left / Right),
MENKEY(Left/Right)
**************************************************************)

unit DisableWinKeys;

interface

uses Windows;

type
PKBDLLHOOKSTRUCT = ^TKBDLLHOOKSTRUCT;
TKBDLLHOOKSTRUCT = packed record
vkCode: DWORD;
scanCode: DWORD;
flags: DWORD;
time: DWORD;
dwExtraInfo: DWORD;
end;

function DisableWindowsUI: Boolean;
function EnableWindowsUI: Boolean;
function IsWindowsUIDisabled: Boolean;

const
WH_KEYBOARD_LL = 13;

const
LLKHF_ALTDOWN = $0020;

var
hKeyboardHook: HHOOK = 0;

implementation

function LowLevelKeyboardHook(nCode: Integer; wParam: WPARAM; lParam: LPARAM): HRESULT; stdcall;
var
pkbhs: PKBDLLHOOKSTRUCT;
begin
pkbhs := PKBDLLHOOKSTRUCT(lParam);
if nCode = HC_ACTION then
begin
// Disable CTRL+ESC
if (pkbhs^.vkCode = VK_ESCAPE) and WordBool(GetAsyncKeyState(VK_CONTROL) and $8000) then
begin
Result := 1;
Exit;
end;
// Disable ALT+TAB
if (pkbhs^.vkCode = VK_TAB) and LongBool(pkbhs^.flags and LLKHF_ALTDOWN) then
begin
Result := 1;
Exit;
end;
// Disable ALT+ESC
if (pkbhs^.vkCode = VK_ESCAPE) and LongBool(pkbhs^.flags and LLKHF_ALTDOWN) then
begin
Result := 1;
Exit;
end;
//=============START:modified=====================================================
// Disable CTRL+ENTER
if (pkbhs^.vkCode = VK_RETURN) and LongBool(pkbhs^.flags and LLKHF_ALTDOWN) then
begin
Result := 1;
Exit;
end;
// Disable winkey(Left / Right)
if (pkbhs^.vkCode = VK_LWIN) or (pkbhs^.vkCode = VK_RWIN) then
begin
Result := 1;
Exit;
end;
// Disable menukey(Left/Right)
if (pkbhs^.vkCode = VK_MENU) or (pkbhs^.vkCode = VK_LMENU) or (pkbhs^.vkCode = VK_RMENU) then
begin
Result := 1;
Exit;
end;
//==================END:Modified================================================================
end;
Result := CallNextHookEx(hKeyboardHook, nCode, wParam, lParam);
end;

function DisableWindowsUI: Boolean;
begin
if hKeyboardHook = 0 then
hKeyboardHook := SetWindowsHookEx(WH_KEYBOARD_LL, @LowLevelKeyboardHook, HInstance, 0);
Result := (hKeyboardHook <> 0)
end;

function EnableWindowsUI: Boolean;
begin
Result := False;
if (hKeyboardHook <> 0) and UnhookWindowsHookEx(hKeyboardHook) then
begin
hKeyboardHook := 0;
Result := True;
end;
end;

function IsWindowsUIDisabled: Boolean;
begin
Result := (hKeyboardHook <> 0)
end;

end.

