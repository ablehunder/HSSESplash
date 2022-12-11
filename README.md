# ![l](res/HSSESplash.ico) HSSE Splash - Ergonomic / Office Exercise Splash Screen

A simple *splash screen* application which showing content in specific duration and appear again in some interval.

## Download 

You may find and download binary files from [release page](https://github.com/ablehunder/HSSESplash/releases).

## Table of contents
<!-- vscode-markdown-toc -->
 1. [About](#About)
 2. [Features](#Features)
 3. [Strength & Weakness](#StrengthWeakness)
 4. [Demo](#Demo)
 5. [Installation](#Installation)
 6. [Configuration File](#ConfigurationFile)
 7. [Command](#Command)
 8. [User Log Activity and Logging Server](#UserLogActivityandLoggingServer)
 9. [BuildSource](#BuildSource)
 10. [Known bugs](#Knownbugs)
 11. [License & Disclaimer](#LicenseDisclaimer)

<!-- vscode-markdown-toc-config
	numbering=true
	autoSave=true
	/vscode-markdown-toc-config -->
<!-- /vscode-markdown-toc -->

##  1. <a name='About'></a>About
First version (formerly named *ergoworks*) was developed in 2013, intended to interrupt user activity and remind to exercise.
Content can be image or video about office ergonomic exercise or other HSSE campaign.

##  2. <a name='Features'></a>Features
This application has some features and flexibilities that can be customized in application argument or mostly can be saved in configuration file.
- **Dynamic Content** not only static image, but also video or web page, which can be provided from local computer or server (e.g. YouTube).<br>
This will ensure you can change your content at anytime to prevent user getting bored by same content over and over, and then ignore the meaning of this reminder.
- **Flexible Size** of splash screen in your display (full screen, half, etc.) 
- **Adjustable Duration** of content (regardless length of video displayed)
- **Adjustable Interval** 
- **Notification** prior splash, where the caption or message of notification can be modified
- **Cancellable or Force-Shown**, can be configured that splash can be canceled (e.g. whenever user has to present and won't be interrupted), or forced so user cannot avoid to see it
- **Multi-screen**, appear in all display (if computer connected to more than one display monitor)
- **Interactive**, since the splash using [WebView2](https://developer.microsoft.com/en-us/microsoft-edge/webview2/) component, web content can be provided to (such as survey or campaign)
- **AutoRun** when computer start
- **Log User Activity** of viewing the splash into computer log event or send it to server (for any kind of purpose; see disclaimer)
- ***Designed for you***, you may change icon, caption, or message of this application

##  3. <a name='StrengthWeakness'></a>Strength & Weakness
This application only provides basic need of remind and interrupt user activities, therefore will force computer user to have a break for a while, instead of full-featured occupational health application such as [WorkRave](https://workrave.org/), [Stretchly](https://github.com/hovancik/stretchly), [RSIBreak](https://apps.kde.org/rsibreak/).<br>Why interrupt? Yes, YOUR HEALTH is TOP PRIORITY.<br>It also intended for companies which needs to reassure their employees follow rules on HSSE policy.<br>
- (+) Configurable, easy to set up & redistributable<br>
    Using Global Policy Object (GPO) + Task Scheduler + WSH/VBS/PS<br>
    Using console / command line and config (.ini) file
- (+) Fast & lightweight<br>
    x86 arch, fully compatible with x64; MS Windows Family (8, 10, 11); small program executable file using WebView component (rely on installed MS Edge browser)<br>
    if you need it for another OS and arch, please let me know :)
- (+) Modifiable & licensable (*)<br>
    Source code (in [Pascal](src/fpc/)) are available. 
    <br>**Update: now also available in [C#](src/dotnet/)**.
- (-) Can be terminated by user via Task Manager<br>
    To prevent termination, program should be run with administrator privilege

It has more features compare to another application with the same aim such as [RestStop](https://github.com/gazugafan/RestStop) or [Awareness](http://iamfutureproof.com/tools/awareness) , but with different approaches.<br>
And it may take you a short time to learn to configure the file :)

##  4. <a name='Demo'></a>Demo

- When no url provided, splash will show help on command usage
- Open image / video on local computer

        HSSESplash.exe -c "HSSE Splash " -m "Simple routine exercise"  -t 20 -de -u "file:///C:\Documents\HSSESplash\samples\StretchAtYourDesk.mp4"

        HSSESplash.exe -c "Your Company Splash" -m "Exercise and relaxation"  -s 100 -t 20 -de -u "file:///C:\Documents\HSSESplash\samples\ergo.id.png"
        
        HSSESplash.exe -c "My Splash" -m "Ergonomic Guide"  -s 75 -t 20 -de -u "file:///C:\Documents\HSSESplash\samples\panduan.png"

- Open youtube videos

        HSSESplash.exe -c "Health Splash" -m "Health Splash will be shown..."  -t 20 -db -dm -dk  -u "https://www.youtube.com/embed/KBaSGF6kYqw?start=2&rel=0&autoplay=1"

    argument explanation:<br>
        - only shown in one screen/display/monitor (using -dm)<br>
        - enable keys function while showing (using -dk)<br>
        - allow interaction with web content (using -db)
- Open content from local HTML

        HSSESplash.exe -c "HSSE Splash" -m "HSSE Splash will be shown..."  -t 20 -db -dm -dk  -u "file:///C:\Documents\HSSESplash\samples\randomredirect.html"
    
    Those commands can be loaded from config file using -f option or saved into using -w option

##  5. <a name='Installation'></a>Installation
- Just copy `HSSESplash.exe` & `WebView2Loader.dll` into any directory. 
- You may also put `HSSESplash.ico` file in directory as icon for splash
- Configure `HSSESplash.ini` or `HSSESplash.xml` file as necessary
- See specific documentation for [.NET deployment](src/dotnet/README.md)

##  6. <a name='ConfigurationFile'></a>Configuration File
This application has **hot configuration reload** feature, which mean, some of these options can be changed while application is active and reloaded without having to restart. 
- **url** (*string*): url of web page to load. Using https://, http://, or file:// for local computer, or any file content either absolute path or relative path to current working directory. (equal to argument `-u`)
- **appName** (*string*): Identifier to system or log. (default = SPLASH, equal to argument `-a`)
- **caption** (*string*): application caption, Caption/text shown in notification or window form. Use your version of caption so user can easily understand the notification. (equal to argument `-c`)
- **duration** (*integer*, default = 10; 0 = forever): duration splash will be shown in second. (equal to argument `-t`)
- **interval** (*decimal*, default = 0): Interval between splash shown, unit in hour (e.g 1.5 means one and half hour or 90 minutes). Interval=0 means it only occur once and then application will exit. If omitted splash will be shown once. (equal to argument `–i`)
- **size** (*integer*, default = 50): Window size, in percentage of screen. Put value 100 to show it in full screen. (equal to argument `-s`)
- **messageNotify** (*string*): Message shown prior splash appear. Leave it blank to have the notification not popping up. (equal to argument `-m`).
- **notificationClick**  (*string(open|skip)*, default=open): What action will occur whenever user click on *messageNotify*. It can be 'open' to open the splash immediately, or 'skip' so user can suspend watching it. Please consider to have *messageNotify* accordingly with this option. (equal to argument `-nc`)
- **maxCancel** (*integer*, default = 0, it means never force splash to be shown): How many times user may skip watching splash before splash is forced to be shown. Put value=0 if you allow user skip it many times. (equal to argument `-mc`)
- **cancelNotify** (*string*): Message shown whenever user choose to skip splash. Use blank/space to disable this notification.
- **noSkipNotify** (*string*): Message shown whenever user has reach maximum *maxCancel* skip counter and forced to watch splash. Use blank/space to disable this notification.
- **disableLogging**  (*true/false*, default = false): Disable log user activity, either to local computer event logger or server. (equal to argument `-dl`)
- **logServer** (*string*): URL server to log user activity. Leave it blank to disable sending log to server regardless 'disableLogging' value. (equal to argument `-l`)
- **disableEscape**  (*true/false*, default = false): Allow user to press 'escape key' to skip watching the splash content. (equal to argument `-de`)
- **disableKeyLock**  (*true/false*, default = false): Disable locking some function of keyboard to OS, such as alt-tab, etc. (equal to argument `-dk`)
icon (equal to argument `–sm`)
- **showCloseButton** (*true/false*, default = false): Show close button in splash window. It is useful if you want to keep user unable to escape from playing the content, or you want to maintain the show with less border window form but give user ability to skip by pressing 'escape key'. Not working when option *disableEscape*=true. (equal to argument `–x`)
- **disableDragMove**  (*true/false*, default = false): User able to move window using mouse over the inside window instead of using form header. It is useful to have window movable by dragging inside part of the form when 'showCloseButton=false'. (equal to argument `-dd`) 
- **disablePageBlock** (*true/false*, default = false): Whenever disabled, user can access content interactively. Keep the value to 'false' so user cannot access content. (equal to argument `-db`) 
- **disableMultiScreen**  (*true/false*, default = false): When you have multiple monitor/display, this application will automatically show splash onto other screen. (equal to argument `–dm`)
- **disableTrayIcon** (*true/false*, default = false): Show context menu whenver user right-click icon in notification area in conjunction to 'disableTrayIcon=false'. You may set it to 'false' so user cannot pause or exit from this application. (equal to argument `–dt`)
- **showTrayMenu** (*true/false*, default = true): Show context menu whenver user right-click icon in notification area in conjunction to 'disableTrayIcon=false'. You may set it to 'false' so user cannot pause or exit from this application.
- **autoRun** (*true/false*, default = false): Add registry to have this application run whenever computer is started.

##  7. <a name='Command'></a>Command

Usage:
`HSSESplash -u url [-t time] [-i interval] [-i size] [-mc maxcnt] [-a appname] [-c caption] [-m msg] [-l urlLog] [-f filename] [-db] [-dl] [-de] [-dk] [-dm] [-dd] [-dt] [-sm] [-mi] [-x] [-w] [-r|-dr]`

    -u url     : url of web page to load. Using https://, http://, or file:/// to load file in local computer
    -t time    : duration splash will show in second (default = 10 seconds; 0 = forever)
    -i interval: interval hours the splash will be shown, if omitted splash will be shown once
    -s size    : display size within screen in percentage  (default = 50)
    -c caption : application/notification caption (default = {pCaption})
    -m msg     : notification message prior splash appears
    -nc mode   : behavior after click on notification message, open or skip (default = open)
    -a appname : log identifier name (default = HSSESplash)
    -l urlLog  : url server to log usage of splash by user
    -dl        : disable logging
    -x         : show close button, not working with -de option
    -db        : disable web page block / enable interactive mode, implies -x
    -dd        : disable drag/move splash, not working in interactive mode
    -de        : disable Esc key
    -dk        : disable system key lock
    -dm        : disable show into multiple screen
    -dt        : disable tray icon
    -sm        : show context menu in tray icon
    -mc maxcnt : max cancel splash count before splash is forced to be shown (default = 0, it means never force splash to be shown)
    -mi        : enable multiple instance running application
    -stop      : stop existing single instance running
    -f filename: xml or ini configuration file (default = HSSESplash.xml or HSSESplash.ini). Any changes to this file will take effect immediately to current running application
    -w         : write to file config as defined in -f option, then exit
    -r, -dr    : add/remove config to automatically run application when computer start
    all parameters in bracket are optional

##  8. <a name='UserLogActivityandLoggingServer'></a>User Log Activity and Logging Server
User activity (either cancel or view splash) can be acquired from Event Log Viewer of Windows.<br>
This application also has ability to send some data to server, which may useful to analyze user behavior (may your Health Department need to have wide information computer usages of their employees).<br>
It send a server request to log activity in **HTTP GET** params:

        a = appname (default=SPLASH)
        h = COMPUTERNAME (from windows computer name in environment value )
        d = USERDOMAIN
        u = USERNAME
        i = GetIPAddress
        t = DateTime String (in format 'yyyyMMddHHmmss000’)
        s = logtype (ON/END/LOCK)
        l = duration (in second)

Edit `logServer` configuration to any http server. You may develop simple web app that can save the data into a database.<br>
Or just activate its `access.log`, there are a bunch of analyzers that can dig out data from it.

## 9. <a name="BuildSource"></a>Build Source
* this application rely on [WebView4Delphi](https://github.com/salvadordf/WebView4Delphi), therefore you need to add webview4delphi.lpk package to your lib and copy `WebView2Loader.dll` to the path of your file executable binary 
* need to change some code for tray icon ini LCL component. see directory `lcl-hack`.
* for development using C#, see the [documentation](src/dotnet/README.md)

##  10. <a name='Knownbugs'></a>Known bugs
Need to handle balloon tooltips and cancel counter.

##  11. <a name='LicenseDisclaimer'></a>License & Disclaimer
I'd like to include some license such as LGPL, etc. Let me read https://choosealicense.com/licenses/ later.<br>
But just to let you know that this application is
- Provided as is 
- Held no responsibility on any log collected or any privacy and confidentiality breach

## Credits
- <a href="https://www.flaticon.com/free-icons/splash" title="Splash icons">Splash icons created by Freepik - Flaticon</a>