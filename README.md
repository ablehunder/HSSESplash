# ![l](src/fpc/HSSESplash.ico) HSSE Splash - Ergonomic / Office Exercise Splash Screen

A simple *splash screen* application which showing content in specific duration and appear again in some interval

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
- First version (formerly named *ergoworks*) was developed in 2013, intended to interrupt user activity and remind to exercise 
- Content can be image or video about office ergonomic exercise or other HSSE campaign

##  2. <a name='Features'></a>Features
This application has some features and flexibilities that can be customized in application argument or mostly can be saved in configuration file.
- **Dynamic Content** not only static image, but also video or web page, which can be provided from local computer or server (e.g. YouTube).<br>
This will ensure you can change your content at anytime to prevent user getting bored by same content and ignore the meaning of this reminder.
- **Flexible Size** of splash screen in your display (full screen, half, etc.) 
- **Adjustable Duration** of content (regardless length of video displayed)
- **Adjustable Interval** 
- **Notification** prior splash, where the caption or message of notification can be modified
- **Cancellable or Force-Shown**, can be configured that splash can be canceled (e.g. whenever user has to present and won't be interrupted), or forced so user cannot avoid to see it
- **Multi-screen**, appear in all display (if computer connected to more than one display monitor)
- **Interactive**, since the splash using webview component, web content can be provided to (such as survey or campaign)
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
    Source code (in Pascal) are available
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
- Configure `HSSESplash.ini` file as necessary

##  6. <a name='ConfigurationFile'></a>Configuration File
This application has **hot configuration reload** feature, which mean, some of these options can be changed while application is active and reloaded without having to restart. 
- **url** (*string*): url of web page to load. Using https://, http://, or file:// for local computerAppName (string)
- **duration** (*decimal*): duration splash will show in second (default = 10 seconds; 0 = forever). (equal to argument -t)
- **interval** (*integer*): interval hours the splash will be shown, if omitted splash will be shown once. (equal to argument –i)
- **size** (*integer*): display size within screen in percentage. (default = 50, equal to argument -s)
- **messageNotify** (*string*): notification message prior splash appears. (equal to argument -m)
- **logServer** (*string*): url server to log usage of splash by user. (equal to argument -l)
- **appName** (*string*): log identifier name. (equal to argument -a) 
- **caption** (*string*): application caption
- **disablePageBlock** (*true/false*): disable web page block / enable interactive mode. (equal to argument -db) 
- **disableLogging**  (*true/false*): disable logging. (equal to argument -dl)
- **disableEscape**  (*true/false*): disable Esc key . (equal to argument -de)
- **disableKeyLock**  (*true/false*): disable system key lock. (equal to argument -dk)
- **disableMultiScreen**  (*true/false*): disable show into multiple screen. (equal to argument –dm)
- **disableTrayIcon** (*true/false*): disable tray icon (equal to argument –dt)
- **maxCancel** (*integer*): max cancel splash count before splash is forced to be shown (default = 0, it means never force splash to be shown)
- **showCloseButton** (*true/false*): show close button, not working when option disableEscape=true. (equal to argument –x)

##  7. <a name='Command'></a>Command
Usage: `HSSESplash.Exe -u url [-t time] [-i interval] [-i size] [-mc maxcnt] [-a appname] [-c caption] [-m msg] [-l urlLog] [-f filename] [-db] [-dl] [-de] [-dk] [-dm] [-dd] [-dt] [-mi] [-x] [-w] [-r|-dr]`

        -u url     : url of web page to load. Using https://, http://, or file:/// to load file in local computer
        -t time    : duration splash will show in second (default = 10 seconds; 0 = forever)
        -i interval: interval hours the splash will be shown, if omitted splash will be shown once
        -s size    : display size within screen in percentage (default = 50)
        -c caption : application/notification caption (default = Splash)
        -m msg     : notification message prior splash appears
        -a appname : log identifier name (default = SPLASH)
        -l urlLog  : url server to log usage of splash by user
        -dl        : disable logging
        -x         : show close button, not working with -de option
        -db        : disable web page block / enable interactive mode, implies -x
        -dd        : disable drag/move splash, not working in interactive mode
        -de        : disable Esc key
        -dk        : disable system key lock
        -dm        : disable show into multiple screen
        -dt        : disable tray icon
        -mc maxcnt : max cancel splash count before splash is forced to be shown (default = 0, it means never force splash to be shown)
        -mi        : enable multiple instance running application
        -stop      : stop existing single instance running
        -f filename: .ini configuration file (default = HSSESPlash.ini). Any changes to this file will take effect immediately to current running application
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

##  10. <a name='Knownbugs'></a>Known bugs
Need to handle balloon tooltips and cancel counter.

##  11. <a name='LicenseDisclaimer'></a>License & Disclaimer
I'd like to include some license such as LGPL, etc. Let me read https://choosealicense.com/licenses/ later.<br>
But just to let you know that this application is
- Provided as is 
- Held no responsibility on any log collected or any privacy and confidentiality breach
