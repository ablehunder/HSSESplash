using System;
using System.ComponentModel;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.IO;
using System.Reflection;
using System.Resources;
using System.Text;
using System.Windows.Forms;
using System.Xml.Serialization;
using Microsoft.Win32;
using System.Diagnostics;

namespace HSSESplash
{
    /// <summary>
    /// Main Form that will be executed at first time
    /// this form will be shown with little box at top left corner to handle user input, and hide when this application idle
    /// when the play timer executed, this form open splash form that will show main content.
    /// </summary>
    public partial class FrmMain : Form
    {
        private string pUrl;
        private string pUrlLog;
        private string pAppName = "SPLASH";
        private string pMessageNotify = "Splash will be shown.\nClick here to show immediately or press Esc to cancel.";
        private string pCancelNotify = "Skip splash.\nAlready skip {0} time(s), maximum {1} allowed.";
        private string pNoSkipNotify = "Force show.\nReach maximum {0} time(s) skip.";
        private string pNotificationClick = "open"; // mode:open/skip
        private int pDuration = 10;
        protected internal int pSize = 50;
        private double pInterval = 0;
        private bool pDisableLogging = false;
        private bool pDisableKeyLock = false;
        protected internal bool pDisableEscape = false;
        private bool pDisableTrayIcon = false;
        private bool pShowTrayMenu = true;
        private int pMaxCancel = 0;
        private int pCancelCounter = 0;
        private bool pDisableMultiScreen = false;
        private string pCaption = "Splash";
        private string pUsage = "";
        private string pIniFile;
        private DateTime pIniFileTimestamp;
        private int pIniCounter;
        private bool pIsInitialized = false;
        private bool pIsScreenLocked = false;
        private int pCounter;
        private List<FrmSplash> pCloned;
        protected internal bool pDisablePageBlock;
        protected internal bool pShowCloseButton;
        protected internal bool pDisableDragMove;
        static ComponentResourceManager resources =
                new ComponentResourceManager(typeof(FrmMain));

        public FrmMain()
        {
            InitializeComponent();
        }

        /// <summary>
        ///  Help usage 
        /// </summary>
        /// <returns>string usage of this application</returns>
        protected internal string usage()
        {
            if (pUsage != "") return pUsage;

            StringBuilder buff = new StringBuilder("");

            buff.Append("Usage: \n");
            string file = GetType().Assembly.Location;
            file = Assembly.GetExecutingAssembly().Location;
            string app = Path.GetFileNameWithoutExtension(file);
            //buff.Append(Path.GetFileName(file));
            buff.Append(Application.ProductName);
            buff.Append(" -u url [-t time] [-i interval] [-i size] [-mc maxcnt] ");
            buff.Append("[-a appname] [-c caption] [-m msg] [-l urlLog] [-f filename] ");
            buff.Append("[-db] [-dl] [-de] [-dk] [-dm] [-dd] [-dt] [-mi] [-x] [-w] [-r|-dr]\n");
            buff.Append(" -u url     : url of web page to load.");
            buff.Append(" Using https://, http://, or file:/// to load file in local computer\n");
            buff.Append(" -t time    : duration splash will show in second (default = ");
            buff.Append($"{pDuration} seconds; 0 = forever)\n");
            buff.Append(" -i interval: interval hours the splash will be shown, ");
            buff.Append("if omitted splash will be shown once\n");
            buff.Append(" -s size    : display size within screen in percentage ");
            buff.Append($" (default = {pSize})\n");
            buff.Append(" -c caption : application/notification caption (default = {pCaption})\n");
            buff.Append(" -m msg     : notification message prior splash appears\n");
            buff.Append(" -nc mode   : behavior after click on notification message, open or skip (default = open)\n");
            buff.AppendFormat(" -a appname : log identifier name (default = {0})\n", pAppName);
            buff.Append(" -l urlLog  : url server to log usage of splash by user\n");
            buff.Append(" -dl        : disable logging\n");
            buff.Append(" -x         : show close button, not working with -de option\n");
            buff.Append(" -db        : disable web page block / enable interactive mode, implies -x\n");
            buff.Append(" -dd        : disable drag/move splash, not working in interactive mode\n");
            buff.Append(" -de        : disable Esc key\n");
            buff.Append(" -dk        : disable system key lock\n");
            buff.Append(" -dm        : disable show into multiple screen\n");
            buff.Append(" -dt        : disable tray icon\n");
            buff.Append(" -mc maxcnt : max cancel splash count before splash is forced to be shown (default = 0, it means never force splash to be shown)\n");
            buff.Append(" -mi        : enable multiple instance running application\n");
            buff.Append(" -stop      : stop existing single instance running\n");
            buff.Append($" -f filename: xml configuration file (default = {app}.xml). Any changes to this file will take effect immediately to current running application\n");
            buff.Append(" -w         : write to file config as defined in -f option, then exit\n");
            buff.Append(" -r, -dr    : add/remove config to automatically run application when computer start\n");
            buff.Append("all parameters in bracket are optional\n");

            return buff.ToString();
        }

        /// <summary>
        /// get file modification time from the last time check 
        /// </summary>
        /// <param name="aFileName">file name to be checked</param>
        /// <param name="aFileTime">returned date time</param>
        /// <returns>true on success, false on any file access error</returns>
        private bool GetFileModTime(string aFileName, ref DateTime aFileTime)
        {
            try
            {
                aFileTime = File.GetLastWriteTime(aFileName);
                return true;
            }
            catch (Exception exc)
            {
                Console.WriteLine(exc.Message);
                return false;
            }
        }

        /// <summary>
        /// Initialize application using default config or from command line or from XML config file.
        /// XML Config file could be defined from command argument (-f), or if omitted will be default to XML at the same directory (if any), otherwise it will use default value.
        /// </summary>
        /// <param name="readIni">shall we read file config?</param>
        /// <param name="paramList">list of parameter, usually Environment.GetCommandLineArgs</param>
        private void initialize(bool readIni = true, List<string> paramList = null)
        {

            bool pWriteIni = false;
            bool pMultipleInstance = false;
            bool pAutoRun = false;
            XMLConfig cfg = null;

            if (paramList == null) paramList = new List<string>();

            string asmfile = Assembly.GetExecutingAssembly().Location;
            string app = Path.GetFileNameWithoutExtension(asmfile);

            // first, check if we sould get config from ini
            string buff = SplashUtils.getParam(paramList, "-f");
            if (!String.IsNullOrWhiteSpace(buff)) pIniFile = buff;
            else
            {
                if (!pIsInitialized)
                    pIniFile = String.Concat(Application.LocalUserAppDataPath,
                        Path.DirectorySeparatorChar, app, 
                        Path.DirectorySeparatorChar, app, ".xml");
            }
            if (!File.Exists(pIniFile))
                pIniFile = String.Concat(app, ".xml");
            if (!File.Exists(pIniFile))
                pIniFile = String.Concat(Path.GetDirectoryName(Application.ExecutablePath), Path.DirectorySeparatorChar, app, ".xml");
            pWriteIni = SplashUtils.getParamBool(paramList, "-w", false);

            // check for multiple instance
            pMultipleInstance = SplashUtils.getParamBool(paramList, "-mi", false);
            if ((!pMultipleInstance))// and (pSingleInstance==null))
            { // single instance only for now
            }

            // here we go, set all variable
            if (!pIsInitialized) pAppName = pCaption = Application.ProductName;

            XmlRootAttribute xRoot = new XmlRootAttribute();
            xRoot.ElementName = this.GetType().Namespace;
            //xRoot.Namespace = "http://www.cpandl.com";
            xRoot.IsNullable = true;
            XmlSerializer serializer = new XmlSerializer(typeof(XMLConfig), xRoot);

            serializer.UnknownNode += new XmlNodeEventHandler((sender, ea) => { Console.WriteLine($"Unknown Node: {ea.Name}"); });
            serializer.UnknownAttribute += new XmlAttributeEventHandler((sender, ea) => { Console.WriteLine($"Unknown Attribute: {ea.Attr}"); });

            if (!File.Exists(pIniFile))
            {
                cfg = new XMLConfig();
                cfg.appName = pAppName;
                cfg.autoRun = pAutoRun;
                cfg.caption = pCaption;
                cfg.disableDragMove = pDisableDragMove;
                cfg.disableEscape = pDisableEscape;
                cfg.disableKeyLock = pDisableKeyLock;
                cfg.disableLogging = pDisableLogging;
                cfg.disableMultiScreen = pDisableMultiScreen;
                cfg.disablePageBlock = pDisablePageBlock;
                cfg.disableTrayIcon = pDisableTrayIcon;
                cfg.showTrayMenu = pShowTrayMenu;
                cfg.duration = pDuration;
                cfg.interval = pInterval;
                cfg.logServer = pUrlLog;
                cfg.maxCancel = pMaxCancel;
                cfg.messageNotify = pMessageNotify;
                cfg.cancelNotify = pCancelNotify;
                cfg.noSkipNotify = pNoSkipNotify;
                cfg.notificationClick = pNotificationClick;
                cfg.showCloseButton = pShowCloseButton;
                cfg.size = pSize;
                cfg.url = pUrl;
            }
            else
            {
                FileStream fs = new FileStream(pIniFile, FileMode.Open);
                cfg = (XMLConfig)serializer.Deserialize(fs);
                fs.Close();
                Type t = cfg.GetType();
                foreach (FieldInfo m in t.GetFields()) // trim whitespace
                    if (typeof(System.String).Equals(m.FieldType)
                            && m.GetValue(cfg) != null)
                        m.SetValue(cfg, ((string)m.GetValue(cfg)).Trim());

            }

            Console.WriteLine(this.usage());
            Console.WriteLine(cfg);

            if (!pIsInitialized) pUrl = "";
            if (readIni) pUrl = cfg.url ?? pUrl;
            buff = SplashUtils.getParam(paramList, "-u");
            //assume the first parameter is url
            if (String.IsNullOrWhiteSpace(buff))
            {
                if (String.IsNullOrWhiteSpace(pUrl) && paramList.Capacity > 1) 
                    pUrl = paramList[1];
                if (pUrl != null && pUrl.StartsWith("-")) pUrl = "";
            }
            else
                pUrl = buff;
            if (pWriteIni) cfg.url = pUrl;

            if (!pIsInitialized) pUrlLog = "";
            if (readIni) pUrlLog = cfg.logServer ?? pUrlLog;
            buff = SplashUtils.getParam(paramList, "-l");
            if (!String.IsNullOrWhiteSpace(buff)) pUrlLog = buff;
            if (pWriteIni) cfg.logServer = pUrlLog;

            if (readIni) pAppName = cfg.appName ?? pAppName;
            buff = SplashUtils.getParam(paramList, "-a");
            if (!String.IsNullOrWhiteSpace(buff)) pAppName = buff;
            if (pWriteIni) cfg.appName = pAppName;

            if (readIni) pDisableKeyLock = cfg.disableKeyLock;
            if (SplashUtils.getParamBool(paramList, "-dk", false))
                pDisableKeyLock = true;
            if (pWriteIni) cfg.disableKeyLock = pDisableKeyLock;

            if (readIni) pDisableLogging = cfg.disableLogging;
            if (SplashUtils.getParamBool(paramList, "-dl", false))
                pDisableLogging = true;
            if (pWriteIni) cfg.disableLogging = pDisableLogging;

            if (!pIsInitialized) this.pShowCloseButton = false;
            if (readIni) this.pShowCloseButton = cfg.showCloseButton;
            if (SplashUtils.getParamBool(paramList, "-x", false)) 
                pShowCloseButton = true;
            if (pWriteIni) cfg.showCloseButton = this.pShowCloseButton;

            if (!pIsInitialized) this.pDisablePageBlock = false;
            if (readIni) this.pDisablePageBlock = cfg.disablePageBlock;
            if (SplashUtils.getParamBool(paramList, "-db", false))
                pDisablePageBlock = true;
            if (pWriteIni) cfg.disablePageBlock = this.pDisablePageBlock;
            if (this.pDisablePageBlock) this.pShowCloseButton = true;

            if (this.pDisablePageBlock) this.pShowCloseButton = true;
            if (readIni) pDisableMultiScreen = cfg.disableMultiScreen;
            if (SplashUtils.getParamBool(paramList, "-dm", false))
                pDisableMultiScreen = true;
            if (pWriteIni) cfg.disableMultiScreen = pDisableMultiScreen;

            if (readIni) pDuration = cfg.duration;
            pDuration = SplashUtils.getParamInt(paramList, "-t", pDuration);
            if (pWriteIni) cfg.duration = pDuration;

            if (readIni) pDisableEscape = cfg.disableEscape;
            if (SplashUtils.getParamBool(paramList, "-de", false) && (pDuration > 0)) 
                pDisableEscape = true;
            if (pWriteIni) cfg.disableEscape = pDisableEscape;

            if (readIni) pDisableTrayIcon = cfg.disableTrayIcon;
            if (SplashUtils.getParamBool(paramList, "-dt", false))
                pDisableTrayIcon = true;
            if (pWriteIni) cfg.disableTrayIcon = pDisableTrayIcon;

            if (readIni) pShowTrayMenu = cfg.showTrayMenu;
            if (SplashUtils.getParamBool(paramList, "-sm", false))
                pShowTrayMenu = true;
            if (pWriteIni) cfg.showTrayMenu = pShowTrayMenu;

            if (readIni) pMaxCancel = cfg.maxCancel;
            pMaxCancel = SplashUtils.getParamInt(paramList, "-mc", pMaxCancel);
            if (pMaxCancel <= 0) pMaxCancel = 0;
            if (pWriteIni) cfg.maxCancel = pMaxCancel;

            if (readIni) pSize = cfg.size;
            pSize = SplashUtils.getParamInt(paramList, "-s", pSize);
            if (pSize <= 0) pSize = 80;
            if (pWriteIni) cfg.size = pSize;

            if (!pIsInitialized) this.pDisableDragMove = false;
            if (readIni) pDisableDragMove = cfg.disableDragMove;
            if (SplashUtils.getParamBool(paramList, "-dd", false))
                pDisableDragMove = true;
            if (pWriteIni) cfg.disableDragMove = pDisableDragMove;
            // only able to move not in fullscreen
            pDisableDragMove = (pSize >= 100) || pDisableDragMove;

            if (readIni) pInterval = cfg.interval;
            pInterval = SplashUtils.getParamDouble(paramList, "-i", pInterval);
            if (pInterval < 0) pInterval = 0;
            if (pWriteIni) cfg.interval = pInterval;

            if (readIni) pCaption = cfg.caption ?? pCaption;
            buff = SplashUtils.getParam(paramList, "-c");
            if (!String.IsNullOrWhiteSpace(buff)) pCaption = buff;
            if (pWriteIni) cfg.caption = pCaption;

            if (readIni) pMessageNotify = cfg.messageNotify ?? pMessageNotify;
            buff = SplashUtils.getParam(paramList, "-m");
            if (!String.IsNullOrWhiteSpace(buff)) pMessageNotify = buff;
            if (pWriteIni) cfg.messageNotify = pMessageNotify;

            if (readIni) pNotificationClick = cfg.notificationClick ?? pNotificationClick;
            buff = SplashUtils.getParam(paramList, "-nc");
            if (!String.IsNullOrWhiteSpace(buff)) pNotificationClick = buff;
            if (pWriteIni) cfg.notificationClick = pNotificationClick;

            if (readIni) pCancelNotify = cfg.cancelNotify ?? pCancelNotify;
            if (pWriteIni) cfg.cancelNotify = pCancelNotify;
            if (readIni) pNoSkipNotify = cfg.noSkipNotify ?? pNoSkipNotify;
            if (pWriteIni) cfg.noSkipNotify = pNoSkipNotify;

            if (readIni) pAutoRun = cfg.autoRun;
            pAutoRun |= SplashUtils.getParamBool(paramList, "-r", false);
            if (SplashUtils.getParamBool(paramList, "-dr", false)) pAutoRun = false;
            if (pWriteIni) cfg.autoRun = pAutoRun;

            // write config file as necessary
            if (pWriteIni)
            {
                try
                {
                    FileStream fs = new FileStream(pIniFile, FileMode.Open);
                    serializer.Serialize(fs, cfg);
                    fs.Close();
                }
                finally { }
            }

            if (!String.IsNullOrWhiteSpace(pIniFile))
            {
                GetFileModTime(pIniFile, ref pIniFileTimestamp);
                tmrInstanceServer.Enabled = true;
            }

            Microsoft.Win32.RegistryKey pRegistry = null;
            try
            {
                // Set for current user only
                // Ensure registry can be written to
                buff = @"SOFTWARE\Microsoft\Windows\CurrentVersion\Run\";
                pRegistry = Registry.CurrentUser.OpenSubKey(buff, true);
                if (pRegistry != null)
                {
                    if (pAutoRun)
                        pRegistry.SetValue(Application.ProductName,
                            String.Concat(Application.StartupPath, app, ".exe"));
                    else
                        pRegistry.DeleteValue(Application.ProductName);
                }
            }
            catch (System.ArgumentException exc)
            {
                Console.WriteLine(exc);
            }
            finally
            {
                if (pRegistry != null)
                    pRegistry.Close();
            }

            FrmMain thisform = this;
            pauseIcon = (Icon) resourceManager.GetObject("PAUSE-32x32");
            // draw tray icon menus
            if (pShowTrayMenu)
                trayNotify.ContextMenuStrip = new ContextMenuStrip()
                {
                    Items = {
                    new ToolStripLabel(pAppName),
                    new ToolStripSeparator(),
                    new ToolStripMenuItem("Pause", null, (s,e)=>{
                        lastIcon = tmrPlay.Enabled?trayNotify.Icon:lastIcon;
                        tmrPlay.Enabled = !tmrPlay.Enabled;
                        trayNotify.Icon = tmrPlay.Enabled?lastIcon:pauseIcon;
                        ((ToolStripMenuItem) s).Text =
                            tmrPlay.Enabled?"Pause":"Resume";
                    }, "PAUSE"),
                    new ToolStripMenuItem("Exit", null, (s,e)=>{
                        goodBye();
                    }, "EXIT")
                    }
                };

            if (SplashUtils.getParamBool(paramList, "-stop", false))
                goodBye(); // just stop.

            pIsInitialized = true;
            Console.WriteLine("Initialized");
        }

        private Icon lastIcon = null;
        private Icon pauseIcon = null;

        /// <summary>
        /// log activities, either to event logger or server
        /// </summary>
        /// <param name="logtype">any status/type of activity to record</param>
        private void logMe(string logtype)
        {
            if (pDisableLogging) return;

            SplashUtils.log(pAppName, pUrlLog, logtype, pCounter);

            if (!"ON".Equals(logtype))
            {
                StringBuilder msg = new StringBuilder();
                msg.Append($"{pAppName} : {Environment.UserDomainName}\\{Environment.UserName} - {(logtype)}; Total session: ");
                if (pCounter >= 60 * 60) msg.Append($"{(pCounter/(60*60))} hour(s) ");
                if (pCounter >= 60) msg.Append($"{(pCounter / 60)} minute(s) ");
                msg.Append($"{(pCounter % 60)} second(s)");

                Console.WriteLine(msg);
                try
                {
                    if (EventLog.SourceExists(pAppName))
                        EventLog.WriteEntry(pAppName, msg.ToString(),
                        EventLogEntryType.Information, 1001);
                }
                catch (Exception exc)
                {
                    // pass
                    Console.WriteLine(exc.Message);
                }

            }
        }

        /// <summary>
        /// basically just a other component initialization outside designer, that we can call it over and over
        /// it also load configuration from XML file (formerly we used .INI file, but since C#/.NET recommend using XML)
        /// </summary>
        private void initComponent()
        {
            // turn it off first
            tmrNotify.Enabled = false;
            tmrPlay.Enabled = false;

            // start log
            try
            {
                string buff = $@"SYSTEM\CurrentControlSet\Services\EventLog\Application\{pAppName}";
                Registry.LocalMachine.CreateSubKey(buff, true);
                if (!EventLog.SourceExists(pAppName))
                {
                    //An event log source should not be created and immediately used.
                    //There is a latency time to enable the source, it should be created
                    //prior to executing the application that uses the source.
                    //Execute this sample a second time to use the new source.
                    Console.WriteLine("Create EventSource");
                    EventLog.CreateEventSource(pAppName, "Application");
                    Console.WriteLine("Exiting, execute the application a second time to use the source.");
                    // The source is created.  Exit the application to allow it to be registered.
                }
            }
            catch (Exception exc)
            {
                Console.WriteLine(exc.Message);
                // pass
            }

            this.Text = pCaption;
            editFocus.Text = "";
            editFocus.BorderStyle = BorderStyle.None;
            this.FormBorderStyle = FormBorderStyle.None;

            this.DesktopLocation = new Point(0, 0);
            this.MinimumSize = this.ClientSize = imgLogo.Size;

            try
            {
                string file = GetType().Assembly.Location;
                string app = Path.GetFileNameWithoutExtension(file);
                string filename = String.Concat(Application.StartupPath,
                    Path.DirectorySeparatorChar, app, ".ico");
                this.Icon = Icon.ExtractAssociatedIcon(filename);
            }
            finally { }

            imgLogo.Image = Bitmap.FromHicon(this.Icon.Handle);

            trayNotify.Icon = this.Icon;

            trayNotify.BalloonTipTitle = pCaption;
            trayNotify.BalloonTipText = pMessageNotify;
            trayNotify.BalloonTipIcon = ToolTipIcon.Info;
            trayNotify.Visible = !pDisableTrayIcon;

            tmrSplash.Interval = pDuration * 1000;
            tmrNotify.Enabled = true;

            if (pInterval > 0)
            {
                tmrPlay.Interval = (int)(pInterval * 1000 * 60 * 60);
                tmrPlay.Enabled = true;
            }

            logMe("ON");
            this.WindowState = FormWindowState.Normal;
            this.BringToFront();

            trayNotify.BalloonTipIcon = ToolTipIcon.Info;
            trayNotify.BalloonTipText = pMessageNotify;
            if (!String.IsNullOrWhiteSpace(pMessageNotify))
                trayNotify.ShowBalloonTip(tmrNotify.Interval);

            SetKeyFocus();
        }

        public void SetKeyFocus()
        {
            Console.WriteLine("SetKeyFocus");
            if (!tmrSplash.Enabled) return;
            if (pDisablePageBlock) return;
            try { if (this.Visible) this.Activate(); this.Focus(); } finally { }
            try { if (this.Visible) editFocus.Focus(); } finally { }
        }

        /// <summary>
        /// destroy any FrmSplash clone after showing
        /// </summary>
        private void destroyClones()
        {
            int i;
            FrmSplash frm;

            if (pCloned != null) for (i = pCloned.Capacity - 1; i >= 0; i--)
                {
                    try
                    {
                        frm = pCloned[i];
                        /**
                        ** do destroy splash form resource to clean it up from memory
                        **/
                        if (frm != null)
                        {
                            frm.formCloseRequested = true;
                            frm.Hide();
                            frm.Close();
                            frm.Dispose();
                            pCloned[i] = null;
                        }
                    }
                    catch (Exception) { }
                    finally { }
                }
        }

        /// <summary>
        /// no sorry, no worry..
        /// </summary>
        private void goodBye()
        {
            try
            {
                this.trayNotify.Dispose();
            }
            catch (Exception exc)
            {
                Console.WriteLine(exc.Message);
            }
            Console.WriteLine("Goodbye..");
            this.Close();
            this.Dispose();
            Application.Exit();
        }

        /// <summary>
        /// Clone FrmSplash as many as screens attached to this computer.
        /// </summary>
        private void showSplash()
        {

            Console.WriteLine("showSplash");
            FrmSplash frmcl;

            this.Hide();
            this.Show();

            /**
            * TODO do something with keylock
            **/
            /*
            if (!pDisableKeyLock)
                DisableWinKeys.DisableWindowsUI;
            */

            if (this.pDuration >= 0)
            {
                tmrSplash.Interval = pDuration * 1000;
                tmrSplash.Enabled = true;
                tmrCounter.Enabled = true;
                pCounter = 0;
            }

            SetKeyFocus();

            /**
            ** do something on multiple monitor, unless showing on multiscreen is disabled
            **/
            if (pDisableMultiScreen)
                pCloned = new List<FrmSplash>(1);
            else
                pCloned = new List<FrmSplash>(Screen.AllScreens.Count());

            this.BringToFront();
            for (int i = 0; i < pCloned.Capacity; i++)
            {
                frmcl = new FrmSplash(this, i, pSize);
                pCloned.Add(frmcl);
            }

            for (int i = 0; i < pCloned.Capacity; i++)
            {
                pCloned[i].setUrl(pUrl);
                pCloned[i].Hide(); // somehow, this 'hide' before 'show; will make icon on top exist if formborderstyle is not none
                pCloned[i].Show();
                pCloned[i].BringToFront();
                pCloned[i].Activate();
            }

            SetKeyFocus();
        }

        /// <summary>
        /// hide any clones of FrmSplash 
        /// </summary>
        /// <param name="restartIntervalTimer">re-enable play interval after splash are closed?</param>
        private void hideSplash(bool restartIntervalTimer = true)
        {
            /**
            ** TODO: do something with keylock here
            **/
            //if (!pDisableKeyLock)
            //    DisableWinKeys.EnableWindowsUI;

            // re-enable play interval after splash are closed
            if (restartIntervalTimer)
            {
                tmrPlay.Enabled = false;
                tmrPlay.Enabled = (pInterval > 0);
            }

            this.Hide();
            destroyClones();
            /**
            ** TODO: force to hide app from desktop, if necessary
            **/
            //ShowWindow(Application.Handle, SW_HIDE);
        }

        private static ResourceManager resourceManager = 
            new ResourceManager("HSSESplash.HSSESplash", Assembly.GetExecutingAssembly());
            
        /// <summary>
        /// cancel/skip showing splash.
        /// this will also increase cancel counter.
        /// </summary>
        /// <param name="logType">any kind of status you want to log, OFF|ON|END etc.. </param>
        protected internal void cancelSplash(string logType)
        {
            Console.WriteLine($"cancelSplash {logType}");
            // cannot cancel 
            if ((pCancelCounter >= pMaxCancel) && (pMaxCancel > 0))
            {
                if (!this.IsDisposed) trayNotify.Icon = this.Icon;
                trayNotify.BalloonTipText = String.Format(pNoSkipNotify, pMaxCancel);
                trayNotify.BalloonTipIcon = ToolTipIcon.Warning;
                if (!String.IsNullOrWhiteSpace(pNoSkipNotify))
                    trayNotify.ShowBalloonTip(tmrNotify.Interval);
                if (pCloned != null) for (int i = pCloned.Capacity - 1; i >= 0; i--)
                    {
                        try
                        {
                            FrmSplash frm = pCloned[i];
                            if (frm != null) frm.formCloseRequested = false;
                        }
                        finally { }
                    }
                return;
            }
            pCancelCounter = pCancelCounter + 1;
            hideSplash(false);
            if (pMaxCancel > 0)
            {
                if (!this.IsDisposed) trayNotify.Icon = this.Icon;
                trayNotify.BalloonTipText =
                    String.Format(pCancelNotify, pCancelCounter, pMaxCancel);
                trayNotify.BalloonTipIcon = ToolTipIcon.Warning;
                if (!String.IsNullOrWhiteSpace(pCancelNotify))
                    trayNotify.ShowBalloonTip(tmrNotify.Interval);
            }
            trayNotify.Text =  $"{pCaption}\nCancel: {pCancelCounter} time{(pCancelCounter>1?"s":"")}";

            /**
            ** do some tray icon change here
            **/
            try{
                String ico = String.Format("NUM-{0}-32x32", pCancelCounter > 9 ? "X" : pCancelCounter.ToString());
                Icon numicon = (Icon) resourceManager.GetObject(ico);
                if (!this.IsDisposed) trayNotify.Icon = (pCancelCounter <= 0 ? this.Icon : numicon) ?? this.Icon;
            } finally{}

            logMe(logType);
            tmrSplash.Enabled = false;
            tmrNotify.Enabled = false;
            if (pInterval <= 0)
                goodBye();
        }

    }

    [XmlRoot("HSSESplash", IsNullable = true)]
    public class XMLConfig
    {
        public string url;
        public string logServer;
        public string appName;
        public bool disableKeyLock;
        public bool disableLogging;
        public bool showCloseButton;
        public bool disablePageBlock;
        public bool disableMultiScreen;
        public int duration;
        public bool disableEscape;
        public int size;
        public bool disableDragMove;
        public double interval;
        public string caption;
        public string messageNotify;
        public string cancelNotify;
        public string noSkipNotify;
        public string notificationClick;
        public bool autoRun;
        public int maxCancel;
        public bool disableTrayIcon;
        public bool showTrayMenu;

        public override String ToString()
        {
            StringBuilder s = new StringBuilder("Configuration: \n");
            s.Append($" appName = {this.appName}\n");
            s.Append($" caption = {this.caption}\n");
            s.Append($" disableDragMove = {this.disableDragMove}\n");
            s.Append($" disableEscape = {this.disableEscape}\n");
            s.Append($" disableKeyLock = {this.disableKeyLock}\n");
            s.Append($" disableLogging = {this.disableLogging}\n");
            s.Append($" disablePageBlock = {this.disablePageBlock}\n");
            s.Append($" disableTrayIcon = {this.disableTrayIcon}\n");
            s.Append($" showTrayMenu = {this.showTrayMenu}\n");
            s.Append($" duration = {this.duration}\n");
            s.Append($" interval = {this.interval}\n");
            s.Append($" logServer = {this.logServer}\n");
            s.Append($" maxCancel = {this.maxCancel}\n");
            s.Append($" messageNotify = {this.messageNotify}\n");
            s.Append($" cancelNotify = {this.cancelNotify}\n");
            s.Append($" noSkipNotify = {this.noSkipNotify}\n");
            s.Append($" noSkipNotify = {this.notificationClick}\n");
            s.Append($" showCloseButton = {this.showCloseButton}\n");
            s.Append($" size = {this.size}\n");
            s.Append($" url = {this.url}\n");
            return s.ToString();
        }
    }
}
