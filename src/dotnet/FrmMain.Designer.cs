using System;
using System.Collections.Generic;
#if DEBUG
using System.Runtime.InteropServices;
#endif
using System.Windows.Forms;

namespace HSSESplash
{
    partial class FrmMain
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        // TODO: find simple component to make user this app only has single instance
        private System.ComponentModel.IContainer components = null;
        private TextBox editFocus = new TextBox();
        private PictureBox imgLogo = new PictureBox();
        private Timer tmrInstanceServer;
        private Timer tmrCounter;
        private Timer tmrNotify;
        private Timer tmrPlay;
        private Timer tmrSplash;
        private NotifyIcon trayNotify = new NotifyIcon();

        /// <summary>
        ///  Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        ///  Required method for Designer support - do not modify
        ///  the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FrmMain));
            this.SuspendLayout();

            //
            // imgLogo
            //
            imgLogo.Location = new System.Drawing.Point(0, 0);
            imgLogo.Size = new System.Drawing.Size(33, 33);
            imgLogo.Click += imgLogo_Click;
            this.Controls.Add(imgLogo);

            //
            // editFocus
            //
            editFocus.Location = new System.Drawing.Point(40, 0);
            editFocus.Size = new System.Drawing.Size(128, 23);
            editFocus.KeyUp += editFocus_KeyUp;
            editFocus.LostFocus += editFocus_LostFocus;
            this.Controls.Add(editFocus);

            //
            // FrmMain
            //
            this.ResumeLayout(false);
            this.Text = "HSSESplash";
            this.ClientSize = new System.Drawing.Size(800, 450);
            //this.SetStyle(ControlStyles.UserPaint, true);
            //this.SetStyle(ControlStyles.OptimizedDoubleBuffer, true);
            //this.SetStyle(ControlStyles.SupportsTransparentBackColor, true);
            //this.BackColor = System.Drawing.Color.FromArgb(12, 128,255,255) ;
            this.BackColor = System.Drawing.Color.White;
            this.Opacity=0.5;
            //this.TransparencyKey = this.BackColor;
            //this.BackColor = System.Drawing.Color.Transparent;
            this.MinimizeBox = false;
            this.MaximizeBox = false;
            this.ShowInTaskbar = false;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.KeyUp += editFocus_KeyUp;
            this.Load += FrmMain_Load;
            this.FormClosed += FrmMain_Close;
            this.FormClosing += FrmMain_FormClosing;

            tmrInstanceServer = new Timer(components);
            tmrInstanceServer.Enabled = false;
            tmrInstanceServer.Interval = 100;
            tmrInstanceServer.Tick += tmrInstanceServer_Tick;

            tmrCounter = new Timer(components);
            tmrCounter.Enabled = false;
            tmrCounter.Interval = 1000;
            tmrCounter.Tick += tmrCounter_Tick;

            tmrNotify = new Timer(components);
            tmrNotify.Enabled = false;
            tmrNotify.Interval = 7000;
            tmrNotify.Tick += tmrNotify_Tick;

            tmrPlay = new Timer(components);
            tmrPlay.Enabled = false;
            tmrPlay.Tick += tmrPlay_Tick;

            tmrSplash = new Timer(components);
            tmrSplash.Enabled = false;
            tmrSplash.Tick += tmrSplash_Tick;
            
        }

        #endregion


#if DEBUG
        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        static extern bool AllocConsole();
#endif
        private void FrmMain_Load(object sender, EventArgs e)
        {
            /**
            ** TODO: comment this after debugging complete
            */
#if DEBUG
            AllocConsole();
#endif

            this.initialize(true, new List<string>(Environment.GetCommandLineArgs()));

            trayNotify.BalloonTipClicked += trayNotify_BalloonClick;
            trayNotify.BalloonTipClosed += trayNotify_BalloonClosed;
            trayNotify.MouseUp += trayNotify_Click;
            
            initComponent();
        }

        private void trayNotify_Click(object sender, MouseEventArgs arg)
        {
            Console.WriteLine("trayNotify_Click");
            // to prevent multiple splash at the same time
            if (!tmrSplash.Enabled
                && !tmrNotify.Enabled
                && (arg.Button == MouseButtons.Left))
                tmrNotify_Tick(null, null);
            SetKeyFocus();
        }

        private void trayNotify_BalloonClick(object sender, EventArgs arg)
        {
            Console.WriteLine("trayNotify_BalloonClick");
            // to prevent multiple splash at the same time
            if (!tmrSplash.Enabled && (pMessageNotify.Equals(trayNotify.BalloonTipText)))
            {
                if ("open".Equals(pNotificationClick))
                    tmrNotify_Tick(null, null);
                else
                {
                    cancelSplash("OFF");
                }
            }
            SetKeyFocus();
        }

        private void trayNotify_BalloonClosed(object sender, EventArgs arg)
        {
            Console.WriteLine("trayNotify_BalloonClosed");
            NotifyIcon ni = (NotifyIcon)sender;
            //Console.WriteLine(ni);
            /**
            ** TODO: this event is not fired correctly
            ** even if balloon closed by itself, this event is fired
            **/
            //if (!pDisableEscape) cancelSplash("OFF");
        }

        private void FrmMain_Close(object sender, FormClosedEventArgs arg)
        {
            cancelSplash("OFF");
        }

        private void imgLogo_Click(object sender, EventArgs eventArgs)
        {
            MessageBox.Show(String.Concat(usage(), "\n\n", (char)169, " @ablehunder :)"));
        }

        private void FrmMain_FormClosing(object sender, FormClosingEventArgs e)
        {
            e.Cancel = !pDisableEscape;
        }

        /// <summary>
        /// tmrPlay used to play splash between interval
        /// </summary>
        private void tmrPlay_Tick(object sender, EventArgs e)
        {
            Console.WriteLine("tmrPlay_Tick");
            // to avoid new splash while existing still played
            if (!tmrSplash.Enabled && !tmrNotify.Enabled)
            {
                logMe("ON");
                tmrNotify.Enabled = true;
                this.WindowState = FormWindowState.Normal;
                this.Hide();
                this.Show();
                this.Activate();
                this.BringToFront();
                trayNotify.BalloonTipTitle = pCaption;
                trayNotify.BalloonTipText = pMessageNotify;
                trayNotify.BalloonTipIcon = ToolTipIcon.Info;
                if (!String.IsNullOrWhiteSpace(pMessageNotify))
                    trayNotify.ShowBalloonTip(tmrNotify.Interval);
            }
        }

        /// <summary>
        /// just simple counter on how long the splash played in second, 
        /// rather than calculate time between start and end
        /// </summary>
        private void tmrCounter_Tick(object sender, EventArgs e)
        {
            pCounter++;
        }

        private void tmrInstanceServer_Tick(object sender, EventArgs e)
        {
            DateTime filetimestamp = new DateTime();
            /**
            ** TODO: do something with single instance check here;
            **/

            /**
            ** do something with ini file change here;
            **/
            // TODO make sure we do not want to reinitialized twice at the same time
            if (!String.IsNullOrWhiteSpace(pIniFile))
            {
                pIniCounter++;
                if (pIniCounter % 10 == 0)
                {
                    pIniCounter = 0;
                    // check change on ini file
                    if (GetFileModTime(pIniFile, ref filetimestamp))
                        if (filetimestamp != pIniFileTimestamp)
                        {
                            Console.WriteLine("Config changed");
                            // do something when ini file is changed
                            hideSplash(false);
                            initialize();
                            initComponent();
                        }
                }
            }
        }

        /// <summary>
        /// timer for splash according to duration
        /// </summary>
        private void tmrSplash_Tick(object sender, EventArgs e)
        {
            Console.WriteLine("tmrSplash_Tick");
            /**
            ** TODO: do something with keylock
            **/
            //if (!pDisableKeyLock)
            //    DisableWinKeys.EnableWindowsUI;

            hideSplash();
            pCancelCounter = 0;
            trayNotify.Icon = this.Icon;
            trayNotify.Text = pCaption;
            tmrSplash.Enabled = false;
            tmrCounter.Enabled = false;
            pCounter = pCounter + 1;
            logMe("END");
            Console.WriteLine(String.Concat("pInterval<=0?", pInterval <= 0));
            if (pInterval <= 0)
                goodBye();
        }

        /// <summary>
        /// events on tmrNotify. executed after notification message dissapear
        /// and then splash is shown.
        /// </summary>
        private void tmrNotify_Tick(object sender, EventArgs e)
        {
            Console.WriteLine("tmrNotify_Tick");

            tmrNotify.Enabled = false;
            
            if (!pIsScreenLocked)  // not showing splash when screen is locked
                showSplash();
            else
            {
                hideSplash();
                tmrSplash.Enabled = false;
                tmrCounter.Enabled = false;
                logMe("LOCK");
                if (pInterval <= 0)
                    goodBye();
            }
        }

        private void editFocus_KeyUp(object sender, KeyEventArgs e)
        {
            if ((e.KeyData == Keys.Escape) && !pDisableEscape)
                cancelSplash("OFF");
        }

        private void editFocus_LostFocus(object sender, EventArgs eventArgs)
        {

        }    
    
    }
}
