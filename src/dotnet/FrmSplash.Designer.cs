using System;
using System.Drawing;
using System.Windows.Forms;

namespace HSSESplash
{
    partial class FrmSplash
    {
        /// <summary>
        ///  Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;
        private Panel panelBlocker = new TransparentPanel();
        private Label lblDummy = new Label();
        private TextBox editUrl = new TextBox();
        private Microsoft.Web.WebView2.WinForms.WebView2 wbFlash;
        private Timer tmrVDesktop;

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
            //System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FrmSplash));
            this.SuspendLayout();

            // 
            // panelBlocker
            // 
            panelBlocker.Dock = DockStyle.Fill;
            panelBlocker.MouseDown += FrmSplash_MouseDown;
            //panelBlocker.MouseClick += FrmSplash_MouseDown;
            panelBlocker.MouseMove += FrmSplash_MouseMove;
            panelBlocker.MouseUp += FrmSplash_MouseUp;
            this.Controls.Add(panelBlocker);

            // 
            // editUrl
            // 
            editUrl.BorderStyle = BorderStyle.None;
            editUrl.ReadOnly = true;
            editUrl.TabStop = false;
            editUrl.KeyUp += FrmSplash_KeyUp;
            this.Controls.Add(editUrl);

            // 
            // lblDummy
            // 
            lblDummy.Dock = DockStyle.Fill;
            lblDummy.MouseDown += FrmSplash_MouseDown;
            lblDummy.MouseMove += FrmSplash_MouseMove;
            lblDummy.MouseUp += FrmSplash_MouseUp;
            this.Controls.Add(lblDummy);

            // 
            // wbFlash
            // 
            this.wbFlash = new Microsoft.Web.WebView2.WinForms.WebView2();
            ((System.ComponentModel.ISupportInitialize)(this.wbFlash)).BeginInit();
            this.wbFlash.NavigationCompleted += wbFlash_NavigationCompleted;
            this.wbFlash.Dock = DockStyle.Fill;                       
            this.wbFlash.KeyUp += FrmSplash_KeyUp;
            this.Controls.Add(this.wbFlash);
            // don't forget to add this...
            ((System.ComponentModel.ISupportInitialize)(this.wbFlash)).EndInit();

            // 
            // FrmSplash
            // 
            this.ResumeLayout(false);
            this.StartPosition = FormStartPosition.Manual;
            this.TopMost = true;
            this.ShowInTaskbar = false;
            this.MinimizeBox = false;
            this.MaximizeBox = false;
            this.Visible= false;
            this.FormBorderStyle = FormBorderStyle.None;
            this.FormClosing += FrmSplash_FormClosing;
            this.Load += FrmSplash_Load;
            this.KeyUp += FrmSplash_KeyUp;
            this.MouseDown += FrmSplash_MouseDown;
            this.MouseMove += FrmSplash_MouseMove;
            this.MouseUp += FrmSplash_MouseUp;
            this.ResizeEnd += FrmSplash_Resize;
            this.Shown += FrmSplash_Shown;

            tmrVDesktop = new Timer();
            tmrVDesktop.Tick += tmrVDesktop_Tick;
            tmrVDesktop.Enabled=true;            
        }

        #endregion        

        private void FrmSplash_Resize(Object sender, EventArgs e)
        {
            resizeForm();
        }

        private void WebView_CoreWebView2InitializationCompleted(object sender, Microsoft.Web.WebView2.Core.CoreWebView2InitializationCompletedEventArgs e)
        {
            if (pTxtToReplaceUrl!=null)
                wbFlash.NavigateToString(pTxtToReplaceUrl);
        }

        private async void FrmSplash_Load(object sender, EventArgs e)
        {
            try{
                wbFlash.CoreWebView2InitializationCompleted += WebView_CoreWebView2InitializationCompleted;
                await InitializeAsyncWebView2();
            } catch(Exception) { } finally{}
            
            SetKeyFocus();
        }

        private void wbFlash_NavigationCompleted(Object sender, EventArgs eventArgs)
        {
            editUrl.Visible = false;
        }

        private void FrmSplash_KeyUp(object sender, KeyEventArgs e){
            if ((e.KeyData == Keys.Escape) &&  !formMain.pDisableEscape)
                formMain.cancelSplash("OFF");
        }

        protected internal bool formCloseRequested;
        private void FrmSplash_FormClosing(object sender, FormClosingEventArgs e)
        {
            // form closing handled by frmMain destroyClone.            
            if (formCloseRequested) return;
            if (!formMain.pDisableEscape) {
                e.Cancel = true;
                formCloseRequested = true;
                formMain.cancelSplash("OFF");
            }
        }

        private bool pMouseIsDown;
        private int PX, PY;
        // handle window dragging
        private void FrmSplash_MouseDown(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            if (formMain.pDisablePageBlock) return;
            
            if (e.Button == MouseButtons.Left) {
                // only able to move not in fullscreen
                pMouseIsDown = (!formMain.pDisableDragMove) && (formMain.pSize<100) && true;
                PX = e.X;
                PY = e.Y;
            }
        }

        // handle window dragging and move
        private void FrmSplash_MouseMove(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            
            if (formMain.pDisablePageBlock) return;
            if (pMouseIsDown) {
                SetBounds(Left + (e.X - PX), Top + (e.Y - PY), Width, Height);
            }
            else{
                if (e.Button == MouseButtons.Left) {
                    // only able to move not in fullscreen
                    pMouseIsDown = (!formMain.pDisableDragMove) && (formMain.pSize<100) && true;
                    PX = e.X;
                    PY = e.Y;
                }
            }
        }

        // handle window dragging and move
        private void FrmSplash_MouseUp(object sender, System.Windows.Forms.MouseEventArgs e)
        {
            Console.WriteLine("FrmSplash.SetKeyFocus");
            if (formMain.pDisablePageBlock) return;
            pMouseIsDown=false;
            SetKeyFocus();
        }

        private void FrmSplash_Shown(Object sender, EventArgs e)
        {
            this.BringToFront();
        }

        // handle windows virtual desktop, to display in multiple desktop
        // virtual desktop pretty much different to multiple screen/monitor
        private VirtualDesktopManager vdm;
        private VirtualDesktopManager getVDM()
        {
            if (vdm==null) vdm= new VirtualDesktopManager();
            return vdm;
        }        

        private void tmrVDesktop_Tick(Object sender, EventArgs eventArgs)
        {
            bool onCurrentDesktop = true;
            NewWindow nw=null;
            // old windows (prior Win10) don't have this feature, so we ignore any exception occur
            try{
                VirtualDesktopManager vdm = getVDM();
                if (!this.IsDisposed && Handle!=null)
                    onCurrentDesktop = vdm.IsWindowOnCurrentVirtualDesktop(this.Handle);
                    //Console.WriteLine(string.Format("IsOnCurrentDesktop {0} : {1}", this.Handle, onCurrentDesktop));
                if (!onCurrentDesktop)
                {
                    //vdm.MoveWindowToDesktop(this.Handle, vdm.GetWindowDesktopId());
                    try{
                        using (nw = new NewWindow())
                        {
                            nw.ClientSize = new Size(10,10);
                            nw.StartPosition = FormStartPosition.CenterScreen;
                            nw.Show(null);
                            vdm.MoveWindowToDesktop(Handle, vdm.GetWindowDesktopId(nw.Handle));
                        }
                    } catch(Exception) { }
                    finally{
                        // showmessage('get over here');
                        this.Hide();
                        this.Show();
                        this.Activate();
                    }
                }
            }catch(Exception e){
                Console.WriteLine(e);
            }
            finally{
                if (nw!=null) nw.Dispose();
            }            
        }
    }
}