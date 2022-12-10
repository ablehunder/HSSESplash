using System;
using System.Drawing;
using System.IO;
using System.Windows.Forms;
using System.Runtime.InteropServices;
using System.Threading.Tasks;

namespace HSSESplash
{

    /// <summary>
    /// this just a window that will show content in webview2 
    /// this should be simple task, just pass url to webview2 and show
    /// </summary>
    public partial class FrmSplash : Form
    {
        private FrmMain formMain;
        private int pMonitorNum;
        private int formSize = 75;

        public FrmSplash(FrmMain parentForm, int screenNum, int formPctSize)
        {
            formMain = parentForm;
            pMonitorNum = screenNum;
            formSize = formPctSize;

            this.ClientSize = new Size(
                (Screen.AllScreens[this.pMonitorNum].Bounds.Width * formSize / 100),
                (Screen.AllScreens[this.pMonitorNum].Bounds.Height * formSize / 100));

            Point newlocation = new Point(
                0 + Screen.AllScreens[this.pMonitorNum].Bounds.Left
                    + (Screen.AllScreens[this.pMonitorNum].Bounds.Width / 2 * (100 - formSize) / 100),
                0 + Screen.AllScreens[this.pMonitorNum].Bounds.Top
                    + (Screen.AllScreens[this.pMonitorNum].Bounds.Height / 2 * (100 - formSize) / 100));

            this.DesktopLocation = newlocation;

            InitializeComponent();

            this.Icon = formMain.Icon;
            this.Text = formMain.Text;
            lblDummy.Text = formMain.usage();
            this.Visible =true;
            this.WindowState = FormWindowState.Normal;

            if (formSize >= 100) 
                this.WindowState = FormWindowState.Maximized;
            else 
                if (formMain.pShowCloseButton) this.FormBorderStyle = FormBorderStyle.FixedSingle;
                
            resizeForm();
        }

        /// <summary>
        /// resize control inside this form
        /// </summary>
        private void resizeForm()
        {
            editUrl.Top = this.ClientSize.Height - editUrl.Height;;
            editUrl.Left = 0;
            editUrl.Width = this.ClientSize.Width;
        }

        private string pTxtToReplaceUrl = null;
        /// <summary>
        /// set url to webview2, and it will automatically shown
        /// whenever url cannot be navigated properly, we probe any possible url or file could be shown
        /// </summary>
        public void setUrl(string url){
            if (!String.IsNullOrWhiteSpace(url)) {
                lblDummy.Visible = false;
                try{
                    this.wbFlash.Source = new Uri(url);
                }catch(Exception emain){
                    // maybe its a file??
                    try{
                        Uri newuri = new Uri(Path.Combine(Environment.CurrentDirectory, @url));
                        this.wbFlash.Source = newuri;
                    }catch(Exception esub){
                        Console.WriteLine(esub.Message);
                        // last try as file text
                        try{
                            string stxt = File.ReadAllText(@url);
                        if ((wbFlash == null) || (wbFlash.CoreWebView2 == null))
                            pTxtToReplaceUrl = stxt;
                        else
                            wbFlash.NavigateToString(stxt); 
                        }catch(Exception elast){
                            Console.WriteLine(elast.Message);
                            MessageBox.Show(emain.Message, 
                                formMain.Text, MessageBoxButtons.OK, MessageBoxIcon.Error);
                        }
                    } 
                }
            }
            else
                lblDummy.Visible = true;
                
            panelBlocker.Visible = 
            editUrl.Visible = wbFlash.Visible = !lblDummy.Visible;

            editUrl.Text = url;
        }
        
        private async Task InitializeAsyncWebView2()
        {
            await wbFlash.EnsureCoreWebView2Async(null);
        }

        private void SetKeyFocus()
        {
            if (formMain.pDisablePageBlock) return;
            /*
            formMain.BringToFront();
            try{if (formMain.Visible) formMain.Focus();} finally{}            
            try{if (formMain.Visible) formMain.SetKeyFocus();} finally{}
            */
        }
    }

    [ComImport, InterfaceType(ComInterfaceType.InterfaceIsIUnknown), Guid("a5cd92ff-29be-454c-8d04-d82879fb3f1b")]
    [System.Security.SuppressUnmanagedCodeSecurity]
    public interface IVirtualDesktopManager
    {
        [PreserveSig]
        int IsWindowOnCurrentVirtualDesktop(
            [In] IntPtr TopLevelWindow,
            [Out] out int OnCurrentDesktop
            );
        [PreserveSig]
        int GetWindowDesktopId(
            [In] IntPtr TopLevelWindow,
            [Out] out Guid CurrentDesktop
            );

        [PreserveSig]
        int MoveWindowToDesktop(
            [In] IntPtr TopLevelWindow,
            [MarshalAs(UnmanagedType.LPStruct)]
            [In]Guid CurrentDesktop
            );
    }

    /// <summary>
    /// dummy form to detect window handle in virtual desktop manager
    /// </summary>
    public class NewWindow : Form // dummy
    {
    }
    [ComImport, Guid("aa509086-5ca9-4c25-8f95-589d3c07b48a")]
    public class CVirtualDesktopManager
    {

    }
    
    public class VirtualDesktopManager
    {
        public VirtualDesktopManager()
        {
            cmanager = new CVirtualDesktopManager();
            manager = (IVirtualDesktopManager)cmanager;
        }
        ~VirtualDesktopManager()
        {
            manager = null;
            cmanager = null;
        }
        private CVirtualDesktopManager cmanager = null;
        private IVirtualDesktopManager manager;

        public bool IsWindowOnCurrentVirtualDesktop(IntPtr TopLevelWindow)
        {
            int result;
            int hr;
            if ((hr = manager.IsWindowOnCurrentVirtualDesktop(TopLevelWindow, out result)) != 0)
            {
                Marshal.ThrowExceptionForHR(hr);
            }
            return result != 0;
        }

        public Guid GetWindowDesktopId(IntPtr TopLevelWindow)
        {
            Guid result;
            int hr;
            if ((hr = manager.GetWindowDesktopId(TopLevelWindow, out result)) != 0)
            {
                Marshal.ThrowExceptionForHR(hr);
            }
            return result;
        }

        public void MoveWindowToDesktop(IntPtr TopLevelWindow, Guid CurrentDesktop)
        {
            int hr;
            if ((hr = manager.MoveWindowToDesktop(TopLevelWindow, CurrentDesktop)) != 0)
            {
                Marshal.ThrowExceptionForHR(hr);
            }
        }
    }
}