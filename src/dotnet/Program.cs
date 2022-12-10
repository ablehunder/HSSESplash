using System;
using System.Threading;
using System.Windows.Forms;

namespace HSSESplash
{
    static class Program
    {
        // use Mutex for single instance application
        static Mutex mutex = new Mutex(true, "HSSESplash.Mutex");

        /// <summary>
        ///  The main entry point for the application.
        ///  This application will run in single instance.
        ///  Notification appears when there is an instance already run.
        /// </summary>
        [STAThread]
        static void Main()
        {
            if(mutex.WaitOne(TimeSpan.Zero, true)) {
                Application.SetHighDpiMode(HighDpiMode.SystemAware);
                Application.EnableVisualStyles();
                Application.SetCompatibleTextRenderingDefault(false);
                Application.Run(new FrmMain());
                mutex.ReleaseMutex();
            } else {
                MessageBox.Show("This application is already running.\nOnly one instance at a time.", 
                    Application.ProductName, MessageBoxButtons.OK, MessageBoxIcon.Stop);
            }
        }
    }
    
}
