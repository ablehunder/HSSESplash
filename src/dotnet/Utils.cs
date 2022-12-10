using System;
using System.Collections.Generic;
using System.Windows.Forms;
using System.Net;
using System.Threading.Tasks;

namespace HSSESplash
{
    public class TransparentPanel : Panel
    {
        protected override CreateParams CreateParams 
        {            
            get {
                CreateParams cp =  base.CreateParams;
                cp.ExStyle |= 0x00000020; // WS_EX_TRANSPARENT
                return cp;
                }
        }
        
        protected override void OnPaintBackground(PaintEventArgs e) 
        {
            //base.OnPaintBackground(e);
        }
        
    }

    public class SplashUtils
    {
        protected internal static string getParam(List<string> paramList, string paramName, bool hasValue = true)
        {
            int i=0;
            string buff="";
            for (i = 0; i< paramList.Capacity; i++)
            {
                if (paramList[i] == paramName)
                {
                    if (hasValue) buff = paramList[i+1];
                    else buff = paramList[i];
                    break;
                }
            }
            return buff;
        }

        
        protected internal static bool getParamBool(List<string>paramList,  string paramName, bool hasValue = true)
        {
            string buff = getParam(paramList, paramName, hasValue);
            return ("true".Equals(buff.ToLower()) || (!hasValue && buff.Equals(paramName)));
        }


        protected internal static int getParamInt(List<string>paramList,  string paramName, int defaultValue =0)
        {
            int r = defaultValue;
            try{
                string buff = getParam(paramList, paramName);
                r = String.IsNullOrWhiteSpace(buff)?r:int.Parse(buff);
            }
            catch(Exception exc){
                r = defaultValue;            
                Console.WriteLine(exc.Message);
            }            
            return r;
        }

        protected internal static double getParamDouble(List<string>paramList, string paramName, double defaultValue=0)
        {
            double r  = defaultValue;
            try {
                string buff = getParam(paramList, paramName);
                r = String.IsNullOrWhiteSpace(buff)?r:double.Parse(buff);
            }
            catch(Exception exc){
                r = defaultValue;
                Console.WriteLine(exc.Message);
            }
            return r;
        }
        private static System.Net.Http.HttpClient client = new System.Net.Http.HttpClient();

        protected internal static async void log(string appname, string urlLog, string logtype,int duration = 0)
        {
            if (String.IsNullOrWhiteSpace(urlLog)) return;

            DateTime pDate = DateTime.Now;

            if (String.IsNullOrWhiteSpace(urlLog)) appname = "SPLASH";

            string pParamPost = string.Format(
                "a={0}&h={1}&d={2}&u={3}&i={4}&t={5}&s={6}&l={7}", appname, 
                Environment.MachineName, Environment.UserDomainName,
                Environment.UserName, GetLocalIPAddress(), 
                pDate.ToString("yyyyMMddHHmmss"), logtype, duration);

            try{
                await requestHttp(urlLog, pParamPost); // just fire and forget
            }catch(Exception exc){
                Console.WriteLine(exc.Message);
            }finally{}

        }

        private static async Task<string> requestHttp(string urlLog, string parampost)
        {
            System.Net.Http.HttpClient client = new System.Net.Http.HttpClient();
            string responseText = await client.GetStringAsync(
                string.Concat(urlLog,"?", parampost));
        
            return responseText;
        }

        protected internal static string GetLocalIPAddress()
        {
            IPHostEntry host = Dns.GetHostEntry(Dns.GetHostName());
            foreach (var ip in host.AddressList)
            {
                if (ip.AddressFamily == System.Net.Sockets.AddressFamily.InterNetwork)
                {
                    return ip.ToString();
                }
            }
            return null;//throw new Exception("No network adapters with an IPv4 address in the system!");
        }
    }
}