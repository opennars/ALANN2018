using System;
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.Threading;

namespace ALANNShell
{
    public delegate void UpdateText(string msg);

    public class Shell
    {
        static readonly string serverAddr = "127.0.0.1";
        static readonly int serverPort = 5000;
        static readonly string clientAddr = "127.0.0.1";
        static readonly int clientPort = 5003;

        static IPEndPoint serverEndPoint = new IPEndPoint(IPAddress.Parse(serverAddr), serverPort);
        static IPEndPoint clientEndPoint = new IPEndPoint(IPAddress.Any, clientPort);
        static UdpClient outSocket = new UdpClient();
        static UdpClient inSocket = new UdpClient(clientEndPoint);

        static readonly string logPath = @"d:\logs\ALANNShell.log";

        public Shell()
        {
            ThreadStart ts = new ThreadStart(Listening);
            Thread th = new Thread(ts);
            th.Start();

            outSocket.Connect(serverEndPoint);
        }

        private async void Listening()
        {
            UpdateText updateMsg = UpdateMsg;

            while (true)
            {
                var result = await inSocket.ReceiveAsync();
                var data = result.Buffer;
                string msg = Encoding.ASCII.GetString(data, 0, data.Length);
                updateMsg(msg);
            }
        }

        private void UpdateMsg(string msg)
        {
            var msgType = msg.Substring(0, 1);

            switch (msgType)
            {
                case "?":
                    Console.WriteLine(msg);
                    LogMsg(msg, logPath);
                    break;

                default:
                    ;
                    break;
            }
        }

        public void SendMsg(string str)
        {
            var lines = str.Split('\n');

            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine == "") return;

                var data = Encoding.ASCII.GetBytes(trimmedLine);
                outSocket.SendAsync(data, data.Length);
                LogMsg(trimmedLine, logPath);
            }
        }

        private void LogMsg(string msg, string path)
        {
            if (!File.Exists(path))
            {
                File.Create(path);
                TextWriter tw = new StreamWriter(path);
                tw.WriteLine(msg);
                tw.Close();
            }
            else
            {
                using (var tw = new StreamWriter(path, true))
                {
                    tw.WriteLine(msg);
                }
            }
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Shell shell = new Shell();

            Console.WriteLine("ALANN Shell running");

            while (true)
            {
                var msg = Console.ReadLine();
                shell.SendMsg(msg);
            }
        }
    }
}
