using System;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Net;
using System.Net.Sockets;
using System.Threading;

namespace Pong2
{
    public delegate void UpdateText(string msg);

    public partial class Form1 : Form
    {
        Graphics graphics;
        int x = 150;
        int y = 100;
        int dx = 3;
        int dy = 2;

        int b_x = 20;
        int b_y = 1;
        int b_dx = 5;

        static string serverAddr = "127.0.0.1";
        static int serverPort = 5000;
        static int clientPort = 5002;

        static IPEndPoint serverEndPoint = new IPEndPoint(IPAddress.Parse(serverAddr), serverPort);
        static IPEndPoint clientEndPoint = new IPEndPoint(IPAddress.Any, clientPort);
        static UdpClient outSocket = new UdpClient();
        static UdpClient inSocket = new UdpClient(clientEndPoint);
        StringBuilder sb = new StringBuilder();

        public Form1()
        {
            InitializeComponent();
            this.Paint += new PaintEventHandler(paintBall);
            this.Paint += new PaintEventHandler(paintBat);
            this.DoubleBuffered = true;

            outSocket.Connect(serverEndPoint);
            ThreadStart ts = new ThreadStart(Listening);
            Thread th = new Thread(ts);
            th.Start();
        }

        private async void Listening()
        {
            UpdateText updateMsg = UpdateMsg;

            while (true)
            {
                var result = await inSocket.ReceiveAsync();
                var data = result.Buffer;
                string msg = Encoding.ASCII.GetString(data, 0, data.Length);
                this.Invoke(updateMsg, msg);
            }
        }
        private void UpdateMsg(string msg)
        {
            switch (msg)
            {
                case "left":
                    MoveLeft();
                    break;

                case "right":
                    MoveRight();
                    break;

                default:
                    break;
            }
        }
        private void paintBall(object sender, PaintEventArgs e)
        {
            graphics = e.Graphics;
            SolidBrush blueBallBrush = new SolidBrush(Color.Blue);
            graphics.FillEllipse(blueBallBrush, x, y, 10, 10);
        }

        private void paintBat(object sender, PaintEventArgs e)
        {
            graphics = e.Graphics;
            SolidBrush redBatBrush = new SolidBrush(Color.Red);
            graphics.FillRectangle(redBatBrush, b_x, b_y, 100, 10);
        }

        private bool HitBat()
        {
            var bat = new Rectangle(b_x, b_y, 100, 10);
            var ball = new Rectangle(x, y, 10, 10);

            bool bHit = !Rectangle.Intersect(bat, ball).IsEmpty;

            if (bHit)
            {
                toolStripStatusLabel1.Text = "Hit Bat - wahoo! (" + x.ToString() + " " + y.ToString() + ")";

                sb.Clear();
                sb.AppendFormat("<[hit] --> percept>.", x, y);
                SendStatement(sb.ToString());
            }

            return bHit;
        }

        private void MoveBall()
        {
            int newBall_x = x + dx;
            int newBall_y = y + dy;

            int t_y = toolStripStatusLabel1.Height;

            if (HitBat())
            {
                dy = -dy;
            }
            // temp fix for out of bounds
            if (x < 0)
            {
                x = 0;
                //dx = 3;
            }

            if (y < 0)
            {
                y = 0;
                //dy = 2;
            }
            // end of fix

            if (newBall_x < 0 || newBall_x > this.ClientSize.Width - 10) dx = -dx;
            if (newBall_y == 0 || newBall_y > this.ClientSize.Height - (10 + t_y)) dy = -dy;
            if (newBall_y == 0)
            {
                sb.Clear();
                sb.AppendFormat("<[hit] --> percept>. {{0.0 0.9}}");
                SendStatement(sb.ToString());

                toolStripStatusLabel1.Text = "Missed - boohoo!";
            }

            x += dx;
            y += dy;
        }

        private void MoveBat()
        {
            int newBat_x = b_x + b_dx;

            if (newBat_x < 5 || newBat_x > (this.ClientRectangle.Width - 100)) b_dx = -b_dx;

            b_x += b_dx;
        }

        private void MoveLeft()
        {
            int newBat_x = b_x + b_dx;

            b_dx = -b_dx;
            b_x += b_dx;

            sb.Clear();
            sb.AppendFormat("<[left] --> action>.");
            SendStatement(sb.ToString());
            sb.Clear();
            sb.AppendFormat("<[left] --> percept>.");
            SendStatement(sb.ToString());
        }

        private void MoveRight()
        {
            int newBat_x = b_x + b_dx;

            b_x += b_dx;

            sb.Clear();
            sb.AppendFormat("<[right] --> action>.");
            SendStatement(sb.ToString());
            sb.Clear();
            sb.AppendFormat("<[right] --> percept>.");
            SendStatement(sb.ToString());
        }


        private void Form1_Load(object sender, EventArgs e)
        {

        }
        private void SendStatement(string str)
        {
            var data = Encoding.ASCII.GetBytes(str);
            outSocket.SendAsync(data, data.Length);

        }

        int tickCount = 0;

        private void timer1_Tick(object sender, EventArgs e)
        {
            MoveBall();
            MoveBat();
            Invalidate();
            tickCount++;

            if (tickCount % 10 == 0)
            {
                sb.Clear();
                sb.AppendFormat("<X_{0} --> ballposX>.", x / 10);
                SendStatement(sb.ToString());

                sb.Clear();
                sb.AppendFormat("<Y_{0} --> ballposY>.", y / 10);
                SendStatement(sb.ToString());

                sb.Clear();
                sb.AppendFormat("<X_{0} --> batpos>.", b_x / 10);
                SendStatement(sb.ToString());
            }

        }

    }
}
