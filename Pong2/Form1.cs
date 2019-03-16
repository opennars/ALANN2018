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
        int ball_x = 150;
        int ball_y = 100;
        int ball_dx = 3;
        int ball_dy = 2;

        int bat_x = 20;
        int bat_y = 1;
        int bat_dx = 5;

        readonly int ball_diameter = 10;
        readonly int bat_width = 50;
        readonly int bat_height = 10;

        static string serverAddr = "127.0.0.1";
        static int serverPort = 5000;
        static int clientPort = 5002;
        static int graphitePort = 8125;

        static IPEndPoint serverEndPoint = new IPEndPoint(IPAddress.Parse(serverAddr), serverPort);
        static IPEndPoint clientEndPoint = new IPEndPoint(IPAddress.Any, clientPort);
        static UdpClient outSocket = new UdpClient();
        static UdpClient inSocket = new UdpClient(clientEndPoint);

        static IPEndPoint graphiteEndPoint = new IPEndPoint(IPAddress.Parse(serverAddr), graphitePort);
        static UdpClient graphiteOutSocket = new UdpClient();

        StringBuilder sb = new StringBuilder();

        public Form1()
        {
            InitializeComponent();
            this.Paint += new PaintEventHandler(paintBall);
            this.Paint += new PaintEventHandler(paintBat);
            this.DoubleBuffered = true;

            outSocket.Connect(serverEndPoint);
            graphiteOutSocket.Connect(graphiteEndPoint);

            ThreadStart ts = new ThreadStart(Listening);
            Thread th = new Thread(ts);
            th.Start();
        }
        private void SendMetric(string metric)
        {
            var data = Encoding.ASCII.GetBytes(metric);
            graphiteOutSocket.SendAsync(data, data.Length);
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
            graphics.FillEllipse(blueBallBrush, ball_x, ball_y, ball_diameter, ball_diameter);
        }

        private void paintBat(object sender, PaintEventArgs e)
        {
            graphics = e.Graphics;
            SolidBrush redBatBrush = new SolidBrush(Color.Red);
            graphics.FillRectangle(redBatBrush, bat_x, bat_y, bat_width, bat_height);
        }

        private bool HitBat(int x, int y)
        {
            var bat = new Rectangle(bat_x, bat_y, bat_width, bat_height);
            var ball = new Rectangle(x, y, ball_diameter, ball_diameter);

            bool bHit = !Rectangle.Intersect(bat, ball).IsEmpty;

            if (bHit)
            {
                toolStripStatusLabel1.Text = "Hit Bat - wahoo! (" + x.ToString() + " " + y.ToString() + ")";

                sb.Clear();
                sb.AppendFormat("<BALLPOS --> [hit]>.");
                SendStatement(sb.ToString());

                sb.Clear();
                sb.AppendFormat("<BALLPOS --> [hit]>!");
                SendStatement(sb.ToString());

                SendMetric("ALANN.Hit:1|c\n");
            }

            return bHit;
        }

        private void MoveBall()
        {
            int newBall_x = ball_x + ball_dx;
            int newBall_y = ball_y + ball_dy;

            int t_y = toolStripStatusLabel1.Height;

            if (HitBat(newBall_x, newBall_y))
            {
                ball_dy = -ball_dy;
                ball_y = bat_height;
            }

            if (newBall_y <= 0) // miss
            {
                ball_dy = -ball_dy;

                sb.Clear();
                sb.AppendFormat("<BALLPOS --> [hit]>. {{0.00 0.90}}");
                SendStatement(sb.ToString());

                sb.Clear();
                sb.AppendFormat("<BALLPOS --> [hit]>!");
                SendStatement(sb.ToString());

                toolStripStatusLabel1.Text = "Missed - boohoo!";

                SendMetric("ALANN.Miss:1|c\n");
                var missDistance = Math.Abs(ball_x - bat_x);
                SendMetric("ALANN.MissDistance:" + missDistance.ToString() + "|c\n");
            }

            if (newBall_x <= 0 || newBall_x >= this.ClientSize.Width - ball_diameter) ball_dx = -ball_dx;
            if (newBall_y >= this.ClientSize.Height - (ball_diameter + t_y)) ball_dy = -ball_dy;

            ball_x += ball_dx;
            ball_y += ball_dy;
        }

        private void MoveBat()
        {
            if(bat_dx > 0 && bat_x >= (this.ClientRectangle.Width - bat_width)) bat_dx = -bat_dx;
            else if(bat_dx < 0 && bat_x <= 0) bat_dx = -bat_dx;

            bat_x += bat_dx;
        }

        private void MoveLeft()
        {
            if(bat_dx > 0) bat_dx = -bat_dx;

            sb.Clear();
            sb.AppendFormat("<[left] --> action>.");
            SendStatement(sb.ToString());
        }

        private void MoveRight()
        {
            if (bat_dx < 0) bat_dx = -bat_dx;

            sb.Clear();
            sb.AppendFormat("<[right] --> action>.");
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
                if (ball_x < (bat_x + ball_diameter))
                {
                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [left]>.");
                    SendStatement(sb.ToString());

                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [right]>. {{0.0 0.9}}");
                    SendStatement(sb.ToString());

                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [equal]>. {{0.0 0.9}}");
                    SendStatement(sb.ToString());
                }
                else if (ball_x > (bat_x + bat_width))
                {
                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [left]>. {{0.0 0.9}}");
                    SendStatement(sb.ToString());

                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [right]>.");
                    SendStatement(sb.ToString());

                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [equal]>. {{0.0 0.9}}");
                    SendStatement(sb.ToString());
                }
                else
                {
                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [left]>. {{0.0 0.9}}");
                    SendStatement(sb.ToString());

                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [right]>. {{0.0 0.9}}");
                    SendStatement(sb.ToString());

                    sb.Clear();
                    sb.AppendFormat("<BALLPOS --> [equal]>.");
                    SendStatement(sb.ToString());
                }
            }
        }
    }
}
