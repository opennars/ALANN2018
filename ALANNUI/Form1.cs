/* 
 * The MIT License
 *
 * Copyright 2018 The ALANN2018 authors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
*/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Net;
using System.Net.Sockets;
using System.Threading;

namespace ALANNUI
{
    public delegate void UpdateText(string msg);

    public partial class Form1 : Form
    {
        static string serverAddr = "127.0.0.1";
        static int serverPort = 5000;
        static string clientAddr = "127.0.0.1";
        static int clientPort = 5001;

        static IPEndPoint serverEndPoint = new IPEndPoint(IPAddress.Parse(serverAddr), serverPort);
        static IPEndPoint clientEndPoint = new IPEndPoint(IPAddress.Any, clientPort);
        static UdpClient outSocket = new UdpClient();
        static UdpClient inSocket = new UdpClient(clientEndPoint);

        public Form1()
        {
            InitializeComponent();
            outSocket.Connect(serverEndPoint);
            Start();
        }

        private void button1_Click(object sender, EventArgs e)
        {
            var lines = richTextBox1.Text.Split('\n');
            foreach(var line in lines)
            {
                var data = Encoding.ASCII.GetBytes(line);
                outSocket.SendAsync(data, data.Length);
                //Thread.Sleep(10);
            }
            //richTextBox1.Text = "";
        }

        private void Start()
        {
            ThreadStart ts = new ThreadStart(Listening);
            Thread th = new Thread(ts);
            th.Start();
            //Thread listenThread = new Thread(new ThreadStart(Listening));
        }


        private async void Listening()
        {
            byte[] data;

            Thread.Sleep(100);
            
            while(true)
            {
                var result = await inSocket.ReceiveAsync();
                data = result.Buffer;
                string msg = Encoding.ASCII.GetString(data, 0, data.Length);
                msg += "\n";
                UpdateText updateMsg = UpdateMsg;
                this.Invoke(updateMsg, msg);
            }
        }

        private void UpdateMsg(string msg)
        {
            var text = richTextBox2.Text;
            var len = text.Length;

            text = text.Substring(0, Math.Min(len, 5000));
            text = msg + text;
            richTextBox2.Text = text;
        }

        private string makeRndStatement()
        {
            var rnd = new Random();
            var sb = new StringBuilder();
            sb = sb.AppendFormat("<term{0} --> term{1}>.", rnd.Next(1000), rnd.Next(1000));
            return sb.ToString();
        }

        private void button2_Click(object sender, EventArgs e)
        {
            foreach(var str in samples)
            {
                var data = Encoding.ASCII.GetBytes(str);
                outSocket.SendAsync(data, data.Length);
                Thread.Sleep(10);
            }

            //for (var i = 0; i < 100000000; i++)
            //{
            //    var data = Encoding.ASCII.GetBytes(makeRndStatement());
            //    outSocket.SendAsync(data, data.Length);

            //    if (i % 100000 == 0)
            //        Thread.Sleep(10);
            //}

        }

        string[] samples =
        {
         "<cat --> animal>.",
         "<dog --> animal>.",
         "<(cat * fish) --> eats>.",
         "<(dog * meat) --> eats>.",
         "<<$X --> (eats / _ meat)> ==> <$X --> dog>>.",
         "<dog --> [barks]>.",
         "<cat --> [meows purrs]>.",
         "<cat --> [furry meows]>.",
         "<cat --> [purrs]>.",
         "<[cat dog] --> animal>.",
         "<[meows purrs] --> cat>.",
         "<[furry meows] --> cat>.",
         "<{tom} --> cat>.",
         "<tree --> plant>.",
         "<orange --> fruit>.",
         "<apple --> fruit>.",
         "<fruit --> edible>.",
         "<orange --> color>.",
         "<blue --> color>.",
         "<green --> color>.",
         "<brown --> color>.",
         "<{lexie} --> female>.",
         "<{lexie} --> human>.",
         "<({lexie} * human) --> genus>.",
         "<({lexie} * tree) --> climbs>.",
         "<car --> vehicle>.",
         "<bus --> vehicle>.",
         "<truck --> vehicle>.",
         "<(human * vehicle) --> drive>.",
         "<(human * fruit) --> eats>.",
         "<({lexie} * hair) --> [brown]>.",
         "<plant --> genus>.",
         "<human --> genus>.",
         "<{john tom alan} --> boys>.",
         "<cat --> (eats / _ fish)>.",
         "<a --> b>.",
         "<b --> c>.",
         "<c --> d>.",
         "<d --> e>.",
         "<e --> f>.",
         "<f --> g>.",
         "<g --> h>.",
         "<h --> i>.",
         "<{tweety} --> bird>.",
         "<bird --> [short]>.",
         "<{tweety} --> canary>.",
         "<bird --> animal>.",
         "<animal --> [living]>.",
         "<{tweety} --> [yellow]>.",
         "<[yellow] --> color>.",
         "<plant --> [living]>.",
         "<fruit --> plant>.",
         "<apple --> fruit>.",
         "<orange --> fruit>.",
         "<bird --> [feathered]>.",
         "<bird --> [flyer]>.",
         "<fruit --> [edible]>.",
         "<car --> [driveable]>.",
         "<car --> machine>.",
         "<orange --> edible>.",
         "<{tash} --> human>.",
         "<human --> [living]>.",
         "<human --> [hair]>.",
         "<{tash} --> [tall]>.",
         "<{tash} --> [fast]>.",
         "<{tash} --> runner>.",
         "<girl --> human>.",
         "<girl --> [hair]>.",
         "<[hair] --> [brown]>.",
         "<[red] --> color>.",
         "<[green] --> color>.",
         "<[blue] --> color>.",
         "<[white] --> color>.",
         "<{black} --> color>.",
         "<apple --> [red]>.",
         "<apple --> [green]>.",
         "<[red] <-> [green]>.",
         "<[red] <-> [blue]>.",
         "<[blue] <-> [green]>.",
         "<[red green] <-> [blue green]>.",
         "<person-->human>.",
         "<human-->[driver]>.",
         "<(person * car) --> drive>.",
         "<cod-->fish>.",
         "<haddock-->fish>.",
         "<plaice-->fish>.",
         "<tuna-->fish>.",
         "<salmon-->fish>.",
         "<trout-->fish>.",
         "<sole-->fish>.",
         "<ball --> toy>.",
         "<block --> toy>.",
         "<car --> toy>.",
         "<doll --> toy>.",
         "<train --> toy>.",
         "<plane --> toy>.",
         "<fish --> food>.",
         "<fruit --> food>.",
         "<cheese --> food>.",
         "<milk --> food>.",
         "<cream --> food>.",
         "<bread --> food>.",
         "<eggs --> food>.",
         "<ham --> food>.",
         "<bacon --> food>.",
         "<pork --> food>.",
         "<meat --> food>.",
         "<lamb --> food>.",
         "<beef --> food>.",
         "<chicken --> food>.",
         "<fish --> food>.",
         "<meat --> food>.",
         "<beer --> drink>.",
         "<cider --> drink>.",
         "<drink --> food>.",
         "<human --> (eats / _ food)>.",
         "<walk --> action>.",
         "<run --> action>.",
         "<sit --> action>.",
         "<stand --> action>.",
         "<sleep --> action>.",
         "<wake --> action>.",
         "<eat --> action>.",
         "<pick_up --> action>.",
         "<put_down --> action>.",
         "<push --> action>.",
         "<pull --> action>.",
         "<person <-> human>.",
         "<person --> (does / _ action)>.",
         "<(<$X --> female> && <($X * $Y) --> daughter>) ==> <($Y * $X) --> mother>>.",
         "<<($X * $Y) --> daughter> ==> <($Y * $X) --> mother>>.",
         "<<$X --> person> ==> <($X * preferences) --> has>>.",
         "<<(car * {SELF}) --> ([fast] & approaching) > ==> <{SELF} --> move>>.",
         "<<($1 * $2) --> larger> ==> <($2 * $1) --> smaller>>.",
         "<<($1 * $2) --> smaller> ==> <($2 * $1) --> larger>>.",
         "<<($1 * $2) --> above> ==> <($2 * $1) --> below>>.",
         "<<($1 * $2) --> next_to> <=> <($2 * $1) --> next_to>>.",
         "<<($1 * $2) --> over> ==> <($2 * $1) --> under>>.",
         "<<($1 * $2) --> under> ==> <($2 * $1) --> over>>.",
         "<<($1 * $2) --> outside> ==> <($2 * $1) --> inside>>.",
         "<<($1 * $2) --> inside> ==> <($2 * $1) --> contains>>.",
         "<<($1 * $2) --> contains> ==> <($1 * $2) --> larger>>.", 
         "<<($1 * $2) --> on> ==> <($2 * $1) --> under>>.",
         "<<({$1} * {$2}) --> inside> ==> <({$2} * {$1}) --> contains>>."
            };
    }
}

