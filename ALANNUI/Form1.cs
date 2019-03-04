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
using System.Text;
using System.Windows.Forms;
using System.Net;
using System.Net.Sockets;
using System.Threading;
using System.Timers;

namespace ALANNUI
{
    public delegate void UpdateText(string msg);

    public partial class Form1 : Form
    {
        static string serverAddr = "127.0.0.1";
        static int serverPort = 5000;
        //static string clientAddr = "127.0.0.1";
        static int clientPort = 5001;

        static IPEndPoint serverEndPoint = new IPEndPoint(IPAddress.Parse(serverAddr), serverPort);
        static IPEndPoint clientEndPoint = new IPEndPoint(IPAddress.Any, clientPort);
        static UdpClient outSocket = new UdpClient();
        static UdpClient inSocket = new UdpClient(clientEndPoint);

        private static System.Timers.Timer timer;
        private bool ServerLive = false;
        const int TextBufferSize = 75000;

        const string ErrorPrefixStr        = "*** ERROR: ";
        const string StartServerWarningStr = "Status: ALANN Server Not Running";
        const string MissingNodeNameStr    = "Must Enter 'node name' in Node Term box";
        const string LoadingSampleStr      = "LOADING SAMPLE...";
        const string SampleLoadedStr       = "SAMPLE LOADED...";
        private void DisplayWelcomeMessage()
        {
            var msg =
                "*** WELCOME TO ALANN ***\n\n" +
                "REMEMBER TO START THE ALANN SERVER\n";

            inputRTB.Text = msg;
        }

        public Form1()
        {
            InitializeComponent();
            outSocket.Connect(serverEndPoint);
            Start();
            SetTimer();
        }

        private void SetTimer()
        {
            timer = new System.Timers.Timer(1000.0);
            timer.AutoReset = true;
            timer.Elapsed += OnTimedEvent;
            timer.Enabled = true;
        }

        private void OnTimedEvent(Object source, ElapsedEventArgs e)
        {
            if (!ServerLive)
            {
                statusLabel.Text = StartServerWarningStr;
            }
            ServerLive = false;
        }

        private void InputBtn_Click(object sender, EventArgs e)
        {
            var lines = inputRTB.Text.Split('\n');

            foreach (var line in lines)
            {
                var trimmedLine = line.Trim();
                if (trimmedLine == "") continue;

                if (trimmedLine.StartsWith("//")) continue;
                else if (trimmedLine.StartsWith("**")) continue;
                else if (trimmedLine.StartsWith("'")) continue;

                var data = Encoding.ASCII.GetBytes(trimmedLine);
                outSocket.SendAsync(data, data.Length);
                EchoCmd("", trimmedLine);
            }
        }

        private void Start()
        {
            ThreadStart ts = new ThreadStart(Listening);
            Thread th = new Thread(ts);
            th.Start();

            DisplayWelcomeMessage();
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

        private void UpdateRTB(RichTextBox rtb, char prefix, string msg)
        {
            var len = Math.Min(rtb.Text.Length, TextBufferSize);
            var start = Math.Max(0, rtb.Text.Length - TextBufferSize);

            rtb.Text = rtb.Text.Substring(start, len) + msg.TrimStart(prefix) + "\n";
            rtb.SelectionStart = rtb.Text.Length;
            rtb.ScrollToCaret();
        }

        private void ProcessAnswerMsg(string msg)
        {
            UpdateRTB(answersRTB, '?', msg);
        }

        private void ProcessBeliefMsg(string msg)
        {
            UpdateRTB(beliefsRTB, '!', msg);
        }

        private void ProcessCmdMsg(string msg)
        {
            UpdateRTB(cmdLogRTB, '#', msg);
        }

        private void ProcessInferenceMsg(string msg)
        {
            UpdateRTB(inferenceRTB, '$', msg);
        }

        private void ProcessStatusMsg(string msg)
        {
            var statusMsg = msg.TrimStart(':');

            if (statusMsg.StartsWith("Status"))
            {
                statusLabel.Text = statusMsg;
            }
            else if (statusMsg.StartsWith("Events"))
            {
                eventsPerSecondStatus.Text = statusMsg;
            }
            else if (statusMsg.StartsWith("Node"))
            {
                ActivationsStatus.Text = statusMsg;
            }
            else if (statusMsg.StartsWith("SystemTime"))
            {
                timeStatus.Text = statusMsg;
            }
            ServerLive = true;
        }

        private void UpdateMsg(string msg)
        {
            var msgType = msg.Substring(0, 1);

            switch (msgType)
            {
                case "?":
                    ProcessAnswerMsg(msg);
                    break;

                case "!":
                    ProcessBeliefMsg(msg);
                    break;

                case "$":
                    ProcessInferenceMsg(msg);
                    break;

                case ":":
                    ProcessStatusMsg(msg);
                    break;

                default:
                    ProcessCmdMsg(msg);
                    break;
            }
        }

        private void EchoCmd(string prefix, string msg)
        {
            cmdLogRTB.Text += prefix + msg + "\n";
            cmdLogRTB.SelectionStart = cmdLogRTB.Text.Length;
            cmdLogRTB.ScrollToCaret();
        }

        private void LoadSampleBtn_Click(object sender, EventArgs e)
        {
            EchoCmd(LoadingSampleStr, "");
            foreach (var str in Samples)
            {
                var data = Encoding.ASCII.GetBytes(str);
                outSocket.SendAsync(data, data.Length);
                EchoCmd("", str);
                Thread.Sleep(20);
            }
            EchoCmd(SampleLoadedStr, "");
        }

        private void SendTestToServer(string[] test)
        {
            foreach (var str in test)
            {
                var data = Encoding.ASCII.GetBytes(str);
                outSocket.SendAsync(data, data.Length);
                EchoCmd("", str);
                Thread.Sleep(20);
            }
        }

        private void CatBlueSkyTestBtn_Click(object sender, EventArgs e)
        {
            SendTestToServer(CatBlueSkyTest);
        }

        private void CatLikeCodTestBtn_Click(object sender, EventArgs e)
        {
            SendTestToServer(CatLikesCodTest);
        }

        private void ChainingTestBtn_Click(object sender, EventArgs e)
        {
            SendTestToServer(ChainingTest);
        }

        private void ShapeWorldTestBtn_Click(object sender, EventArgs e)
        {
            SendTestToServer(ShapeWorldTest);
        }

        private void MatchToSampleBtn_Click(object sender, EventArgs e)
        {
            SendTestToServer(MatchToSampleTest);
        }

        private void TransformFuncBtn_Click(object sender, EventArgs e)
        {
            SendTestToServer(TransformStimulusFuncTest);
        }

        private void SendCmdToServer(string cmd, params string[] list)
        {
            var cmdStr = cmd + (list.Length == 0 ? "" : list[0]);
            var data = Encoding.ASCII.GetBytes(cmdStr);
            outSocket.SendAsync(data, data.Length);
        }

        private void ShowSimpleBtn_Click(object sender, EventArgs e)
        {
            if(termNameTB.Text.Length > 0)
                SendCmdToServer("#SSB ", termNameTB.Text);
            else
                EchoCmd(ErrorPrefixStr, MissingNodeNameStr);
        }

        private void ShowTemporalBtn_Click(object sender, EventArgs e)
        {
            if (termNameTB.Text.Length > 0)
                SendCmdToServer("#STB ", termNameTB.Text);
            else
                EchoCmd(ErrorPrefixStr, MissingNodeNameStr);
        }

        private void ShowHypothesisBtn_Click(object sender, EventArgs e)
        {
            if (termNameTB.Text.Length > 0)
                SendCmdToServer("#SHB ", termNameTB.Text);
            else
                EchoCmd(ErrorPrefixStr, MissingNodeNameStr);
        }

        private void ShowGeneralisedBtn_Click(object sender, EventArgs e)
        {
            if (termNameTB.Text.Length > 0)
                SendCmdToServer("#SGB ", termNameTB.Text);
            else
                EchoCmd(ErrorPrefixStr, MissingNodeNameStr);
        }

        private void ShowGoalsBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#SGO ");
        }

        private void EnableTraceBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#ET ", termNameTB.Text);
        }

        private void DisableTraceBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#DT ", termNameTB.Text);
        }

        private void NodeCountBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#NC");
        }

        private void ShowNodeBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#SN ", termNameTB.Text);
        }

        private void LoadDataBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#L ", filenameTB.Text);
        }

        private void SaveDataBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#S ", filenameTB.Text);
        }

        private void PauseBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#P");
        }

        private void ContinueBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#C");
        }

        private void ResetBtn_Click(object sender, EventArgs e)
        {
            SendCmdToServer("#RESET");
        }

        private void ClearFormBtn_Click(object sender, EventArgs e)
        {
            answersRTB.Text = "";
            cmdLogRTB.Text = "";
            beliefsRTB.Text = "";
            inputRTB.Text = "";
            inferenceRTB.Text = "";
        }

        string[] CatBlueSkyTest =
        {
            "<{sky}-->[blue]>.",
            "<{tom}-->cat>.",
            "<({tom}*{sky})-->likes>.",
            "<(cat*[blue])-->likes>?"
        };


        string[] CatLikesCodTest =
        {
            "<cat --> animal>.",
            "<animal --> [living]>.",
            "<{tom} --> cat>.",
            "<(cat * fish) --> eats>.",
            "<cod --> fish>.",
            "<{tom} --> (eats / _ cod)>?",
            "<[living] --> (eats / _ cod)>?"
            };

        string[] ChainingTest =
        {
            "<a --> b>.",
            "<b --> c>.",
            "<c --> d>.",
            "<d --> e>.",
            "<e --> f>.",
            "<f --> g>.",
            "<g --> h>.",
            "<h --> i>.",
            "<a --> i>?"
            };


        string[] ShapeWorldTest =
        {
        // Hypotheses
        "<<($1 * $2) --> larger> ==> <($2 * $1) --> smaller>>.",
        "<<($1 * $2) --> smaller> ==> <($2 * $1) --> larger>>.",
        "<<($1 * $2) --> next_to> <=> <($2 * $1) --> next_to>>.",
        "<<($1 * $2) --> over> ==> <($2 * $1) --> under>>.",
        "<<($1 * $2) --> under> ==> <($2 * $1) --> over>>.",
        "<<($1 * $2) --> outside> ==> <($2 * $1) --> inside>>.",
        "<<($1 * $2) --> inside> ==> <($2 * $1) --> contains>>.",
        "<<($1 * $2) --> contains> ==> <($1 * $2) --> larger>>. ",
        "<<($1 * $2) --> on> ==> <($2 * $1) --> under>>.",
        "<<($1 * $2) --> inside> ==> <($2 * $1) --> contains>>.",
        "<(<#Y --> (on / $1 _)> && <#Y --> (on / _ $2)>) ==> <$1 --> (on / _ (on / _ $2))>>.",

        // Facts
        "<{toy} --> [red]>.",
        "<({box} * {floor}) --> on>.",
        "<({toy} * {box}) --> inside>.",
        "<({ball} * {box}) --> on>.",

        // Questions
        "<{?1} --> (on / _ {floor})>?",
        "<{?1} --> (on / {ball} _)>?",
        "<{?1} --> (under / _ {ball})>?",
        "<{?1} --> (contains / _ {toy})>?",
        "<{box} --> (larger / _ {toy})>?",
        "<{toy} --> (smaller / _ {box})>?",
        "<{ball} --> (on / _(on / _ {floor}))>?"
        };

        string[] MatchToSampleTest =
        {
            "<example1 --> (triangle * (left * up))>.",
            "<example1 --> (triangle * (right * up))>.",
            "<example1 --> (triangle * (left * down))>.",
            "<example1 --> (square * (right * down))>.",
            "<example2 --> (square * (left * up))>.",
            "<example2 --> (triangle * (right * up))>.",
            "<example2 --> (triangle * (left * down))>.",
            "<example2 --> (triangle * (right * down))>.",
            "<example3 --> (triangle * (left * up))>.",
            "<example3 --> (triangle * (right * up))>.",
            "<example3 --> (square * (left * down))>.",
            "<example3 --> (triangle * (right * down))>.",
            "<(* example4 _) --> (triangle * (left * up))>.",
            "<(* example4 _) --> (square * (right * up))>.",
            "<(* example4 _) --> (triangle * (left * down))>.",
            "<(* example4 _) --> (triangle * (right * down))>.",
            "<(* example5 _) --> (triangle * (left * up))>.",
            "<(* example5 _) --> (triangle * (right * up))>.",
            "<(* example5 _) --> (square * (left * down))>.",
            "<(* example5 _) --> (square * (right * down))>.",
            "<example1 --> example>.",
            "<example2 --> example>.",
            "<example3 --> example>.",
            "<(* ?what _) --> example>?"
        };

        string[] TransformStimulusFuncTest =
        {
            "<([bad] * [good]) --> opposite>.",
            "<juice --> [good]>.",
            "<\"Juice\" <-> juice>.",
            "<(\"Coffee\" * \"Juice\") --> opposite>.",
            "<(\"Beer\" * \"Juice\") --> better>.",
            "<(better / _ [good]) <-> [great]>.",
            "<\"Beer\" --> [great]>?",
            "<\"Coffee\" --> [bad]>?"
        };

        string[] Samples =
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
         "<cat --> animal>.",
         "<dog --> animal>.",
         "<cat --> [meows purrs]>.",
         "<cat --> [furry meows]>.",
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
         "<{john tom alan} --> boy>.",
         "<{rex} --> dog>.",
         "<{fido} --> dog>.",
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
         "<<({$1} * {$2}) --> inside> ==> <({$2} * {$1}) --> contains>>.",
         "<{tom} --> [living]>?",
         "<{tash} --> [living]>?",
         "<{lexie} --> [living]>?",
         "<{tweety} --> [living]>?",
         "<{tom} --> (eats / _ fish)>?",
         "<{tash} --> (eats / _ food)>?",
         "<{lexie} --> (eats / _ fruit)>?",
         "<{tweety} --> (eats / _ fish)>?",
         "<(bird * seed) --> eats>.",
         "<{tweety} --> (eats / _ seed)>?"
        };
    }
}

