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

namespace ALANNUI
{
    partial class Form1
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
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
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            this.inputRTB = new System.Windows.Forms.RichTextBox();
            this.toolStrip1 = new System.Windows.Forms.ToolStrip();
            this.enterBtn = new System.Windows.Forms.Button();
            this.cmdLogRTB = new System.Windows.Forms.RichTextBox();
            this.loadSampleBtn = new System.Windows.Forms.Button();
            this.termNameTB = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.showGeneralBtn = new System.Windows.Forms.Button();
            this.showTemporalBtn = new System.Windows.Forms.Button();
            this.beliefsRTB = new System.Windows.Forms.RichTextBox();
            this.answersRTB = new System.Windows.Forms.RichTextBox();
            this.label2 = new System.Windows.Forms.Label();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.nodeCountBtn = new System.Windows.Forms.Button();
            this.enableTraceBtn = new System.Windows.Forms.Button();
            this.disableTraceBtn = new System.Windows.Forms.Button();
            this.pauseBtn = new System.Windows.Forms.Button();
            this.continueBtn = new System.Windows.Forms.Button();
            this.stopBtn = new System.Windows.Forms.Button();
            this.loadDataBtn = new System.Windows.Forms.Button();
            this.saveDataBtn = new System.Windows.Forms.Button();
            this.filenameTB = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.nodeInfoBtn = new System.Windows.Forms.Button();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.showVariableBtn = new System.Windows.Forms.Button();
            this.ShowSuperBtn = new System.Windows.Forms.Button();
            this.groupBox4 = new System.Windows.Forms.GroupBox();
            this.statusStrip1 = new System.Windows.Forms.StatusStrip();
            this.timeStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.eventsPerSecondStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.ActivationsStatus = new System.Windows.Forms.ToolStripStatusLabel();
            this.statusLabel = new System.Windows.Forms.ToolStripStatusLabel();
            this.catBlueSkyTestBtn = new System.Windows.Forms.Button();
            this.catLikeCodTestBtn = new System.Windows.Forms.Button();
            this.chainingTestBtn = new System.Windows.Forms.Button();
            this.groupBox5 = new System.Windows.Forms.GroupBox();
            this.button2 = new System.Windows.Forms.Button();
            this.button1 = new System.Windows.Forms.Button();
            this.shapeWorldTestBtn = new System.Windows.Forms.Button();
            this.resetTooltip = new System.Windows.Forms.ToolTip(this.components);
            this.pauseTooltip = new System.Windows.Forms.ToolTip(this.components);
            this.continueTooltip = new System.Windows.Forms.ToolTip(this.components);
            this.clearFromBtn = new System.Windows.Forms.Button();
            this.inferenceRTB = new System.Windows.Forms.RichTextBox();
            this.label7 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.LoadtoolTip = new System.Windows.Forms.ToolTip(this.components);
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.groupBox4.SuspendLayout();
            this.statusStrip1.SuspendLayout();
            this.groupBox5.SuspendLayout();
            this.SuspendLayout();
            // 
            // inputRTB
            // 
            this.inputRTB.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.inputRTB.Location = new System.Drawing.Point(12, 59);
            this.inputRTB.Name = "inputRTB";
            this.inputRTB.Size = new System.Drawing.Size(412, 162);
            this.inputRTB.TabIndex = 0;
            this.inputRTB.Text = "";
            // 
            // toolStrip1
            // 
            this.toolStrip1.Location = new System.Drawing.Point(0, 0);
            this.toolStrip1.Name = "toolStrip1";
            this.toolStrip1.Size = new System.Drawing.Size(1099, 25);
            this.toolStrip1.TabIndex = 1;
            this.toolStrip1.Text = "toolStrip1";
            // 
            // enterBtn
            // 
            this.enterBtn.Location = new System.Drawing.Point(309, 15);
            this.enterBtn.Name = "enterBtn";
            this.enterBtn.Size = new System.Drawing.Size(115, 36);
            this.enterBtn.TabIndex = 2;
            this.enterBtn.Text = "Input";
            this.enterBtn.UseVisualStyleBackColor = true;
            this.enterBtn.Click += new System.EventHandler(this.InputBtn_Click);
            // 
            // cmdLogRTB
            // 
            this.cmdLogRTB.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.cmdLogRTB.Location = new System.Drawing.Point(12, 250);
            this.cmdLogRTB.Name = "cmdLogRTB";
            this.cmdLogRTB.Size = new System.Drawing.Size(537, 211);
            this.cmdLogRTB.TabIndex = 3;
            this.cmdLogRTB.Text = "";
            this.cmdLogRTB.WordWrap = false;
            // 
            // loadSampleBtn
            // 
            this.loadSampleBtn.Location = new System.Drawing.Point(61, 51);
            this.loadSampleBtn.Name = "loadSampleBtn";
            this.loadSampleBtn.Size = new System.Drawing.Size(99, 23);
            this.loadSampleBtn.TabIndex = 4;
            this.loadSampleBtn.Text = "Load Sample";
            this.loadSampleBtn.UseVisualStyleBackColor = true;
            this.loadSampleBtn.Click += new System.EventHandler(this.LoadSampleBtn_Click);
            // 
            // termNameTB
            // 
            this.termNameTB.Location = new System.Drawing.Point(79, 24);
            this.termNameTB.Name = "termNameTB";
            this.termNameTB.Size = new System.Drawing.Size(264, 20);
            this.termNameTB.TabIndex = 5;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(13, 27);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(60, 13);
            this.label1.TabIndex = 6;
            this.label1.Text = "Node Term";
            // 
            // showGeneralBtn
            // 
            this.showGeneralBtn.Location = new System.Drawing.Point(10, 20);
            this.showGeneralBtn.Name = "showGeneralBtn";
            this.showGeneralBtn.Size = new System.Drawing.Size(105, 23);
            this.showGeneralBtn.TabIndex = 0;
            this.showGeneralBtn.Text = "Show Simple";
            this.showGeneralBtn.UseVisualStyleBackColor = true;
            this.showGeneralBtn.Click += new System.EventHandler(this.ShowSimpleBtn_Click);
            // 
            // showTemporalBtn
            // 
            this.showTemporalBtn.Location = new System.Drawing.Point(10, 49);
            this.showTemporalBtn.Name = "showTemporalBtn";
            this.showTemporalBtn.Size = new System.Drawing.Size(105, 23);
            this.showTemporalBtn.TabIndex = 9;
            this.showTemporalBtn.Text = "Show Temporal";
            this.showTemporalBtn.UseVisualStyleBackColor = true;
            this.showTemporalBtn.Click += new System.EventHandler(this.ShowTemporalBtn_Click);
            // 
            // beliefsRTB
            // 
            this.beliefsRTB.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.beliefsRTB.Location = new System.Drawing.Point(565, 250);
            this.beliefsRTB.Name = "beliefsRTB";
            this.beliefsRTB.Size = new System.Drawing.Size(522, 507);
            this.beliefsRTB.TabIndex = 10;
            this.beliefsRTB.Text = "";
            this.beliefsRTB.WordWrap = false;
            // 
            // answersRTB
            // 
            this.answersRTB.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.answersRTB.Location = new System.Drawing.Point(12, 488);
            this.answersRTB.Name = "answersRTB";
            this.answersRTB.Size = new System.Drawing.Size(537, 128);
            this.answersRTB.TabIndex = 11;
            this.answersRTB.Text = "";
            this.answersRTB.WordWrap = false;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label2.Location = new System.Drawing.Point(12, 467);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(60, 15);
            this.label2.TabIndex = 12;
            this.label2.Text = "Answers";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label3.Location = new System.Drawing.Point(12, 38);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(39, 15);
            this.label3.TabIndex = 13;
            this.label3.Text = "Input";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label4.Location = new System.Drawing.Point(12, 229);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(100, 15);
            this.label4.TabIndex = 14;
            this.label4.Text = "Command Log";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(678, 186);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(38, 13);
            this.label5.TabIndex = 15;
            this.label5.Text = "Beliefs";
            // 
            // nodeCountBtn
            // 
            this.nodeCountBtn.Location = new System.Drawing.Point(6, 20);
            this.nodeCountBtn.Name = "nodeCountBtn";
            this.nodeCountBtn.Size = new System.Drawing.Size(99, 23);
            this.nodeCountBtn.TabIndex = 16;
            this.nodeCountBtn.Text = "Node Count";
            this.nodeCountBtn.UseVisualStyleBackColor = true;
            this.nodeCountBtn.Click += new System.EventHandler(this.NodeCountBtn_Click);
            // 
            // enableTraceBtn
            // 
            this.enableTraceBtn.Location = new System.Drawing.Point(6, 78);
            this.enableTraceBtn.Name = "enableTraceBtn";
            this.enableTraceBtn.Size = new System.Drawing.Size(99, 23);
            this.enableTraceBtn.TabIndex = 17;
            this.enableTraceBtn.Text = "Enable Trace";
            this.enableTraceBtn.UseVisualStyleBackColor = true;
            this.enableTraceBtn.Click += new System.EventHandler(this.EnableTraceBtn_Click);
            // 
            // disableTraceBtn
            // 
            this.disableTraceBtn.Location = new System.Drawing.Point(6, 108);
            this.disableTraceBtn.Name = "disableTraceBtn";
            this.disableTraceBtn.Size = new System.Drawing.Size(99, 23);
            this.disableTraceBtn.TabIndex = 18;
            this.disableTraceBtn.Text = "Disable Trace";
            this.disableTraceBtn.UseVisualStyleBackColor = true;
            this.disableTraceBtn.Click += new System.EventHandler(this.DisableTraceBtn_Click);
            // 
            // pauseBtn
            // 
            this.pauseBtn.Font = new System.Drawing.Font("Webdings", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(2)));
            this.pauseBtn.Location = new System.Drawing.Point(111, 15);
            this.pauseBtn.Name = "pauseBtn";
            this.pauseBtn.Size = new System.Drawing.Size(32, 36);
            this.pauseBtn.TabIndex = 19;
            this.pauseBtn.Text = ";";
            this.pauseTooltip.SetToolTip(this.pauseBtn, "Pause system");
            this.pauseBtn.UseVisualStyleBackColor = true;
            this.pauseBtn.Click += new System.EventHandler(this.PauseBtn_Click);
            // 
            // continueBtn
            // 
            this.continueBtn.Font = new System.Drawing.Font("Webdings", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(2)));
            this.continueBtn.Location = new System.Drawing.Point(73, 15);
            this.continueBtn.Name = "continueBtn";
            this.continueBtn.Size = new System.Drawing.Size(32, 36);
            this.continueBtn.TabIndex = 19;
            this.continueBtn.Text = "4";
            this.continueTooltip.SetToolTip(this.continueBtn, "Continue system processing");
            this.continueBtn.UseVisualStyleBackColor = true;
            this.continueBtn.Click += new System.EventHandler(this.ContinueBtn_Click);
            // 
            // stopBtn
            // 
            this.stopBtn.Font = new System.Drawing.Font("Webdings", 16F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(2)));
            this.stopBtn.Location = new System.Drawing.Point(149, 15);
            this.stopBtn.Name = "stopBtn";
            this.stopBtn.Size = new System.Drawing.Size(32, 36);
            this.stopBtn.TabIndex = 19;
            this.stopBtn.Text = "<";
            this.resetTooltip.SetToolTip(this.stopBtn, "Reset memory and all streams");
            this.stopBtn.UseVisualStyleBackColor = true;
            this.stopBtn.Click += new System.EventHandler(this.ResetBtn_Click);
            // 
            // loadDataBtn
            // 
            this.loadDataBtn.Location = new System.Drawing.Point(177, 20);
            this.loadDataBtn.Name = "loadDataBtn";
            this.loadDataBtn.Size = new System.Drawing.Size(99, 23);
            this.loadDataBtn.TabIndex = 20;
            this.loadDataBtn.Text = "Load Memory";
            this.LoadtoolTip.SetToolTip(this.loadDataBtn, "Overwrite exisitng memory with file contents\r\n");
            this.loadDataBtn.UseVisualStyleBackColor = true;
            this.loadDataBtn.Click += new System.EventHandler(this.LoadDataBtn_Click);
            // 
            // saveDataBtn
            // 
            this.saveDataBtn.Location = new System.Drawing.Point(177, 51);
            this.saveDataBtn.Name = "saveDataBtn";
            this.saveDataBtn.Size = new System.Drawing.Size(99, 23);
            this.saveDataBtn.TabIndex = 21;
            this.saveDataBtn.Text = "Save Memory";
            this.saveDataBtn.UseVisualStyleBackColor = true;
            this.saveDataBtn.Click += new System.EventHandler(this.SaveDataBtn_Click);
            // 
            // filenameTB
            // 
            this.filenameTB.Location = new System.Drawing.Point(61, 22);
            this.filenameTB.Name = "filenameTB";
            this.filenameTB.Size = new System.Drawing.Size(100, 20);
            this.filenameTB.TabIndex = 22;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(6, 25);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(49, 13);
            this.label6.TabIndex = 23;
            this.label6.Text = "Filename";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.nodeInfoBtn);
            this.groupBox1.Controls.Add(this.enableTraceBtn);
            this.groupBox1.Controls.Add(this.nodeCountBtn);
            this.groupBox1.Controls.Add(this.disableTraceBtn);
            this.groupBox1.Location = new System.Drawing.Point(230, 51);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(113, 137);
            this.groupBox1.TabIndex = 24;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Node Commands";
            // 
            // nodeInfoBtn
            // 
            this.nodeInfoBtn.Location = new System.Drawing.Point(6, 49);
            this.nodeInfoBtn.Name = "nodeInfoBtn";
            this.nodeInfoBtn.Size = new System.Drawing.Size(99, 23);
            this.nodeInfoBtn.TabIndex = 19;
            this.nodeInfoBtn.Text = "Node Info";
            this.nodeInfoBtn.UseVisualStyleBackColor = true;
            this.nodeInfoBtn.Click += new System.EventHandler(this.ShowNodeBtn_Click);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.label6);
            this.groupBox2.Controls.Add(this.loadDataBtn);
            this.groupBox2.Controls.Add(this.saveDataBtn);
            this.groupBox2.Controls.Add(this.filenameTB);
            this.groupBox2.Controls.Add(this.loadSampleBtn);
            this.groupBox2.Location = new System.Drawing.Point(430, 15);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(286, 85);
            this.groupBox2.TabIndex = 25;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Data Commands";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.showVariableBtn);
            this.groupBox3.Controls.Add(this.ShowSuperBtn);
            this.groupBox3.Controls.Add(this.showGeneralBtn);
            this.groupBox3.Controls.Add(this.showTemporalBtn);
            this.groupBox3.Location = new System.Drawing.Point(103, 51);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(121, 137);
            this.groupBox3.TabIndex = 26;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Belief Commands";
            // 
            // showVariableBtn
            // 
            this.showVariableBtn.Location = new System.Drawing.Point(10, 108);
            this.showVariableBtn.Name = "showVariableBtn";
            this.showVariableBtn.Size = new System.Drawing.Size(105, 23);
            this.showVariableBtn.TabIndex = 27;
            this.showVariableBtn.Text = "Show Generalised";
            this.showVariableBtn.UseVisualStyleBackColor = true;
            this.showVariableBtn.Click += new System.EventHandler(this.ShowGeneralisedBtn_Click);
            // 
            // ShowSuperBtn
            // 
            this.ShowSuperBtn.Location = new System.Drawing.Point(11, 79);
            this.ShowSuperBtn.Name = "ShowSuperBtn";
            this.ShowSuperBtn.Size = new System.Drawing.Size(104, 23);
            this.ShowSuperBtn.TabIndex = 10;
            this.ShowSuperBtn.Text = "Show Hypotheses";
            this.ShowSuperBtn.UseVisualStyleBackColor = true;
            this.ShowSuperBtn.Click += new System.EventHandler(this.ShowHypothesisBtn_Click);
            // 
            // groupBox4
            // 
            this.groupBox4.Controls.Add(this.termNameTB);
            this.groupBox4.Controls.Add(this.groupBox3);
            this.groupBox4.Controls.Add(this.groupBox1);
            this.groupBox4.Controls.Add(this.label1);
            this.groupBox4.Location = new System.Drawing.Point(722, 15);
            this.groupBox4.Name = "groupBox4";
            this.groupBox4.Size = new System.Drawing.Size(365, 206);
            this.groupBox4.TabIndex = 27;
            this.groupBox4.TabStop = false;
            this.groupBox4.Text = "Commands Requiring a Node";
            // 
            // statusStrip1
            // 
            this.statusStrip1.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.statusStrip1.Font = new System.Drawing.Font("Segoe UI", 9F, System.Drawing.FontStyle.Bold);
            this.statusStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.timeStatus,
            this.eventsPerSecondStatus,
            this.ActivationsStatus,
            this.statusLabel});
            this.statusStrip1.Location = new System.Drawing.Point(0, 774);
            this.statusStrip1.Name = "statusStrip1";
            this.statusStrip1.Size = new System.Drawing.Size(1099, 22);
            this.statusStrip1.TabIndex = 28;
            this.statusStrip1.Text = "statusStrip1";
            // 
            // timeStatus
            // 
            this.timeStatus.Name = "timeStatus";
            this.timeStatus.Size = new System.Drawing.Size(105, 17);
            this.timeStatus.Text = "SystemTime = [0]";
            // 
            // eventsPerSecondStatus
            // 
            this.eventsPerSecondStatus.Name = "eventsPerSecondStatus";
            this.eventsPerSecondStatus.Size = new System.Drawing.Size(88, 17);
            this.eventsPerSecondStatus.Text = "Events: [0]/sec";
            // 
            // ActivationsStatus
            // 
            this.ActivationsStatus.Name = "ActivationsStatus";
            this.ActivationsStatus.Size = new System.Drawing.Size(122, 17);
            this.ActivationsStatus.Text = "Node Activations 0/s";
            // 
            // statusLabel
            // 
            this.statusLabel.Name = "statusLabel";
            this.statusLabel.Size = new System.Drawing.Size(201, 17);
            this.statusLabel.Text = "Status: ALANN Server Not  Started";
            // 
            // catBlueSkyTestBtn
            // 
            this.catBlueSkyTestBtn.Location = new System.Drawing.Point(61, 19);
            this.catBlueSkyTestBtn.Name = "catBlueSkyTestBtn";
            this.catBlueSkyTestBtn.Size = new System.Drawing.Size(99, 23);
            this.catBlueSkyTestBtn.TabIndex = 24;
            this.catBlueSkyTestBtn.Text = "Cat-Blue-Sky";
            this.catBlueSkyTestBtn.UseVisualStyleBackColor = true;
            this.catBlueSkyTestBtn.Click += new System.EventHandler(this.CatBlueSkyTestBtn_Click);
            // 
            // catLikeCodTestBtn
            // 
            this.catLikeCodTestBtn.Location = new System.Drawing.Point(176, 19);
            this.catLikeCodTestBtn.Name = "catLikeCodTestBtn";
            this.catLikeCodTestBtn.Size = new System.Drawing.Size(100, 23);
            this.catLikeCodTestBtn.TabIndex = 25;
            this.catLikeCodTestBtn.Text = "Cat-Likes-Cod";
            this.catLikeCodTestBtn.UseVisualStyleBackColor = true;
            this.catLikeCodTestBtn.Click += new System.EventHandler(this.CatLikeCodTestBtn_Click);
            // 
            // chainingTestBtn
            // 
            this.chainingTestBtn.Location = new System.Drawing.Point(61, 48);
            this.chainingTestBtn.Name = "chainingTestBtn";
            this.chainingTestBtn.Size = new System.Drawing.Size(99, 23);
            this.chainingTestBtn.TabIndex = 26;
            this.chainingTestBtn.Text = "a --> i Chaining";
            this.chainingTestBtn.UseVisualStyleBackColor = true;
            this.chainingTestBtn.Click += new System.EventHandler(this.ChainingTestBtn_Click);
            // 
            // groupBox5
            // 
            this.groupBox5.Controls.Add(this.button2);
            this.groupBox5.Controls.Add(this.button1);
            this.groupBox5.Controls.Add(this.shapeWorldTestBtn);
            this.groupBox5.Controls.Add(this.catBlueSkyTestBtn);
            this.groupBox5.Controls.Add(this.chainingTestBtn);
            this.groupBox5.Controls.Add(this.catLikeCodTestBtn);
            this.groupBox5.Location = new System.Drawing.Point(430, 107);
            this.groupBox5.Name = "groupBox5";
            this.groupBox5.Size = new System.Drawing.Size(286, 114);
            this.groupBox5.TabIndex = 29;
            this.groupBox5.TabStop = false;
            this.groupBox5.Text = "Standard Tests";
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(176, 79);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(100, 23);
            this.button2.TabIndex = 29;
            this.button2.Text = "button2";
            this.button2.UseVisualStyleBackColor = true;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(61, 79);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(99, 23);
            this.button1.TabIndex = 28;
            this.button1.Text = "button1";
            this.button1.UseVisualStyleBackColor = true;
            // 
            // shapeWorldTestBtn
            // 
            this.shapeWorldTestBtn.Location = new System.Drawing.Point(176, 48);
            this.shapeWorldTestBtn.Name = "shapeWorldTestBtn";
            this.shapeWorldTestBtn.Size = new System.Drawing.Size(100, 23);
            this.shapeWorldTestBtn.TabIndex = 27;
            this.shapeWorldTestBtn.Text = "Shape World";
            this.shapeWorldTestBtn.UseVisualStyleBackColor = true;
            this.shapeWorldTestBtn.Click += new System.EventHandler(this.ShapeWorldTestBtn_Click);
            // 
            // clearFromBtn
            // 
            this.clearFromBtn.Location = new System.Drawing.Point(187, 15);
            this.clearFromBtn.Name = "clearFromBtn";
            this.clearFromBtn.Size = new System.Drawing.Size(116, 36);
            this.clearFromBtn.TabIndex = 30;
            this.clearFromBtn.Text = "Clear Form";
            this.clearFromBtn.UseVisualStyleBackColor = true;
            this.clearFromBtn.Click += new System.EventHandler(this.ClearFormBtn_Click);
            // 
            // inferenceRTB
            // 
            this.inferenceRTB.Font = new System.Drawing.Font("Microsoft Sans Serif", 10F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.inferenceRTB.Location = new System.Drawing.Point(13, 641);
            this.inferenceRTB.Name = "inferenceRTB";
            this.inferenceRTB.Size = new System.Drawing.Size(536, 116);
            this.inferenceRTB.TabIndex = 31;
            this.inferenceRTB.Text = "";
            this.inferenceRTB.WordWrap = false;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label7.Location = new System.Drawing.Point(15, 620);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(127, 15);
            this.label7.TabIndex = 32;
            this.label7.Text = "Inference Samples";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Font = new System.Drawing.Font("Microsoft Sans Serif", 9F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label8.Location = new System.Drawing.Point(565, 228);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(51, 15);
            this.label8.TabIndex = 33;
            this.label8.Text = "Beliefs";
            // 
            // Form1
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Control;
            this.ClientSize = new System.Drawing.Size(1099, 796);
            this.Controls.Add(this.label8);
            this.Controls.Add(this.label7);
            this.Controls.Add(this.inferenceRTB);
            this.Controls.Add(this.clearFromBtn);
            this.Controls.Add(this.groupBox5);
            this.Controls.Add(this.statusStrip1);
            this.Controls.Add(this.groupBox4);
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.stopBtn);
            this.Controls.Add(this.continueBtn);
            this.Controls.Add(this.pauseBtn);
            this.Controls.Add(this.label5);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.answersRTB);
            this.Controls.Add(this.beliefsRTB);
            this.Controls.Add(this.cmdLogRTB);
            this.Controls.Add(this.enterBtn);
            this.Controls.Add(this.toolStrip1);
            this.Controls.Add(this.inputRTB);
            this.Name = "Form1";
            this.Text = "A.L.A.N.N. Client GUI";
            this.resetTooltip.SetToolTip(this, "Reset memory and all streams");
            this.groupBox1.ResumeLayout(false);
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox4.ResumeLayout(false);
            this.groupBox4.PerformLayout();
            this.statusStrip1.ResumeLayout(false);
            this.statusStrip1.PerformLayout();
            this.groupBox5.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.RichTextBox inputRTB;
        private System.Windows.Forms.ToolStrip toolStrip1;
        private System.Windows.Forms.Button enterBtn;
        private System.Windows.Forms.RichTextBox cmdLogRTB;
        private System.Windows.Forms.Button loadSampleBtn;
        private System.Windows.Forms.TextBox termNameTB;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Button showGeneralBtn;
        private System.Windows.Forms.Button showTemporalBtn;
        private System.Windows.Forms.RichTextBox beliefsRTB;
        private System.Windows.Forms.RichTextBox answersRTB;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Button nodeCountBtn;
        private System.Windows.Forms.Button enableTraceBtn;
        private System.Windows.Forms.Button disableTraceBtn;
        private System.Windows.Forms.Button pauseBtn;
        private System.Windows.Forms.Button continueBtn;
        private System.Windows.Forms.Button stopBtn;
        private System.Windows.Forms.Button loadDataBtn;
        private System.Windows.Forms.Button saveDataBtn;
        private System.Windows.Forms.TextBox filenameTB;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.GroupBox groupBox4;
        private System.Windows.Forms.Button nodeInfoBtn;
        private System.Windows.Forms.StatusStrip statusStrip1;
        private System.Windows.Forms.ToolStripStatusLabel statusLabel;
        private System.Windows.Forms.Button catBlueSkyTestBtn;
        private System.Windows.Forms.Button catLikeCodTestBtn;
        private System.Windows.Forms.Button chainingTestBtn;
        private System.Windows.Forms.GroupBox groupBox5;
        private System.Windows.Forms.Button shapeWorldTestBtn;
        private System.Windows.Forms.ToolTip resetTooltip;
        private System.Windows.Forms.ToolTip pauseTooltip;
        private System.Windows.Forms.ToolTip continueTooltip;
        private System.Windows.Forms.Button clearFromBtn;
        private System.Windows.Forms.ToolStripStatusLabel eventsPerSecondStatus;
        private System.Windows.Forms.RichTextBox inferenceRTB;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.ToolStripStatusLabel ActivationsStatus;
        private System.Windows.Forms.ToolStripStatusLabel timeStatus;
        private System.Windows.Forms.ToolTip LoadtoolTip;
        private System.Windows.Forms.Button ShowSuperBtn;
        private System.Windows.Forms.Button showVariableBtn;
        private System.Windows.Forms.Button button2;
        private System.Windows.Forms.Button button1;
    }
}

