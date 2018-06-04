<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class GUI
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbLearningRate = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbMomentum = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbType = New System.Windows.Forms.ComboBox()
        Me.cbSize = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.cbEpoch = New System.Windows.Forms.ComboBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.CheckRnd = New System.Windows.Forms.CheckBox()
        Me.MP = New System.Windows.Forms.TabControl()
        Me.Tab1 = New System.Windows.Forms.TabPage()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.tbError = New System.Windows.Forms.TextBox()
        Me.tbOutput = New System.Windows.Forms.TextBox()
        Me.tbTarget = New System.Windows.Forms.TextBox()
        Me.tbInput = New System.Windows.Forms.TextBox()
        Me.cbGroupSel = New System.Windows.Forms.ComboBox()
        Me.tbStatus = New System.Windows.Forms.TextBox()
        Me.bView = New System.Windows.Forms.Button()
        Me.BTrain = New System.Windows.Forms.Button()
        Me.bLoad = New System.Windows.Forms.Button()
        Me.bSave = New System.Windows.Forms.Button()
        Me.bCalculate = New System.Windows.Forms.Button()
        Me.BExecute = New System.Windows.Forms.Button()
        Me.bAddData = New System.Windows.Forms.Button()
        Me.bReset = New System.Windows.Forms.Button()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.tbGlobalError = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.tbDelta = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.tbDebugOutput = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.tbDebugInput = New System.Windows.Forms.TextBox()
        Me.Tab2 = New System.Windows.Forms.TabPage()
        Me.tbInfo = New System.Windows.Forms.TextBox()
        Me.tbNet = New System.Windows.Forms.ComboBox()
        Me.bSaveStructure = New System.Windows.Forms.Button()
        Me.MP.SuspendLayout()
        Me.Tab1.SuspendLayout()
        Me.Tab2.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(7, 11)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(109, 16)
        Me.Label2.TabIndex = 5
        Me.Label2.Text = "Learning Rate:"
        '
        'cbLearningRate
        '
        Me.cbLearningRate.BackColor = System.Drawing.Color.Gainsboro
        Me.cbLearningRate.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbLearningRate.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbLearningRate.ForeColor = System.Drawing.Color.Blue
        Me.cbLearningRate.FormattingEnabled = True
        Me.cbLearningRate.Items.AddRange(New Object() {"0.01", "0.05", "0.08", "0.1", "0.12", "0.15", "0.2", "0.25", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1", "1.1", "1.2", "1.5", "2"})
        Me.cbLearningRate.Location = New System.Drawing.Point(10, 26)
        Me.cbLearningRate.Name = "cbLearningRate"
        Me.cbLearningRate.Size = New System.Drawing.Size(124, 21)
        Me.cbLearningRate.TabIndex = 6
        Me.cbLearningRate.Text = "0.15"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(141, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(86, 16)
        Me.Label1.TabIndex = 7
        Me.Label1.Text = "Momentum:"
        '
        'cbMomentum
        '
        Me.cbMomentum.BackColor = System.Drawing.Color.Gainsboro
        Me.cbMomentum.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbMomentum.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbMomentum.ForeColor = System.Drawing.Color.Blue
        Me.cbMomentum.FormattingEnabled = True
        Me.cbMomentum.Items.AddRange(New Object() {"0.01", "0.05", "0.1", "0.12", "0.15", "0.2", "0.25", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1", "1.1", "1.2", "1.5", "2"})
        Me.cbMomentum.Location = New System.Drawing.Point(144, 26)
        Me.cbMomentum.Name = "cbMomentum"
        Me.cbMomentum.Size = New System.Drawing.Size(124, 21)
        Me.cbMomentum.TabIndex = 8
        Me.cbMomentum.Text = "0.75"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(7, 49)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(103, 16)
        Me.Label3.TabIndex = 9
        Me.Label3.Text = "Update Type:"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(141, 50)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(117, 16)
        Me.Label4.TabIndex = 10
        Me.Label4.Text = "Mini Batch Size:"
        '
        'cbType
        '
        Me.cbType.BackColor = System.Drawing.Color.Gainsboro
        Me.cbType.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbType.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbType.ForeColor = System.Drawing.Color.Blue
        Me.cbType.FormattingEnabled = True
        Me.cbType.Items.AddRange(New Object() {"Stochastic", "Mini-Batch", "Batch"})
        Me.cbType.Location = New System.Drawing.Point(10, 64)
        Me.cbType.Name = "cbType"
        Me.cbType.Size = New System.Drawing.Size(124, 21)
        Me.cbType.TabIndex = 11
        Me.cbType.Text = "Stochastic"
        '
        'cbSize
        '
        Me.cbSize.BackColor = System.Drawing.Color.Gainsboro
        Me.cbSize.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbSize.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbSize.ForeColor = System.Drawing.Color.Blue
        Me.cbSize.FormattingEnabled = True
        Me.cbSize.Items.AddRange(New Object() {"2", "4", "8", "16", "32", "64", "128", "256", "512", "1024", "2048", "4096", "8192", "16384"})
        Me.cbSize.Location = New System.Drawing.Point(144, 64)
        Me.cbSize.Name = "cbSize"
        Me.cbSize.Size = New System.Drawing.Size(124, 21)
        Me.cbSize.TabIndex = 12
        Me.cbSize.Text = "2"
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label5.Location = New System.Drawing.Point(7, 87)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(64, 16)
        Me.Label5.TabIndex = 13
        Me.Label5.Text = "Epochs:"
        '
        'cbEpoch
        '
        Me.cbEpoch.BackColor = System.Drawing.Color.Gainsboro
        Me.cbEpoch.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbEpoch.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbEpoch.ForeColor = System.Drawing.Color.Blue
        Me.cbEpoch.FormattingEnabled = True
        Me.cbEpoch.Items.AddRange(New Object() {"1", "5", "10", "50", "100", "500", "1000", "5000", "10000", "50000", "100000", "500000", "1000000"})
        Me.cbEpoch.Location = New System.Drawing.Point(10, 102)
        Me.cbEpoch.Name = "cbEpoch"
        Me.cbEpoch.Size = New System.Drawing.Size(124, 21)
        Me.cbEpoch.TabIndex = 14
        Me.cbEpoch.Text = "100"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label6.Location = New System.Drawing.Point(274, 11)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(183, 16)
        Me.Label6.TabIndex = 15
        Me.Label6.Text = "Neural Network Structure:"
        '
        'CheckRnd
        '
        Me.CheckRnd.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.CheckRnd.Location = New System.Drawing.Point(145, 92)
        Me.CheckRnd.Name = "CheckRnd"
        Me.CheckRnd.Size = New System.Drawing.Size(133, 31)
        Me.CheckRnd.TabIndex = 16
        Me.CheckRnd.Text = "Random Deactive Neuron"
        Me.CheckRnd.TextAlign = System.Drawing.ContentAlignment.BottomCenter
        Me.CheckRnd.UseVisualStyleBackColor = True
        '
        'MP
        '
        Me.MP.Controls.Add(Me.Tab1)
        Me.MP.Controls.Add(Me.Tab2)
        Me.MP.Location = New System.Drawing.Point(12, 1)
        Me.MP.Name = "MP"
        Me.MP.SelectedIndex = 0
        Me.MP.Size = New System.Drawing.Size(783, 415)
        Me.MP.TabIndex = 17
        '
        'Tab1
        '
        Me.Tab1.BackColor = System.Drawing.Color.LightCyan
        Me.Tab1.Controls.Add(Me.bSaveStructure)
        Me.Tab1.Controls.Add(Me.tbNet)
        Me.Tab1.Controls.Add(Me.Label15)
        Me.Tab1.Controls.Add(Me.Label14)
        Me.Tab1.Controls.Add(Me.Label13)
        Me.Tab1.Controls.Add(Me.Label12)
        Me.Tab1.Controls.Add(Me.Label11)
        Me.Tab1.Controls.Add(Me.tbError)
        Me.Tab1.Controls.Add(Me.tbOutput)
        Me.Tab1.Controls.Add(Me.tbTarget)
        Me.Tab1.Controls.Add(Me.tbInput)
        Me.Tab1.Controls.Add(Me.cbGroupSel)
        Me.Tab1.Controls.Add(Me.tbStatus)
        Me.Tab1.Controls.Add(Me.bView)
        Me.Tab1.Controls.Add(Me.BTrain)
        Me.Tab1.Controls.Add(Me.bLoad)
        Me.Tab1.Controls.Add(Me.bSave)
        Me.Tab1.Controls.Add(Me.bCalculate)
        Me.Tab1.Controls.Add(Me.BExecute)
        Me.Tab1.Controls.Add(Me.bAddData)
        Me.Tab1.Controls.Add(Me.bReset)
        Me.Tab1.Controls.Add(Me.Label10)
        Me.Tab1.Controls.Add(Me.tbGlobalError)
        Me.Tab1.Controls.Add(Me.Label9)
        Me.Tab1.Controls.Add(Me.tbDelta)
        Me.Tab1.Controls.Add(Me.Label8)
        Me.Tab1.Controls.Add(Me.tbDebugOutput)
        Me.Tab1.Controls.Add(Me.Label7)
        Me.Tab1.Controls.Add(Me.tbDebugInput)
        Me.Tab1.Controls.Add(Me.Label2)
        Me.Tab1.Controls.Add(Me.CheckRnd)
        Me.Tab1.Controls.Add(Me.Label6)
        Me.Tab1.Controls.Add(Me.cbLearningRate)
        Me.Tab1.Controls.Add(Me.cbEpoch)
        Me.Tab1.Controls.Add(Me.Label1)
        Me.Tab1.Controls.Add(Me.Label5)
        Me.Tab1.Controls.Add(Me.cbMomentum)
        Me.Tab1.Controls.Add(Me.cbSize)
        Me.Tab1.Controls.Add(Me.Label3)
        Me.Tab1.Controls.Add(Me.cbType)
        Me.Tab1.Controls.Add(Me.Label4)
        Me.Tab1.Location = New System.Drawing.Point(4, 22)
        Me.Tab1.Name = "Tab1"
        Me.Tab1.Padding = New System.Windows.Forms.Padding(3)
        Me.Tab1.Size = New System.Drawing.Size(775, 389)
        Me.Tab1.TabIndex = 0
        Me.Tab1.Text = "Setup"
        '
        'Label15
        '
        Me.Label15.AutoSize = True
        Me.Label15.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label15.Location = New System.Drawing.Point(421, 177)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(42, 16)
        Me.Label15.TabIndex = 51
        Me.Label15.Text = "Error"
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label14.Location = New System.Drawing.Point(275, 177)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(52, 16)
        Me.Label14.TabIndex = 50
        Me.Label14.Text = "Output"
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label13.Location = New System.Drawing.Point(142, 177)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(54, 16)
        Me.Label13.TabIndex = 49
        Me.Label13.Text = "Target"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label12.Location = New System.Drawing.Point(8, 177)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(41, 16)
        Me.Label12.TabIndex = 48
        Me.Label12.Text = "Input"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label11.Location = New System.Drawing.Point(8, 142)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(100, 16)
        Me.Label11.TabIndex = 47
        Me.Label11.Text = "Trainning Set"
        '
        'tbError
        '
        Me.tbError.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbError.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbError.Location = New System.Drawing.Point(424, 193)
        Me.tbError.Multiline = True
        Me.tbError.Name = "tbError"
        Me.tbError.ReadOnly = True
        Me.tbError.Size = New System.Drawing.Size(137, 155)
        Me.tbError.TabIndex = 46
        '
        'tbOutput
        '
        Me.tbOutput.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbOutput.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbOutput.Location = New System.Drawing.Point(277, 193)
        Me.tbOutput.Multiline = True
        Me.tbOutput.Name = "tbOutput"
        Me.tbOutput.ReadOnly = True
        Me.tbOutput.Size = New System.Drawing.Size(141, 155)
        Me.tbOutput.TabIndex = 45
        '
        'tbTarget
        '
        Me.tbTarget.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbTarget.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbTarget.Location = New System.Drawing.Point(145, 193)
        Me.tbTarget.Multiline = True
        Me.tbTarget.Name = "tbTarget"
        Me.tbTarget.ReadOnly = True
        Me.tbTarget.Size = New System.Drawing.Size(127, 155)
        Me.tbTarget.TabIndex = 44
        '
        'tbInput
        '
        Me.tbInput.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbInput.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbInput.Location = New System.Drawing.Point(11, 193)
        Me.tbInput.Multiline = True
        Me.tbInput.Name = "tbInput"
        Me.tbInput.ReadOnly = True
        Me.tbInput.Size = New System.Drawing.Size(127, 155)
        Me.tbInput.TabIndex = 43
        '
        'cbGroupSel
        '
        Me.cbGroupSel.BackColor = System.Drawing.Color.Gainsboro
        Me.cbGroupSel.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbGroupSel.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbGroupSel.ForeColor = System.Drawing.Color.Blue
        Me.cbGroupSel.FormattingEnabled = True
        Me.cbGroupSel.Items.AddRange(New Object() {"Train", "Validate", "Check", "Predict"})
        Me.cbGroupSel.Location = New System.Drawing.Point(11, 157)
        Me.cbGroupSel.Name = "cbGroupSel"
        Me.cbGroupSel.Size = New System.Drawing.Size(550, 21)
        Me.cbGroupSel.TabIndex = 42
        Me.cbGroupSel.Text = "Train"
        '
        'tbStatus
        '
        Me.tbStatus.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbStatus.BackColor = System.Drawing.Color.Bisque
        Me.tbStatus.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbStatus.Location = New System.Drawing.Point(10, 354)
        Me.tbStatus.Multiline = True
        Me.tbStatus.Name = "tbStatus"
        Me.tbStatus.Size = New System.Drawing.Size(757, 29)
        Me.tbStatus.TabIndex = 41
        '
        'bView
        '
        Me.bView.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bView.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bView.ForeColor = System.Drawing.Color.Red
        Me.bView.Location = New System.Drawing.Point(567, 304)
        Me.bView.Name = "bView"
        Me.bView.Size = New System.Drawing.Size(63, 33)
        Me.bView.TabIndex = 32
        Me.bView.Text = "Report"
        Me.bView.UseVisualStyleBackColor = False
        '
        'BTrain
        '
        Me.BTrain.BackColor = System.Drawing.Color.WhiteSmoke
        Me.BTrain.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.BTrain.ForeColor = System.Drawing.Color.SeaGreen
        Me.BTrain.Location = New System.Drawing.Point(567, 263)
        Me.BTrain.Name = "BTrain"
        Me.BTrain.Size = New System.Drawing.Size(63, 34)
        Me.BTrain.TabIndex = 31
        Me.BTrain.Text = "Train"
        Me.BTrain.UseVisualStyleBackColor = False
        '
        'bLoad
        '
        Me.bLoad.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bLoad.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bLoad.ForeColor = System.Drawing.Color.DarkGoldenrod
        Me.bLoad.Location = New System.Drawing.Point(705, 303)
        Me.bLoad.Name = "bLoad"
        Me.bLoad.Size = New System.Drawing.Size(63, 34)
        Me.bLoad.TabIndex = 30
        Me.bLoad.Text = "Load"
        Me.bLoad.UseVisualStyleBackColor = False
        '
        'bSave
        '
        Me.bSave.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bSave.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bSave.ForeColor = System.Drawing.Color.DarkGoldenrod
        Me.bSave.Location = New System.Drawing.Point(636, 303)
        Me.bSave.Name = "bSave"
        Me.bSave.Size = New System.Drawing.Size(63, 34)
        Me.bSave.TabIndex = 29
        Me.bSave.Text = "Save"
        Me.bSave.UseVisualStyleBackColor = False
        '
        'bCalculate
        '
        Me.bCalculate.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bCalculate.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bCalculate.ForeColor = System.Drawing.Color.DarkCyan
        Me.bCalculate.Location = New System.Drawing.Point(704, 263)
        Me.bCalculate.Name = "bCalculate"
        Me.bCalculate.Size = New System.Drawing.Size(63, 34)
        Me.bCalculate.TabIndex = 28
        Me.bCalculate.Text = "Calculate"
        Me.bCalculate.UseVisualStyleBackColor = False
        '
        'BExecute
        '
        Me.BExecute.BackColor = System.Drawing.Color.WhiteSmoke
        Me.BExecute.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.BExecute.ForeColor = System.Drawing.Color.Navy
        Me.BExecute.Location = New System.Drawing.Point(636, 263)
        Me.BExecute.Name = "BExecute"
        Me.BExecute.Size = New System.Drawing.Size(63, 34)
        Me.BExecute.TabIndex = 27
        Me.BExecute.Text = "Execute"
        Me.BExecute.UseVisualStyleBackColor = False
        '
        'bAddData
        '
        Me.bAddData.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bAddData.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bAddData.ForeColor = System.Drawing.Color.Red
        Me.bAddData.Location = New System.Drawing.Point(477, 122)
        Me.bAddData.Name = "bAddData"
        Me.bAddData.Size = New System.Drawing.Size(84, 28)
        Me.bAddData.TabIndex = 26
        Me.bAddData.Text = "Add Data"
        Me.bAddData.UseVisualStyleBackColor = False
        '
        'bReset
        '
        Me.bReset.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bReset.ForeColor = System.Drawing.Color.Red
        Me.bReset.Location = New System.Drawing.Point(529, 82)
        Me.bReset.Name = "bReset"
        Me.bReset.Size = New System.Drawing.Size(73, 21)
        Me.bReset.TabIndex = 25
        Me.bReset.Text = "Reset"
        Me.bReset.UseVisualStyleBackColor = True
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label10.Location = New System.Drawing.Point(608, 196)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(98, 16)
        Me.Label10.TabIndex = 24
        Me.Label10.Text = "Debug_MSE:"
        '
        'tbGlobalError
        '
        Me.tbGlobalError.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbGlobalError.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbGlobalError.Location = New System.Drawing.Point(611, 212)
        Me.tbGlobalError.Multiline = True
        Me.tbGlobalError.Name = "tbGlobalError"
        Me.tbGlobalError.Size = New System.Drawing.Size(159, 43)
        Me.tbGlobalError.TabIndex = 23
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label9.Location = New System.Drawing.Point(608, 134)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(103, 16)
        Me.Label9.TabIndex = 22
        Me.Label9.Text = "Debug_Delta:"
        '
        'tbDelta
        '
        Me.tbDelta.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbDelta.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbDelta.Location = New System.Drawing.Point(611, 150)
        Me.tbDelta.Multiline = True
        Me.tbDelta.Name = "tbDelta"
        Me.tbDelta.Size = New System.Drawing.Size(159, 43)
        Me.tbDelta.TabIndex = 21
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label8.Location = New System.Drawing.Point(608, 73)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(110, 16)
        Me.Label8.TabIndex = 20
        Me.Label8.Text = "Debug_Output:"
        '
        'tbDebugOutput
        '
        Me.tbDebugOutput.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbDebugOutput.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbDebugOutput.Location = New System.Drawing.Point(611, 89)
        Me.tbDebugOutput.Multiline = True
        Me.tbDebugOutput.Name = "tbDebugOutput"
        Me.tbDebugOutput.Size = New System.Drawing.Size(159, 43)
        Me.tbDebugOutput.TabIndex = 19
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label7.Location = New System.Drawing.Point(608, 11)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(99, 16)
        Me.Label7.TabIndex = 18
        Me.Label7.Text = "Debug_Input:"
        '
        'tbDebugInput
        '
        Me.tbDebugInput.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbDebugInput.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbDebugInput.Location = New System.Drawing.Point(611, 27)
        Me.tbDebugInput.Multiline = True
        Me.tbDebugInput.Name = "tbDebugInput"
        Me.tbDebugInput.Size = New System.Drawing.Size(159, 43)
        Me.tbDebugInput.TabIndex = 17
        '
        'Tab2
        '
        Me.Tab2.Controls.Add(Me.tbInfo)
        Me.Tab2.Location = New System.Drawing.Point(4, 22)
        Me.Tab2.Name = "Tab2"
        Me.Tab2.Padding = New System.Windows.Forms.Padding(3)
        Me.Tab2.Size = New System.Drawing.Size(775, 389)
        Me.Tab2.TabIndex = 1
        Me.Tab2.Text = "Info"
        Me.Tab2.UseVisualStyleBackColor = True
        '
        'tbInfo
        '
        Me.tbInfo.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbInfo.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbInfo.Location = New System.Drawing.Point(6, 6)
        Me.tbInfo.Multiline = True
        Me.tbInfo.Name = "tbInfo"
        Me.tbInfo.Size = New System.Drawing.Size(763, 377)
        Me.tbInfo.TabIndex = 18
        '
        'tbNet
        '
        Me.tbNet.DrawMode = System.Windows.Forms.DrawMode.OwnerDrawVariable
        Me.tbNet.FormattingEnabled = True
        Me.tbNet.ItemHeight = 50
        Me.tbNet.Location = New System.Drawing.Point(278, 26)
        Me.tbNet.Name = "tbNet"
        Me.tbNet.Size = New System.Drawing.Size(324, 56)
        Me.tbNet.Sorted = True
        Me.tbNet.TabIndex = 52
        Me.tbNet.Text = "2=3.Leaky_RELU+4.Soft_Plus=2.Bent_Identity"
        '
        'bSaveStructure
        '
        Me.bSaveStructure.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bSaveStructure.ForeColor = System.Drawing.Color.SeaGreen
        Me.bSaveStructure.Location = New System.Drawing.Point(278, 82)
        Me.bSaveStructure.Name = "bSaveStructure"
        Me.bSaveStructure.Size = New System.Drawing.Size(73, 21)
        Me.bSaveStructure.TabIndex = 53
        Me.bSaveStructure.Text = "Save"
        Me.bSaveStructure.UseVisualStyleBackColor = True
        '
        'GUI
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.LightCyan
        Me.ClientSize = New System.Drawing.Size(801, 419)
        Me.Controls.Add(Me.MP)
        Me.Name = "GUI"
        Me.RightToLeftLayout = True
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "GUI"
        Me.MP.ResumeLayout(False)
        Me.Tab1.ResumeLayout(False)
        Me.Tab1.PerformLayout()
        Me.Tab2.ResumeLayout(False)
        Me.Tab2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Label2 As Label
    Friend WithEvents cbLearningRate As ComboBox
    Friend WithEvents Label1 As Label
    Friend WithEvents cbMomentum As ComboBox
    Friend WithEvents Label3 As Label
    Friend WithEvents Label4 As Label
    Friend WithEvents cbType As ComboBox
    Friend WithEvents cbSize As ComboBox
    Friend WithEvents Label5 As Label
    Friend WithEvents cbEpoch As ComboBox
    Friend WithEvents Label6 As Label
    Friend WithEvents CheckRnd As CheckBox
    Friend WithEvents MP As TabControl
    Friend WithEvents Tab1 As TabPage
    Friend WithEvents bReset As Button
    Friend WithEvents Label10 As Label
    Friend WithEvents tbGlobalError As TextBox
    Friend WithEvents Label9 As Label
    Friend WithEvents tbDelta As TextBox
    Friend WithEvents Label8 As Label
    Friend WithEvents tbDebugOutput As TextBox
    Friend WithEvents Label7 As Label
    Friend WithEvents tbDebugInput As TextBox
    Friend WithEvents Tab2 As TabPage
    Friend WithEvents BTrain As Button
    Friend WithEvents bLoad As Button
    Friend WithEvents bSave As Button
    Friend WithEvents bCalculate As Button
    Friend WithEvents BExecute As Button
    Friend WithEvents bAddData As Button
    Friend WithEvents tbError As TextBox
    Friend WithEvents tbOutput As TextBox
    Friend WithEvents tbTarget As TextBox
    Friend WithEvents tbInput As TextBox
    Friend WithEvents cbGroupSel As ComboBox
    Friend WithEvents tbStatus As TextBox
    Friend WithEvents bView As Button
    Friend WithEvents Label15 As Label
    Friend WithEvents Label14 As Label
    Friend WithEvents Label13 As Label
    Friend WithEvents Label12 As Label
    Friend WithEvents Label11 As Label
    Friend WithEvents tbInfo As TextBox
    Friend WithEvents tbNet As ComboBox
    Friend WithEvents bSaveStructure As Button
End Class
