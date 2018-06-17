Option Explicit On
Imports System.Text.RegularExpressions

Public Class GUI

    Private DNN As NeuralNet
    Private locker As New Object

    Public WithEvents myTimer As New Timers.Timer()

    Public Data As New Dictionary(Of String, List(Of String))
    Public DB As LiteDb

    '****************************************************************************************************************************************
    '//Sub-Method handle events on winform (button - listbox - combobox)
    '****************************************************************************************************************************************

    Private Sub bCalculate_Click(sender As Object, e As EventArgs) Handles bCalculate.Click
        If DNN.Trained_Time = 0 Then MsgBox("Neural Network was not trained, please train it first !!") : Exit Sub
        If UBound(Split(tbDebugInput.Text, "|")) - LBound(Split(tbDebugInput.Text, "|")) + 1 <> DNN.InputSize Then _
                MsgBox("Number of input value is not correct, you could try: " & DNN.InputSize & " inputs seperate by |") : Exit Sub

        'Predict with manually input don't have target so it'll return no error to meansure
        tbGlobalError.Text = "N/A"
        tbDebugOutput.Text = ""

        With DNN
            .SetInput(C2List(tbDebugInput.Text).ConvertAll(New Converter(Of Object, Double)(Function(x) CDbl(x))))
            .Calculate()

            'Update string Result array
            tbDebugOutput.Text = .Current_Result
            'update string error -> will update this function late
            tbGlobalError.Text = .Current_MSError
        End With

    End Sub

    Private Sub BExecute_Click(sender As Object, e As EventArgs) Handles BExecute.Click
        Dim listArr1, listArr2 As List(Of Double)
        If DNN.Active = False Or DNN.Trained_Time = 0 Then MsgBox("Neural Network was not trained, please train it first !!") : Exit Sub

        listArr1 = C2List(tbInput.Text).ConvertAll(New Converter(Of String, Double)(Function(x) CDbl(x)))
        listArr2 = C2List(tbTarget.Text).ConvertAll(New Converter(Of String, Double)(Function(x) CDbl(x)))

        With DNN
            .PrepareInOut(listArr1, listArr2)
            'Update string Result array
            tbOutput.Text = .Result
            'update string error
            tbError.Text = .MSError
        End With
    End Sub

#Region "Save/Load Handle"
    Private Sub bLoad_Click(sender As Object, e As EventArgs) Handles bLoad.Click

        DNN.Load_NeuralNet()
        'Update current userform
        With Me
            .cbLearningRate.Text = CStr(DNN.LearningRate)
            .cbMomentum.Text = CType((1 - DNN.Momentum), String)
            .cbNetSetting.Text = DNN.Network_Structure
        End With
    End Sub

    Private Sub bSave_Click(sender As Object, e As EventArgs) Handles bSave.Click
        DNN.Save_NeuralNet("", "")
    End Sub
#End Region

#Region "cbGroupSel combobox to select training set events handle"
    Private Sub cbGroupSel_TabIndexChanged(sender As Object, e As EventArgs) Handles cbGroupSel.TabIndexChanged
        cbGroupSel_Click()
    End Sub
    Private Sub cbGroupSel_TextChanged(sender As Object, e As EventArgs) Handles cbGroupSel.TextChanged
        cbGroupSel_Click()
    End Sub
    Private Sub cbGroupSel_Click()
        Dim var As List(Of String)
        If Data.ContainsKey(cbGroupSel.Text) Then
            var = Data(cbGroupSel.Text)
            tbInput.Text = var(1)
            tbTarget.Text = var(2)
            tbOutput.Text = var(3)
            tbError.Text = var(4)
        End If
    End Sub

    Private Sub cbGroupSel_Click(sender As Object, e As EventArgs) Handles cbGroupSel.Click

    End Sub
    Private Sub cbGroupSel_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbGroupSel.SelectedIndexChanged
        Dim cb = DirectCast(sender, ComboBox)
        ' SelectedValue is an Object - you can get the name of its actual type with .SelectedValue.GetType().Name
        cbGroupSel.Text = cb.SelectedValue.ToString
    End Sub
#End Region

    Private Sub bAddData_Click(sender As Object, e As EventArgs) Handles bAddData.Click
        Me.Hide()
        AddTrainingSet.DB = DB
        AddTrainingSet.curSetting = cbNetSetting.Text
        AddTrainingSet.Show()
    End Sub

    Private Sub bReset_Click(sender As Object, e As EventArgs) Handles bReset.Click
        DNN.Reset()
        Data.Clear()
    End Sub

    Private Sub tbInfo_Click(sender As Object, e As EventArgs) Handles tbInfo.Click
        With tbInfo
            .Text = ""
            .Text = DNN.Network_Information
        End With
    End Sub

    Private Sub bView_Click(sender As Object, e As EventArgs) Handles bView.Click
        Dim NumberOfInput%, NumberOfOutput%
        Dim IO As Byte
        Dim listTmp As List(Of String) = New List(Of String)

        NumberOfInput = DNN.InputSize
        NumberOfOutput = DNN.OutputSize
        ' IO: 1= I, 2 = O
        If Math.Max(NumberOfInput, NumberOfOutput) = NumberOfInput Then IO = 1 Else IO = 2

        With listTmp
            .Add(cbNetSetting.Text)
            .Add("Report Created: " & Format(Now(), "dd.mm.yyyy - hh:mm:ss"))
            .Add(String.Concat("Type", vbTab, "Group", vbTab, "Input", vbTab, "Target", vbTab, "Output", vbTab, "Error"))
            For ii = 0 To Data.Count - 1
                .Add(ii & Data.Keys(ii) & vbTab & Data.Values(ii).ToStr("|"))
            Next
        End With
    End Sub

    Private Sub bSaveStructure_Click(sender As Object, e As EventArgs) Handles bSaveStructure.Click
        If DB.Query(String.Format("SELECT Count(*) FROM SettingList WHERE Value_Setting='{0}'", Me.cbNetSetting.Text), 2)(0) = "0" Then
            DB.Execute(String.Format("INSERT INTO SettingList (Name_Setting, Value_Setting, dateCreated) VALUES ('{0}' , '{1}' , '{2}') ",
                                     Replace(Regex.Replace(Regex.Replace(cbNetSetting.Text, "[a-zA-Z\+\._]", ""), "(?<=\=).*?(?=\=)", ""), "==", "="),
                                     cbNetSetting.Text, Today().ToString("s")))

            Update_cbNetSetting()
            MsgBox("Saved")
        Else
            MsgBox("Already in the list")
        End If
    End Sub
    Private Sub Update_cbNetSetting()
        With cbNetSetting
            .Items.Clear()
            .Items.AddRange(CType(DB.Query("SELECT Value_Setting FROM SettingList").toArray(), Object()))
        End With
    End Sub

    Private Sub cbNetSetting_MeasureItem(ByVal sender As Object, ByVal e As MeasureItemEventArgs) Handles cbNetSetting.MeasureItem
        e.ItemHeight = 30
        e.ItemWidth = 324
    End Sub
    Private Sub cbNetSetting_DrawItem(ByVal sender As Object, ByVal e As DrawItemEventArgs) Handles cbNetSetting.DrawItem

        If e.Index < 0 Then
            Return
        End If
        e.Graphics.TextRenderingHint = Drawing.Text.TextRenderingHint.AntiAlias
        Dim CB As ComboBox = TryCast(sender, ComboBox)
        If (e.State And DrawItemState.Selected) = DrawItemState.Selected Then
            e.Graphics.FillRectangle(New SolidBrush(Color.OrangeRed), e.Bounds)
        Else
            e.Graphics.FillRectangle(New SolidBrush(CB.BackColor), e.Bounds)
        End If
        e.Graphics.DrawString(CB.Items(e.Index).ToString(), e.Font, New SolidBrush(CB.ForeColor), New Point(e.Bounds.X, e.Bounds.Y))
        e.DrawFocusRectangle()

    End Sub


    '****************************************************************************************************************************************
    '//Train Method of GUI
    '****************************************************************************************************************************************
    Private listInput As List(Of Double) = New List(Of Double)
    Private listTarget As List(Of Double) = New List(Of Double)
    Private Sub BTrain_Click(sender As Object, e As EventArgs) Handles BTrain.Click
        'Get List input
        If Data.Count = 0 Then MsgBox("No input, abort Train !!") : Exit Sub

        '//Loop throught each line to find those group, each group will be split by number of input
        '//(ex; 10 values, but 2 inputs then we'll have 5 input groups)
        cbGroupSel_Click()
        listInput = C2List(tbInput.Text).ConvertAll(Of Double)(New Converter(Of String, Double)(Function(x) CDbl(x)))
        listTarget = C2List(tbTarget.Text).ConvertAll(Of Double)(New Converter(Of String, Double)(Function(x) CDbl(x)))

        With DNN
            '//Setup network
            If Not .Active Then
                .Name = "Adhoc Neural Network created at " & Format(Now(), "YYYYMMDD HH:MM:SS")
                If .Build_NeuralNet(cbNetSetting.Text) = False Then MsgBox("Failed to setup network, check again!!") : Exit Sub
            End If

            .LearningRate = CDbl(cbLearningRate.Text)
            .Momentum = 1 - CDbl(cbMomentum.Text)
            .InData.Clear()
            .TargetData.Clear()

            '//Active update GUI
            myTimer.Interval = 1000.0#
            myTimer.Enabled = True

            '//Train the network
            vType = cbType.Text
            vEpoch = cbEpoch.Text
            vSize = cbSize.Text
            vRnd = CheckRnd.Checked
            TrainTask = New Task(AddressOf Train_TaskHelper)
            TrainTask.Start()

            'TrainTask.Wait()
            '//Output the result
            tbOutput.Text = .Result
            tbError.Text = .MSError

        End With
    End Sub
    Private TrainTask As Task
    Private vType, vEpoch, vSize As String
    Private vRnd As Boolean
    Public Sub Train_TaskHelper()
        DNN.TrainSpecial(vType, CInt(vEpoch), listInput, listTarget, vRnd, CInt(vSize))
        myTimer.Enabled = False
        UpdateWP()
        Console.WriteLine(String.Format("Trained Time:{0} -- Trained Epoch:{1}", DNN.Trained_Time, DNN.Trained_Total_Epoch))
        Console.WriteLine(String.Format("Number of loop time:{0}", DNN.test.Length))
    End Sub

    '****************************************************************************************************************************************
    '//Update Winform Code per 1000ms
    '****************************************************************************************************************************************
    Private Sub ElapsedUpdate(sender As Object, e As Timers.ElapsedEventArgs) Handles myTimer.Elapsed
        With DNN
            Console.WriteLine(String.Format("Update interval called:{0} - At time:{1} - ThreadID:{2}", .st_CurrEpoch, .ElapsedTime, Threading.Thread.CurrentThread.ManagedThreadId))
            If .Trained_Time = 0 And .st_Train = False Then Console.WriteLine("DNN not trained yet - Abort update GUI") : Exit Sub
            UpdateWP()
        End With
    End Sub

    Public Sub UpdateWP()
        Dim Delta$ = "", AccDelta$ = ""
        Application.DoEvents()
        Try
            If InvokeRequired Then
                Console.WriteLine(String.Format("Called InvokeRequired UpdateWP -- ThreadID:{0}", Threading.Thread.CurrentThread.ManagedThreadId))
                MyBase.Invoke(New MethodInvoker(AddressOf UpdateWP))
            Else
                Console.WriteLine(String.Format("Called Invoke UpdateWP -- ThreadID:{0}", Threading.Thread.CurrentThread.ManagedThreadId))
                With DNN
                    If Not .ThreadSafe Then
                        Console.WriteLine("waiting Training task before Update GUI -- Train Thread is not safe yet")
                        Threading.SpinWait.SpinUntil(Function() .ThreadSafe = True)
                        Console.WriteLine("start Update GUI -- Train Thread is safe but not sure =.=")
                    End If
                    '//update value on screen
                    'Me.Repaint
                    tbDebugInput.Text = .st_Input
                    tbDebugOutput.Text = .Current_Result

                    tbDelta.Text = ""
                    For d = 0 To .Layers(.Layers.Count - 1)._Count - 1
                        If Len(Delta) = 0 Then
                            Delta = Math.Round(.Layers(.Layers.Count - 1)._Delta(d), 15).ToString
                        Else
                            Delta &= "|" & Math.Round(.Layers(.Layers.Count - 1)._Delta(d), 15)
                        End If
                        If Len(AccDelta) = 0 Then
                            AccDelta = Math.Round(.Layers(.Layers.Count - 1)._AccDelta(d), 15).ToString
                        Else
                            AccDelta &= "|" & Math.Round(.Layers(.Layers.Count - 1)._AccDelta(d), 15)
                        End If
                    Next d
                    tbDelta.Text = Delta & vbCrLf & AccDelta

                    tbGlobalError.Text = Math.Round(.globalError, 15).ToString("R")
                    tbStatus.Text = .Process

                    If MP.SelectedIndex = 1 Then Me.tbInfo_Click(Me, New EventArgs())

                    MyBase.Refresh()
                End With
                Console.WriteLine(String.Format("Updated by UpdateWP on ThreadID:{0}", Threading.Thread.CurrentThread.ManagedThreadId))
            End If

        Catch e As Exception
            Console.WriteLine("Error occurs on UpdateWP: " & e.Message)
        End Try

    End Sub


    '****************************************************************************************************************************************
    '//Default method Winform
    '****************************************************************************************************************************************
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        With Me
            .tbGlobalError.Text = "N/A"
            .cbNetSetting.Text = "2=3.Leaky_RELU+4.Soft_Plus=2.Bent_Identity"
            .tbStatus.Text = "Status: Please add your input/target value into list using 'Add Data' button"
        End With
        DNN = New NeuralNet
        DB = New LiteDb("NNStore.db")
        createDefaultTable()
        'Console.WriteLine(DB.FullPath)

        cbNetSetting.Items.AddRange(CType(DB.Query("SELECT Value_Setting FROM Settinglist").toArray(), Object()))
        If DB.Query("SELECT * FROM TrainingSET").Rows.Count > 0 Then
            cbGroupSel.DataSource = DB.Query("SELECT (date(dateCreated) || ' (' || setGroupName || ')..' || SUBSTR(SetInput,1,20) || '..' || SUBSTR(SetTarget,1,20) || '..' || SUBSTR(SetOutput,1,15) || '..' || SUBSTR(setError,1,15)) as Col1, setGroupName || SetID as Col2 FROM TrainingSET")
            cbGroupSel.DisplayMember = "Col1"
            cbGroupSel.ValueMember = "Col2"
            Data = DB.Query("SELECT setGroupName || SetID as GName, SetInput, SetTarget, SetOutput, setError FROM TrainingSET").toDict("GName")
        End If

    End Sub
    Private Sub GUI_Enter(sender As Object, e As EventArgs) Handles MyBase.Enter
        If DB.Query("SELECT * FROM TrainingSET").Rows.Count > 0 Then
            cbGroupSel.DataSource = DB.Query("SELECT (date(dateCreated) || ' (' || setGroupName || ')..' || SUBSTR(SetInput,1,20) || '..' || SUBSTR(SetTarget,1,20) || '..' || SUBSTR(SetOutput,1,15) || '..' || SUBSTR(setError,1,15)) as Col1, setGroupName || SetID as Col2 FROM TrainingSET")
            cbGroupSel.DisplayMember = "Col1"
            cbGroupSel.ValueMember = "Col2"
            Data = DB.Query("SELECT setGroupName || SetID as GName, SetInput, SetTarget, SetOutput, setError FROM TrainingSET").toDict("GName")
        End If
    End Sub

    Private Sub GUI_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        CheckForIllegalCrossThreadCalls = True
    End Sub

    Public Sub createDefaultTable()
        'Table Store Value Set
        Dim strSQL$ = "CREATE TABLE IF NOT EXISTS TrainingSET (" &
                      "SetID INTEGER  PRIMARY KEY, SetGroupName TEXT, dateCreated TEXT, SetInput TEXT, SetTarget TEXT, SetOutput TEXT, SetError TEXT )"
        DB.Execute(strSQL)

        'Table Store all Setting
        strSQL = "CREATE TABLE IF NOT EXISTS SettingList (" &
                 "ID_Setting INTEGER  PRIMARY KEY, Name_Setting TEXT, Value_Setting TEXT, dateCreated TEXT )"
        DB.Execute(strSQL)
        If DB.Query(String.Format("SELECT Count(*) FROM SettingList WHERE Value_Setting='{0}'", "2=3.Leaky_RELU+4.Soft_Plus=2.Bent_Identity"), 2)(0) = "0" Then
            DB.Execute(String.Format("INSERT INTO SettingList (Name_Setting, Value_Setting, dateCreated) VALUES ('{0}' , '{1}' , '{2}') ",
                                     Regex.Replace("2=3.Leaky_RELU+4.Soft_Plus=2.Bent_Identity", "[a-zA-Z\._]", ""), "2=3.Leaky_RELU+4.Soft_Plus=2.Bent_Identity", Today().ToString("s")))
        End If

        'Table store neural network
        strSQL = "CREATE TABLE IF NOT EXISTS Network (" &
                 "ID_Network INTEGER  PRIMARY KEY, ID_Setting integer, Value_Network TEXT , dateCreated TEXT )"
        DB.Execute(strSQL)
    End Sub

End Class
