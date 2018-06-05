Option Explicit On
Imports System.Text.RegularExpressions

Public Class GUI
    Private DNN As NeuralNet
    Private TrClock#(0 To 1)
    Public Data As New Dictionary(Of String, List(Of String))
    Public DB As LiteDb

    Private Sub bCalculate_Click(sender As Object, e As EventArgs) Handles bCalculate.Click
        Dim NumberOfInput&
        Dim listInput As List(Of Double) = New List(Of Double)

        NumberOfInput = Split(cbNetSetting.Text, "=")(0)

        If DNN.Active = False Then MsgBox("Neural Network was not trained, please train it first !!") : Exit Sub
        If UBound(Split(tbDebugInput.Text, "|")) - LBound(Split(tbDebugInput.Text, "|")) + 1 <> NumberOfInput Then _
                MsgBox("Number of input value is not correct, you could try: " & NumberOfInput & " inputs seperate by |") : Exit Sub
        'Predict with manually input don't have target so it'll return no error to meansure
        tbGlobalError.Text = "N/A"

        listInput.Add(tbDebugInput.Text)
        tbDebugOutput.Text = ""

        With DNN

            .SetInput(C2List(tbDebugInput.Text).ConvertAll(Of Double)(New Converter(Of Object, Double)(Function(x) CDbl(x))))
            .Calculate()

            'Update string Result array
            If Len(tbDebugOutput.Text) = 0 Then
                tbDebugOutput.Text = "(" & .Result & ")"
            Else
                tbDebugOutput.Text = tbDebugOutput.Text & " (" & .Result & ")"
            End If

            'update string error -> will update this function late

        End With

    End Sub

    Private Sub BExecute_Click(sender As Object, e As EventArgs) Handles BExecute.Click
        Dim jj&, g&, NumberOfInput&, NumberOfOutput&
        Dim MaxIndex&
        Dim listRes As List(Of Double)
        Dim TempErr#
        Dim listArr1 As List(Of String), listArr2 As List(Of String)
        Dim listInput As List(Of String) = New List(Of String)
        Dim listTarget As List(Of String) = New List(Of String)

        If DNN.Active = False Then MsgBox("Neural Network was not trained, please train it first !!") : Exit Sub

        NumberOfInput = Split(cbNetSetting.Text, "=")(0)
        NumberOfOutput = CDbl(Split(Split(cbNetSetting.Text, "=")(UBound(Split(cbNetSetting.Text, "="))), ".")(0))

        '//Loop throught each line to find those group, each group will be split by number of input
        '//(ex; 10 values, but 2 inputs then we'll have 5 input groups)


        listArr1 = C2List(tbInput.Text)
        listArr2 = C2List(tbTarget.Text)
        For jj = 0 To listArr1.Count - 1 Step NumberOfInput
            'prevent incorrect number of input raise error
            If listArr1.Count Mod NumberOfInput <> 0 Then If jj = listArr1.Count - 1 Then Exit For
            'Add input each loop
            listInput.Add(listArr1.GetRange(jj, NumberOfInput).ToStr("|"))
            MaxIndex = MaxIndex + 1
        Next
        For jj = 0 To listArr2.Count - 1 Step NumberOfOutput
            'prevent incorrect number of input raise error
            If listArr2.Count Mod NumberOfOutput <> 0 Then If jj = listArr2.Count - 1 Then Exit For
            'Add input each loop
            listTarget.Add(listArr2.GetRange(jj, NumberOfOutput).ToStr("|"))
        Next

        With DNN
            tbOutput.Text = vbNullString
            tbError.Text = vbNullString
            For jj = 0 To listInput.Count - 1
                .SetInput(C2List(listInput(jj)).ConvertAll(Of Double)(New Converter(Of Object, Double)(Function(x) CDbl(x))))
                .Calculate()

                'Update string Result array
                If Len(tbOutput.Text) = 0 Then
                    tbOutput.Text = .Result
                Else
                    tbOutput.Text = tbOutput.Text & "+" & .Result
                End If

                'update string error
                TempErr = 0
                listRes = C2List(.Result).ConvertAll(Of Double)(New Converter(Of Object, Double)(Function(x) CDbl(x)))
                For g = 0 To listRes.Count - 1
                    TempErr = TempErr + (C2List(listTarget(jj))(g) - listRes(g)) ^ 2
                Next
                If Len(tbError.Text) = 0 Then
                    tbError.Text = Math.Round((TempErr / NumberOfOutput) ^ 0.5, 8)
                Else
                    tbError.Text = tbError.Text & "|" & Math.Round((TempErr / NumberOfOutput) ^ 0.5, 8)
                End If
            Next
        End With
    End Sub

    Private Sub bLoad_Click(sender As Object, e As EventArgs) Handles bLoad.Click

        DNN.Load_NeuralNet()
        'Update current userform
        With Me
            .cbLearningRate.Text = DNN.LearningRate
            .cbMomentum.Text = (1 - DNN.Momentum)
            .cbNetSetting.Text = DNN.Network_Structure
        End With
    End Sub

    Private Sub bSave_Click(sender As Object, e As EventArgs) Handles bSave.Click
        DNN.Save_NeuralNet("", "")
    End Sub
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

    Private Sub cbGroupSel_eClick(sender As Object, e As EventArgs) Handles cbGroupSel.Click

    End Sub

    Private Sub bAddData_Click(sender As Object, e As EventArgs) Handles bAddData.Click
        Me.Hide()
        AddTrainingSet.DB = DB
        AddTrainingSet.curSetting = cbNetSetting.Text
        AddTrainingSet.Show()
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

    Private Sub Update_cbNetSetting()
        With cbNetSetting
            .Items.Clear()
            .Items.AddRange(DB.Query("SELECT Value_Setting FROM SettingList").toArray())
        End With
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

    Private Sub BTrain_Click(sender As Object, e As EventArgs) Handles BTrain.Click
        Dim jj&, g&, NumberOfInput&, NumberOfOutput&
        Dim TempErr#
        Dim listInput As List(Of Double) = New List(Of Double)
        Dim listTarget As List(Of Double) = New List(Of Double)

        Dim listRes As List(Of Double)

        'Get List input
        If Data.Count = 0 Then MsgBox("No input, abort Train !!") : Exit Sub

        NumberOfInput = Split(Me.cbNetSetting.Text, "=")(0)
        NumberOfOutput = CDbl(Split(Split(Me.cbNetSetting.Text, "=")(UBound(Split(Me.cbNetSetting.Text, "="))), ".")(0))

        '//Loop throught each line to find those group, each group will be split by number of input
        '//(ex; 10 values, but 2 inputs then we'll have 5 input groups)


        cbGroupSel_Click()
        listInput = C2List(tbInput.Text).ConvertAll(Of Double)(New Converter(Of Object, Double)(Function(x) CDbl(x)))
        listTarget = C2List(tbTarget.Text).ConvertAll(Of Double)(New Converter(Of Object, Double)(Function(x) CDbl(x)))


        With DNN

            '//Setup network
            If Not .Active Then
                .Name = "Adhoc Neural Network created at " & Format(Now(), "YYYYMMDD HH:MM:SS")
                If .Build_NeuralNet(cbNetSetting.Text) = False Then MsgBox("Failed to setup network, check again!!") : Exit Sub
            End If

            .LearningRate = cbLearningRate.Text
            .Momentum = 1 - cbMomentum.Text
            .InData.Clear()
            .TargetData.Clear()

            setTime()
            TrClock(0) = 0
            TrClock(1) = 0

            '//Train the network
            .TrainSpecial(cbType.Text, cbEpoch.Text, listInput, listTarget, CheckRnd.Checked, cbSize.Text, Me)

            '//Output the result
            'Update list view
            'set input -> execute -> get result


            tbOutput.Text = vbNullString
            tbError.Text = vbNullString
            For jj = 0 To .InData.Count - 1
                .SetInput(.InData(jj))
                .Calculate()

                'Update string Result array
                If Len(tbOutput.Text) = 0 Then
                    tbOutput.Text = .Result
                Else
                    tbOutput.Text = tbOutput.Text & vbCrLf & .Result
                End If

                'update string error
                TempErr = 0
                listRes = C2List(.Result).ConvertAll(Of Double)(New Converter(Of Object, Double)(Function(x) CDbl(x)))
                For g = 0 To listRes.Count - 1
                    TempErr = TempErr + (.TargetData(jj)(g) - listRes(g)) ^ 2
                Next
                If Len(tbError.Text) = 0 Then
                    tbError.Text = Math.Round((TempErr / NumberOfOutput) ^ 0.5, 8)
                Else
                    tbError.Text = tbError.Text & "|" & Math.Round((TempErr / NumberOfOutput) ^ 0.5, 8)
                End If
            Next


        End With
    End Sub

    Public Overrides Sub Refresh()
        Dim d&
        Dim Delta$ = "", AccDelta$ = ""
        With DNN
            TrClock(0) = TrClock(0) + GetTime()
            If TrClock(0) - TrClock(1) >= 1 Then
                '//update value on screen
                'Me.Repaint
                tbDebugInput.Text = .st_Input
                tbDebugOutput.Text = .Result

                tbDelta.Text = ""
                For d = 0 To .Layers(.Layers.Count - 1).Neurons.Count - 1
                    If Len(Delta) = 0 Then
                        Delta = .Layers(.Layers.Count - 1).Neurons(d).Delta
                    Else
                        Delta = Delta & "|" & .Layers(.Layers.Count - 1).Neurons(d).Delta
                    End If
                    If Len(AccDelta) = 0 Then
                        AccDelta = .Layers(.Layers.Count - 1).Neurons(d).AccDelta
                    Else
                        AccDelta = AccDelta & "|" & .Layers(.Layers.Count - 1).Neurons(d).AccDelta
                    End If
                Next d
                tbDelta.Text = Math.Round(CDbl(Delta), 15) & vbCrLf & Math.Round(CDbl(AccDelta), 15)

                tbDebugOutput.Visible = False
                tbDebugOutput.Visible = True
                Me.tbGlobalError.Text = Math.Round(.globalError, 15).ToString("R")
                tbStatus.Text = "Status: FF(" & .st_FF & " - " & Math.Round(.st_FF / TrClock(0), 2) _
                                    & ") Updated(" & .st_Updated_N & " - " & Math.Round(.st_Updated_N / TrClock(1), 2) _
                                    & ") Epoch(" & .st_CurrEpoch & "/" & cbEpoch.Text & " - " & Math.Round(.st_CurrEpoch / cbEpoch.Text * 100, 2) _
                                    & ") EpS(" & Math.Round(.st_CurrEpoch / TrClock(0), 2) _
                                    & ") - " & Math.Round(TrClock(0), 4) & " sec."
                If MP.SelectedIndex = 1 Then Me.tbInfo_Click(Me, New EventArgs())
                TrClock(1) = TrClock(0)
            End If
            Application.DoEvents()
        End With
    End Sub


    Private Sub bView_Click(sender As Object, e As EventArgs) Handles bView.Click
        Dim ii&
        Dim NumberOfInput&, NumberOfOutput&
        Dim IO As Byte
        Dim listTmp As List(Of String) = New List(Of String)

        NumberOfInput = Split(cbNetSetting.Text, "=")(0)
        NumberOfOutput = CDbl(Split(Split(cbNetSetting.Text, "=")(UBound(Split(cbNetSetting.Text, "="))), ".")(0))
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
        Console.WriteLine(DB.FullPath)

        cbNetSetting.Items.AddRange(DB.Query("SELECT Value_Setting FROM Settinglist").toArray())
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
        CheckForIllegalCrossThreadCalls = False
    End Sub

    Private Sub cbGroupSel_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbGroupSel.SelectedIndexChanged
        Dim cb = DirectCast(sender, ComboBox)
        ' SelectedValue is an Object - you can get the name of its actual type with .SelectedValue.GetType().Name
        cbGroupSel.Text = cb.SelectedValue.ToString
    End Sub
End Class
