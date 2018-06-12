Imports Microsoft.Office.Interop
Imports System.Text.RegularExpressions


Public Class AddTrainingSet
    Private xlApp As Excel.Application
    Public DB As LiteDb
    Public curSetting$
    Private Declare Function SetForegroundWindow Lib "user32" (ByVal hwnd As Integer) As IntPtr

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        Try
            xlApp = CType(GetObject(, "Excel.Application"), Excel.Application)
        Catch e As Exception
            Console.WriteLine("Error can't get Excell app, Err:" & e.Message)
        End Try
    End Sub

    Public Sub ShowExcel()
        With xlApp
            If .WindowState = Excel.XlWindowState.xlMinimized Then .WindowState = Excel.XlWindowState.xlNormal
            .WindowState = Excel.XlWindowState.xlMaximized
            .ShowWindowsInTaskbar = True
            .Visible = True
        End With
        SetForegroundWindow(xlApp.Hwnd)
    End Sub
    Public Sub ShowMe()
        SetForegroundWindow(CInt(Handle))
    End Sub

    Private Sub bOk_Click(sender As Object, e As EventArgs) Handles bOk.Click
        'a-zA-Z\._
        DB.Execute(String.Format("INSERT INTO TrainingSET (datecreated, setGroupName, SetInput, SetTarget, SetOutput, setError) VALUES ('{0}','{1}','{2}','{3}','{4}', '{5}')",
                                 Today().ToString("s"),
                                 cbGroup.Text & "|" & Replace(Regex.Replace(Regex.Replace(curSetting, "[a-zA-Z\+\._]", ""), "(?<=\=).*?(?=\=)", ""), "==", "="),
                                 tbInputRange.Text, tbTargetRange.Text, "N/A", "N/A"))
        DB = Nothing

        GUI.Show()
        Me.Dispose()
    End Sub
    Private Sub bLoadtxt_Click(sender As Object, e As EventArgs) Handles bLoadtxt.Click
        Dim str$ = Getfile(False)
        If str <> "0" Then
            str = ReadFile(str, "||")
            tbInputRange.Text = Split(str, "||")(0)
            tbTargetRange.Text = Split(str, "||")(1)
        End If
    End Sub
    Private Sub tbTargetRange_Click(sender As Object, e As EventArgs) Handles tbTargetRange.Click
        Dim rng As Excel.Range
        Dim Arr As VariantType(,)

        On Error Resume Next
        If Not xlApp Is Nothing Then
            Me.Hide()
ReSelect:
            ShowExcel()
            rng = TryCast(xlApp.InputBox("Select Input Range", "Range", "", , , , , 8), Excel.Range)
            On Error GoTo 0

            If Not rng Is Nothing Then
                If rng.Columns.Count > 1 And rng.Rows.Count > 1 Then MsgBox("You can only select 1 row or 1 column !!") : GoTo ReSelect
                Arr = CType(rng.Value2, VariantType(,))
                If Arr.Rank = 0 Then
                    tbTargetRange.Text = "1=(" & Arr.ToString & ")"
                Else
                    Arr = CType(Trans1D(Arr), VariantType(,))
                    tbTargetRange.Text = UBound(Arr) + 1 & "=(" & Arr.ToStr("|") & ")"
                End If
                rng = Nothing
            End If
            Me.Show()
            ShowMe()
        End If
    End Sub
    Private Sub tbInputRange_Click(sender As Object, e As EventArgs) Handles tbInputRange.Click
        Dim rng As Excel.Range
        Dim Arr As VariantType(,)

        On Error Resume Next
        If Not xlApp Is Nothing Then
            Me.Hide()
ReSelect:
            ShowExcel()
            rng = CType(xlApp.InputBox("Select Input Range", "Range", "", , , , , 8), Excel.Range)
            On Error GoTo 0

            If Not rng Is Nothing Then
                If rng.Columns.Count > 1 And rng.Rows.Count > 1 Then MsgBox("You can only select 1 row or 1 column !!") : GoTo ReSelect
                Arr = CType(rng.Value2, VariantType(,))
                If Arr.rank = 0 Then
                    tbInputRange.Text = "1=(" & Arr.ToString & ")"
                Else
                    Arr = CType(Trans1D(Arr), VariantType(,))
                    tbInputRange.Text = UBound(Arr) + 1 & "=(" & Arr.ToStr("|") & ")"
                End If
                rng = Nothing
            End If
            Me.Show()
            ShowMe()
        End If
    End Sub
    Private Sub AddTrainingSet_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        If e.CloseReason = CloseReason.UserClosing Then
            GUI.Show()
            Me.Dispose()
        End If
    End Sub
    Private Sub bCancel_Click(sender As Object, e As EventArgs) Handles bCancel.Click
        GUI.Show()
        Me.Dispose()
    End Sub
End Class