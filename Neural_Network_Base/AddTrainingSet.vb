Imports Microsoft.Office.Interop

Public Class AddTrainingSet
    Private xlApp As Object
    Public RefData As Dictionary(Of String, List(Of String))
    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        xlApp = GetObject(, "Excel.Application")
    End Sub

    Private Sub bOk_Click(sender As Object, e As EventArgs) Handles bOk.Click
        Dim L As New List(Of String)
        With L
            If Len(tbInputRange.Text) * Len(tbInputRange.Text) <> 0 Then
                L.Add(tbInputRange.Text)
                L.Add(tbTargetRange.Text)
                L.Add("N/A")
                L.Add("N/A")
            End If
        End With
        If RefData.ContainsKey(cbGroup.Text) Then
            RefData(cbGroup.Text) = L
        Else
            RefData.Add(cbGroup.Text, L)
        End If
        GUI.Data = RefData
        Me.Hide()
        GUI.Show()
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
        Dim Arr

        On Error Resume Next
        If Not xlApp Is Nothing Then
            Me.Hide()
ReSelect:
            rng = xlApp.InputBox("Select Input Range", "Range", "", , , , , 8)
            On Error GoTo 0

            If Not rng Is Nothing Then
                If rng.Columns.Count > 1 And rng.Rows.Count > 1 Then MsgBox("You can only select 1 row or 1 column !!") : GoTo ReSelect
                Arr = rng.value2
                If Arr.rank = 0 Then
                    tbTargetRange.Text = "1=(" & Arr & ")"
                Else
                    Arr = Trans1D(Arr)
                    tbTargetRange.Text = UBound(Arr) & "=(" & Join(Arr, "|") & ")"
                End If
                rng = Nothing
            End If
            Me.Show()
        End If
    End Sub
    Private Sub tbInputRange_Click(sender As Object, e As EventArgs) Handles tbInputRange.Click
        Dim rng As Excel.Range
        Dim Arr

        On Error Resume Next
        If Not xlApp Is Nothing Then
            Me.Hide()
ReSelect:
            rng = xlApp.InputBox("Select Input Range", "Range", "", , , , , 8)
            On Error GoTo 0

            If Not rng Is Nothing Then
                If rng.Columns.Count > 1 And rng.Rows.Count > 1 Then MsgBox("You can only select 1 row or 1 column !!") : GoTo ReSelect
                Arr = rng.value2
                If Arr.rank = 0 Then
                    tbInputRange.Text = "1=(" & Arr & ")"
                Else
                    Arr = Trans1D(Arr)
                    tbInputRange.Text = UBound(Arr) & "=(" & Join(Arr, "|") & ")"
                End If
                rng = Nothing
            End If
            Me.Show()
        End If
    End Sub


End Class