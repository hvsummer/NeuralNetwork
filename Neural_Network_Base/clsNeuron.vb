﻿Public Class Neuron
    'Public Parent As Object

    Public ID%
    Public IDGlobal$
    Public ActivationFunction$
    Public Bias#
    Public SumWB#
    Public Delta#
    Public AccDelta#
    Public Value#
    Public Weights As List(Of Double)

    Public Shared Operator *(N1 As Neuron, N2 As Neuron) As Double
        Return N1.Value * N2.Value
    End Operator

    Public Sub New()
        Weights = New List(Of Double)
    End Sub

    Public ReadOnly Property Info()
        Get
            'Neuron information will have 2 part
            '1 general information: type, Id
            '2 It''s value
            Info = IDGlobal & "-" & ActivationFunction
            Info = Info & vbTab & "Bias=" & Math.Round(Bias, 6) & IIf(Len(Bias) > 6, "...", "") & StrDup(Math.Max(0, 8 - Len(Math.Round(Bias, 6))), " "c)
            Info = Info & vbTab & "Delta=" & Math.Round(Delta, 6) & IIf(Len(Delta) > 6, "...", "") & StrDup(Math.Max(0, 8 - Len(Math.Round(Delta, 6))), " "c)
            Info = Info & vbTab & "Output=" & Math.Round(Value, 6)
            If Split(IDGlobal, "-")(0) = "1" Then
                Info = Info & vbTab & "Weights=" & "Null"
            Else
                Info = Info & vbTab & "Weights=" & Weights.ToStr("|")
            End If
        End Get
    End Property
    Public Property DNA() As List(Of String)
        Get
            Dim Res As List(Of String)
            Res = New List(Of String)
            With Res
                .Add(ID)
                .Add(IDGlobal)
                .Add(Me.ActivationFunction)
                .Add(Me.Bias)
                .Add(Me.SumWB)
                .Add(Me.Delta)
                .Add(Me.AccDelta)
                .Add(Value)
                .Add(Weights.ToStr("|"))
            End With
            DNA = Res
        End Get
        Set(gen As List(Of String))
            With Me
                .ID = gen(0)
                .IDGlobal = gen(1)
                .ActivationFunction = gen(2)
                .Bias = gen(3)
                .SumWB = gen(4)
                .Delta = gen(5)
                .AccDelta = gen(6)
                Value = gen(7)
                Weights.ImportFromArray(Split(gen(8), "|"))
            End With
        End Set
    End Property

    Public Sub Create(NoInput&)
        Dim ii&
        Weights.Clear()

        For ii = 1 To NoInput
            Weights.Add((New Random).Next(100000, 500000) / 1000000)
        Next
        If Len(ActivationFunction) = 0 Then ActivationFunction = "Leaky_RELU"
        If Left(IDGlobal, 1) <> "1" Then Bias = Math.Round(Rnd(), 6)
    End Sub

    Public Sub UpdateNN(NoInput&)
        Dim ii&
        For ii = Weights.Count To NoInput
            Weights.Add(Math.Round(Rnd(), 6))
        Next
    End Sub


End Class