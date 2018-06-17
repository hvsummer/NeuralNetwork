Imports System.Runtime.CompilerServices
<Serializable>
Public Class Layer

    Private Shared __Instances% = 1

    Public _ID%

    Public _AF As NeuralNet.ActivationFunc
    Public _Weights(,) As Double
    Public _Bias#()
    Public _Delta#()
    Public _AccDelta#()
    Public _Value#()
    Public _Count%
    Public ReadOnly Property InputSize() As Integer
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Get
            Return _Weights.GetUpperBound(0)
        End Get
    End Property
    Public ReadOnly Property OutputSize() As Integer
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Get
            Return _Weights.GetUpperBound(1)
        End Get
    End Property

    Public Sub New(Input%, Output%, AF As NeuralNet.ActivationFunc)
        ReDim _Weights(0 To Input - 1, 0 To Output - 1)
        ReDim _Bias(0 To Output - 1)
        ReDim _Delta(0 To Output - 1)
        ReDim _AccDelta(0 To Output - 1)
        ReDim _Value(0 To Output - 1)
        _Count = Output
        _Weights.fill(0.2 / (Output * Input), 0.8 / (Output * Input))
        _Bias.Fill(-1 / (Output * Input), 1 / (Output * Input))
        _Delta.Fill(0)
        _AF = AF
        _ID = __Instances
        __Instances += 1
    End Sub

    Public ReadOnly Property Info(Index%) As String
        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Get
            Dim Builder As New Text.StringBuilder
            'Neuron information will have 2 part
            '1 general information: type, Id
            '2 It''s value
            Builder.Append(_ID).Append("-").Append(Index).Append("-").Append(_AF.Method.Name).Append(vbTab)
            Builder.Append("Bias=").Append(Math.Round(_Bias(Index), 6)).Append(If(Len(_Bias(Index)) > 6, "...", "")).Append(StrDup(Math.Max(0, 8 - Len(Math.Round(_Bias(Index), 6))), " "c)).Append(vbTab)
            Builder.Append("Delta=").Append(Math.Round(_Delta(Index), 6)).Append(If(Len(_Delta(Index)) > 6, "...", "")).Append(StrDup(Math.Max(0, 8 - Len(Math.Round(_Delta(Index), 6))), " "c)).Append(vbTab)
            Builder.Append("Output=").Append(Math.Round(_Value(Index), 6)).Append(vbTab)
            If _ID = 1 Then
                Builder.Append("Weights=").Append("Null")
            Else
                Builder.Append("Weights=").Append(_Weights.ToStr("|"))
            End If
            Return Builder.ToString
        End Get
    End Property

End Class

Module Layer_Support
    <Extension()>
    Public Function Fill(ByRef S As Double(), Value As Double) As Double()
        Return S.Zip(Enumerable.Range(0, S.GetUpperBound(0)), Function() Value).ToArray
    End Function
    <Extension()>
    Public Function Fill(ByRef S As Double(,), Value As Double) As Double(,)
        For x = S.GetLowerBound(0) To S.GetUpperBound(0)
            For y = S.GetUpperBound(1) To S.GetUpperBound(1)
                S(x, y) = Value
            Next
        Next
        Return S
    End Function

    <Extension()>
    Public Function Fill(ByRef S As Double(), StartValue As Double, EndValue As Double) As Double()
        Dim r As New Random(CInt(Date.Now.Ticks And &HFFFF))
        Dim e As Integer = CInt(Math.Truncate(Math.Max(Math.Abs(Math.Log10(StartValue)), Math.Abs(Math.Log10(EndValue)))))
        For i = 0 To S.Length - 1
            S(i) = r.Next(CInt(StartValue * 10 ^ e), CInt(EndValue * 10 ^ e)) / 10 ^ e
        Next
        Return S
    End Function
    <Extension()>
    Public Function Fill(ByRef S As Double(,), StartValue As Double, EndValue As Double) As Double(,)
        Dim r As New Random(CInt(Date.Now.Ticks And &HFFFF))
        Dim e As Integer = CInt(Math.Truncate(Math.Max(Math.Abs(Math.Log10(StartValue)), Math.Abs(Math.Log10(EndValue)))))
        For x = S.GetLowerBound(0) To S.GetUpperBound(0)
            For y = S.GetUpperBound(1) To S.GetUpperBound(1)
                S(x, y) = r.Next(CInt(StartValue * 10 ^ e), CInt(EndValue * 10 ^ e)) / 10 ^ e
            Next
        Next
        Return S
    End Function

    <Extension()>
    Public Function ToStr(ByRef source As Double(,), Optional Delimiter$ = ",") As String
        If source.Length = 0 Then ToStr = "Null" : Exit Function
        Dim sb As New Text.StringBuilder
        For Each g In source
            sb.Append(g).Append(Delimiter)
        Next
        Return sb.Remove(sb.Length - 1, 1).ToString
    End Function

End Module