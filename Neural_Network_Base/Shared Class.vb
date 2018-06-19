Imports System.Reflection
Imports System.Runtime.CompilerServices
Imports Neural_Network_Base

Public NotInheritable Class Training_Method

    ''' <summary>
    ''' list of training method for Neural Network
    ''' </summary>

    Public Class iRprop_Plus

        Private _Net As NeuralNet

        Private Layers As New List(Of iLayer)
        Private Class iLayer
            Inherits Layer

#Region "Update new variable for iRprop Plus Method"
            Public _wGradsAcc(,) As Double
            Public _wPrevGradsAcc(,) As Double
            Public _wPrevD(,) As Double
            Public _wPrevChanges(,) As Double
            Public _bGradsAcc() As Double
            Public _bPrevGradsAcc() As Double
            Public _bPrevD() As Double
            Public _bPrevChanges() As Double
#End Region

            'Public Sub New(Input As Integer, Output As Integer, AF As FunctionList.Delegate_NeuralNet.ActivationFunc)
            'MyBase.New(Input, Output, AF)
            'End Sub

            Public Sub New(ByRef iClass As Layer)
                MyBase.New(iClass.InputSize, iClass.OutputSize, iClass._AF)
                Dim InSize% = iClass.InputSize - 1
                Dim OutSize% = iClass.OutputSize - 1

                _Weights = iClass._Weights
                _Bias = iClass._Bias
                _Grad = iClass._Grad
                _AccGrad = iClass._AccGrad
                _Value = iClass._Value
                _Count = iClass._Count
                _ID = iClass._ID
                ReDim _wGradsAcc(0 To InSize, 0 To OutSize)
                ReDim _wPrevGradsAcc(0 To InSize, 0 To OutSize)
                ReDim _wPrevD(0 To InSize, 0 To OutSize)
                ReDim _wPrevChanges(0 To InSize, 0 To OutSize)

                ReDim _bGradsAcc(0 To OutSize)
                ReDim _bPrevGradsAcc(0 To OutSize)
                ReDim _bPrevD(0 To OutSize)
                ReDim _bPrevChanges(0 To OutSize)

                '_wGradsAcc.Fill(0)
                '_wPrevGradsAcc.Fill(0)
                _wPrevD.Fill(_DeltaIni)
                '_wPrevChanges.Fill(0)
                _bPrevD.Fill(_DeltaIni)

            End Sub

            Public Sub ReturnWeights(ByRef iClass As Layer)
                With iClass
                    ._Weights = _Weights
                    ._Bias = _Bias
                    ._Value = _Value
                    ._Grad = _Grad
                End With
            End Sub

        End Class

        'Const (can be modify) of iRprop Training Method
        Public Shared _DeltaMax As Double = 10
        Public Shared _DeltaMin As Double = 0.001
        Public Shared _DeltaIni As Double = 0.5
        Public Shared _EtaPositive As Double = 1.2
        Public Shared _EtaNegative As Double = 0.5
        Public Shared _ZeroTolerance As Double = 1.0E-17

        Private _MSEPrev As Double
        Private _MSELast As Double

        Public Sub New(ByRef Net As NeuralNet)
            _Net = Net
            For i = 0 To _Net.Layers.Count - 1
                Layers.Add(New iLayer(_Net.Layers(i)))
            Next
            _MSEPrev = 0
            _MSELast = 0
        End Sub

        'Method use to train will handle 1 epoch train
        Public Sub iRprop_plus(AF As FunctionList.Delegate_NeuralNet.ActivationFunc,
                               LF As FunctionList.Delegate_NeuralNet.LossFunction)
            'TODO update Optional BatchSize% = 0
            Dim cLayer As iLayer
            Dim SSE# = 0, outErr# = 0
            Dim Sum# = 0

            With _Net
                _MSEPrev = _MSELast
                _MSELast = 0
                .Trained_Total_Epoch += 1

                'Loop through all training input of current epoch
                For i = 0 To .InData.Count - 1
                    '#Set Input 
                    .SetInput(.InData(i))

                    '******************************************************************************************
                    '#FeedForward 
                    .Calculate(AF)

                    '******************************************************************************************
                    '#BackPropangation with iRpop+ algothrim

                    'Normal Process to find Grad of each neuron from Output to Hidden Layer
                    'Output Layer
                    cLayer = Layers(Layers.Count - 1)

                    For j = 0 To cLayer._Count - 1
                        outErr = LF.Invoke(.TargetData(i)(j), cLayer._Value(j))
                        cLayer._Grad(j) = cLayer._AF.Invoke(cLayer._Value(j), True) * outErr
                        SSE += outErr ^ 2
                    Next

                    'Hidden Layer
                    For j = Layers.Count - 2 To 1 'Loop through each layer
                        cLayer = Layers(j)
                        Sum = 0
                        With cLayer
                            For k = 0 To ._Count - 1 'Loop through each neuron/Node
                                For l = 0 To Layers(j + 1)._Count - 1 'Loop through each neuron/Node of Right-Layer (index +1)
                                    Sum += Layers(j + 1)._Weights(k, l) * Layers(j + 1)._Grad(l)
                                Next
                                ._Grad(k) = ._AF.Invoke(._Value(k), True) * Sum
                            Next
                        End With
                    Next

                    'iRprop+ implement here:
                    'Update Acc Grad Input
                    For j = Layers.Count - 1 To 1 Step -1

                        'We don't need to fill innitial weight Grads Accumulation = 0 
                        'due to New iLayer redim this array into default value of varriable data type
                        'Layers(j)._wGradsAcc.Fill(0)

                        cLayer = Layers(j)
                        With cLayer
                            For k = 0 To ._Count - 1
                                'Update Weight
                                For l = 0 To .InputSize - 1
                                    ._wGradsAcc(l, k) += ._Grad(k) * Layers(j - 1)._Value(l)
                                Next l

                                'Update Bias
                                ._bGradsAcc(k) += ._Grad(k) * ._Bias(k)

                            Next k

                            'Copy data Grad to PrevGrad
                            ._wGradsAcc.CopyTo(._wPrevGradsAcc, 0)

                        End With

                    Next j
                Next

                Parallel.For(0, Layers.Count - 1, Sub(x)
                                                      iRprop_Plus_wUpdate(x)
                                                      iRprop_Plus_bUpdate(x)
                                                  End Sub)


                'Return weight + bias to Layer form iLayer
                For i = 0 To Layers.Count - 1
                    Layers(i).ReturnWeights(_Net.Layers(i))
                Next

                _MSELast /= _Net.InData.Count * _Net.OutputSize
            End With
        End Sub

        Private Sub iRprop_Plus_wUpdate(x%)
            Dim weightChange As Double = 0
            Dim delta As Double = 0
            Dim oLayer As iLayer = Layers(x)

            With oLayer
                For iweight = 0 To .InputSize - 1
                    For ineuron = 0 To .OutputSize - 1
                        Dim gradSign As Double = Sign(._wPrevGradsAcc(iweight, ineuron) * ._wGradsAcc(iweight, ineuron))

                        If gradSign > 0 Then
                            delta = Math.Min(._wPrevD(iweight, ineuron) * _EtaPositive, _DeltaMax)
                            ._wPrevD(iweight, ineuron) = delta
                            weightChange = Sign(._wGradsAcc(iweight, ineuron)) * delta
                        ElseIf gradSign < 0 Then
                            delta = Math.Max(._wPrevD(iweight, ineuron) * _EtaNegative, _DeltaMin)
                            ._wPrevD(iweight, ineuron) = delta
                            If _MSELast > _MSEPrev Then weightChange = - ._wPrevChanges(iweight, ineuron)
                            ._wGradsAcc(iweight, ineuron) = 0
                        Else
                            delta = ._wPrevD(iweight, ineuron)
                            weightChange = Sign(._wGradsAcc(iweight, ineuron)) * delta
                        End If

                        ._Weights(iweight, ineuron) += weightChange
                        ._wPrevChanges(iweight, ineuron) = weightChange
                    Next
                Next
            End With

        End Sub

        Private Sub iRprop_Plus_bUpdate(x%)
            Dim BiasChange As Double = 0
            Dim delta As Double = 0
            Dim oLayer As iLayer = Layers(x)

            With oLayer
                For indexNeuron = 0 To ._Count - 1

                    Dim gradSign As Double = Sign(._bPrevGradsAcc(indexNeuron) * ._bGradsAcc(indexNeuron))

                    If gradSign > 0 Then
                        delta = Math.Min(._bPrevD(indexNeuron) * _EtaPositive, _DeltaMax)
                        ._bPrevD(indexNeuron) = delta
                        BiasChange = Sign(._bGradsAcc(indexNeuron)) * delta
                    ElseIf gradSign < 0 Then
                        delta = Math.Max(._bPrevD(indexNeuron) * _EtaNegative, _DeltaMin)
                        ._bPrevD(indexNeuron) = delta
                        If _MSELast > _MSEPrev Then BiasChange = - ._bPrevChanges(indexNeuron)
                        ._bGradsAcc(indexNeuron) = 0
                    Else
                        delta = ._bPrevD(indexNeuron)
                        BiasChange = Sign(._bGradsAcc(indexNeuron)) * delta
                    End If

                    ._Bias(indexNeuron) += BiasChange
                    ._bPrevChanges(indexNeuron) = BiasChange

                Next
            End With

        End Sub


        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Private Function Sign(ByVal value As Double) As Double
            If Math.Abs(value) <= _ZeroTolerance Then Return 0 Else Return Math.Sign(value)
        End Function

    End Class



End Class


Public NotInheritable Class FunctionList

    ''' <summary>
    ''' list of Default function support for Activation and Loss
    ''' </summary>

    Public NotInheritable Class Delegate_NeuralNet
        Public Delegate Sub DelegateTrain(Layers As List(Of Layer), AF As ActivationFunc, LF As LossFunction)
        Public Delegate Function ActivationFunc(X As Double, GD As Boolean) As Double
        Public Delegate Function LossFunction(yHat As Double, y As Double) As Double
    End Class

    Public NotInheritable Class Activation

        <MethodImpl(MethodImplOptions.AggressiveInlining)>
        Public Shared Function CallByName(strName$) As [Delegate]
            Dim T As Type = GetType(Activation)
            Dim M As MethodInfo = T.GetMethod(strName)
            Dim AF As Delegate_NeuralNet.ActivationFunc = DirectCast(M.CreateDelegate(GetType(Delegate_NeuralNet.ActivationFunc)), Delegate_NeuralNet.ActivationFunc)
            Return AF
        End Function

        Public Shared Function INPUT(Value As Double, Optional GD As Boolean = False) As Double
            Return Value
        End Function


        Public Shared Function Sigmoid(Value As Double, Optional GD As Boolean = False) As Double
            If Not GD Then
                Return 1 / (1 + Math.Exp(-Value))
            Else
                Return Sigmoid(Value) * (1 - Sigmoid(Value))
            End If
        End Function

        Public Shared Function LeakyRELU(Value As Double, Optional GD As Boolean = False) As Double
            If Not GD Then
                Return If(Value < 0, 0.01 * Value, Value)
            Else
                Return If(Value < 0, 0.01, 1)
            End If
        End Function

        Public Shared Function SoftPlus(Value As Double, Optional GD As Boolean = False) As Double
            If Not GD Then
                Return Math.Log(1 + Math.Exp(Value))
            Else
                Return 1 / (1 + Math.Abs(Value)) ^ 2
            End If
        End Function

        Public Shared Function BentIndentity(Value As Double, Optional GD As Boolean = False) As Double
            If Not GD Then
                Return ((Value ^ 2 + 1) ^ 0.5 - 1) / 2 + Value
            Else
                Return Value / (2 * (Value ^ 2 + 1) ^ 0.5) + 1
            End If
        End Function

    End Class

    Public NotInheritable Class Loss

        ''' <summary>
        ''' Loss function: error is calculated as the difference between the actual output and the predicted output
        ''' </summary>


        Public Shared Function Hinge(yHat As List(Of Double), y As List(Of Double)) As Double
            'Used for classification
            Dim sum As Double
            For i = 0 To y.Count - 1
                sum = Math.Max(0, 1 - yHat(i) * y(i))
            Next
            Return sum
        End Function

        Public Shared Function MAE(yHat As List(Of Double), y As List(Of Double)) As Double
            'L1 Loss
            Dim sum As Double
            For i = 0 To y.Count - 1
                sum += Math.Abs(yHat(i) - y(i))
            Next
            Return sum
        End Function

        Public Shared Function MSE(yHat As List(Of Double), y As List(Of Double)) As Double
            'L2 Loss
            Dim sum As Double
            For i = 0 To y.Count - 1
                sum += (yHat(i) - y(i)) ^ 2
            Next
            Return sum / (2 * y.Count)
        End Function
        Public Shared Function MSE(yHat As DArray, y As DArray) As DArray
            'de of L2 Loss
            Return yHat - y
        End Function
        Public Shared Function MSE(yHat As Double, y As Double) As Double
            'de of L2 Loss
            Return yHat - y
        End Function

    End Class

End Class