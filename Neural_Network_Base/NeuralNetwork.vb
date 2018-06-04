Option Explicit On
'Imports Neural_Network_Base

Public Structure Status
    Public st_FF&
    Public st_Updated_N&
    Public st_Input$
    Public st_CurrEpoch&
    Public CurrentIndex&
End Structure

'// Define Neural Network ================================================================================================================================================
'//
Public Class NeuralNet

    '//Variable used in network//
    Public Layers As List(Of layer)
    Private NetName As String
    Public LearningRate#
    Public Momentum#
    Public globalError#
    Private errList As List(Of Double)
    Public Active As Boolean

    Public st_FF&
    Public st_Updated_N&
    Public st_Input$
    Public st_CurrEpoch&
    Public CurrentIndex&

    Public InData As List(Of List(Of Double))
    Public TargetData As List(Of List(Of Double))


    '//Default
    Public Property Status() As Status
        Get
            Dim s As Status
            s.st_CurrEpoch = Me.st_CurrEpoch
            s.st_FF = Me.st_FF
            s.st_Updated_N = Me.st_Updated_N
            s.st_Input = Me.st_Input
            s.CurrentIndex = Me.CurrentIndex
            Return s
        End Get
        Set(value As Status)
            With value
                Me.st_CurrEpoch = .st_CurrEpoch
                Me.st_FF = .st_FF
                Me.st_Updated_N = .st_Updated_N
                Me.st_Input = .st_Input
                Me.CurrentIndex = .CurrentIndex
            End With
        End Set
    End Property
    Public ReadOnly Property InputNum&()
        Get
            InputNum = Layers(0).Neurons.Count
        End Get
    End Property
    Public ReadOnly Property OutputNum&()
        Get
            OutputNum = Layers(Layers.Count - 1).Neurons.Count
        End Get
    End Property
    Public ReadOnly Property ErrorArray() As List(Of Double)
        Get
            ErrorArray = errList
        End Get
    End Property
    Public Sub New()
        Layers = New List(Of layer)
        'default value
        LearningRate = 0.25
        Momentum = 0.3
        InData = New List(Of List(Of Double))
        TargetData = New List(Of List(Of Double))
        errList = New List(Of Double)
    End Sub
    Protected Overrides Sub Finalize()
        Layers.Clear()
        Layers = Nothing
        InData = Nothing
        TargetData = Nothing
        errList = Nothing
        MyBase.Finalize()
    End Sub

    Property Name() As String
        Get
            Name = NetName
        End Get
        Set(value As String)
            NetName = value
        End Set
    End Property

    Public ReadOnly Property Network_Information()
        Get
            Dim ii&, jj&
            Dim Str = ""
            For ii = 0 To Me.Layers.Count - 1
                For jj = 0 To Me.Layers(ii).Neurons.Count - 1
                    If Len(Str) = 0 Then
                        Str = Me.Layers(ii).Neurons(jj).Info
                    Else
                        Str = Str & vbCrLf & Me.Layers(ii).Neurons(jj).Info
                    End If
                Next
            Next
            Return Str
        End Get
    End Property
    Public ReadOnly Property Network_Structure()
        Get
            Dim ii&
            Dim str$ = ""
            For ii = 1 To Me.Layers.Count - 2
                If Len(str) = 0 Then
                    str = Me.Layers(ii).Neurons.Count & "." & Me.Layers(ii).Neurons(0).ActivationFunction
                Else
                    str = str & "+" & Me.Layers(ii).Neurons.Count & "." & Me.Layers(ii).Neurons(0).ActivationFunction
                End If
            Next ii
            str = str & "=" & Me.Layers(Me.Layers.Count - 1).Neurons.Count & "." & Me.Layers(Me.Layers.Count - 1).Neurons(0).ActivationFunction
            Return Me.Layers(0).Neurons.Count & "=" & str
        End Get
    End Property

    '//method
    Public Function ShallowCopy() As NeuralNet
        Return DirectCast(Me.MemberwiseClone(), NeuralNet)
    End Function
    Public Sub Save_NeuralNet(Name$, Path$)
        Dim ii&, jj&
        Dim Str As List(Of String) : Str = New List(Of String)
        With Str
            .Add(NetName)
            .Add(Network_Structure())
            .Add(LearningRate & "-" & Momentum & "-" & Math.Round(globalError, 15))

            For ii = 0 To Layers.Count - 1
                For jj = 0 To Layers(ii).Neurons.Count - 1
                    .Add(Layers(ii).Neurons(jj).DNA.ToStr("|"))
                Next
            Next
        End With
        If Len(Path) < 3 Then
            MsgBox("Path does not exist or lenght < 3") : Exit Sub
        Else
            'Save to txt file
            WriteFile(Str, Format(Now(), "ddmmyyyyhhmmss") & ".txt", "D:\")
        End If
        Str = Nothing
    End Sub
    Public Sub Load_NeuralNet()

        Dim ii&, jj&
        Dim Str As List(Of String) : Str = New List(Of String)
        Dim c& = 0
        Str = ReadFile(Getfile(False, "Select text file content Neural Net structure"))

        With Me
            .Reset()
            .Name = Str(0)
            .Build_NeuralNet(Str(1))
            .LearningRate = CDbl(Split(Str(2), "-")(0))
            .Momentum = CDbl(Split(Str(2), "-")(1))
            .globalError = CDbl(Split(Str(2), "-")(2))
            For ii = 0 To Me.Layers.Count - 1
                For jj = 0 To Me.Layers(ii).Neurons.Count - 1
                    .Layers(ii).Neurons(jj).DNA = C2List(Str(c + 3), " ")
                    c = c + 1
                Next
            Next
        End With
    End Sub

    Public Sub Reset()
        Layers.Clear()
        Layers = New List(Of layer)
        errList.Clear()
        errList = New List(Of Double)
        Active = False
        globalError = 0
    End Sub

    '// 2 method to build up network
    Public Function Build_NeuralNet(Genetic As String) As Boolean
        Dim strNetwork$, ii&, jj&
        If Not Active Then
            With Me
                For ii = 0 To UBound(Split(Genetic, "="))
                    strNetwork = Split(Genetic, "=")(ii)
                    If InStr(1, strNetwork, ".") > 0 Then
                        For jj = 0 To UBound(Split(strNetwork, "+"))
                            .AddLayer(CDbl(Split(Split(strNetwork, "+")(jj), ".")(0)), CStr(Split(Split(strNetwork, "+")(jj), ".")(1)))
                        Next
                    Else
                        .AddLayer(CDbl(strNetwork))
                    End If
                Next
            End With
            Active = True
            Build_NeuralNet = True
        Else
            Build_NeuralNet = False
        End If
    End Function

    Public Sub AddLayer(NoNeuron&, Optional AF$ = "Leaky_RELU")
        If Me.Active = True Then Exit Sub
        Dim L As layer = New layer
        With L
            .ActivationFunction = IIf(Layers.Count = 0, "", AF)
            .ID = Layers.Count + 1
            If Layers.Count = 0 Then
                .Create(NoNeuron, 0, "")
            Else
                .Create(NoNeuron, Layers(Layers.Count - 1).Neurons.Count)
            End If
        End With
        Layers.Add(L)
    End Sub
    '//input with setinput, output with result
    Public Function SetInput(InArray As List(Of Double))
        Dim ii&
        If InArray.Count > Layers.Item(0).Neurons.Count Then _
    MsgBox("Input array's Size: " & InArray.Count & " does not match with network: " & Layers.Item(0).Neurons.Count) : SetInput = False : Exit Function

        For ii = 0 To Layers(0).Neurons.Count - 1
            Layers(0).Neurons(ii).Value = InArray(ii)
        Next
        st_Input = InArray.ToStr("|")
        SetInput = True
    End Function
    Public Function Result(Optional Deli$ = "|") As String
        Dim ii&
        Dim R$ = ""
        'Output
        For ii = 0 To OutputNum - 1
            If ii > 0 Then
                R = R & Deli & Math.Round(CDbl(Layers(Layers.Count - 1).Neurons(ii).Value), 8)
            Else
                R = Math.Round(CDbl(Layers(Layers.Count - 1).Neurons(ii).Value), 8)
            End If
        Next
        Result = R
    End Function

    '//Execute input with calculate
    Public Function Calculate()
        Dim ii&, jj&, k&
        Dim sum#
        On Error GoTo ErrHandle
        Calculate = False
        For ii = 1 To Layers.Count - 1
            For jj = 0 To Layers(ii).Neurons.Count - 1
                With Layers(ii).Neurons(jj)
                    'Perf_Lap()
                    sum = 0
                    For k = 0 To Layers(ii - 1).Neurons.Count - 1
                        sum = sum + Layers(ii - 1).Neurons(k).Value * .Weights(k)
                    Next k
                    .SumWB = sum + .Bias
                    .Value = ActiveF(.ActivationFunction, .SumWB)
                End With
            Next
        Next
        Calculate = True
        Exit Function
ErrHandle:
        Debug.Print(" Error while calculate network!! " & vbTab & Err.Description)
    End Function

    Public Function MSE(Optional m& = -1)
        'Calculate Error RSM
        Dim ii&
        Dim ErrorSignal As Double = 0
        If m = -1 Then m = CurrentIndex
        For ii = 0 To OutputNum - 1
            With Layers(Layers.Count - 1).Neurons(ii)
                ErrorSignal = ErrorSignal + (TargetData(m)(ii) - .Value) ^ 2
            End With
        Next ii

        Return 0.5 * ErrorSignal / OutputNum
    End Function
    Public Sub PrepareInOut(setInputList As List(Of Double), setTargetList As List(Of Double))
        Dim NoInput = Me.InputNum
        Dim NoTarget = Me.OutputNum
        For ii = 0 To setInputList.Count - 1 Step NoInput
            If setInputList.Count Mod NoInput <> 0 Then If ii = setInputList.Count - 1 Then Exit For
            InData.Add(setInputList.GetRange(ii, NoInput))
        Next
        For ii = 0 To setTargetList.Count - 1 Step NoTarget
            If setTargetList.Count Mod NoTarget <> 0 Then If ii = setTargetList.Count - 1 Then Exit For
            TargetData.Add(setTargetList.GetRange(ii, NoTarget))
        Next
    End Sub

    Public Delegate Sub DelegateTrain(Layers As List(Of layer), InData As List(Of List(Of Double)), TargetData As List(Of List(Of Double)), stt As Status)
    Public Delegate Function ActivationFunc(X As Decimal) As Decimal

    Private Function ErrorS(AFType As String, sum As Double, Output As Double) As Double
        Select Case AFType
            Case "Sigmoid"
                ErrorS = Output * (1 - Output)
            Case "Leaky_RELU"
                ErrorS = IIf(sum < 0, 0.01, 1)
            Case "Soft_Plus"
                ErrorS = 1 / (1 + Math.Abs(sum)) ^ 2
            Case "Bent_Identity"
                ErrorS = sum / (2 * (sum ^ 2 + 1) ^ 0.5) + 1
        End Select
        Return ErrorS
    End Function
    Private Function ActiveF(AFType As String, sum As Double) As Double
        Select Case AFType
            Case "Sigmoid"
                ActiveF = 1 / (1 + Math.Exp(-sum))
            Case "Leaky_RELU"
                ActiveF = IIf(sum < 0, 0.01 * sum, sum)
            Case "Soft_Plus"
                ActiveF = Math.Log(1 + Math.Exp(sum))
            Case "Bent_Identity"
                ActiveF = ((sum ^ 2 + 1) ^ 0.5 - 1) / 2 + sum
            Case Else
                ActiveF = sum
        End Select
    End Function

    Public Sub Train_Flexible(Epoch&, FuncTrainAlgothym As DelegateTrain)

        If InData Is Nothing And TargetData Is Nothing Then MsgBox("Please feed data into network with PrepareInOut Method before train",, "Warning") : Exit Sub
        For i = 1 To Epoch
            Me.st_CurrEpoch = i
            FuncTrainAlgothym.Invoke(Layers, InData, TargetData, Me.Status)
        Next
    End Sub


    Public Sub TrainSpecial(TrainType$, Epoch&, setInputList As List(Of Double), setTargetList As List(Of Double), Optional Random As Boolean = False, Optional miniBatchSize& = 2, Optional ByRef oUserform As Object = Nothing)
        Dim ii&, jj&, kk&, T&
        Dim m&, n&, R&
        Dim ErrorSignal#, SumES#
        Dim ErrAccumulated#

        Dim miniBatch As List(Of Long)
        Dim NoMB#

        If setInputList Is Nothing Or setTargetList Is Nothing Then Exit Sub

        'New Train Method
        'Input all input by list object, 1 by 1 to inputList with Setinput method
        'Use batchSize to get from list

        'Prepare - this step split those input/target list into group
        PrepareInOut(setInputList, setTargetList)

        st_FF = 0
        st_Updated_N = 0
        st_Input = ""
        st_CurrEpoch = 0

        'SGD = loop 1 by 1
        'Batch = loop all item, accumulate all delta then update 1 time
        'Mini-Batch = loop all mini batch, accumulate all delta then update at the end of mini batch, next minibatch

        'Main section
        For T = 1 To Epoch
            Me.st_CurrEpoch = T
            Select Case TrainType
                Case "Stochastic"

                    ErrAccumulated = 0
                    For m = 0 To InData.Count - 1
                        '//Feedforward

                        '2 Random mode will select random layer and update only that layer


                        If SetInput(InData(m)) = False Then Exit Sub
                        CurrentIndex = m
                        Calculate()
                        Me.st_FF = st_FF + 1

                        'Backward propangation
                        'Delta of output layer
                        For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                            With Layers(Layers.Count - 1).Neurons(ii)
                                ErrorSignal = ErrorS(.ActivationFunction, .SumWB, .Value)
                                .Delta = ErrorSignal * (TargetData(m)(ii) - .Value) * (1 - Momentum) + .Delta * Momentum
                            End With
                        Next ii
                        'Delta of hidden layer
                        For jj = Layers.Count - 2 To 1 Step -1
                            For kk = 0 To Layers(jj).Neurons.Count - 1
                                ErrorSignal = 0
                                SumES = 0
                                With Layers(jj).Neurons(kk)
                                    ErrorSignal = ErrorS(.ActivationFunction, .SumWB, .Value)
                                    For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                        SumES = SumES + Layers(jj + 1).Neurons(ii).Weights(kk) * Layers(jj + 1).Neurons(ii).Delta
                                    Next ii
                                    .Delta = ErrorSignal * SumES * (1 - Momentum) + .Delta * Momentum
                                End With
                            Next kk
                        Next jj

                        'Update
                        For ii = Layers.Count - 1 To 1 Step -1
                            For jj = 0 To Layers(ii).Neurons.Count - 1
                                With Layers(ii).Neurons(jj)
                                    'Update Bias
                                    .Bias = .Bias + (Me.LearningRate * .Delta)
                                    'Update Weight
                                    For kk = 0 To .Weights.Count - 1
                                        .Weights(kk) = .Weights(kk) + (Me.LearningRate * .Delta * Layers(ii - 1).Neurons(kk).Value)
                                    Next kk
                                End With
                            Next jj
                        Next ii
                        'SGD Feedforward and backpropangation update 1 by 1
                        Me.st_Updated_N = Me.st_FF

                        ErrAccumulated = ErrAccumulated + MSE()
                        If Not oUserform Is Nothing Then oUserform.Refresh
                    Next m

                    ErrAccumulated = ErrAccumulated / InData.Count
                    globalError = ErrAccumulated
                    errList.Add(globalError)

                Case "Mini-Batch"

                    ErrAccumulated = 0
                    If miniBatchSize >= 0.55 * InData.Count Then
                        Select Case InData.Count
                            Case Is <= 10
                                miniBatchSize = 2
                            Case Is <= 100
                                miniBatchSize = 10
                            Case Is <= 1000
                                miniBatchSize = CLng(0.1 * InData.Count)
                            Case Else
                                miniBatchSize = CLng(0.2 * InData.Count)
                        End Select
                    End If

                    NoMB = InData.Count \ miniBatchSize

                    For m = 0 To InData.Count - 1 Step miniBatchSize

                        miniBatch = New List(Of Long)
                        For R = m To Math.Min(m + miniBatchSize - 1, InData.Count - 1)
                            miniBatch.Add(R)
                        Next

                        '//Feedforward
                        For n = 0 To miniBatch.Count - 1
                            If SetInput(InData(miniBatch(n))) = False Then Exit Sub
                            CurrentIndex = miniBatch(n)
                            Calculate()
                            Me.st_FF = st_FF + 1

                            'Backward propangation
                            'Delta of output layer
                            For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                With Layers(Layers.Count - 1).Neurons(ii)
                                    ErrorSignal = ErrorS(.ActivationFunction, .SumWB, .Value)
                                    .AccDelta = .AccDelta + ErrorSignal * (TargetData(miniBatch(n))(ii) - .Value) '* (1 - Momentum) + .Delta * Momentum
                                End With
                            Next ii
                            'Delta of hidden layer
                            For jj = Layers.Count - 2 To 1 Step -1
                                For kk = 0 To Layers(jj).Neurons.Count - 1
                                    ErrorSignal = 0
                                    SumES = 0
                                    With Layers(jj).Neurons(kk)
                                        ErrorSignal = ErrorS(.ActivationFunction, .SumWB, .Value)
                                        For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                            SumES = SumES + Layers(jj + 1).Neurons(ii).Weights(kk) * Layers(jj + 1).Neurons(ii).AccDelta
                                        Next ii
                                        .AccDelta = ErrorSignal * SumES '* (1 - Momentum) + .Delta * Momentum
                                    End With
                                Next kk
                            Next jj

                            'Calculate Error MSE
                            ErrAccumulated = ErrAccumulated + MSE()
                            If Not oUserform Is Nothing Then oUserform.Refresh
                        Next n

                        'Update
                        For ii = Layers.Count - 1 To 1 Step -1
                            For jj = 0 To Layers(ii).Neurons.Count - 1
                                With Layers(ii).Neurons(jj)
                                    'Update Bias
                                    .Bias = .Bias + (LearningRate * .Delta)
                                    'Update Weight
                                    For kk = 0 To .Weights.Count - 1
                                        .Weights(kk) = .Weights(kk) + (LearningRate * Layers(ii - 1).Neurons(kk).Value) * (.AccDelta * (1 - Momentum) + .Delta * Momentum)
                                        .Delta = .AccDelta
                                        .AccDelta = 0
                                    Next kk
                                End With
                            Next jj
                        Next ii
                        Me.st_Updated_N = st_Updated_N + 1
                        If Not oUserform Is Nothing Then oUserform.Refresh

                    Next m

                    ErrAccumulated = ErrAccumulated / m
                    globalError = ErrAccumulated
                    errList.Add(globalError)

                Case "Batch"
                    ErrAccumulated = 0

                    For m = 0 To InData.Count - 1

                        'random have no effect in batch trainning =.= random pos ?? no affect

                        '//Feedforward
                        If SetInput(InData(m)) = False Then Exit Sub
                        CurrentIndex = m
                        Calculate()
                        Me.st_FF = st_FF + 1
                        If Not oUserform Is Nothing Then oUserform.Refresh
                        'Backward propangation
                        'Delta of output layer
                        For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                            With Layers(Layers.Count - 1).Neurons(ii)
                                ErrorSignal = ErrorS(.ActivationFunction, .SumWB, .Value)
                                .AccDelta = .AccDelta + ErrorSignal * (TargetData(m)(ii) - .Value) '* (1 - Momentum) + .Delta * Momentum
                            End With
                        Next ii
                        'Delta of hidden layer
                        For jj = Layers.Count - 2 To 1 Step -1
                            For kk = 0 To Layers(jj).Neurons.Count - 1
                                ErrorSignal = 0
                                SumES = 0
                                With Layers(jj).Neurons(kk)
                                    ErrorSignal = ErrorS(.ActivationFunction, .SumWB, .Value)
                                    For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                        SumES = SumES + Layers(jj + 1).Neurons(ii).Weights(kk) * Layers(jj + 1).Neurons(ii).AccDelta
                                    Next ii
                                    .AccDelta = ErrorSignal * SumES '* (1 - Momentum) + .Delta * Momentum
                                End With
                            Next kk
                        Next jj


                        'Calculate Error MSE
                        ErrAccumulated = ErrAccumulated + MSE()
                    Next m

                    'Update
                    For ii = Layers.Count - 1 To 1 Step -1
                        For jj = 0 To Layers(ii).Neurons.Count - 1
                            With Layers(ii).Neurons(jj)
                                'Update Bias
                                .Bias = .Bias + (Me.LearningRate * .Delta)
                                'Update Weight
                                For kk = 0 To .Weights.Count - 1
                                    .Weights(kk) = .Weights(kk) + (Me.LearningRate * Layers(ii - 1).Neurons(kk).Value) * (.AccDelta * (1 - Momentum) + .Delta * Momentum)
                                    .Delta = .AccDelta
                                    .AccDelta = 0
                                Next kk
                            End With
                        Next jj
                    Next ii
                    Me.st_Updated_N = st_Updated_N + 1
                    If Not oUserform Is Nothing Then oUserform.Refresh0
                    ErrAccumulated = ErrAccumulated / m
                    globalError = ErrAccumulated
                    errList.Add(globalError)
            End Select
            If Not oUserform Is Nothing Then oUserform.Refresh
        Next T
        If Not oUserform Is Nothing Then oUserform.Refresh
    End Sub


    '================================================================
    '//Class for network//
    Public Class layer
        Public ID%
        Public ActivationFunction$
        Public Neurons As List(Of Neuron)
        Private BlockGate As Boolean

        Public Sub New()
            Neurons = New List(Of Neuron)
        End Sub

        Public Sub Create(NoNeuron&, NoInput&, Optional AF$ = "LeakyRELU")
            If BlockGate Then MsgBox("Can't create this layer again, it already created !! please use add method") : Exit Sub
            If Len(ID) = 0 Then MsgBox("Please assign ID for this layer !! Abort creating layer") : Exit Sub
            Dim ii&
            Dim n As Neuron
            For ii = 1 To NoNeuron
                n = New Neuron
                With n
                    .ID = ii
                    .IDGlobal = Me.ID & "-" & ii
                    .ActivationFunction = IIf(Len(ActivationFunction) = 0, AF, ActivationFunction)
                    .Create(NoInput)
                End With
                Neurons.Add(n)
            Next
            BlockGate = True
        End Sub

        Public Sub Add(ByRef n As Neuron)
            Neurons.Add(n)
        End Sub

    End Class
    Public Class Neuron
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

    '//Support function//

End Class
