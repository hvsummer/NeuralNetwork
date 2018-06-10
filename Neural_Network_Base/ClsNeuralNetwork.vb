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
Public Class NeuralNet

#Region "//Variable used in network//"

    Public Layers As List(Of layer)
    Private NetName As String
    Public LearningRate#
    Public Momentum#
    Public globalError#
    Private errList As List(Of Double)
    Public Active As Boolean


    Dim str As New Text.StringBuilder
    'Status
    Public st_FF&
    Public st_Updated_N&
    Public st_Input$
    Public st_CurrEpoch&
    Public CurrentIndex&
    Public st_Train As Boolean

    Private MaxEpoch&
    Public Trained_Total_Epoch%
    Public Trained_Time%

    Public ElapsedTime#

    Private Train_Thread As Threading.Thread

    '2 set of input data and target data using in each loop
    Public InData As List(Of List(Of Double))
    Public TargetData As List(Of List(Of Double))
#End Region

#Region "Delegate"

    Public Delegate Sub DelegateTrain(Layers As List(Of layer), InData As List(Of List(Of Double)), TargetData As List(Of List(Of Double)), stt As Status, AF As ActivationFunc)
    Public Delegate Function ActivationFunc(X As Decimal) As Decimal


#End Region

#Region "Properties"

    '//Default
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
        On Error Resume Next
        Train_Thread.Abort()
        On Error GoTo 0
        Layers.Clear()
        Layers = Nothing
        InData = Nothing
        TargetData = Nothing
        errList = Nothing
        MyBase.Finalize()
    End Sub

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

    Public ReadOnly Property Process() As String
        Get
            Dim strB As New Text.StringBuilder
            ElapsedTime += Perf_Lap()
            strB.Append("Status: FF(").Append(st_FF).Append(" - ").Append(Math.Round(st_FF / ElapsedTime, 2))
            strB.Append(") Updated(").Append(st_Updated_N).Append(" - ").Append(Math.Round(st_Updated_N / ElapsedTime, 2))
            strB.Append(") Epoch(").Append(st_CurrEpoch).Append("/").Append(MaxEpoch).Append(" - ").Append(Math.Round(st_CurrEpoch / MaxEpoch * 100, 2))
            strB.Append(") EpS(").Append(Math.Round(st_CurrEpoch / ElapsedTime, 2))
            strB.Append(") - ").Append(Math.Round(ElapsedTime, 4)).Append(" sec.")
            Return strB.ToString
        End Get
    End Property

    Public ReadOnly Property InputSize() As Long
        Get
            Return Layers(0).Neurons.Count
        End Get
    End Property
    Public ReadOnly Property OutputSize() As Long
        Get
            Return Layers(Layers.Count - 1).Neurons.Count
        End Get
    End Property
    Public ReadOnly Property ErrorArray() As List(Of Double)
        Get
            Return errList
        End Get
    End Property

    Property Name() As String
        Get
            Return NetName
        End Get
        Set(value As String)
            NetName = value
        End Set
    End Property

    Public ReadOnly Property Network_Information()
        Get
            Dim ii&, jj&
            Dim Str As New Text.StringBuilder
            For ii = 0 To Me.Layers.Count - 1
                For jj = 0 To Me.Layers.Item(ii).Neurons.Count - 1
                    If Str.Length = 0 Then
                        Str.Append(Layers.Item(ii).Neurons.Item(jj).Info)
                    Else
                        Str.Append(vbCrLf).Append(Me.Layers.Item(ii).Neurons.Item(jj).Info)
                    End If
                Next
            Next
            Return Str.ToString
        End Get
    End Property

    Public ReadOnly Property Network_Structure()
        Get
            Dim ii&
            Dim str As New Text.StringBuilder

            For ii = 1 To Me.Layers.Count - 2
                If str.Length = 0 Then
                    str.Append(Layers.Item(ii).Neurons.Count).Append(".").Append(Layers.Item(ii).Neurons.Item(0).ActivationFunction)
                Else
                    str.Append("+").Append(Layers.Item(ii).Neurons.Count).Append(".").Append(Layers.Item(ii).Neurons.Item(0).ActivationFunction)
                End If
            Next ii
            str.Append("=").Append(Me.Layers.Item(Me.Layers.Count - 1).Neurons.Count).Append(".").Append(Me.Layers.Item(Me.Layers.Count - 1).Neurons.Item(0).ActivationFunction)

            Return Me.Layers.Item(0).Neurons.Count & "=" & str.ToString
        End Get
    End Property

    Public ReadOnly Property Current_Result(Optional Deli$ = "|") As String
        Get
            Dim R$ = ""
            'Output
            For ii = 0 To OutputSize - 1
                If ii > 0 Then
                    R = R & Deli & Math.Round(Layers(Layers.Count - 1).Neurons(ii).Value, 8)
                Else
                    R = Math.Round(Layers(Layers.Count - 1).Neurons(ii).Value, 8)
                End If
            Next
            Return R
        End Get
    End Property
    Public ReadOnly Property Result(Optional Deli$ = vbCrLf) As String
        Get
            Dim s As New Text.StringBuilder
            For jj = 0 To InData.Count - 1
                SetInput(InData(jj))
                Calculate()
                'Update string Result array
                s.Append(Current_Result).Append(Deli)
            Next
            Return s.ToString
        End Get
    End Property
    Public ReadOnly Property Current_MSError(Optional Deli$ = vbCrLf) As String
        Get
            Return method_MSE()
        End Get
    End Property
    Public ReadOnly Property MSError(Optional Deli$ = vbCrLf) As String
        Get
            Dim s As New Text.StringBuilder
            Dim TempErr# = 0#
            For jj = 0 To InData.Count - 1
                SetInput(InData(jj))
                Calculate()
                s.Append(method_MSE()).Append(Deli)
            Next
            SetInput(InData(CurrentIndex))
            Return s.ToString
        End Get
    End Property

#End Region

    Public Sub Reset()
        Layers.Clear()
        Layers = New List(Of layer)
        errList.Clear()
        errList = New List(Of Double)
        Active = False
        globalError = 0
    End Sub

    '//method
    Public Function ShallowCopy() As NeuralNet
        Return DirectCast(Me.MemberwiseClone(), NeuralNet)
    End Function

#Region "Save and Load Network"
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
#End Region


#Region "Method Build Network"
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

#End Region


    Public Sub Train_Flexible(Epoch&, FuncTrainAlgothym As DelegateTrain, Optional AF As ActivationFunc = Nothing)

        If InData Is Nothing And TargetData Is Nothing Then MsgBox("Please feed data into network with PrepareInOut Method before train",, "Warning") : Exit Sub

        If AF Is Nothing Then
            'AF = New ActivationFunc(AddressOf FunctionList.Activation.Sigmoid)
            AF = AddressOf FunctionList.Activation.LeakyRELU
        End If

        For i = 1 To Epoch
            Me.st_CurrEpoch = i
            FuncTrainAlgothym.Invoke(Layers, InData, TargetData, Me.Status, AF)
        Next

    End Sub

    Public NotInheritable Class FunctionList

        ''' <summary>
        ''' list of Default function support for Activation and Loss
        ''' </summary>

        Public NotInheritable Class Activation

            Public Shared Function Sigmoid(Value As Decimal, Optional GD% = 0)
                If GD = 0 Then
                    Return 1 / (1 + Math.Exp(-Value))
                Else
                    Return Sigmoid(Value) * (1 - Sigmoid(Value))
                End If
            End Function

            Public Shared Function LeakyRELU(Value As Decimal, Optional GD% = 0)
                If GD = 0 Then
                    Return IIf(Value < 0, 0.01 * Value, Value)
                Else
                    Return IIf(Value < 0, 0.01, 1)
                End If
            End Function

            Public Shared Function SoftPlus(Value As Decimal, Optional GD% = 0)
                If GD = 0 Then
                    Return Math.Log(1 + Math.Exp(Value))
                Else
                    Return 1 / (1 + Math.Abs(Value)) ^ 2
                End If
            End Function

            Public Shared Function BentIndentity(Value As Decimal, Optional GD% = 0)
                If GD = 0 Then
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


            Public Shared Function Hinge(yHat As List(Of Decimal), y As List(Of Decimal)) As Decimal
                'Used for classification
                Dim sum As Decimal
                For i = 0 To y.Count - 1
                    sum = Math.Max(0, 1 - yHat(i) * y(i))
                Next
                Return sum
            End Function

            Public Shared Function MAE(yHat As List(Of Decimal), y As List(Of Decimal)) As Decimal
                'L1 Loss
                Dim sum As Decimal
                For i = 0 To y.Count - 1
                    sum += Math.Abs(yHat(i) - y(i))
                Next
                Return sum
            End Function

            Public Shared Function MSE(yHat As List(Of Decimal), y As List(Of Decimal)) As Decimal
                'L2 Loss
                Dim sum As Decimal
                For i = 0 To y.Count - 1
                    sum += (yHat(i) - y(i)) ^ 2
                Next
                Return sum / (2 * y.Count)
            End Function

        End Class

    End Class

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
        Return ActiveF
    End Function
    Private Function ErrorS(AFType As String, sum As Double) As Double
        Select Case AFType
            Case "Sigmoid"
                ErrorS = ActiveF(AFType, sum) * (1 - ActiveF(AFType, sum))
            Case "Leaky_RELU"
                ErrorS = IIf(sum < 0, 0.01, 1)
            Case "Soft_Plus"
                ErrorS = 1 / (1 + Math.Abs(sum)) ^ 2
            Case "Bent_Identity"
                ErrorS = sum / (2 * (sum ^ 2 + 1) ^ 0.5) + 1
        End Select
        Return ErrorS
    End Function

    Public Function method_MSE(Optional m& = -1) As Double
        'Calculate Error RSM
        Dim ErrorSignal As Double = 0
        If m = -1 Then m = CurrentIndex
        For ii = 0 To OutputSize - 1
            With Layers(Layers.Count - 1).Neurons(ii)
                ErrorSignal = ErrorSignal + (TargetData(m)(ii) - .Value) ^ 2
            End With
        Next ii

        Return ErrorSignal / (2 * OutputSize)
    End Function

#Region "Method to Set Input/Target Network"
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

    Public Sub PrepareInOut(setInputList As List(Of Double), setTargetList As List(Of Double))
        Dim NoInput& = InputSize
        Dim NoTarget& = OutputSize
        For ii = 0 To setInputList.Count - 1 Step NoInput
            If setInputList.Count Mod NoInput <> 0 Then If ii = setInputList.Count - 1 Then Exit For
            InData.Add(setInputList.GetRange(ii, NoInput))
        Next
        For ii = 0 To setTargetList.Count - 1 Step NoTarget
            If setTargetList.Count Mod NoTarget <> 0 Then If ii = setTargetList.Count - 1 Then Exit For
            TargetData.Add(setTargetList.GetRange(ii, NoTarget))
        Next
    End Sub
#End Region

    '//Execute input with calculate
    Public Overloads Function Calculate()
        Dim sum#
        Try
            For ii = 1 To Layers.Count - 1
                For jj = 0 To Layers.Item(ii).Neurons.Count - 1
                    With Layers.Item(ii).Neurons.Item(jj)
                        'Perf_Lap()
                        sum = 0
                        For k = 0 To Layers.Item(ii - 1).Neurons.Count - 1
                            sum = sum + Layers.Item(ii - 1).Neurons.Item(k).Value * .Weights.Item(k)
                        Next k
                        .Value = ActiveF(.ActivationFunction, sum + .Bias)
                    End With
                Next
            Next
            Return True
        Catch e As Exception
            Debug.Print(" Error while calculate network!! " & vbTab & Err.Description & vbCrLf & e.Message)
            Return False
        End Try

    End Function
    Public Overloads Function Calculate(AF As ActivationFunc)
        Dim sum#
        On Error GoTo ErrHandle
        Calculate = False
        For ii = 1 To Layers.Count - 1
            For jj = 0 To Layers.Item(ii).Neurons.Count - 1
                With Layers.Item(ii).Neurons.Item(jj)
                    'Perf_Lap()
                    sum = 0
                    For k = 0 To Layers.Item(ii - 1).Neurons.Count - 1
                        sum = sum + Layers.Item(ii - 1).Neurons.Item(k).Value * .Weights.Item(k)
                    Next k
                    .Value = AF.Invoke(sum + .Bias)
                End With
            Next
        Next
        Calculate = True
        Exit Function
ErrHandle:
        Debug.Print(" Error while calculate network!! " & vbTab & Err.Description)
    End Function

#Region "Default Method Trainning"
    Public Sub TrainSpecial(TrainType$, Epoch&, setInputList As List(Of Double), setTargetList As List(Of Double), Optional Random As Boolean = False, Optional miniBatchSize& = 2)

        Dim ErrorSignal#, SumES#
        Dim ErrAccumulated#

        Dim miniBatch As List(Of Long)
        Dim NoMB#


        If setInputList Is Nothing Or setTargetList Is Nothing Or st_Train Then Exit Sub
        st_Train = False

        Perf_Start()
        ElapsedTime = Perf_Lap()
        'New Train Method
        'Input all input by list object, 1 by 1 to inputList with Setinput method
        'Use batchSize to get from list

        'Prepare - this step split those input/target list into group
        PrepareInOut(setInputList, setTargetList)

        st_FF = 0
        st_Updated_N = 0
        st_Input = ""
        st_CurrEpoch = 0
        MaxEpoch = Epoch

        'SGD = loop 1 by 1
        'Batch = loop all item, accumulate all delta then update 1 time
        'Mini-Batch = loop all mini batch, accumulate all delta then update at the end of mini batch, next minibatch

        'Main section

        ElapsedTime += Perf_Lap()
        Select Case TrainType
            Case "Stochastic"
#Region "Stochastic"
                ShowThreads()
                'Train_Thread = New Threading.Thread(AddressOf Train_Stochastic)
                'Train_Thread.Priority = Threading.ThreadPriority.AboveNormal
                'Train_Thread.IsBackground = True
                'Train_Thread.Start(myTimer)
                Train_Stochastic()
                'Console.WriteLine(Train_Thread.ThreadState)
                Console.WriteLine("Start to print process")
                Console.WriteLine(str.ToString)
                ShowThreads()
#End Region
            Case "Mini-Batch"
#Region "Mini-Batch"
                For T = 1 To Epoch
                    Me.st_CurrEpoch = T
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
                            Me.st_FF += 1

                            'Backward propangation
                            'Delta of output layer
                            For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                With Layers(Layers.Count - 1).Neurons(ii)
                                    ErrorSignal = ErrorS(.ActivationFunction, .Value)
                                    .AccDelta += ErrorSignal * (TargetData(miniBatch(n))(ii) - .Value) '* (1 - Momentum) + .Delta * Momentum
                                End With
                            Next ii
                            'Delta of hidden layer
                            For jj = Layers.Count - 2 To 1 Step -1
                                For kk = 0 To Layers(jj).Neurons.Count - 1
                                    ErrorSignal = 0
                                    SumES = 0
                                    With Layers(jj).Neurons(kk)
                                        ErrorSignal = ErrorS(.ActivationFunction, .Value)
                                        For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                            SumES += Layers(jj + 1).Neurons(ii).Weights(kk) * Layers(jj + 1).Neurons(ii).AccDelta
                                        Next ii
                                        .AccDelta = ErrorSignal * SumES '* (1 - Momentum) + .Delta * Momentum
                                    End With
                                Next kk
                            Next jj

                            'Calculate Error MSE
                            ErrAccumulated += method_MSE()

                        Next n

                        'Update
                        For ii = Layers.Count - 1 To 1 Step -1
                            For jj = 0 To Layers(ii).Neurons.Count - 1
                                With Layers(ii).Neurons(jj)
                                    'Update Bias
                                    .Bias += (LearningRate * .Delta)
                                    'Update Weight
                                    For kk = 0 To .Weights.Count - 1
                                        .Weights(kk) += (LearningRate * Layers(ii - 1).Neurons(kk).Value) * (.AccDelta * (1 - Momentum) + .Delta * Momentum)
                                        .Delta = .AccDelta
                                        .AccDelta = 0
                                    Next kk
                                End With
                            Next jj
                        Next ii
                        Me.st_Updated_N += 1


                    Next m

                    ErrAccumulated = ErrAccumulated / InData.Count
                    globalError = ErrAccumulated
                    errList.Add(globalError)

                    ElapsedTime += Perf_Lap()
                Next T
#End Region
            Case "Batch"
#Region "Batch"
                For T = 1 To Epoch
                    Me.st_CurrEpoch = T
                    ErrAccumulated = 0

                    For m = 0 To InData.Count - 1

                        'random have no effect in batch trainning =.= random pos ?? no affect
                        '//Feedforward
                        If SetInput(InData(m)) = False Then Exit Sub
                        CurrentIndex = m
                        Calculate()
                        Me.st_FF += 1

                        'Backward propangation
                        'Delta of output layer
                        For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                            With Layers(Layers.Count - 1).Neurons(ii)
                                ErrorSignal = ErrorS(.ActivationFunction, .Value)
                                .AccDelta += ErrorSignal * (TargetData(m)(ii) - .Value) '* (1 - Momentum) + .Delta * Momentum
                            End With
                        Next ii
                        'Delta of hidden layer
                        For jj = Layers.Count - 2 To 1 Step -1
                            For kk = 0 To Layers(jj).Neurons.Count - 1
                                ErrorSignal = 0
                                SumES = 0
                                With Layers(jj).Neurons(kk)
                                    ErrorSignal = ErrorS(.ActivationFunction, .Value)
                                    For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                        SumES += Layers(jj + 1).Neurons(ii).Weights(kk) * Layers(jj + 1).Neurons(ii).AccDelta
                                    Next ii
                                    .AccDelta = ErrorSignal * SumES '* (1 - Momentum) + .Delta * Momentum
                                End With
                            Next kk
                        Next jj

                        'Calculate Error MSE
                        ErrAccumulated = ErrAccumulated + method_MSE()
                    Next m

                    'Update
                    For ii = Layers.Count - 1 To 1 Step -1
                        For jj = 0 To Layers(ii).Neurons.Count - 1
                            With Layers(ii).Neurons(jj)
                                'Update Bias
                                .Bias += (Me.LearningRate * .Delta)
                                'Update Weight
                                For kk = 0 To .Weights.Count - 1
                                    .Weights(kk) += (Me.LearningRate * Layers(ii - 1).Neurons(kk).Value) * (.AccDelta * (1 - Momentum) + .Delta * Momentum)
                                    .Delta = .AccDelta
                                    .AccDelta = 0
                                Next kk
                            End With
                        Next jj
                    Next ii

                    Me.st_Updated_N += 1

                    ErrAccumulated = ErrAccumulated / InData.Count
                    globalError = ErrAccumulated
                    errList.Add(globalError)

                    ElapsedTime += Perf_Lap()
                Next T
#End Region
        End Select
        Perf_Stop()
        Console.WriteLine(String.Format("Mainthread stop - end of main train method call -- State:{0} ThreadID:{1}", Threading.Thread.CurrentThread.ThreadState, Threading.Thread.CurrentThread.ManagedThreadId))
    End Sub

#End Region

    Private Sub ShowThreads()
        Console.WriteLine(String.Format("ThreadID:{0} -- ThreadState:{1}", Threading.Thread.CurrentThread.ManagedThreadId, Threading.Thread.CurrentThread.ThreadState))
    End Sub


    Public Sub Train_Stochastic()
        Dim ErrorSignal#, SumES#
        Dim ErrAccumulated#

        If st_Train Then Exit Sub

        st_Train = True
        For T = 1 To MaxEpoch
            Me.st_CurrEpoch = T
            ErrAccumulated = 0
            For m = 0 To InData.Count - 1
                '//Feedforward

                '//TODO = Random deative neuron 

                If SetInput(InData(m)) = False Then Exit Sub
                CurrentIndex = m
                Calculate()
                Me.st_FF += 1

                'Backward propangation
                'Delta of output layer
                For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                    With Layers(Layers.Count - 1).Neurons(ii)
                        ErrorSignal = ErrorS(.ActivationFunction, .Value)
                        'New * (1-p) + Old * p ; New = GD of Activation * GD of Loss
                        .Delta = ErrorSignal * (TargetData(m)(ii) - .Value) * (1 - Momentum) + .Delta * Momentum
                    End With
                Next ii
                'Delta of hidden layer
                For jj = Layers.Count - 2 To 1 Step -1
                    For kk = 0 To Layers(jj).Neurons.Count - 1
                        ErrorSignal = 0
                        SumES = 0
                        With Layers(jj).Neurons(kk)
                            ErrorSignal = ErrorS(.ActivationFunction, .Value)
                            'New in this situation = GD of Activation * Sum of (Weight(i+1) * Delta(i+1)) of layer i + 1 
                            For ii = 0 To Layers(Layers.Count - 1).Neurons.Count - 1
                                SumES += Layers(jj + 1).Neurons(ii).Weights(kk) * Layers(jj + 1).Neurons(ii).Delta
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
                            .Bias += (Me.LearningRate * .Delta)
                            'Update Weight
                            For kk = 0 To .Weights.Count - 1
                                .Weights(kk) += (Me.LearningRate * .Delta * Layers(ii - 1).Neurons(kk).Value)
                            Next kk
                        End With
                    Next jj
                Next ii
                'SGD Feedforward and backpropangation update 1 by 1
                Me.st_Updated_N = Me.st_FF
                ErrAccumulated += method_MSE()

            Next m
            ErrAccumulated = ErrAccumulated / InData.Count
            globalError = ErrAccumulated
            errList.Add(globalError)
            ElapsedTime += Perf_Lap()

            str.Append("Finished batch " & T).AppendLine()
            Console.WriteLine("Finished batch " & T)
        Next T

        ElapsedTime = 0
        st_Train = False
        Console.WriteLine("Finish train")

    End Sub



End Class


Public Class NN_Array

End Class