Option Explicit On


Public Structure Status
    Public st_FF%
    Public st_Updated_N%
    Public st_Input$
    Public st_CurrEpoch%
    Public CurrentIndex%
End Structure


'// Define Neural Network ================================================================================================================================================

Public Class NeuralNet

#Region "//Variable used in network//"

    Public Layers As List(Of Layer)
    Private NetName As String
    Public LearningRate#
    Public Momentum#
    Public globalError#
    Private errList As List(Of Double)
    Public Active As Boolean
    Private minibatchsize%

    'Status
    Public st_FF%
    Public st_Updated_N%
    Public st_Input$
    Public st_CurrEpoch%
    Public CurrentIndex%
    Public st_Train As Boolean

    Private MaxEpoch%
    Public Trained_Total_Epoch%
    Public Trained_Time%


    Public ElapsedTime#

    Public ThreadSafe As Boolean

    '2 set of input data and target data using in each loop
    Public InData As List(Of List(Of Double))
    Public TargetData As List(Of List(Of Double))
#End Region

#Region "Delegate"

    Public Delegate Sub DelegateTrain(AF As FunctionList.Delegate_NeuralNet.ActivationFunc, LF As FunctionList.Delegate_NeuralNet.LossFunction)

#End Region

#Region "Properties"

    '//Default
    Public Sub New()
        Layers = New List(Of Layer)
        'default value
        LearningRate = 0.25
        Momentum = 0.3
        InData = New List(Of List(Of Double))
        TargetData = New List(Of List(Of Double))
        errList = New List(Of Double)
        ThreadSafe = False
    End Sub
    Protected Overrides Sub Finalize()
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
            ElapsedTime += GetTime()
            strB.Append("Status: FF(").Append(st_FF).Append(" - ").Append(Math.Round(st_FF / ElapsedTime, 2))
            strB.Append(") Updated(").Append(st_Updated_N).Append(" - ").Append(Math.Round(st_Updated_N / ElapsedTime, 2))
            strB.Append(") Epoch(").Append(st_CurrEpoch).Append("/").Append(MaxEpoch).Append(" - ").Append(Math.Round(st_CurrEpoch / MaxEpoch * 100, 2))
            strB.Append(") EpS(").Append(Math.Round(st_CurrEpoch / ElapsedTime, 2))
            strB.Append(") - ").Append(Math.Round(ElapsedTime, 4)).Append(" sec.")
            Return strB.ToString
        End Get
    End Property

    Public ReadOnly Property InputSize() As Integer
        Get
            Return Layers(0)._Count
        End Get
    End Property
    Public ReadOnly Property OutputSize() As Integer
        Get
            Return Layers(Layers.Count - 1)._Count
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

    Public ReadOnly Property Network_Information() As String
        Get
            Dim Str As New Text.StringBuilder
            For ii = 0 To Me.Layers.Count - 1
                For jj = 0 To Layers.Item(ii)._Count - 1
                    If Str.Length = 0 Then
                        Str.Append(Layers.Item(ii).Info(jj))
                    Else
                        Str.Append(vbCrLf).Append(Layers.Item(ii).Info(jj))
                    End If
                Next
            Next
            Return Str.ToString
        End Get
    End Property

    Public ReadOnly Property Network_Structure() As String
        Get
            Dim str As New Text.StringBuilder

            For ii = 1 To Me.Layers.Count - 2
                If str.Length = 0 Then
                    str.Append(Layers.Item(ii)._Count).Append(".").Append(Layers.Item(ii)._AF.Method.Name)
                Else
                    str.Append("=").Append(Layers.Item(ii)._Count).Append(".").Append(Layers.Item(ii)._AF.Method.Name)
                End If
            Next ii
            str.Append("=").Append(OutputSize).Append(".").Append(Layers.Item(Me.Layers.Count - 1)._AF.Method.Name)

            Return InputSize & "=" & str.ToString
        End Get
    End Property

    Public Function InputToString() As String
        Dim s As New Text.StringBuilder
        For i = 0 To InData.Count - 1
            s.Append(InData(i).ToStr(vbTab)).AppendLine()
        Next
        Return s.ToString
    End Function

    Public Function Current_Result(Optional Deli$ = "|") As String
        Dim R$ = ""
        'Output
        For ii = 0 To OutputSize - 1
            If ii > 0 Then
                R &= Deli & Math.Round(Layers(Layers.Count - 1)._Value(ii), 8)
            Else
                R = Math.Round(Layers(Layers.Count - 1)._Value(ii), 8).ToString
            End If
        Next
        Return R
    End Function

    Public ReadOnly Property Result(Optional Deli$ = vbCrLf) As String
        Get
            Dim s As New Text.StringBuilder
            For jj = 0 To InData.Count - 1
                SetInput(InData(jj))
                Calculate()
                'Update string Result array
                s.Append(Me.Current_Result).Append(Deli)
            Next
            Return s.ToString
        End Get
    End Property

    Public Function Current_MSError(Optional Deli$ = vbCrLf) As String
        Return method_MSE().ToString
    End Function

    Public ReadOnly Property MSError(Optional Deli$ = vbCrLf) As String
        Get
            Dim s As New Text.StringBuilder
            Dim TempErr# = 0#
            For jj = 0 To InData.Count - 1
                SetInput(InData(jj))
                Calculate()
                s.Append(method_MSE()).Append(Deli)
            Next
            'SetInput(InData(CurrentIndex))
            Return s.ToString
        End Get
    End Property

#End Region

    Public Sub Reset()
        Layers.Clear()
        Layers = New List(Of Layer)
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

    End Sub
    Public Sub Load_NeuralNet()

    End Sub
#End Region


#Region "Method Build Network"
    '// 2 method to build up network
    Public Function Build_NeuralNet(Genetic As String) As Boolean
        Dim strLayer$
        Dim t() As String
        If Not Active Then
            With Me
                For ii = 0 To UBound(Split(Genetic, "="))
                    strLayer = Split(Genetic, "=")(ii)
                    If InStr(1, strLayer, ".") > 0 Then
                        t = Split(strLayer, ".")
                        .AddLayer(CInt(Split(Split(Genetic, "=")(ii - 1), ".")(0)),
                                  CInt(t(0)),
                                  DirectCast(FunctionList.Activation.CallByName(t(1)), FunctionList.Delegate_NeuralNet.ActivationFunc))
                    ElseIf ii = 0 Then
                        .AddLayer(CInt(strLayer),
                                  CInt(strLayer))
                    Else
                        .AddLayer(CInt(Split(Split(Genetic, "=")(ii - 1), ".")(0)), CInt(strLayer))
                    End If
                    Threading.Thread.Sleep(1000)
                Next
            End With
            Active = True
            Build_NeuralNet = True
        Else
            Build_NeuralNet = False
        End If
    End Function

    Public Sub AddLayer(Input%, Output%, Optional AF As FunctionList.Delegate_NeuralNet.ActivationFunc = Nothing)
        If Me.Active = True Then Exit Sub

        If Layers.Count = 0 Then AF = AddressOf FunctionList.Activation.INPUT
        If AF Is Nothing Then AF = AddressOf FunctionList.Activation.LeakyRELU
        Dim L As Layer = New Layer(Input, Output, AF)
        Layers.Add(L)

    End Sub

#End Region



    Private Function ActiveF(AFType As String, sum As Double) As Double
        Select Case AFType
            Case "Sigmoid"
                ActiveF = 1 / (1 + Math.Exp(-sum))
            Case "Leaky_RELU"
                ActiveF = If(sum < 0, 0.01 * sum, sum)
            Case "Soft_Plus"
                ActiveF = Math.Log(1 + Math.Exp(sum))
            Case "Bent_Identity"
                ActiveF = ((sum ^ 2 + 1) ^ 0.5 - 1) / 2 + sum
            Case Else
                ActiveF = sum
        End Select
        Return ActiveF
    End Function
    Private Function dActiveF(AFType As String, sum As Double) As Double
        Select Case AFType
            Case "Sigmoid"
                dActiveF = ActiveF(AFType, sum) * (1 - ActiveF(AFType, sum))
            Case "Leaky_RELU"
                dActiveF = If(sum < 0, 0.01, 1)
            Case "Soft_Plus"
                dActiveF = 1 / (1 + Math.Abs(sum)) ^ 2
            Case "Bent_Identity"
                dActiveF = sum / (2 * (sum ^ 2 + 1) ^ 0.5) + 1
        End Select
        Return dActiveF
    End Function

    Public Function method_MSE(Optional m% = -1) As Double
        'Calculate Error RSM
        Dim dActiveFignal As New List(Of Double)
        If m = -1 Then m = CurrentIndex
        For ii = 0 To OutputSize - 1
            With Layers(Layers.Count - 1)
                dActiveFignal.Add(Math.Abs(TargetData(m)(ii) - ._Value(ii)))
            End With
        Next ii
        Return dActiveFignal.Max()
    End Function

#Region "Method to Set Input/Target Network"
    '//input with setinput, output with result
    Public Function SetInput(InArray As List(Of Double)) As Boolean

        If InArray.Count > InputSize Then
            MsgBox("Input array's Size: " & InArray.Count & " does not match with current input size: " & InputSize)
            Return False
            Exit Function
        End If

        For ii = 0 To Layers(0)._Count - 1
            Layers(0)._Value(ii) = InArray(ii)
        Next

        st_Input = InArray.ToStr("|")
        Return True
    End Function

    Public Sub PrepareInOut(setInputList As List(Of Double), setTargetList As List(Of Double))
        Dim NoInput% = InputSize
        Dim NoTarget% = OutputSize

        InData.Clear()
        For ii = 0 To setInputList.Count - 1 Step NoInput
            If setInputList.Count Mod NoInput <> 0 Then If ii = setInputList.Count - 1 Then Exit For
            InData.Add(setInputList.GetRange(ii, NoInput))
        Next

        TargetData.Clear()
        For ii = 0 To setTargetList.Count - 1 Step NoTarget
            If setTargetList.Count Mod NoTarget <> 0 Then If ii = setTargetList.Count - 1 Then Exit For
            TargetData.Add(setTargetList.GetRange(ii, NoTarget))
        Next
    End Sub
#End Region

    '//Execute input with calculate
    Public Overloads Function Calculate() As Boolean
        Dim sum#
        Try
            For ii = 1 To Layers.Count - 1
                For jj = 0 To Layers.Item(ii)._Count - 1
                    With Layers.Item(ii)
                        sum = 0
                        For k = 0 To Layers.Item(ii - 1)._Count - 1
                            sum = sum + Layers.Item(ii - 1)._Value(k) * ._Weights(k, jj)
                        Next k
                        ._Value(jj) = ._AF.Invoke(sum + ._Bias(jj), False)
                    End With
                Next
            Next
            Return True
        Catch e As Exception
            Console.WriteLine(" Error while calculate network!! " & vbTab & Err.Description & vbCrLf & e.Message)
            Return False
        End Try
    End Function
    Public Overloads Function Calculate(AF As FunctionList.Delegate_NeuralNet.ActivationFunc) As Boolean
        Dim sum#
        Try
            For ii = 1 To Layers.Count - 1
                For jj = 0 To Layers.Item(ii)._Count - 1
                    With Layers.Item(ii)
                        sum = 0
                        For k = 0 To Layers.Item(ii - 1)._Count - 1
                            sum = sum + Layers.Item(ii - 1)._Value(k) * ._Weights(k, jj)
                        Next k
                        ._Value(jj) = AF.Invoke(sum + ._Bias(jj), False)
                    End With
                Next
            Next
            Return True
        Catch e As Exception
            Console.WriteLine(" Error while calculate network!! " & vbTab & Err.Description & vbCrLf & e.Message)
            Return False
        End Try
    End Function

#Region "Default Method Trainning"
    Public Sub TrainSpecial(TrainType$, Epoch%, setInputList As List(Of Double), setTargetList As List(Of Double), Optional Random As Boolean = False, Optional mbSize% = 2)

        If setInputList Is Nothing Or setTargetList Is Nothing Or st_Train Then Exit Sub
        st_Train = False

        setTime()
        ElapsedTime = 0
        'New Train Method
        'Input all input by list object, 1 by 1 to inputList with Setinput method
        'Use batchSize to get from list

        'Prepare - this step split those input/target list into group
        PrepareInOut(setInputList, setTargetList)

        'Reset status field
        st_FF = 0
        st_Updated_N = 0
        st_Input = ""
        st_CurrEpoch = 0
        MaxEpoch = Epoch
        minibatchsize = mbSize

        'SGD = loop 1 by 1
        'Batch = loop all item, accumulate all delta then update 1 time
        'Mini-Batch = loop all mini batch, accumulate all delta then update at the end of mini batch, next minibatch

        'Main section

        ElapsedTime += GetTime()
        Select Case TrainType
            Case "Stochastic"

                'Case use new thread to train -- TODO: Create new class handle trainning at new thread in background / update multithread per loop for each layer (1 thread for 1 neuron)
                'Train_Thread = New Threading.Thread(AddressOf Train_Stochastic)
                'Train_Thread.Priority = Threading.ThreadPriority.AboveNormal
                'Train_Thread.IsBackground = True
                'Train_Thread.Start(myTimer)
                Train_Stochastic()

            Case "Mini-Batch"

                Train_MiniBatch()

            Case "Batch"

                Train_Batch()

        End Select
        '//After finish train
        Trained_Time += 1
        Trained_Total_Epoch += Epoch

        'Console.WriteLine(String.Format("Mainthread of traing stop -- ThreadID:{0} State:{1}", Threading.Thread.CurrentThread.ManagedThreadId, Threading.Thread.CurrentThread.ThreadState))
    End Sub

    Public Sub Train_Flexible(Epoch%, FuncTrainAlgothym As DelegateTrain, Optional AF As FunctionList.Delegate_NeuralNet.ActivationFunc = Nothing, Optional LF As FunctionList.Delegate_NeuralNet.LossFunction = Nothing)

        If InData Is Nothing And TargetData Is Nothing Then MsgBox("Please feed data into network with PrepareInOut Method before train",, "Warning") : Exit Sub

        If AF Is Nothing Then AF = AddressOf FunctionList.Activation.LeakyRELU
        If LF Is Nothing Then LF = AddressOf FunctionList.Loss.MSE
        'AF = New ActivationFunc(AddressOf FunctionList.Activation.Sigmoid)

        For i = 1 To Epoch
            Me.st_CurrEpoch = i
            FuncTrainAlgothym.Invoke(AF, LF)
        Next
        Trained_Time += 1
        Trained_Total_Epoch += Epoch

    End Sub

#End Region

#Region "Stochastic"
    Public Sub Train_Stochastic()
        Dim dActFunc#, SumES#
        Dim ErrAccumulated As New List(Of Double)
        Dim nnError# = 0#
        Dim desired_error# = 0.0001

        If st_Train Then Exit Sub
        st_Train = True

        'Start train
        For T = 1 To MaxEpoch
            ThreadSafe = False
            Me.st_CurrEpoch = T
            ErrAccumulated.Clear()
            For m = 0 To InData.Count - 1
                '//Feedforward
                '//TODO = Random deative neuron 

                If SetInput(InData(m)) = False Then Exit Sub
                CurrentIndex = m
                Calculate()
                Me.st_FF += 1

                'Backward propangation
                'Delta of output layer
                For ii = 0 To Layers(Layers.Count - 1)._Count - 1
                    With Layers(Layers.Count - 1)

                        dActFunc = ._AF.Invoke(._Value(ii), True)
                        'New * (1-p) + Old * p which New = GD of Activation * GD of Loss
                        ._Grad(ii) = dActFunc * (TargetData(m)(ii) - ._Value(ii)) * (1 - Momentum) + ._Grad(ii) * Momentum

                    End With
                Next ii
                'Delta of hidden layer
                For jj = Layers.Count - 2 To 1 Step -1
                    For kk = 0 To Layers(jj)._Count - 1
                        dActFunc = 0
                        SumES = 0
                        With Layers(jj)
                            'New in this situation = GD of Activation * Sum of [ Weight(i+1) * Delta(i+1) ] of layer i + 1 
                            For ii = 0 To Layers(jj + 1)._Count - 1
                                SumES += Layers(jj + 1)._Weights(kk, ii) * Layers(jj + 1)._Grad(ii)
                            Next ii

                            dActFunc = ._AF.Invoke(._Value(kk), True)

                            ._Grad(kk) = dActFunc * SumES * (1 - Momentum) + ._Grad(kk) * Momentum

                        End With
                    Next kk
                Next jj

                'Update
                For ii = Layers.Count - 1 To 1 Step -1
                    For jj = 0 To Layers(ii)._Count - 1
                        With Layers(ii)
                            'Update Bias
                            ._Bias(jj) += (Me.LearningRate * ._Grad(jj))
                            'Update Weight
                            For kk = 0 To .InputSize - 1
                                ._Weights(kk, jj) += (LearningRate * ._Grad(jj) * Layers(ii - 1)._Value(kk))
                            Next kk
                        End With
                    Next jj
                Next ii


                'SGD Feedforward and backpropangation update 1 by 1
                Me.st_Updated_N = Me.st_FF
                ErrAccumulated.Add(method_MSE())
            Next m
            nnError = ErrAccumulated.Max

            globalError = nnError
            errList.Add(globalError)
            'Update time elapsed
            ElapsedTime += GetTime()
            ThreadSafe = True

            If T Mod 10 ^ If(T < 100, 1, CInt(Math.Floor(Math.Log10(T)))) = 0 Or T = MaxEpoch Or nnError < desired_error Then
                Console.WriteLine(String.Format("Epoch: {0}/{1}, max Err: {2} vs {3}", T, MaxEpoch, Math.Round(nnError, 8), desired_error))
            End If

            'Console.WriteLine("Finished batch " & T)
        Next T

        st_Train = False
        Console.WriteLine("Finish train")
    End Sub
#End Region

#Region "Mini-Batch"
    Public Sub Train_MiniBatch()
        Dim dActiveFignal#, SumES#
        Dim ErrAccumulated#
        Dim miniBatch As List(Of Integer)
        Dim NoMB#

        If st_Train Then Exit Sub
        st_Train = True

        For T = 1 To MaxEpoch
            ThreadSafe = False
            Me.st_CurrEpoch = T
            ErrAccumulated = 0
            If minibatchsize >= 0.55 * InData.Count Then
                Select Case InData.Count
                    Case Is <= 10
                        minibatchsize = 2
                    Case Is <= 100
                        minibatchsize = 10
                    Case Is <= 1000
                        minibatchsize = CInt(0.1 * InData.Count)
                    Case Else
                        minibatchsize = CInt(0.2 * InData.Count)
                End Select
            End If

            NoMB = InData.Count \ minibatchsize

            For m = 0 To InData.Count - 1 Step minibatchsize

                miniBatch = New List(Of Integer)
                For R = m To Math.Min(m + minibatchsize - 1, InData.Count - 1)
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
                    For ii = 0 To Layers(Layers.Count - 1)._Count - 1
                        With Layers(Layers.Count - 1)
                            dActiveFignal = ._AF.Invoke(._Value(ii), True)
                            ._AccGrad(ii) += dActiveFignal * (TargetData(miniBatch(n))(ii) - ._Value(ii)) '* (1 - Momentum) + .Delta * Momentum
                        End With
                    Next ii
                    'Delta of hidden layer
                    For jj = Layers.Count - 2 To 1 Step -1
                        For kk = 0 To Layers(jj)._Count - 1
                            dActiveFignal = 0
                            SumES = 0
                            With Layers(jj)
                                dActiveFignal = ._AF.Invoke(._Value(kk), True)
                                For ii = 0 To Layers(jj + 1)._Count - 1
                                    SumES += Layers(jj + 1)._Weights(ii, kk) * Layers(jj + 1)._AccGrad(ii)
                                Next ii
                                ._AccGrad(kk) = dActiveFignal * SumES '* (1 - Momentum) + .Delta * Momentum
                            End With
                        Next kk
                    Next jj

                    'Calculate Error MSE
                    ErrAccumulated += method_MSE()

                Next n

                'Update
                For ii = Layers.Count - 1 To 1 Step -1
                    For jj = 0 To Layers(ii)._Count - 1
                        With Layers(ii)
                            'Update Bias
                            ._Bias(jj) += (LearningRate * ._Grad(jj))
                            'Update Weight
                            For kk = 0 To .InputSize - 1
                                ._Weights(kk, jj) += (LearningRate * Layers(ii - 1)._Value(kk)) * (._AccGrad(jj) * (1 - Momentum) + ._Grad(jj) * Momentum)
                                ._Grad(jj) = ._AccGrad(jj)
                                ._AccGrad(jj) = 0
                            Next kk
                        End With
                    Next jj
                Next ii
                Me.st_Updated_N += 1

            Next m

            ErrAccumulated = ErrAccumulated / InData.Count
            globalError = ErrAccumulated
            errList.Add(globalError)
            ElapsedTime += GetTime()
            ThreadSafe = True
        Next T

        st_Train = False
    End Sub
#End Region

#Region "Batch"
    Public Sub Train_Batch()
        Dim dAF#, SumES#
        Dim ErrAccumulated#

        If st_Train Then Exit Sub
        st_Train = True

        For T = 1 To MaxEpoch
            ThreadSafe = False
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
                For ii = 0 To Layers(Layers.Count - 1)._Count - 1
                    With Layers(Layers.Count - 1)
                        dAF = ._AF.Invoke(._Value(ii), True)
                        ._AccGrad(ii) += dAF * (TargetData(m)(ii) - ._Value(ii)) '* (1 - Momentum) + .Delta * Momentum
                    End With
                Next ii
                'Delta of hidden layer
                For jj = Layers.Count - 2 To 1 Step -1
                    For kk = 0 To Layers(jj)._Count - 1
                        dAF = 0
                        SumES = 0
                        With Layers(jj)
                            dAF = ._AF.Invoke(._Value(kk), True)
                            For ii = 0 To Layers(jj + 1)._Count - 1
                                SumES += Layers(jj + 1)._Weights(ii, kk) * Layers(jj + 1)._AccGrad(ii)
                            Next ii
                            ._AccGrad(kk) = dAF * SumES '* (1 - Momentum) + .Delta * Momentum
                        End With
                    Next kk
                Next jj

                'Calculate Error MSE
                ErrAccumulated = ErrAccumulated + method_MSE()
            Next m

            'Update
            For ii = Layers.Count - 1 To 1 Step -1
                For jj = 0 To Layers(ii)._Count - 1
                    With Layers(ii)
                        'Update Bias
                        ._Bias(jj) += (Me.LearningRate * ._Grad(jj))
                        'Update Weight
                        For kk = 0 To .InputSize - 1
                            ._Weights(kk, jj) += (Me.LearningRate * Layers(ii - 1)._Value(kk)) * (._AccGrad(jj) * (1 - Momentum) + ._Grad(jj) * Momentum)
                            ._Grad(jj) = ._AccGrad(jj)
                            ._AccGrad(jj) = 0
                        Next kk
                    End With
                Next jj
            Next ii

            Me.st_Updated_N += 1

            ErrAccumulated = ErrAccumulated / InData.Count
            globalError = ErrAccumulated
            errList.Add(globalError)

            ElapsedTime += GetTime()
            ThreadSafe = True
        Next T
        st_Train = False
    End Sub
#End Region

End Class

'**************************************************************************************************************************************************************
'//Test new neural network with array (DArray Class)
'**************************************************************************************************************************************************************
Public Class ArrayNeural
    Private Shared stored_ID%

    Public ID%

    Public InputLayer As DArray

    Public Layers As List(Of DArray)
    Public Layers_Bias As List(Of DArray)
    Public Layers_Output As List(Of DArray)
    Public Layers_Grad As List(Of DArray)

    Public Active As Boolean = False
    Public LearningRate As Double = 0.4
    Public minibatchsize As Integer = 2
    Public MaxEpoch As Integer
    Public Momentum As Double = 0

    '2 set of input data and target data using in each loop
    Public InData As List(Of List(Of Double))
    Public TargetData As List(Of List(Of Double))
    Public verifyIN As List(Of List(Of Double))
    Public verifyOUT As List(Of List(Of Double))

    'Delegate
    Public Delegate Function AF(v As Double, GD As Boolean) As Double
    Public Delegate Function LF(l1 As DArray, l2 As DArray) As DArray
    Public ActivationFunction As AF
    Public LossFunction As LF

    'Property
    Public ReadOnly Property InputSize() As Integer
        Get
            Return If(InputLayer Is Nothing, 0, InputLayer.Columns)
        End Get
    End Property
    Public ReadOnly Property OutputSize() As Integer
        Get
            Return If(Layers_Output Is Nothing, 0, Layers_Output(Layers_Output.Count - 1).Columns)
        End Get
    End Property

    Public Sub New()
        ID = stored_ID
        stored_ID += 1

        Layers = New List(Of DArray)
        Layers_Bias = New List(Of DArray)
        Layers_Output = New List(Of DArray)
        Layers_Grad = New List(Of DArray)

        InData = New List(Of List(Of Double))
        TargetData = New List(Of List(Of Double))
        verifyIN = New List(Of List(Of Double))
        verifyOUT = New List(Of List(Of Double))
    End Sub
    Protected Overrides Sub Finalize()
        InData.Clear()
        TargetData.Clear()
        Layers.Clear()
        MyBase.Finalize()
    End Sub

    Public Shared Function count() As Integer
        Return stored_ID
    End Function


    Public Function Build_NeuralNet(Genetic As String, Optional Delimiter$ = "=") As Boolean
        If Not Active Then
            With Me
                'Example: 2=3=5=2; ii = {0,1,2,3} --- Layers( input matrix = 1x2 -- Hidden Matrix 2x3 : 3x5 -- Output matrix 5x2 -- 1 result matrix  = 1x2 )
                For ii = 0 To UBound(Split(Genetic, Delimiter))
                    If ii = 0 Then
                        .InputLayer = (New DArray(1, CInt(Split(Genetic, Delimiter)(ii)))).Fill(-0.5, 0.5)
                    Else
                        .AddLayer(CInt(Split(Genetic, Delimiter)(ii - 1)), CInt(Split(Genetic, Delimiter)(ii)))
                    End If
                Next
            End With
            Active = True
            Build_NeuralNet = True
        Else
            Build_NeuralNet = False
        End If
    End Function
    Public Sub AddLayer(Input%, Output%, Optional Bias As Boolean = True)
        If Me.Active = True Then Exit Sub
        Dim L As New DArray(Input, Output)
        L.Fill(0.1, 0.8)
        Layers.Add(L)
        If Bias Then
            Layers_Bias.Add((New DArray(InputLayer.Rows, Output)).Fill(-0.5, 0.5))
            Layers_Output.Add((New DArray(InputLayer.Rows, Output)).Fill(0))
            Layers_Grad.Add((New DArray(InputLayer.Rows, Output)).Fill(0))
        End If
    End Sub

#Region "Method to Set Input/Target Network"
    '//input with setinput, output with result
    Public Function SetInput(InArray As List(Of Double)) As Boolean
        If InArray.Count > InputLayer.Columns Then _
    MsgBox("Input array's Size: " & InArray.Count & " does not match with network: " & InputLayer.Columns) : SetInput = False : Exit Function

        Dim t As New DArray(InArray)
        InputLayer = t
        SetInput = True
    End Function

    Public Sub PrepareInOut(setInputList As List(Of Double), setTargetList As List(Of Double))
        Dim NoInput% = InputSize
        Dim NoTarget% = OutputSize

        InData.Clear()
        For ii = 0 To setInputList.Count - 1 Step NoInput
            If setInputList.Count Mod NoInput <> 0 Then If ii = setInputList.Count - 1 Then Exit For
            InData.Add(setInputList.GetRange(ii, NoInput))
        Next

        TargetData.Clear()
        For ii = 0 To setTargetList.Count - 1 Step NoTarget
            If setTargetList.Count Mod NoTarget <> 0 Then If ii = setTargetList.Count - 1 Then Exit For
            TargetData.Add(setTargetList.GetRange(ii, NoTarget))
        Next
    End Sub
    Public Sub SetVerify(InArray As List(Of Double), TargetArray As List(Of Double))
        Dim NoInput% = InputSize
        Dim NoTarget% = OutputSize

        verifyIN.Clear()
        For ii = 0 To InArray.Count - 1 Step NoInput
            If InArray.Count Mod NoInput <> 0 Then If ii = InArray.Count - 1 Then Exit For
            verifyIN.Add(InArray.GetRange(ii, NoInput))
        Next

        verifyOUT.Clear()
        For ii = 0 To TargetArray.Count - 1 Step NoTarget
            If TargetArray.Count Mod NoTarget <> 0 Then If ii = TargetArray.Count - 1 Then Exit For
            verifyOUT.Add(TargetArray.GetRange(ii, NoTarget))
        Next
    End Sub
#End Region

    Public Function current_Result() As DArray
        Return Layers_Output(Layers_Output.Count - 1).ApplyFunc(Function(x) Math.Round(x, 8))
    End Function
    Public Function Result() As String
        Dim s As New Text.StringBuilder
        For i = 0 To InData.Count - 1
            SetInput(InData(i))
            Calculate()
            s.Append(current_Result.ToString).AppendLine()
        Next
        Return s.ToString
    End Function
    Public Function InputToString() As String
        Dim s As New Text.StringBuilder
        For i = 0 To InData.Count - 1
            s.Append(InData(i).ToStr(vbTab)).AppendLine()
        Next
        Return s.ToString
    End Function
    Public Function WeightsToString() As String
        Dim s As New Text.StringBuilder
        For i = 0 To Layers.Count - 1
            s.Append(Layers(i).ToString).AppendLine()
        Next
        Return s.ToString
    End Function
    Public Function BiasToString() As String
        Dim s As New Text.StringBuilder
        For i = 0 To Layers_Bias.Count - 1
            s.Append(Layers_Bias(i).ToString).AppendLine()
        Next
        Return s.ToString
    End Function

    Public Sub Calculate()
        Dim t As DArray = InputLayer
        For i = 0 To Layers.Count - 1
            t = t * Layers(i) + Layers_Bias(i)
            t.ApplyFunc(Function(x) ActivationFunction.Invoke(x, False), True)
            Layers_Output(i) = t
        Next
    End Sub

    Public Function MaxError() As Double
        Dim E As New List(Of Double)
        Dim t As DArray
        For i = 0 To InData.Count - 1
            SetInput(InData(i))
            Calculate()
            t = LossFunction.Invoke((New DArray(TargetData(i))), Layers_Output(Layers_Output.Count - 1)).ApplyFunc(Function(x) Math.Abs(x))
            E.Add(t.Max())
        Next
        Return E.Max
    End Function

    Public Function VerifyError() As Double
        Dim E As New List(Of Double)
        Dim t As DArray
        For i = 0 To verifyIN.Count - 1
            SetInput(verifyIN(i))
            Calculate()
            t = LossFunction.Invoke((New DArray(verifyOUT(i))), Layers_Output(Layers_Output.Count - 1)).ApplyFunc(Function(x) Math.Abs(x))
            E.Add(t.Max())
        Next
        Return E.Max
    End Function
    Public Function VErrToString() As String
        Dim s As New Text.StringBuilder
        s.Append("IN:").AppendLine()
        For i = 0 To verifyIN.Count - 1
            s.Append(verifyIN(i).ToStr(vbTab)).AppendLine()
        Next
        s.Append("OUT:").AppendLine()
        For i = 0 To verifyIN.Count - 1
            SetInput(verifyIN(i))
            Calculate()
            s.Append(current_Result.ToString).AppendLine()
        Next
        Return s.ToString
    End Function
    Public Sub Tweak(Optional TotalError# = 3.0#, Optional TargetFix As Double = 1.5)
        Dim n%
        Do While MaxError() > TotalError And n < 1000
            n += 1
            For i = 0 To Layers.Count - 1
                Layers(i).Fill(-0.95, 0.95, TargetFix)
                Layers_Bias(i).Fill(-1.5, 1.5, TargetFix)
                Layers_Grad(i).Fill(0)
            Next
        Loop
    End Sub

    Public Sub Train_Default(epoch_limit%, desired_error#, Optional ConsoleMod% = 10)
        Dim Epoch%
        Dim nnError# = 1000
        Dim E As DArray
        Dim D As DArray
        Dim Errors As New List(Of Double)
        Dim dErr As Double

        MaxEpoch = epoch_limit
        Try
            Tweak(1, 0)
            Console.WriteLine("First Tweak, MaxError:" & MaxError().ToString("0.###"))
            Do
                Errors.Clear()
                For i = 0 To InData.Count - 1
                    '************************************************************************************************
                    'Feed Forward
                    SetInput(InData(i))
                    Calculate()

                    '************************************************************************************************
                    'Back Propangation

                    'Error Matrix of output Layer
                    E = LossFunction.Invoke((New DArray(TargetData(i))), Layers_Output(Layers_Output.Count - 1))
                    Errors.Add(E.ApplyFunc(Function(x) Math.Abs(x)).Max())

                    'Array Delta = error * (array of output with applied d_activationfunction) ??? is this correct ?
                    D = E * Layers_Output(Layers_Output.Count - 1).ApplyFunc(Function(x) ActivationFunction.Invoke(x, True))

                    Layers_Grad(Layers_Grad.Count - 1) = Layers_Grad(Layers_Grad.Count - 1) * Momentum + D * (1 - Momentum)

                    Layers(Layers.Count - 1) += Layers_Output(Layers_Output.Count - 2).T * Layers_Grad(Layers_Grad.Count - 1) * LearningRate

                    Layers_Bias(Layers_Bias.Count - 1) += D * LearningRate

                    'Error matrix of Hidden Layer
                    For index = Layers.Count - 2 To 0 Step -1
                        E = D * Layers(index + 1).T
                        D = E * Layers_Output(index).ApplyFunc(Function(x) ActivationFunction.Invoke(x, True))

                        Layers_Grad(index) = Layers_Grad(index) * Momentum + D * (1 - Momentum)

                        Layers(index) += If(index = 0, InputLayer.T, Layers_Output(index - 1).T) * Layers_Grad(index) * LearningRate
                        Layers_Bias(index) += D * LearningRate
                    Next

                Next
                If Errors.Max > nnError Then dErr += 1 Else dErr -= 0.01
                nnError = Errors.Max()
                If nnError > 10 Or dErr > 50 Then
                    Console.WriteLine(String.Format("Reset NN with tweak method (refill all Layers)" & vbCrLf & "current Err:{0}, No of delta Err: {1}, Epoch:{2}", nnError, Math.Round(dErr, 4), Epoch))
                    Tweak(1)
                    dErr = 0
                    nnError = desired_error + 1
                Else
                    If Epoch Mod ConsoleMod ^ If(Epoch < 100, 1, CInt(Math.Floor(Math.Log10(Epoch)))) = 0 OrElse Epoch = epoch_limit OrElse (nnError < desired_error And VerifyError() < desired_error) Then
                        Console.WriteLine(String.Format("Epoch: {0}/{1}, max Err: {2} vs {3}, Verify Err:{4}", Epoch, epoch_limit, Math.Round(nnError, 8), desired_error, Math.Round(VerifyError, 8)))
                    End If
                    Epoch += 1
                End If
            Loop While Epoch < epoch_limit And (nnError >= desired_error Or VerifyError() >= desired_error)
            Console.WriteLine("Finish Trainning !!")
        Catch g As Exception
            Console.WriteLine("Error message: " & g.Message)
        End Try
    End Sub

    Public NotInheritable Class FunctionList

        ''' <summary>
        ''' list of Default function support for Activation and Loss
        ''' </summary>

        Public NotInheritable Class Activation

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

        End Class

    End Class

End Class


