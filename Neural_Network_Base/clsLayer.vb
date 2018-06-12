Public Class layer
    Public ID%
    Public ActivationFunction$
    Public Neurons As List(Of Neuron)
    Private BlockGate As Boolean

    Public Sub New()
        Neurons = New List(Of Neuron)
    End Sub

    Public Sub Create(NoNeuron%, NoInput%, Optional AF$ = "LeakyRELU")
        If BlockGate Then MsgBox("Can't create this layer again, it already created !! please use add method") : Exit Sub
        If Len(ID) = 0 Then MsgBox("Please assign ID for this layer !! Abort creating layer") : Exit Sub

        Dim n As Neuron
        For ii = 1 To NoNeuron
            n = New Neuron
            With n
                .ID = ii
                .IDGlobal = Me.ID & "-" & ii
                .ActivationFunction = If(Len(ActivationFunction) = 0, AF, ActivationFunction)
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
