<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class AddTrainingSet
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.tbTargetRange = New System.Windows.Forms.TextBox()
        Me.tbInputRange = New System.Windows.Forms.TextBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.cbGroup = New System.Windows.Forms.ComboBox()
        Me.bOk = New System.Windows.Forms.Button()
        Me.bCancel = New System.Windows.Forms.Button()
        Me.bLoadtxt = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'tbTargetRange
        '
        Me.tbTargetRange.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbTargetRange.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbTargetRange.ForeColor = System.Drawing.Color.Blue
        Me.tbTargetRange.Location = New System.Drawing.Point(188, 23)
        Me.tbTargetRange.Multiline = True
        Me.tbTargetRange.Name = "tbTargetRange"
        Me.tbTargetRange.Size = New System.Drawing.Size(170, 100)
        Me.tbTargetRange.TabIndex = 46
        '
        'tbInputRange
        '
        Me.tbInputRange.AccessibleRole = System.Windows.Forms.AccessibleRole.TitleBar
        Me.tbInputRange.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.tbInputRange.ForeColor = System.Drawing.Color.Blue
        Me.tbInputRange.Location = New System.Drawing.Point(12, 23)
        Me.tbInputRange.Multiline = True
        Me.tbInputRange.Name = "tbInputRange"
        Me.tbInputRange.Size = New System.Drawing.Size(170, 100)
        Me.tbInputRange.TabIndex = 45
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label13.Location = New System.Drawing.Point(361, 6)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(94, 16)
        Me.Label13.TabIndex = 44
        Me.Label13.Text = "Group Train:"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label12.Location = New System.Drawing.Point(185, 6)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(102, 16)
        Me.Label12.TabIndex = 43
        Me.Label12.Text = "Target Value:"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label11.Location = New System.Drawing.Point(8, 6)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(89, 16)
        Me.Label11.TabIndex = 42
        Me.Label11.Text = "Input Value:"
        '
        'cbGroup
        '
        Me.cbGroup.BackColor = System.Drawing.Color.Gainsboro
        Me.cbGroup.FlatStyle = System.Windows.Forms.FlatStyle.Popup
        Me.cbGroup.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cbGroup.ForeColor = System.Drawing.Color.Blue
        Me.cbGroup.FormattingEnabled = True
        Me.cbGroup.Items.AddRange(New Object() {"Train", "Validate", "Check", "Predict"})
        Me.cbGroup.Location = New System.Drawing.Point(364, 25)
        Me.cbGroup.Name = "cbGroup"
        Me.cbGroup.Size = New System.Drawing.Size(126, 21)
        Me.cbGroup.TabIndex = 41
        Me.cbGroup.Text = "Train"
        '
        'bOk
        '
        Me.bOk.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bOk.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bOk.ForeColor = System.Drawing.Color.SeaGreen
        Me.bOk.Location = New System.Drawing.Point(364, 64)
        Me.bOk.Name = "bOk"
        Me.bOk.Size = New System.Drawing.Size(63, 34)
        Me.bOk.TabIndex = 47
        Me.bOk.Text = "Add"
        Me.bOk.UseVisualStyleBackColor = False
        '
        'bCancel
        '
        Me.bCancel.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bCancel.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bCancel.ForeColor = System.Drawing.Color.Red
        Me.bCancel.Location = New System.Drawing.Point(433, 64)
        Me.bCancel.Name = "bCancel"
        Me.bCancel.Size = New System.Drawing.Size(63, 34)
        Me.bCancel.TabIndex = 48
        Me.bCancel.Text = "Cancel"
        Me.bCancel.UseVisualStyleBackColor = False
        '
        'bLoadtxt
        '
        Me.bLoadtxt.BackColor = System.Drawing.Color.WhiteSmoke
        Me.bLoadtxt.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.bLoadtxt.ForeColor = System.Drawing.Color.SeaGreen
        Me.bLoadtxt.Location = New System.Drawing.Point(154, 127)
        Me.bLoadtxt.Name = "bLoadtxt"
        Me.bLoadtxt.Size = New System.Drawing.Size(63, 34)
        Me.bLoadtxt.TabIndex = 47
        Me.bLoadtxt.Text = "Load File"
        Me.bLoadtxt.UseVisualStyleBackColor = False
        '
        'AddTrainingSet
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(501, 166)
        Me.Controls.Add(Me.bCancel)
        Me.Controls.Add(Me.bLoadtxt)
        Me.Controls.Add(Me.bOk)
        Me.Controls.Add(Me.tbTargetRange)
        Me.Controls.Add(Me.tbInputRange)
        Me.Controls.Add(Me.Label13)
        Me.Controls.Add(Me.Label12)
        Me.Controls.Add(Me.Label11)
        Me.Controls.Add(Me.cbGroup)
        Me.Name = "AddTrainingSet"
        Me.Text = "AddTrainingSet"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents tbTargetRange As TextBox
    Friend WithEvents tbInputRange As TextBox
    Friend WithEvents Label13 As Label
    Friend WithEvents Label12 As Label
    Friend WithEvents Label11 As Label
    Friend WithEvents cbGroup As ComboBox
    Friend WithEvents bOk As Button
    Friend WithEvents bCancel As Button
    Friend WithEvents bLoadtxt As Button
End Class
