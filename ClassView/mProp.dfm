object PropForm: TPropForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #12463#12521#12473#12499#12517#12540
  ClientHeight = 177
  ClientWidth = 241
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object BarPosLabel: TLabel
    Left = 8
    Top = 8
    Width = 72
    Height = 13
    Caption = #12496#12540#12398#20301#32622'(&P):'
    FocusControl = BarPosComboBox
  end
  object ParamLabel: TLabel
    Left = 8
    Top = 56
    Width = 135
    Height = 13
    Caption = 'Ctags'#12408#12398#36861#21152#12497#12521#12513#12540#12479'(&A):'
    FocusControl = ParamEdit
  end
  object Bevel: TBevel
    Left = 0
    Top = 136
    Width = 241
    Height = 9
    Shape = bsTopLine
  end
  object BarPosComboBox: TComboBox
    Left = 8
    Top = 24
    Width = 129
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    Items.Strings = (
      #24038
      #19978
      #21491
      #19979)
  end
  object ParamEdit: TEdit
    Left = 8
    Top = 72
    Width = 225
    Height = 21
    TabOrder = 1
  end
  object OKButton: TButton
    Left = 64
    Top = 144
    Width = 81
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object CancelButton: TButton
    Left = 152
    Top = 144
    Width = 81
    Height = 25
    Cancel = True
    Caption = #12461#12515#12531#12475#12523
    ModalResult = 2
    TabOrder = 4
  end
  object AutoRefreshCheckBox: TCheckBox
    Left = 8
    Top = 104
    Width = 217
    Height = 17
    Caption = #33258#21205#26356#26032'(&R)'
    TabOrder = 2
  end
end
