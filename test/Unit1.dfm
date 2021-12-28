object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Test PNG/QOI'
  ClientHeight = 485
  ClientWidth = 550
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  PixelsPerInch = 96
  DesignSize = (
    550
    485)
  TextHeight = 15
  object imgShow: TImage
    Left = 16
    Top = 72
    Width = 519
    Height = 405
    Anchors = [akLeft, akTop, akRight, akBottom]
    Stretch = True
    ExplicitWidth = 497
  end
  object btnPNG: TButton
    Left = 16
    Top = 16
    Width = 169
    Height = 41
    Caption = 'PNG'
    TabOrder = 0
    OnClick = btnPNGClick
  end
  object btnQOI: TButton
    Left = 191
    Top = 16
    Width = 169
    Height = 41
    Caption = 'Qoi(OBJ)'
    TabOrder = 1
    OnClick = btnQOIClick
  end
  object btnQoiImage: TButton
    Left = 366
    Top = 16
    Width = 169
    Height = 41
    Caption = 'QoiImage'
    TabOrder = 2
    OnClick = btnQoiImageClick
  end
end
