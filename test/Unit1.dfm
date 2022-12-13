object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Test PNG/QOI'
  ClientHeight = 611
  ClientWidth = 725
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  DesignSize = (
    725
    611)
  PixelsPerInch = 96
  TextHeight = 15
  object imgShow: TImage
    Left = 16
    Top = 72
    Width = 694
    Height = 531
    Anchors = [akLeft, akTop, akRight, akBottom]
    Stretch = True
    ExplicitWidth = 497
    ExplicitHeight = 405
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
    Caption = 'TQoiImage'
    TabOrder = 2
    OnClick = btnQoiImageClick
  end
  object btnQoiDBYOUNG: TButton
    Left = 541
    Top = 16
    Width = 169
    Height = 41
    Caption = 'Qoi(dbyoung)'
    TabOrder = 3
    OnClick = btnQoiDBYOUNGClick
  end
end
