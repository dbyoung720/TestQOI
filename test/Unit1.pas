unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnPNG: TButton;
    btnQOI: TButton;
    imgShow: TImage;
    btnQoiImage: TButton;
    btnQoiDBYOUNG: TButton;
    btnQoilossy: TButton;
    procedure btnPNGClick(Sender: TObject);
    procedure btnQoiClick(Sender: TObject);
    procedure btnQoiImageClick(Sender: TObject);
    procedure btnQoiDBYOUNGClick(Sender: TObject);
    procedure btnQoilossyClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses uQOI, db.QOI, Vcl.Imaging.QOI;

{$R *.dfm}

type
  TBMPAccess         = class(TBitmap);
  TBitmapImageAccess = class(TBitmapImage);

procedure TForm1.btnPNGClick(Sender: TObject);
var
  pngEnc, pngDec: TPngImage;
  bmpEnc, bmpDec: TBitmap;
  T1, T2        : Int64;
  memTemp       : TMemoryStream;
begin
  btnPNG.Enabled  := False;
  imgShow.Picture := nil;
  Application.ProcessMessages;

  pngEnc  := TPngImage.Create;
  pngDec  := TPngImage.Create;
  bmpEnc  := TBitmap.Create;
  bmpDec  := TBitmap.Create;
  memTemp := TMemoryStream.Create;
  try
    bmpEnc.LoadFromFile('..\..\4K.bmp');
    bmpEnc.PixelFormat := pf32bit;
    bmpDec.PixelFormat := pf32bit;

    { PNG encode }
    with TStopwatch.StartNew do
    begin
      pngEnc.Assign(bmpEnc);
      pngEnc.SaveToStream(memTemp);
      T1 := ElapsedMilliseconds;
    end;

    { PNG decode }
    with TStopwatch.StartNew do
    begin
      memTemp.Position := 0;
      pngDec.LoadFromStream(memTemp);
      bmpDec.Width  := pngDec.Width;
      bmpDec.Height := pngDec.Height;
      bmpDec.Canvas.CopyRect(bmpDec.Canvas.ClipRect, pngDec.Canvas, bmpDec.Canvas.ClipRect);
      T2 := ElapsedMilliseconds;
    end;

    imgShow.Picture.Bitmap.Assign(bmpDec);
  finally
    bmpEnc.Free;
    bmpDec.Free;
    pngEnc.Free;
    pngDec.Free;
    memTemp.Free;
    btnPNG.Enabled := True;
  end;
  Caption := Format('PNG encode��%d ms��decode��%d ms', [T1, T2]);
end;

procedure TForm1.btnQoiClick(Sender: TObject);
var
  bmpSrc   : TBitmap;
  bmpDst   : TBitmap;
  T1, T2   : Int64;
  srcBits  : Pointer;
  pixelsQOI: PByte;
  pixelsRGB: PByte;
  pQoi     : qoi_desc;
  pQoi_db  : qoi_desc;
  outlen   : Integer;
begin
  btnQOI.Enabled  := False;
  imgShow.Picture := nil;
  Application.ProcessMessages;

  bmpSrc := TBitmap.Create;
  bmpDst := TBitmap.Create;
  try
    bmpSrc.LoadFromFile('..\..\4K.bmp');
    bmpSrc.PixelFormat := pf32bit;
    srcBits            := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;

    { QOI encode }
    with TStopwatch.StartNew do
    begin
      pQoi.Width      := bmpSrc.Width;
      pQoi.Height     := bmpSrc.Height;
      pQoi.channels   := 4;
      pQoi.colorspace := 0;
      pixelsQOI       := qoi_encode(srcBits, @pQoi, outlen);
      T1              := ElapsedMilliseconds;
    end;

    { QOI decode }
    with TStopwatch.StartNew do
    begin
      pixelsRGB := qoi_decode(pixelsQOI, outlen, @pQoi_db, 0);
      T2        := ElapsedMilliseconds;
    end;

    bmpDst.PixelFormat := pf32bit;
    bmpDst.Width       := pQoi_db.Width;
    bmpDst.Height      := pQoi_db.Height;
    Move(pixelsRGB^, TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits^, bmpDst.Width * bmpDst.Height * 4);
    imgShow.Picture.Bitmap.Assign(bmpDst);

    Freemem(pixelsQOI);
    Freemem(pixelsRGB);
  finally
    bmpSrc.Free;
    bmpDst.Free;
    btnQOI.Enabled := True;
  end;
  Caption := Format('Qoi encode��%d ms��decode��%d ms', [T1, T2]);
end;

procedure TForm1.btnQoiImageClick(Sender: TObject);
var
  QOI   : TQoiImage;
  T1, T2: Int64;
  mmQoi : TMemoryStream;
begin
  btnQoiImage.Enabled := False;
  imgShow.Picture     := nil;
  Application.ProcessMessages;

  mmQoi := TMemoryStream.Create;
  try
    { QoiImage encode }
    QOI := TQoiImage.Create;
    try
      QOI.LoadFromFile('..\..\4K.bmp');

      with TStopwatch.StartNew do
      begin
        QOI.SaveToStream(mmQoi);
        T1 := ElapsedMilliseconds;
      end;
    finally
      QOI.Free;
    end;

    { QoiImage decode }
    QOI := TQoiImage.Create;
    try
      mmQoi.Position := 0;
      with TStopwatch.StartNew do
      begin
        QOI.LoadFromStream(mmQoi);
        T2 := ElapsedMilliseconds;
      end;

      imgShow.Picture.Bitmap.Assign(QOI);
      Caption := Format('Qoi encode��%d ms��decode��%d ms', [T1, T2]);
    finally
      QOI.Free;
    end;
  finally
    mmQoi.Free;
    btnQoiImage.Enabled := True;
  end;
end;

procedure TForm1.btnQoiDBYOUNGClick(Sender: TObject);
var
  bmpSrc   : TBitmap;
  bmpDst   : TBitmap;
  T1, T2   : Int64;
  srcBits  : Pointer;
  pixelsQOI: Pointer;
  pixelsRGB: PByte;
  hQOI     : TQOIHEADER;
  outlen   : Integer;
  Count    : Integer;
begin
  btnQoiDBYOUNG.Enabled := False;
  imgShow.Picture       := nil;
  Application.ProcessMessages;

  bmpSrc := TBitmap.Create;
  bmpDst := TBitmap.Create;
  try
    bmpSrc.LoadFromFile('..\..\4K.bmp');
    bmpSrc.PixelFormat := pf32bit;
    srcBits            := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;

    { QOI encode }
    with TStopwatch.StartNew do
    begin
      hQOI.Magic      := QOI_MAGIC;
      hQOI.Width      := bmpSrc.Width;
      hQOI.Height     := bmpSrc.Height;
      hQOI.channels   := 4;
      hQOI.colorspace := 0;
      pixelsQOI       := qoi_encode_pascal_lossl(srcBits, hQOI, outlen);
      T1              := ElapsedMilliseconds;
    end;

    with TMemoryStream.Create do
    begin
      Write(hQOI, 14);
      Write(pixelsQOI^, outlen);
      SaveToFile('..\..\4K.qoi');
      Free;
    end;

    { QOI decode }
    with TStopwatch.StartNew do
    begin
      pixelsRGB := qoi_decode_pascal(pixelsQOI, outlen, 0, hQOI, Count);
      T2        := ElapsedMilliseconds;
    end;

    bmpDst.PixelFormat := pf32bit;
    bmpDst.Width       := hQOI.Width;
    bmpDst.Height      := hQOI.Height;
    Move(pixelsRGB^, TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits^, Count);
    imgShow.Picture.Bitmap.Assign(bmpDst);

    Freemem(pixelsQOI);
    Freemem(pixelsRGB);
  finally
    bmpSrc.Free;
    bmpDst.Free;
    btnQoiDBYOUNG.Enabled := True;
  end;
  Caption := Format('Qoi encode��%d ms��decode��%d ms', [T1, T2]);
end;

procedure TForm1.btnQoilossyClick(Sender: TObject);
var
  bmpSrc   : TBitmap;
  bmpDst   : TBitmap;
  T1, T2   : Int64;
  srcBits  : Pointer;
  pixelsQOI: Pointer;
  pixelsRGB: PByte;
  hQOI     : TQOIHEADER;
  outlen   : Integer;
  Count    : Integer;
  lossyCfg : PlossyCfg;
begin
  btnQoiDBYOUNG.Enabled := False;
  imgShow.Picture       := nil;
  Application.ProcessMessages;

  lossyCfg := nil;
  bmpSrc   := TBitmap.Create;
  bmpDst   := TBitmap.Create;
  try
    bmpSrc.LoadFromFile('..\..\4K.bmp');
    bmpSrc.PixelFormat := pf32bit;
    srcBits            := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;

    { QOI encode }
    with TStopwatch.StartNew do
    begin
      hQOI.Magic      := QOI_MAGIC;
      hQOI.Width      := bmpSrc.Width;
      hQOI.Height     := bmpSrc.Height;
      hQOI.channels   := 4;
      hQOI.colorspace := 0;
      pixelsQOI       := qoi_encode_pascal_lossy(srcBits, hQOI, outlen, lossyCfg);
      T1              := ElapsedMilliseconds;
    end;

    with TMemoryStream.Create do
    begin
      Write(hQOI, 14);
      Write(pixelsQOI^, outlen);
      SaveToFile('..\..\4K.qoiy');
      Free;
    end;

    { QOI decode }
    with TStopwatch.StartNew do
    begin
      pixelsRGB := qoi_decode_pascal(pixelsQOI, outlen, 0, hQOI, Count);
      T2        := ElapsedMilliseconds;
    end;

    bmpDst.PixelFormat := pf32bit;
    bmpDst.Width       := hQOI.Width;
    bmpDst.Height      := hQOI.Height;
    Move(pixelsRGB^, TBitmapImageAccess(TBMPAccess(bmpDst).FImage).FDIB.dsBm.bmBits^, Count);
    imgShow.Picture.Bitmap.Assign(bmpDst);

    Freemem(pixelsQOI);
    Freemem(pixelsRGB);
  finally
    bmpSrc.Free;
    bmpDst.Free;
    btnQoiDBYOUNG.Enabled := True;
  end;
  Caption := Format('Qoi encode��%d ms��decode��%d ms', [T1, T2]);
end;

end.
