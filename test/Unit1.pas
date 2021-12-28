unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Imaging.QOI;

type
  TForm1 = class(TForm)
    btnPNG: TButton;
    btnQOI: TButton;
    imgShow: TImage;
    btnQoiImage: TButton;
    procedure btnPNGClick(Sender: TObject);
    procedure btnQoiClick(Sender: TObject);
    procedure btnQoiImageClick(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

uses uQOI;

{$R *.dfm}

type
  TBMPAccess         = class(TBitmap);
  TBitmapImageAccess = class(TBitmapImage);

procedure TForm1.btnPNGClick(Sender: TObject);
var
  png   : TPngImage;
  bmp   : TBitmap;
  T1, T2: Int64;
begin
  btnPNG.Enabled := False;
  png            := TPngImage.Create;
  bmp            := TBitmap.Create;
  try
    bmp.PixelFormat := pf32bit;
    bmp.LoadFromFile('test.bmp');

    { PNG encode }
    with TStopwatch.StartNew do
    begin
      png.Assign(bmp);
      T1 := ElapsedMilliseconds;
    end;

    { PNG decode }
    with TStopwatch.StartNew do
    begin
      imgShow.Picture.Assign(png);
      T2 := ElapsedMilliseconds;
    end;
  finally
    bmp.Free;
    png.Free;
    btnPNG.Enabled := True;
  end;
  Caption := Format('PNG encode£º%d ms£»decode£º%d ms', [T1, T2]);
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
  pQoi2    : qoi_desc;
  outlen   : Integer;
begin
  btnQOI.Enabled := False;
  bmpSrc         := TBitmap.Create;
  bmpDst         := TBitmap.Create;
  try
    bmpSrc.PixelFormat := pf32bit;
    bmpSrc.LoadFromFile('test.bmp');
    srcBits := TBitmapImageAccess(TBMPAccess(bmpSrc).FImage).FDIB.dsBm.bmBits;

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
      pixelsRGB := qoi_decode(pixelsQOI, outlen, @pQoi2, 0);
      T2        := ElapsedMilliseconds;
    end;

    bmpDst.PixelFormat := pf32bit;
    bmpDst.Width       := pQoi2.Width;
    bmpDst.Height      := pQoi2.Height;
    SetBitmapBits(bmpDst.Handle, bmpDst.Width * bmpDst.Height * 4, pixelsRGB);
    imgShow.Picture.Bitmap.Assign(bmpDst);

    FreeMem(pixelsQOI);
    FreeMem(pixelsRGB);
  finally
    bmpSrc.Free;
    bmpDst.Free;
    btnQOI.Enabled := True;
  end;
  Caption := Format('Qoi(Obj) encode£º%d ms£»decode£º%d ms', [T1, T2]);
end;

procedure TForm1.btnQoiImageClick(Sender: TObject);
var
  QOI   : TQoiImage;
  T1, T2: Int64;
  mmQoi : TMemoryStream;
begin
  btnQoiImage.Enabled := False;
  mmQoi               := TMemoryStream.Create;
  try
    { QoiImage encode }
    QOI := TQoiImage.Create;
    try
      QOI.Image.LoadFromFile('test.bmp');
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

      imgShow.Picture.Bitmap.Assign(QOI.Image);
      Caption := Format('QoiImage encode£º%d ms£»decode£º%d ms', [T1, T2]);
    finally
      QOI.Free;
    end;
  finally
    mmQoi.Free;
    btnQoiImage.Enabled := True;
  end;
end;

end.
