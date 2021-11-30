unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.Diagnostics, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    btnPNG: TButton;
    btnQOI: TButton;
    imgShow: TImage;
    procedure btnPNGClick(Sender: TObject);
    procedure btnQOIClick(Sender: TObject);
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

procedure TForm1.btnQOIClick(Sender: TObject);
var
  bmpSrc   : TBitmap;
  bmpDst   : TBitmap;
  T1, T2   : Int64;
  srcBits  : Pointer;
  pixelsQOI: PByte;
  pixelsRGB: PByte;
  pQOI     : qoi_desc;
  pQOI2    : qoi_desc;
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
      pQOI.Width      := bmpSrc.Width;
      pQOI.Height     := bmpSrc.Height;
      pQOI.channels   := 4;
      pQOI.colorspace := 0;
      pixelsQOI       := qoi_encode(srcBits, @pQOI, outlen);
      T1              := ElapsedMilliseconds;
    end;

    { QOI decode }
    with TStopwatch.StartNew do
    begin
      pixelsRGB := qoi_decode(pixelsQOI, outlen, @pQOI2, 0);
      T2        := ElapsedMilliseconds;
    end;

    bmpDst.PixelFormat := pf32bit;
    bmpDst.Width       := pQOI2.Width;
    bmpDst.Height      := pQOI2.Height;
    SetBitmapBits(bmpDst.Handle, bmpDst.Width * bmpDst.Height * 4, pixelsRGB);
    imgShow.Picture.Bitmap.Assign(bmpDst);

    FreeMem(pixelsQOI);
    FreeMem(pixelsRGB);
  finally
    bmpSrc.Free;
    bmpDst.Free;
    btnQOI.Enabled := True;
  end;
  Caption := Format('QOI encode£º%d ms£»decode£º%d ms', [T1, T2]);
end;

end.
