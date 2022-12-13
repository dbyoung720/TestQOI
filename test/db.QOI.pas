unit db.QOI;
{
  FUNC: QOI Image Encode / Decode
  Auth: dbyoung@sina.com
  Time: 2022-10-01
}

interface

type
  { QOI Header; 14 bytes }
  TQOIHeader = packed record
    Magic: Cardinal;
    Width: Cardinal;
    Height: Cardinal;
    Channels: Byte;
    Colorspace: Byte;
  end;

  PQOIHeader = ^TQOIHeader;

{ QOI ENCODE }
function qoi_encode_pascal(const Buffer: Pointer; const desc: TQOIHeader; var intlen: Integer): Pointer;

{ QOI DECODE }
function qoi_decode_pascal(const Buffer: Pointer; const BufferSize: Integer; const Channels: Integer; var desc: TQOIHeader; var Count: Integer): Pointer;

implementation

uses System.Threading, System.SyncObjs;

const
  QOI_OP_INDEX                        = $0;
  QOI_OP_DIFF                         = $40;
  QOI_OP_LUMA                         = $80;
  QOI_OP_RUN                          = $C0;
  QOI_OP_RGB                          = $FE;
  QOI_OP_RGBA                         = $FF;
  QOI_MASK_2                          = $C0;
  QOI_MAGIC: Cardinal                 = $66696F71;
  QOI_pixels_MAX                      = 400000000;
  qoi_padding_size                    = 8;
  qoi_padding: array [0 .. 7] of Byte = (0, 0, 0, 0, 0, 0, 0, 1);

type
  _RGBA = record
    r, g, b, a: Byte;
  end;

  QOI_RGBA_T = packed record
    case Boolean of
      false:
        (rgba: _RGBA);
      true:
        (V: Cardinal);
  end;

  TQOI_RGBA_T = QOI_RGBA_T;
  PQOI_RGBA_T = ^TQOI_RGBA_T;

  TArrQoi_rgba_t = array [0 .. 63] of TQOI_RGBA_T;
  TArrSixByte    = array [0 .. 5] of Byte;

function QOI_COLOR_HASH(c: TQOI_RGBA_T): Byte; inline;
begin
  Result := (c.rgba.r * 3 + c.rgba.g * 5 + c.rgba.b * 7 + c.rgba.a * 11) and $3F;
end;

procedure qoi_write_32(const P: PByte; const val: Cardinal); inline;
begin
  PCardinal(P)^ := val;
end;

procedure qoi_write_16(const P: PByte; const val: WORD); inline;
begin
  PWORD(P)^ := val;
end;

procedure qoi_write_8(const P: PByte; const val: Byte); inline;
begin
  P^ := val;
end;

procedure qoi_write_arr(const P: PByte; const val: TArrSixByte; const Count: Integer); inline;
begin
  if Count = 1 then
    qoi_write_8(P, val[0])
  else if Count = 2 then
    qoi_write_16(P, PWORD(@val)^)
  else if Count = 4 then
    qoi_write_32(P, PCardinal(@val)^)
  else
    Move(val[0], P^, Count);
end;

function qoi_encode_pascal_parallel(const px: PQOI_RGBA_T): TArrSixByte; inline;
{$J+}
const
  run: Integer          = 0;
  index: TArrQoi_rgba_t = (                                         //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0)  //
    );
  px_prev: TQOI_RGBA_T = (V: $FF000000);
{$J-}
var
  vr, vg, vb, vg_r, vg_b: Integer;
  index_pos             : Integer;
  Count                 : Integer;
begin
  Count := 0;

  if px^.V = px_prev.V then
  begin
    Inc(run);
    if (run = 62) then
    begin
      Result[Count] := QOI_OP_RUN or (run - 1);
      Inc(Count);
      run := 0;
    end;
  end
  else
  begin
    if (run > 0) then
    begin
      Result[Count] := QOI_OP_RUN or (run - 1);
      Inc(Count);
      run := 0;
    end;

    index_pos := QOI_COLOR_HASH(px^);
    if (index[index_pos].V = px^.V) then
    begin
      Result[Count] := QOI_OP_INDEX or index_pos;
      Inc(Count);
    end
    else
    begin
      index[index_pos] := px^;
      if (px^.rgba.a = px_prev.rgba.a) then
      begin
        vr   := px^.rgba.r - px_prev.rgba.r;
        vg   := px^.rgba.g - px_prev.rgba.g;
        vb   := px^.rgba.b - px_prev.rgba.b;
        vg_r := vr - vg;
        vg_b := vb - vg;
        if ((vr > -3) and (vr < 2) and (vg > -3) and (vg < 2) and (vb > -3) and (vb < 2)) then
        begin
          Result[Count] := QOI_OP_DIFF or (vr + 2) shl 4 or (vg + 2) shl 2 or (vb + 2);
          Inc(Count);
        end
        else if ((vg_r > -9) and (vg_r < 8) and (vg > -33) and (vg < 32) and (vg_b > -9) and (vg_b < 8)) then
        begin
          Result[Count + 0] := QOI_OP_LUMA or (vg + 32);
          Result[Count + 1] := (vg_r + 8) shl 4 or (vg_b + 8);
          Inc(Count, 2);
        end
        else
        begin
          Result[Count + 0] := QOI_OP_RGB;
          Result[Count + 1] := px^.rgba.r;
          Result[Count + 2] := px^.rgba.g;
          Result[Count + 3] := px^.rgba.b;
          Inc(Count, 4);
        end
      end
      else
      begin
        Result[Count + 0] := QOI_OP_RGBA;
        Result[Count + 1] := px^.rgba.r;
        Result[Count + 2] := px^.rgba.g;
        Result[Count + 3] := px^.rgba.b;
        Result[Count + 4] := px^.rgba.a;
        Inc(Count, 5);
      end;
    end;
  end;

  Result[5] := Count;
  px_prev   := px^;
end;

{ QOI ENCODE }
function qoi_encode_pascal(const Buffer: Pointer; const desc: TQOIHeader; var intlen: Integer): Pointer;
var
  I, max_size  : Integer;
  intStartPos  : Integer;
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
  Width, Height: Integer;
  bytes        : PByte;
  px           : PQOI_RGBA_T;
  X, Y         : Integer;
  tmpArr       : TArrSixByte;
  intCount     : Integer;
begin
  Result := nil;

  if (Buffer = nil) or (intlen = -1) or (desc.Width = 0) or (desc.Height = 0) or (desc.Channels < 3) or (desc.Channels > 4) or (desc.Colorspace > 1) or (desc.Height >= QOI_pixels_MAX div desc.Width) then
    Exit;

  max_size    := desc.Width * desc.Height * (desc.Channels + 1) + SizeOf(TQOIHeader) + qoi_padding_size;
  bytes       := AllocMem(max_size);
  intStartPos := Integer(bytes);

  // write header
  qoi_write_32(bytes, QOI_MAGIC);
  Inc(bytes, 4);

  qoi_write_32(bytes, desc.Width);
  Inc(bytes, 4);

  qoi_write_32(bytes, desc.Height);
  Inc(bytes, 4);

  qoi_write_8(bytes, desc.Channels);
  Inc(bytes, 1);

  qoi_write_8(bytes, 0);
  Inc(bytes, 1);

  Width         := desc.Width;
  Height        := desc.Height;
  StartScanLine := Integer(Buffer);
  bmpWidthBytes := desc.Width * desc.Channels;

  // start encode
  for Y := 0 to Height - 1 do
  begin
    px    := PQOI_RGBA_T(StartScanLine + Y * bmpWidthBytes);
    for X := 0 to Width - 1 do
    begin
      tmpArr   := qoi_encode_pascal_parallel(px);
      intCount := tmpArr[5];
      if intCount > 0 then
      begin
        qoi_write_arr(bytes, tmpArr, intCount);
        Inc(bytes, intCount);
      end;

      Inc(px);
    end;
  end;

  // write end sign
  for I := 0 to 7 do
    qoi_write_8(bytes, qoi_padding[I]);
  Inc(bytes, 8);

  // return actual length
  intlen := Integer(bytes) - intStartPos;

  // return encode address
  Result := PByte(intStartPos);
end;

function qoi_decode_pascal_parallel(const bytes: PByte; const intPos: Integer; var Count: Integer): Cardinal; inline;
{$J+}
const
  run: Integer          = 0;
  index: TArrQoi_rgba_t = (                                         //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), //
    (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0), (V: 0)  //
    );
  px: TQOI_RGBA_T = (V: $FF000000);
{$J-}
var
  b1, b2: Byte;
  vg    : Integer;
begin
  Count := 0;

  if (run > 0) then
  begin
    Dec(run);
  end
  else
  begin
    b1 := bytes[intPos + Count];
    Inc(Count);

    if (b1 = QOI_OP_RGB) then
    begin
      px.rgba.r := bytes[intPos + Count];
      Inc(Count);
      px.rgba.g := bytes[intPos + Count];
      Inc(Count);
      px.rgba.b := bytes[intPos + Count];
      Inc(Count);
    end
    else if (b1 = QOI_OP_RGBA) then
    begin
      px.V := PCardinal(bytes[intPos])^;
      Inc(Count, 4);
    end
    else if ((b1 and QOI_MASK_2) = QOI_OP_INDEX) then
    begin
      px := index[b1];
    end
    else if (b1 and QOI_MASK_2) = QOI_OP_DIFF then
    begin
      px.rgba.r := px.rgba.r + ((b1 shr 4) and 3) - 2;
      px.rgba.g := px.rgba.g + ((b1 shr 2) and 3) - 2;
      px.rgba.b := px.rgba.b + (b1 and 3) - 2;
    end
    else if (b1 and QOI_MASK_2) = QOI_OP_LUMA then
    begin
      b2 := bytes[intPos + Count];
      Inc(Count);
      vg        := (b1 and $3F) - 32;
      px.rgba.r := px.rgba.r + vg - 8 + ((b2 shr 4) and $F);
      px.rgba.g := px.rgba.g + vg;
      px.rgba.b := px.rgba.b + vg - 8 + (b2 and $F);
    end
    else if (b1 and QOI_MASK_2) = QOI_OP_RUN then
    begin
      run := (b1 and $3F);
    end;

    index[QOI_COLOR_HASH(px)] := px;
  end;

  Result := px.V;
end;

{ QOI DECODE }
function qoi_decode_pascal(const Buffer: Pointer; const BufferSize: Integer; const Channels: Integer; var desc: TQOIHeader; var Count: Integer): Pointer;
var
  bytes        : PByte;
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
  Width, Height: Integer;
  X, Y         : Integer;
  pixels       : PQOI_RGBA_T;
  intPos       : Integer;
  intlen       : Integer;
begin
  Result := nil;
  Count  := 0;

  if (Buffer = nil) or (@desc = nil) or ((Channels <> 0) and (Channels <> 3) and (Channels <> 4)) or (BufferSize < SizeOf(TQOIHeader) + SizeOf(qoi_padding)) then
    Exit;

  bytes := Buffer;
  Move(bytes^, desc, SizeOf(TQOIHeader));

  if (desc.Width = 0) or (desc.Height = 0) or (desc.Channels < 3) or (desc.Channels > 4) or (desc.Colorspace > 1) or (desc.Height >= QOI_pixels_MAX div desc.Width) then
    Exit;

  intPos        := SizeOf(TQOIHeader);
  Width         := desc.Width;
  Height        := desc.Height;
  bmpWidthBytes := desc.Width * desc.Channels;
  Count         := bmpWidthBytes * Height;
  Result        := AllocMem(Count);
  StartScanLine := Integer(Result);

  for Y := Height - 1 downto 0 do
  begin
    pixels := PQOI_RGBA_T(StartScanLine + Y * bmpWidthBytes);
    for X  := 0 to Width - 1 do
    begin
      pixels^.V := qoi_decode_pascal_parallel(bytes, intPos, intlen);
      Inc(intPos, intlen);
      Inc(pixels);
    end;
  end;
end;

end.
