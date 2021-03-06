unit db.QOI;

interface

type
  qoi_desc2 = packed record
    magic: Cardinal;
    width: Cardinal;
    height: Cardinal;
    channels: Byte;
    colorspace: Byte;
  end;

  Tqoi_desc2 = qoi_desc2;
  Pqoi_desc2 = ^Tqoi_desc2;

{ QOI ENCODE }
function qoi_encode_pascal(const data: Pointer; const desc: Pqoi_desc2; var out_len: Integer): Pointer;

{ QOI DECODE }
function qoi_decode_pascal(const data: Pointer; Size: Integer; var desc: Tqoi_desc2; channels: Integer): Pointer;

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
  QOI_HEADER_SIZE                     = 14;
  QOI_pixels_MAX                      = 400000000;
  qoi_padding_size                    = 8;
  qoi_padding: array [0 .. 7] of Byte = (0, 0, 0, 0, 0, 0, 0, 1);

type
  _rgba = record
    r, g, b, a: Byte;
  end;

  qoi_rgba_t = packed record
    case Boolean of
      false:
        (rgba: _rgba);
      true:
        (V: Cardinal);
  end;

  Tqoi_rgba_t = qoi_rgba_t;
  Pqoi_rgba_t = ^Tqoi_rgba_t;

  TArrQoi_rgba_t = array [0 .. 63] of Tqoi_rgba_t;
  TSixByteArray  = array [0 .. 5] of Byte;

function QOI_COLOR_HASH(c: Tqoi_rgba_t): Byte; inline;
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

procedure qoi_write_arr(const P: PByte; const val: TSixByteArray; const Count: Integer); inline;
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

function qoi_encode_pascal_parallel(const px: Pqoi_rgba_t): TSixByteArray; inline;
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
  px_prev: Tqoi_rgba_t = (V: $FF000000);
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
function qoi_encode_pascal(const data: Pointer; const desc: Pqoi_desc2; var out_len: Integer): Pointer;
var
  I, max_size  : Integer;
  intStartPos  : Integer;
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
  width, height: Integer;
  bytes        : PByte;
  px           : Pqoi_rgba_t;
  X, Y         : Integer;
  tmpArr       : TSixByteArray;
  intCount     : Integer;
begin
  Result := nil;

  if (data = nil) or (out_len = -1) or (desc = nil) or //
    (desc^.width = 0) or (desc^.height = 0) or         //
    (desc^.channels < 3) or (desc^.channels > 4) or    //
    (desc^.colorspace > 1) or                          //
    (desc^.height >= QOI_pixels_MAX div desc^.width) then
    Exit;

  max_size    := desc^.width * desc^.height * (desc^.channels + 1) + QOI_HEADER_SIZE + qoi_padding_size;
  bytes       := AllocMem(max_size);
  intStartPos := Integer(bytes);

  qoi_write_32(bytes, QOI_MAGIC);
  Inc(bytes, 4);

  qoi_write_32(bytes, desc^.width);
  Inc(bytes, 4);

  qoi_write_32(bytes, desc^.height);
  Inc(bytes, 4);

  qoi_write_8(bytes, desc^.channels);
  Inc(bytes, 1);

  qoi_write_8(bytes, 0);
  Inc(bytes, 1);

  width         := desc^.width;
  height        := desc^.height;
  StartScanLine := Integer(data);
  bmpWidthBytes := desc^.width * desc^.channels;

  for Y := 0 to height - 1 do
  begin
    px    := Pqoi_rgba_t(StartScanLine + Y * bmpWidthBytes);
    for X := 0 to width - 1 do
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

  for I := 0 to 7 do
    qoi_write_8(bytes, qoi_padding[I]);
  Inc(bytes, 8);

  out_len := Integer(bytes) - intStartPos;
  Result  := PByte(intStartPos);
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
  px: Tqoi_rgba_t = (V: $FF000000);
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
      px.rgba.r := bytes[intPos + Count];
      Inc(Count);
      px.rgba.g := bytes[intPos + Count];
      Inc(Count);
      px.rgba.b := bytes[intPos + Count];
      Inc(Count);
      px.rgba.a := bytes[intPos + Count];
      Inc(Count);
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
function qoi_decode_pascal(const data: Pointer; Size: Integer; var desc: Tqoi_desc2; channels: Integer): Pointer;
var
  bytes        : PByte;
  StartScanLine: Integer;
  bmpWidthBytes: Integer;
  width, height: Integer;
  X, Y         : Integer;
  pixels       : Pqoi_rgba_t;
  intPos       : Integer;
  Count        : Integer;
begin
  Result := nil;

  if (data = nil) or (@desc = nil) or                            //
    ((channels <> 0) and (channels <> 3) and (channels <> 4)) or //
    (Size < QOI_HEADER_SIZE + SizeOf(qoi_padding)) then
    Exit;

  bytes := data;

  Move(bytes^, desc, SizeOf(Tqoi_desc2));

  if (desc.width = 0) or (desc.height = 0) or     //
    (desc.channels < 3) or (desc.channels > 4) or //
    (desc.colorspace > 1) or                      //
    (desc.height >= QOI_pixels_MAX div desc.width) then
    Exit;

  Result := AllocMem(desc.width * desc.height * desc.channels);

  width         := desc.width;
  height        := desc.height;
  StartScanLine := Integer(Result);
  bmpWidthBytes := desc.width * desc.channels;
  intPos        := 14;

  for Y := 0 to height - 1 do
  begin
    pixels := Pqoi_rgba_t(StartScanLine + Y * bmpWidthBytes);
    for X  := 0 to width - 1 do
    begin
      pixels^.V := qoi_decode_pascal_parallel(bytes, intPos, Count);
      Inc(intPos, Count);

      Inc(pixels);
    end;
  end;
end;

end.
