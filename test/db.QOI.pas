unit db.QOI;
{
  FUNC: QOI ͼ�����ͽ���
  Auth: dbyoung@sina.com
  Time: 2021-11-29
}

interface

const
  QOI_MAGIC: Cardinal = $66696F71;

type
  { QOI �ļ�ͷ; 14 bytes }
  PQOIHeader = ^TQOIHeader;
  PlossyCfg  = ^TlossyCfg;

  TQOIHeader = packed record
    Magic: Cardinal;
    Width: Cardinal;
    Height: Cardinal;
    Channels: Byte;
    Colorspace: Byte;
  end;

  {
    QOI ����ѹ���������
    ���ã�QOI_OP_DIFF, QOI_OP_LUMA, QOI_OP_RUN, QOI_OP_INDEX
    ���ã�QOI_OP_RGB,  QOI_OP_RGBA,
  }
  TlossyCfg = packed record
    weight: array [0 .. 3] of Double;
    ThreshLow, ThreshHigh: Double;
    mulAlpha: Integer;
  end;

procedure LogErr(const strTemp: string);

{ QOI ������� }
function qoi_encode_pascal_lossl(const Buffer: Pointer; const desc: TQOIHeader; var intlen: Integer): Pointer;

{ QOI ������� }
function qoi_encode_pascal_lossy(const Buffer: Pointer; const desc: TQOIHeader; var intlen: Integer; const lossyCfg: PlossyCfg): Pointer;

{ QOI ͼ����� }
function qoi_decode_pascal(const Buffer: Pointer; const BufferSize: Integer; const Channels: Integer; var desc: TQOIHeader; var Count: Integer): Pointer;

implementation

uses Winapi.Windows, System.Threading, System.SyncObjs;

const
  QOI_OP_INDEX                        = $0;
  QOI_OP_DIFF                         = $40;
  QOI_OP_LUMA                         = $80;
  QOI_OP_RUN                          = $C0;
  QOI_OP_RGB                          = $FE;
  QOI_OP_RGBA                         = $FF;
  QOI_MASK_2                          = $C0;
  QOI_pixels_MAX                      = 400000000;
  qoi_padding_size                    = 8;
  QOI_IndexTable_len                  = 63;
  qoi_padding: array [0 .. 7] of Byte = (0, 0, 0, 0, 0, 0, 0, 1);

type
  _RGBA = record
    b, g, r, a: Byte;
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

  // 64���ȵ���ɫ������
  TArrQoi_rgba_t = array [0 .. QOI_IndexTable_len] of TQOI_RGBA_T;

  // 6 �ֽ�����
  TArrSixByte = array [0 .. 5] of Byte;

procedure LogErr(const strTemp: string);
begin

end;

{ ��ɫֵ���� HASH ���� }
function QOI_COLOR_HASH(c: TQOI_RGBA_T): Byte; inline;
begin
  Result := (c.rgba.r * 3 + c.rgba.g * 5 + c.rgba.b * 7 + c.rgba.a * 11) and QOI_IndexTable_len;
end;

{ д��32λ������ֵ }
procedure qoi_write_32(var P: PByte; const Val: DWORD); inline;
begin
  PDWORD(P)^ := Val;
end;

{ д��16λWORD��ֵ }
procedure qoi_write_16(var P: PByte; const Val: WORD); inline;
begin
  PWORD(P)^ := Val;
end;

{ д��8λByte��ֵ }
procedure qoi_write_8(var P: PByte; const Val: Byte); inline;
begin
  P^ := Val;
end;

{ һ����д�� }
procedure qoi_write_arr(var P: PByte; const Val: TArrSixByte; const Count: Integer); inline;
begin
  if Count = 1 then
    qoi_write_8(P, Val[0])
  else if Count = 2 then
    qoi_write_16(P, PWORD(@Val[0])^)
  else if Count = 4 then
    qoi_write_32(P, PDWORD(@Val[0])^)
  else
    Move(Val[0], P^, Count);

  Inc(P, Count);
end;

{ ��ʼ QOI ������� }
procedure qoi_encode_pascal_parallel_lossl(const px: PQOI_RGBA_T; var px_prev: TQOI_RGBA_T; var run: Integer; var retResult: TArrSixByte); inline;
{$J+}
const
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
{$J-}
var
  vr, vg, vb, vg_r, vg_b: Integer;
  index_pos             : Integer;
  Count                 : Integer;
begin
  Count := 0;
  {
    �����ǰ����ֵ����һ������ֵ��ͬ������ QOI_OP_RUN ���롣run ������ 1~62 ��
    run ʹ��һ���ֽڵĺ�6λ��ǰ��λ�������λʹ���ˡ��������ֵ�������� 63��
    �� 63 (00,111111) �� QOI_OP_RGBA(11,111111) �ĺ�6λ��ͬ�ˡ��޷������ˡ����� run ���ֵֻ���� 62 ��)��
    ���Ե� run ���� 62 ʱ��Ӧ�ý��ж�� QOI_OP_RUN ���룻
    ��ô����Ŀ���ǿ��Խ���ǰ���ر���Ϊ1���ֽڣ�
    32λλͼ������ѹ����25%��
    24λλͼ������ѹ����33%��
  }
  if (px^.V = px_prev.V) then
  begin
    Inc(run);
    if (run = 62) then
    begin
      retResult[Count] := QOI_OP_RUN or (run - 1);
      Inc(Count);
      run := 0;
    end;
  end
  else
  begin
    { run ��Ϊ 0����ʾ�����˵�ǰ���غ���һ�����ز���ͬ���ˣ�Ҫ���� QOI_OP_RUN ���룻 }
    if (run > 0) then
    begin
      retResult[Count] := QOI_OP_RUN or (run - 1);
      Inc(Count);
      run := 0;
    end;

    { �Ե�ǰ��ɫ����������64���ȵ���ɫ������ }
    index_pos := QOI_COLOR_HASH(px^);
    if (index[index_pos].V = px^.V) then
    begin
      {
        ����������иպ��У����� QOI_OP_INDEX ���룻����ǰ���ر���Ϊ1���ֽڣ�
        32λλͼ������ѹ����25%��
        24λλͼ������ѹ����33%��
      }
      retResult[Count] := QOI_OP_INDEX or index_pos;
      Inc(Count);
    end
    else
    begin
      { û���������У�д�������� }
      index[index_pos] := px^;

      { �����ǰ���غ���һ������͸������ͬ��ȡ R��G��B ��ֵ }
      if (px^.rgba.a = px_prev.rgba.a) then
      begin
        vr   := px^.rgba.r - px_prev.rgba.r;
        vg   := px^.rgba.g - px_prev.rgba.g;
        vb   := px^.rgba.b - px_prev.rgba.b;
        vg_r := vr - vg;
        vg_b := vb - vg;

        if ((vr > -3) and (vr < 2) and (vg > -3) and (vg < 2) and (vb > -3) and (vb < 2)) then
        begin
          {
            ��ǰ��������һ�����ص���ɫֵ�в��� -3 �� 2 ֮�䡣���� QOI_OP_DIFF ���롣Ŀ���ǿ��Խ���ǰ���ر���Ϊ1���ֽڣ�
            32λλͼ������ѹ����25%��
            24λλͼ������ѹ����33%��
          }
          retResult[Count] := QOI_OP_DIFF or (vr + 2) shl 4 or (vg + 2) shl 2 or (vb + 2);
          Inc(Count);
        end
        else if ((vg_r > -9) and (vg_r < 8) and (vg > -33) and (vg < 32) and (vg_b > -9) and (vg_b < 8)) then
        begin
          {
            ��ǰ��������һ�����ص���ɫֵ�в�ϴ󡣽��� QOI_OP_LUMA ���롣Ŀ���ǿ��Խ���ǰ���ر���Ϊ2���ֽڣ�
            32λλͼ������ѹ����50%��
            24λλͼ������ѹ����66%��
          }
          retResult[Count + 0] := QOI_OP_LUMA or (vg + 32);
          retResult[Count + 1] := (vg_r + 8) shl 4 or (vg_b + 8);
          Inc(Count, 2);
        end
        else
        begin
          {
            ���� QOI_OP_RGB ���롣4���ֽڡ�ֱ�Ӵ�����ֵ�ˣ�û���κ�ѹ���ˣ���������һ���ֽڣ�
            24λλͼ������ѹ����133%��
          }
          retResult[Count + 0] := QOI_OP_RGB;
          retResult[Count + 1] := px^.rgba.r;
          retResult[Count + 2] := px^.rgba.g;
          retResult[Count + 3] := px^.rgba.b;
          Inc(Count, 4);
        end
      end
      else
      begin
        {
          ͸���Ȳ���ͬ������ QOI_OP_RGBA ���롣5���ֽڡ�ֱ�Ӵ�����ֵ�ˣ�û���κ�ѹ���ˣ���������һ���ֽڣ�
          32λλͼ������ѹ����125%��
          һ������£�һ��32λλͼ������͸���Ȼ�����һ�µġ����ᷢ���ı䡣�������ﱻִ�е��Ŀ����Ժ�С��
        }
        retResult[Count + 0] := QOI_OP_RGBA;
        retResult[Count + 1] := px^.rgba.r;
        retResult[Count + 2] := px^.rgba.g;
        retResult[Count + 3] := px^.rgba.b;
        retResult[Count + 4] := px^.rgba.a;
        Inc(Count, 5);
      end;
    end;
  end;

  { ���ؽ�� }
  retResult[5] := Count;

  { ��ǰ����ֵ������һ������ }
  px_prev := px^;
end;

{
  QOI �������
  QOI ͼ��ֻ�ܴ��� 24λ RGB �� 32λ RGBA ��ʽ��ͼ��
  QOI ������ر���һ���� 6 �ַ�ʽ: QOI_OP_RGB, QOI_OP_RGBA, QOI_OP_DIFF, QOI_OP_LUMA, QOI_OP_RUN �� QOI_OP_INDEX��
  ѹ����: QOI_OP_RUN �� QOI_OP_INDEX = QOI_OP_DIFF < QOI_OP_LUMA < QOI_OP_RGB = QOI_OP_RGBA��
  ѹ����ԽСԽ�ã�
  ����ı��뺯��Ҳ�ǰ������ѹ�������н��б���ģ�
}
function qoi_encode_pascal_lossl(const Buffer: Pointer; const desc: TQOIHeader; var intlen: Integer): Pointer;
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
  run          : Integer;
  px_prev      : TQOI_RGBA_T;
  index        : TArrQoi_rgba_t;
begin
  Result := nil;

  { ͼ��Ϸ����ж� }
  if (Buffer = nil) or (intlen = -1) or (desc.Width = 0) or (desc.Height = 0) or (desc.Channels < 3) or (desc.Channels > 4) or (desc.Colorspace > 1) or (desc.Height >= QOI_pixels_MAX div desc.Width) then
    Exit;

  { ������ܴ�С�����ֵ��ʵ�ʴ�С����������󣬻᷵��ʵ��ֵ��out_len ������ }
  max_size := desc.Width * desc.Height * (desc.Channels + 1) + SizeOf(TQOIHeader) + qoi_padding_size;
  bytes    := AllocMem(max_size);

  { bytes �׵�ַ }
  intStartPos := Integer(bytes);

  { д���ļ�ͷ }
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

  { ������ʼ�� }
  Width         := desc.Width;
  Height        := desc.Height;
  StartScanLine := Integer(Buffer);
  bmpWidthBytes := desc.Width * desc.Channels;
  run           := 0;
  px_prev.V     := $FF000000;
  FillChar(index, SizeOf(index), #0);

  { ��ʼ���б��� }
  for Y := 0 to Height - 1 do
  begin
    px    := PQOI_RGBA_T(StartScanLine + Y * bmpWidthBytes);
    for X := 0 to Width - 1 do
    begin
      qoi_encode_pascal_parallel_lossl(px, px_prev, run, tmpArr);
      intCount := tmpArr[5];
      if intCount > 0 then
      begin
        { һ����д�� }
        qoi_write_arr(bytes, tmpArr, intCount);
      end;

      Inc(px);
    end;
  end;

  { д�������� }
  for I := 0 to 7 do
    qoi_write_8(bytes, qoi_padding[I]);
  Inc(bytes, 8);

  { bytes ��ǰ��ַ - intStartPos �׵�ַ = ����ʵ�ʳ��� }
  intlen := Integer(bytes) - intStartPos;

  { ���� bytes �ڴ��׵�ַ }
  Result := PByte(intStartPos);
end;

function CompareColor(const px, px_prev: TQOI_RGBA_T; const lossyCfg: PlossyCfg = nil): Boolean; inline;
begin
  Result := false;

  if lossyCfg = nil then
    Exit;
end;

{ ��ʼ QOI ������� }
procedure qoi_encode_pascal_parallel_lossy(const px: PQOI_RGBA_T; var px_prev: TQOI_RGBA_T; var run: Integer; var retResult: TArrSixByte; const lossyCfg: PlossyCfg = nil); inline;
{$J+}
const
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
{$J-}
var
  vr, vg, vb, vg_r, vg_b: Integer;
  index_pos             : Integer;
  Count                 : Integer;
begin
  Count := 0;
  {
    �����ǰ����ֵ����һ������ֵ��ͬ������ QOI_OP_RUN ���롣run ������ 1~62 ��
    run ʹ��һ���ֽڵĺ�6λ��ǰ��λ�������λʹ���ˡ��������ֵ�������� 63��
    �� 63 (00,111111) �� QOI_OP_RGBA(11,111111) �ĺ�6λ��ͬ�ˡ��޷������ˡ����� run ���ֵֻ���� 62 ��)��
    ���Ե� run ���� 62 ʱ��Ӧ�ý��ж�� QOI_OP_RUN ���룻
    ��ô����Ŀ���ǿ��Խ���ǰ���ر���Ϊ1���ֽڣ�
    32λλͼ������ѹ����25%��
    24λλͼ������ѹ����33%��
  }
  if (px^.V = px_prev.V) or (CompareColor(px^, px_prev, lossyCfg)) then
  begin
    Inc(run);
    if (run = 62) then
    begin
      retResult[Count] := QOI_OP_RUN or (run - 1);
      Inc(Count);
      run := 0;
    end;
  end
  else
  begin
    { run ��Ϊ 0����ʾ�����˵�ǰ���غ���һ�����ز���ͬ���ˣ�Ҫ���� QOI_OP_RUN ���룻 }
    if (run > 0) then
    begin
      retResult[Count] := QOI_OP_RUN or (run - 1);
      Inc(Count);
      run := 0;
    end;

    { �Ե�ǰ��ɫ����������64���ȵ���ɫ������ }
    index_pos := QOI_COLOR_HASH(px^);
    if (index[index_pos].V = px^.V) or (CompareColor(px^, px_prev, lossyCfg)) then
    begin
      {
        ����������иպ��У����� QOI_OP_INDEX ���룻����ǰ���ر���Ϊ1���ֽڣ�
        32λλͼ������ѹ����25%��
        24λλͼ������ѹ����33%��
      }
      retResult[Count] := QOI_OP_INDEX or index_pos;
      Inc(Count);
    end
    else
    begin
      { û���������У�д�������� }
      index[index_pos] := px^;

      { �����ǰ���غ���һ������͸������ͬ��ȡ R��G��B ��ֵ }
      if (px^.rgba.a = px_prev.rgba.a) then
      begin
        vr   := px^.rgba.r - px_prev.rgba.r;
        vg   := px^.rgba.g - px_prev.rgba.g;
        vb   := px^.rgba.b - px_prev.rgba.b;
        vg_r := vr - vg;
        vg_b := vb - vg;

        if ((vr > -3) and (vr < 2) and (vg > -3) and (vg < 2) and (vb > -3) and (vb < 2)) or (CompareColor(px^, px_prev, lossyCfg)) then
        begin
          {
            ��ǰ��������һ�����ص���ɫֵ�в��� -3 �� 2 ֮�䡣���� QOI_OP_DIFF ���롣Ŀ���ǿ��Խ���ǰ���ر���Ϊ1���ֽڣ�
            32λλͼ������ѹ����25%��
            24λλͼ������ѹ����33%��
          }
          retResult[Count] := QOI_OP_DIFF or (vr + 2) shl 4 or (vg + 2) shl 2 or (vb + 2);
          Inc(Count);
        end
        else if ((vg_r > -9) and (vg_r < 8) and (vg > -33) and (vg < 32) and (vg_b > -9) and (vg_b < 8)) or (CompareColor(px^, px_prev, lossyCfg)) then
        begin
          {
            ��ǰ��������һ�����ص���ɫֵ�в�ϴ󡣽��� QOI_OP_LUMA ���롣Ŀ���ǿ��Խ���ǰ���ر���Ϊ2���ֽڣ�
            32λλͼ������ѹ����50%��
            24λλͼ������ѹ����66%��
          }
          retResult[Count + 0] := QOI_OP_LUMA or (vg + 32);
          retResult[Count + 1] := (vg_r + 8) shl 4 or (vg_b + 8);
          Inc(Count, 2);
        end
        else
        begin
          {
            ���� QOI_OP_RGB ���롣4���ֽڡ�ֱ�Ӵ�����ֵ�ˣ�û���κ�ѹ���ˣ���������һ���ֽڣ�
            24λλͼ������ѹ����133%��
          }
          retResult[Count + 0] := QOI_OP_RGB;
          retResult[Count + 1] := px^.rgba.r;
          retResult[Count + 2] := px^.rgba.g;
          retResult[Count + 3] := px^.rgba.b;
          Inc(Count, 4);
        end
      end
      else
      begin
        {
          ͸���Ȳ���ͬ������ QOI_OP_RGBA ���롣5���ֽڡ�ֱ�Ӵ�����ֵ�ˣ�û���κ�ѹ���ˣ���������һ���ֽڣ�
          32λλͼ������ѹ����125%��
          һ������£�һ��32λλͼ������͸���Ȼ�����һ�µġ����ᷢ���ı䡣�������ﱻִ�е��Ŀ����Ժ�С��
        }
        retResult[Count + 0] := QOI_OP_RGBA;
        retResult[Count + 1] := px^.rgba.r;
        retResult[Count + 2] := px^.rgba.g;
        retResult[Count + 3] := px^.rgba.b;
        retResult[Count + 4] := px^.rgba.a;
        Inc(Count, 5);
      end;
    end;
  end;

  { ���ؽ�� }
  retResult[5] := Count;

  { ��ǰ����ֵ������һ������ }
  px_prev := px^;
end;

{ QOI ������� }
function qoi_encode_pascal_lossy(const Buffer: Pointer; const desc: TQOIHeader; var intlen: Integer; const lossyCfg: PlossyCfg): Pointer;
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
  run          : Integer;
  px_prev      : TQOI_RGBA_T;
  index        : TArrQoi_rgba_t;
begin
  Result := nil;

  { ͼ��Ϸ����ж� }
  if (Buffer = nil) or (intlen = -1) or (desc.Width = 0) or (desc.Height = 0) or (desc.Channels < 3) or (desc.Channels > 4) or (desc.Colorspace > 1) or (desc.Height >= QOI_pixels_MAX div desc.Width) then
    Exit;

  { ������ܴ�С�����ֵ��ʵ�ʴ�С����������󣬻᷵��ʵ��ֵ��out_len ������ }
  max_size := desc.Width * desc.Height * (desc.Channels + 1) + SizeOf(TQOIHeader) + qoi_padding_size;
  bytes    := AllocMem(max_size);

  { bytes �׵�ַ }
  intStartPos := Integer(bytes);

  { д���ļ�ͷ }
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

  { ������ʼ�� }
  Width         := desc.Width;
  Height        := desc.Height;
  StartScanLine := Integer(Buffer);
  bmpWidthBytes := desc.Width * desc.Channels;
  run           := 0;
  px_prev.V     := $FF000000;
  FillChar(index, SizeOf(index), #0);

  { ��ʼ���б��� }
  for Y := 0 to Height - 1 do
  begin
    px    := PQOI_RGBA_T(StartScanLine + Y * bmpWidthBytes);
    for X := 0 to Width - 1 do
    begin
      qoi_encode_pascal_parallel_lossy(px, px_prev, run, tmpArr, lossyCfg);
      intCount := tmpArr[5];
      if intCount > 0 then
      begin
        { һ����д�� }
        qoi_write_arr(bytes, tmpArr, intCount);
      end;

      Inc(px);
    end;
  end;

  { д�������� }
  for I := 0 to 7 do
    qoi_write_8(bytes, qoi_padding[I]);
  Inc(bytes, 8);

  { bytes ��ǰ��ַ - intStartPos �׵�ַ = ����ʵ�ʳ��� }
  intlen := Integer(bytes) - intStartPos;

  { ���� bytes �ڴ��׵�ַ }
  Result := PByte(intStartPos);
end;

function ReadByte(var P: PByte): Byte; inline;
begin
  Result := P^;
  Inc(P);
end;

function ReadDWORD(var P: PByte): DWORD; inline;
begin
  Result := P^;
  Inc(P, 4);
end;

{ ��ʼ QOI ���� }
procedure qoi_decode_pascal_parallel(var srcQOI: PByte; var dstRGB: PQOI_RGBA_T); inline;
{$J+}
const
  px_prev: TQOI_RGBA_T  = (V: $FF000000);
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
{$J-}
var
  b1, b2: Byte;
  vg    : Integer;
begin
  if (run > 0) then
  begin
    Dec(run);
  end
  else
  begin
    b1 := ReadByte(srcQOI);

    if (b1 = QOI_OP_RGB) then
    begin
      px_prev.rgba.r := ReadByte(srcQOI);
      px_prev.rgba.g := ReadByte(srcQOI);
      px_prev.rgba.b := ReadByte(srcQOI);
    end
    else if (b1 = QOI_OP_RGBA) then
    begin
      px_prev.V := ReadDWORD(srcQOI);
    end
    else if ((b1 and QOI_MASK_2) = QOI_OP_INDEX) then
    begin
      px_prev := index[b1];
    end
    else if (b1 and QOI_MASK_2) = QOI_OP_DIFF then
    begin
      px_prev.rgba.r := px_prev.rgba.r + ((b1 shr 4) and 3) - 2;
      px_prev.rgba.g := px_prev.rgba.g + ((b1 shr 2) and 3) - 2;
      px_prev.rgba.b := px_prev.rgba.b + (b1 and 3) - 2;
    end
    else if (b1 and QOI_MASK_2) = QOI_OP_LUMA then
    begin
      b2             := ReadByte(srcQOI);
      vg             := (b1 and $3F) - 32;
      px_prev.rgba.r := px_prev.rgba.r + vg - 8 + ((b2 shr 4) and $F);
      px_prev.rgba.g := px_prev.rgba.g + vg;
      px_prev.rgba.b := px_prev.rgba.b + vg - 8 + (b2 and $F);
    end
    else if (b1 and QOI_MASK_2) = QOI_OP_RUN then
    begin
      run := (b1 and $3F);
    end;

    index[QOI_COLOR_HASH(px_prev)] := px_prev;
  end;

  dstRGB^.V := px_prev.V;
end;

{ QOI ͼ����� }
function qoi_decode_pascal(const Buffer: Pointer; const BufferSize: Integer; const Channels: Integer; var desc: TQOIHeader; var Count: Integer): Pointer;
var
  Height       : Integer;
  Width        : Integer;
  X, Y         : Integer;
  StartScanLine: Integer;
  srcQOI       : PByte;
  bmpWidthBytes: Integer;
  dstRGB       : PQOI_RGBA_T;
begin
  Result := nil;
  Count  := 0;

  { ͼ��Ϸ����ж� }
  if (Buffer = nil) or ((Channels <> 0) and (Channels <> 3) and (Channels <> 4)) or (BufferSize < SizeOf(TQOIHeader) + SizeOf(qoi_padding)) then
    Exit;

  { �����ļ�ͷ��������ͼ��Ϸ����ж� }
  srcQOI := Buffer;
  Move(srcQOI^, desc, SizeOf(TQOIHeader));
  if (desc.Width = 0) or (desc.Height = 0) or (desc.Channels < 3) or (desc.Channels > 4) or (desc.Colorspace > 1) or (desc.Height >= QOI_pixels_MAX div desc.Width) then
    Exit;

  { ������ʼ�� }
  Width         := desc.Width;
  Height        := desc.Height;
  bmpWidthBytes := desc.Width * desc.Channels;
  Count         := bmpWidthBytes * Height;
  Result        := AllocMem(Count);
  StartScanLine := Integer(Result);
  Inc(srcQOI, SizeOf(TQOIHeader));

  { ��ʼ���н��� }
  for Y := 0 to Height - 1 do
  begin
    dstRGB := PQOI_RGBA_T(StartScanLine + Y * bmpWidthBytes);
    for X  := 0 to Width - 1 do
    begin
      qoi_decode_pascal_parallel(srcQOI, dstRGB);
      Inc(dstRGB);
    end;
  end;
end;

end.
