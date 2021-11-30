unit uQOI;
{
  功能：test qoi lossless image compression / Uncompression
  时间：2021-11-29
  作者：dbyoung@sina.com
}

interface

uses System.Win.Crtl;

type
  Pqoi_desc = ^qoi_desc;

  qoi_desc = record
    width: LongWord;
    height: LongWord;
    channels: Byte;
    colorspace: Byte;
  end;

function qoi_write(const filename: PAnsiChar; const data: Pointer; const desc: Pqoi_desc): Integer; cdecl; external name {$IFDEF WIN32} '_qoi_write'; {$ELSE} 'qoi_write'; {$ENDIF}
function qoi_read(const filename: PAnsiChar; desc: Pqoi_desc; channels: Integer): Pointer; cdecl; external name {$IFDEF WIN32} '_qoi_read'; {$ELSE} 'qoi_read'; {$ENDIF}
function qoi_encode(const data: Pointer; const desc: Pqoi_desc; var outlen: Integer): Pointer; cdecl; external name {$IFDEF WIN32} '_qoi_encode'; {$ELSE} 'qoi_encode'; {$ENDIF}
function qoi_decode(const data: Pointer; size: Integer; desc: Pqoi_desc; channels: Integer): Pointer; cdecl; external name {$IFDEF WIN32} '_qoi_decode'; {$ELSE} 'qoi_decode'; {$ENDIF}

implementation

type
  psize_t = ^size_t;
  size_t  = Cardinal;

  _FILE = record
    curp: ^Byte;
    buffer: ^Byte;
    level: Integer;
    bsize: Integer;
    istemp: Word;
    flags: Word;
    hold: Word;
    fd: shortint;
    token: Byte;
  end;

  P_FILE = ^_FILE;

{$IFDEF WIN32}
{$LINK qoi_x86.obj}
function _fclose(stream: _FILE): Integer; cdecl; external msvcrt name 'fclose';
function _fopen(const filename, mode: PAnsiChar): _FILE; cdecl; external msvcrt name 'fopen';
function _fread(buffer: PByte; size, count: size_t; stream: _FILE): size_t; cdecl; external msvcrt name 'fread';
function _fseek(stream: _FILE; offset: longint; origin: Integer): Integer; cdecl; external msvcrt name 'fseek';
function _ftell(stream: _FILE; offset: longint; origin: Integer): Integer; cdecl; external msvcrt name 'ftell';
function _fwrite(Ptr: Pointer; size: Cardinal; count: Cardinal; stream: Pointer): Cardinal; cdecl; external msvcrt name 'fwrite';
{$ELSE}
{$LINK qoi_x64.obj}
function fclose(stream: _FILE): Integer; cdecl; external msvcrt name 'fclose';
function fopen(const filename, mode: PAnsiChar): _FILE; cdecl; external msvcrt name 'fopen';
function fread(buffer: PByte; size, count: size_t; stream: _FILE): size_t; cdecl; external msvcrt name 'fread';
function fseek(stream: _FILE; offset: longint; origin: Integer): Integer; cdecl; external msvcrt name 'fseek';
function ftell(stream: _FILE; offset: longint; origin: Integer): Integer; cdecl; external msvcrt name 'ftell';
function fwrite(Ptr: Pointer; size: Cardinal; count: Cardinal; stream: Pointer): Cardinal; cdecl; external msvcrt name 'fwrite';
{$ENDIF}

end.
