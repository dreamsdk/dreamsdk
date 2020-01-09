program PECheck;

{$mode objfpc}{$H+}

uses
  Windows,
  SysUtils,
  FSTools;

type
  TPortableExecutableBitness = (pebUnknown, peb16, peb32, peb64);

// Thanks ASerge
// See: https://forum.lazarus.freepascal.org/index.php?topic=36834.0
function GetPortableExecutableBitness(const APath: WideString): TPortableExecutableBitness;
const
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;
  IMAGE_NT_OPTIONAL_HDR64_MAGIC = $20b;

var
  HFile, HFileMap: THandle;
  PMapView: Pointer;
  PIDH: PImageDosHeader;
  PINTH: PImageNtHeaders;

begin
  Result := pebUnknown;
  HFile := CreateFileW(PWideChar(APath), GENERIC_READ, FILE_SHARE_READ, nil,
    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if HFile = INVALID_HANDLE_VALUE then
    Exit;
  HFileMap  := CreateFileMapping(HFile, nil, PAGE_READONLY, 0, 0, nil);
  CloseHandle(HFile);
  if HFileMap = 0 then
    Exit;
  PMapView := MapViewOfFile(HFileMap, FILE_MAP_READ, 0, 0, 0);
  CloseHandle(HFileMap);
  if PMapView = nil then
    Exit;
  PIDH := PImageDosHeader(PMapView);
  if PIDH^.e_magic = IMAGE_DOS_SIGNATURE then
  begin
    PINTH := PImageNtHeaders(PAnsiChar(PMapView) + PIDH^._lfanew);
    if PINTH^.Signature <> IMAGE_NT_SIGNATURE then
      Result := peb16
    else
      case PINTH^.OptionalHeader.Magic of
        IMAGE_NT_OPTIONAL_HDR32_MAGIC:
          Result := peb32;
        IMAGE_NT_OPTIONAL_HDR64_MAGIC:
          Result := peb64;
      end;
  end;
  UnmapViewOfFile(PMapView);
end;

var
  ProgramName: string;
  FileName: TFileName;

{$R *.res}

begin
  Application.Title:='Portable Executable Checker';
  ProgramName := GetProgramName;
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ProgramName, ' <FileName>');
    Exit;
  end;

  FileName := ParamStr(1);
  if FileExists(FileName) then
  begin
    case GetPortableExecutableBitness(WideString(FileName)) of
      pebUnknown:
        begin
          WriteLn('Unknown');
        end;
      peb16:
        begin
          WriteLn('16-bits');
        end;
      peb32:
        begin
          WriteLn('32-bits');
        end;
      peb64:
        begin
          WriteLn('64-bits');
        end;
    end;
  end;
end.

