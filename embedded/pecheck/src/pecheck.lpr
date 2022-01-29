program PECheck;

{$mode objfpc}{$H+}

uses
  Windows,
  SysUtils,
  SysTools,
  FSTools,
  PEUtils;

var
  ProgramName: string;
  FileName: TFileName;

{$R *.res}

begin
  ProgramName := GetProgramName;
  if ParamCount < 1 then
  begin
    WriteLn('Usage: ', ProgramName, ' <FileName>');
    Exit;
  end;

  FileName := ParamStr(1);
  if FileExists(FileName) then
  begin
    case GetPortableExecutableBitness(FileName) of
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

