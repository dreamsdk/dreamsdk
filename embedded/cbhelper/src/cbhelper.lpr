program CodeBlocksHelper;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  CustApp,
  FSTools,
  SysTools,
  CBPatch;

type
  { TCodeBlocksHelperApplication }
  TCodeBlocksHelperApplication = class(TCustomApplication)
  private
    fCodeBlocksPatcher: TCodeBlocksPatcher;
  protected
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Patcher: TCodeBlocksPatcher
      read fCodeBlocksPatcher;
  end;

procedure TCodeBlocksHelperApplication.DoRun;
var
  i: Integer;

begin
  for i := 0 to fCodeBlocksPatcher.Settings.AvailableUsers.Count - 1 do
    WriteLn(fCodeBlocksPatcher.Settings.AvailableUsers[i]);
  Terminate;
end;

constructor TCodeBlocksHelperApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
  fCodeBlocksPatcher := TCodeBlocksPatcher.Create;
end;

destructor TCodeBlocksHelperApplication.Destroy;
begin
  fCodeBlocksPatcher.Free;
  inherited Destroy;
end;

var
  Application: TCodeBlocksHelperApplication;

{$R *.res}

begin
  Application := TCodeBlocksHelperApplication.Create(nil);
  try
    Application.Title := 'Code::Blocks Patcher Helper';
    Application.Run;
  finally
    Application.Free;
  end;
end.

