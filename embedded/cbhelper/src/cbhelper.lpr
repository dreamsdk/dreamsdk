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
  CBTools;

type
  { TCodeBlocksHelperApplication }
  TCodeBlocksHelperApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TCodeBlocksHelperApplication.DoRun;
var
  AvailableUsers: TStringList;
  i: Integer;

begin
  AvailableUsers := TStringList.Create;
  try
    GetCodeBlocksAvailableUsers(AvailableUsers);
    for i := 0 to AvailableUsers.Count - 1 do
      WriteLn(AvailableUsers[i]);
    Terminate;
  finally
    AvailableUsers.Free;
  end;
end;

constructor TCodeBlocksHelperApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  StopOnException := True;
end;

destructor TCodeBlocksHelperApplication.Destroy;
begin
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

