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
  CBTools,
  CBPatch;

type
  { TCodeBlocksHelperApplication }
  TCodeBlocksHelperApplication = class(TCustomApplication)
  private
    fUsersSwitch: Boolean;
    fCleanSwitch: Boolean;
    procedure ParseParameters;
  protected
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure TCodeBlocksHelperApplication.ParseParameters;
const
  USERS_SWITCH = '--get-available-users';
  CLEAN_SWITCH = '--cleanup';

var
  i: Integer;
  Param: string;

begin
  fUsersSwitch := False;
  fCleanSwitch := False;
  for i := 1 to ParamCount do
  begin
    Param := LowerCase(ParamStr(i));
    if IsInString(USERS_SWITCH, Param) then
      fUsersSwitch := True;
    if IsInString(CLEAN_SWITCH, Param) then
      fCleanSwitch := True;
  end;
end;

procedure TCodeBlocksHelperApplication.DoRun;

  procedure GetAvailableUsers;
  var
    AvailableUsers: TStringList;
    i: Integer;

  begin
    AvailableUsers := TStringList.Create;
    try
      GetCodeBlocksAvailableUsers(AvailableUsers);
      for i := 0 to AvailableUsers.Count - 1 do
        WriteLn(AvailableUsers[i]);
    finally
      AvailableUsers.Free;
    end;
  end;

  procedure RemoveProfiles;
  var
    CodeBlocksPatcher: TCodeBlocksPatcher;

  begin
    CodeBlocksPatcher := TCodeBlocksPatcher.Create;
    try
      CodeBlocksPatcher.ResetProfiles;
    finally
      CodeBlocksPatcher.Free;
    end;
  end;

begin
  ParseParameters;
  if fUsersSwitch then
    GetAvailableUsers;
  if fCleanSwitch then
    RemoveProfiles;
  Terminate;
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

