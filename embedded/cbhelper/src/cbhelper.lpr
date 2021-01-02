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
  ECodeBlocksHelper = class(Exception);
  EIllegalSwitchCombinaison = class(ECodeBlocksHelper);

  { TCodeBlocksHelperApplication }
  TCodeBlocksHelperApplication = class(TCustomApplication)
  private
    fUniqueSwitchsCount: Integer;
    fUsersSwitch: Boolean;
    fCleanSwitch: Boolean;
    fDetectSwitch: Boolean;
    fVersionSwitch: Boolean;
    fVersionParamInstallationDirectory: TFileName;
    procedure ParseParameters;
  protected
    procedure DoRun; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  DETECT_SWITCH = '--detect';
  USERS_SWITCH = '--get-available-users';
  CLEAN_SWITCH = '--cleanup';
  VERSION_SWITCH = '--version';

procedure TCodeBlocksHelperApplication.ParseParameters;
var
  i: Integer;
  Param: string;
  ParamValue: Boolean;

begin
  fUniqueSwitchsCount := 0;
  fUsersSwitch := False;
  fCleanSwitch := False;
  fDetectSwitch := False;
  fVersionSwitch := False;
  fVersionParamInstallationDirectory := EmptyStr;
  ParamValue := False;

  for i := 1 to ParamCount do
  begin
    if fVersionSwitch and (not ParamValue) then
    begin
      fVersionParamInstallationDirectory := ParamStr(i);
      ParamValue := True;
    end
    else
    begin
      Param := LowerCase(ParamStr(i));

      // Clean: This removes all DreamSDK references from C::B files
      // This fix can be combined with the other (it's the only one like this)
      if IsInString(CLEAN_SWITCH, Param) then
        fCleanSwitch := True;

      // Get C::B user profiles
      if IsInString(USERS_SWITCH, Param) then
      begin
        fUsersSwitch := True;
        Inc(fUniqueSwitchsCount);
      end;

      // Detect C::B installation directory
      if IsInString(DETECT_SWITCH, Param) then
      begin
        fDetectSwitch := True;
        Inc(fUniqueSwitchsCount);
      end;

      // Detect C::B version
      if IsInString(VERSION_SWITCH, Param) then
      begin
        fVersionSwitch := True;
        Inc(fUniqueSwitchsCount);
      end;
    end;
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

  procedure DetectCodeBlocksInstallation;
  var
    InstallationDirectory: TFileName;

  begin
    InstallationDirectory := GetCodeBlocksDefaultInstallationDirectory;
    WriteLn(InstallationDirectory);
  end;

  procedure DetectCodeBlocksVersion;
  var
    CodeBlocksVersion: TCodeBlocksVersion;

  begin
    CodeBlocksVersion := GetCodeBlocksVersion(fVersionParamInstallationDirectory);
    WriteLn(CodeBlocksVersionToString(CodeBlocksVersion));
  end;

begin
  ParseParameters;

  if fUniqueSwitchsCount > 1 then
    raise EIllegalSwitchCombinaison.CreateFmt('Cannot combine %d switches', [fUniqueSwitchsCount]);

  if fUsersSwitch then
    GetAvailableUsers;
  if fCleanSwitch then
    RemoveProfiles;
  if fDetectSwitch then
    DetectCodeBlocksInstallation;

  if fVersionSwitch then
  begin
    if fVersionParamInstallationDirectory = EmptyStr then
      raise EArgumentException.Create('Missing Code::Blocks installation directory argument');
    DetectCodeBlocksVersion;
  end;

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
  Application.Title:='Code::Blocks Patcher Helper';
    Application.Run;
  finally
    Application.Free;
  end;
end.

