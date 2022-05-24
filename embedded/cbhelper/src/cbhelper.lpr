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
  Version,
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
    fInitializeSwitch: Boolean;
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
  INITIALIZE_SWITCH = '--initialize';

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
  fInitializeSwitch := False;
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
      // This switch can be combined with others
      if IsInString(CLEAN_SWITCH, Param) then
        fCleanSwitch := True;

      // Initialize: This will create a default C::B user profiles for each user
      // This switch can be combined with others
      if IsInString(INITIALIZE_SWITCH, Param) then
        fInitializeSwitch := True;

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
  type
    TInnoSetupTranslation = record
      EnvironmentVariable: string;
      InnoSetupVariable: string;
      Is64BitOnly: Boolean;
    end;

  const
    DIRECTORIES: array[0..3] of TInnoSetupTranslation = (
      (EnvironmentVariable: '%ProgramW6432%'; InnoSetupVariable: '{pf64}'; Is64BitOnly: True),
      (EnvironmentVariable: '%ProgramFiles(x86)%'; InnoSetupVariable: '{pf32}'; Is64BitOnly: True),
      (EnvironmentVariable: '%ProgramFiles%'; InnoSetupVariable: '{pf64}'; Is64BitOnly: True),
      (EnvironmentVariable: '%ProgramFiles%'; InnoSetupVariable: '{pf32}'; Is64BitOnly: False)
    );

  var
    i: Integer;
    InstallationDirectory: TFileName;
    EnvironmentVariable,
    InnoSetupVariable: string;
    InnoSetupTranslation: TInnoSetupTranslation;

  begin
    InstallationDirectory := GetCodeBlocksDefaultInstallationDirectory;
{$IFDEF DEBUG}
    WriteLn('InstallationDirectory: ', InstallationDirectory);
{$ENDIF}
    for i := Low(DIRECTORIES) to High(DIRECTORIES) do
    begin
      InnoSetupTranslation := DIRECTORIES[i];
      if ((InnoSetupTranslation.Is64BitOnly and IsWindows64)
        or (not InnoSetupTranslation.Is64BitOnly and not IsWindows64)) then
      begin
        EnvironmentVariable := InnoSetupTranslation.EnvironmentVariable;
        InnoSetupVariable := InnoSetupTranslation.InnoSetupVariable;
{$IFDEF DEBUG}
        WriteLn(EnvironmentVariable, ' => ', InnoSetupVariable);
{$ENDIF}
        if IsInString(EnvironmentVariable, InstallationDirectory) then
        begin
{$IFDEF DEBUG}
          WriteLn('  Before: ', InstallationDirectory);
{$ENDIF}
          InstallationDirectory := StringReplace(
            InstallationDirectory,
            EnvironmentVariable,
            InnoSetupVariable,
            [rfReplaceAll, rfIgnoreCase]
          );
{$IFDEF DEBUG}
          WriteLn('  After: ', InstallationDirectory);
{$ENDIF}
        end;
      end;
    end;
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

  if fCleanSwitch then
    RemoveProfiles;

  if fInitializeSwitch then
    InitializeCodeBlocksProfiles;

  if fDetectSwitch then
    DetectCodeBlocksInstallation;

  if fVersionSwitch then
  begin
    if fVersionParamInstallationDirectory = EmptyStr then
      raise EArgumentException.Create('Missing Code::Blocks installation directory argument');
    DetectCodeBlocksVersion;
  end;

  if fUsersSwitch then
    GetAvailableUsers;

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

