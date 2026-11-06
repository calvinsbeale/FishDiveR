; Inno Setup script for the standalone FishDiveR Windows app.
;
; Build the app folder first with installer/build.R, which also runs this
; script if Inno Setup 6 is installed. To run it by hand:
;   ISCC.exe /DAppVersion=1.2.0 /DBuildDir=C:\path\to\build installer\FishDiveR.iss
; BuildDir is the folder build.R wrote the app folder (FishDiveR) into.

#ifndef BuildDir
  #define BuildDir "build"
#endif
#ifndef AppVersion
  #define AppVersion "0.0.0"
#endif

[Setup]
AppId={{6E1B9C1A-3F7D-4B8E-9C2A-5D4F1E7A8B90}
AppName=FishDiveR
AppVersion={#AppVersion}
AppVerName=FishDiveR {#AppVersion}
AppPublisher=Calvin Beale
AppPublisherURL=https://calvinbeale.co.uk/fishdiver/
AppSupportURL=https://github.com/calvinsbeale/FishDiveR/issues
; Install for the current user only, so no administrator rights are needed
PrivilegesRequired=lowest
DefaultDirName={localappdata}\Programs\FishDiveR
DisableProgramGroupPage=yes
ArchitecturesAllowed=x64compatible
ArchitecturesInstallIn64BitMode=x64compatible
OutputDir={#BuildDir}\output
OutputBaseFilename=FishDiveR-{#AppVersion}-setup
SetupIconFile=FishDiveR.ico
UninstallDisplayIcon={app}\FishDiveR.ico
Compression=lzma2/max
SolidCompression=yes
WizardStyle=modern

[Tasks]
Name: "desktopicon"; Description: "Create a desktop shortcut"; Flags: unchecked

[Files]
Source: "{#BuildDir}\FishDiveR\*"; DestDir: "{app}"; Flags: recursesubdirs createallsubdirs ignoreversion

[Icons]
Name: "{autoprograms}\FishDiveR"; Filename: "{app}\R\bin\x64\Rscript.exe"; Parameters: "--vanilla ""{app}\app\launch.R"""; WorkingDir: "{app}"; IconFilename: "{app}\FishDiveR.ico"; Comment: "Classify aquatic animal behaviours from depth records"; Flags: runminimized
Name: "{autodesktop}\FishDiveR"; Filename: "{app}\R\bin\x64\Rscript.exe"; Parameters: "--vanilla ""{app}\app\launch.R"""; WorkingDir: "{app}"; IconFilename: "{app}\FishDiveR.ico"; Comment: "Classify aquatic animal behaviours from depth records"; Flags: runminimized; Tasks: desktopicon

[Run]
Filename: "{app}\R\bin\x64\Rscript.exe"; Parameters: "--vanilla ""{app}\app\launch.R"""; WorkingDir: "{app}"; Description: "Start FishDiveR"; Flags: postinstall nowait skipifsilent runminimized
