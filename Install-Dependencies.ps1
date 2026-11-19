param()

$ErrorActionPreference = "Stop"
$RepositoryRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "FULLPIXEL MSI QUANT - INSTALL DEPENDENCIES" -ForegroundColor Cyan
Write-Host ""

function Find-Rscript {
    foreach ($CommandName in @("Rscript.exe", "Rscript")) {
        $Command = Get-Command $CommandName -ErrorAction SilentlyContinue

        if ($null -ne $Command) {
            return $Command.Source
        }
    }

    $SearchRoots = @()

    if ($env:ProgramFiles) {
        $SearchRoots += Join-Path $env:ProgramFiles "R"
    }

    if (${env:ProgramFiles(x86)}) {
        $SearchRoots += Join-Path ${env:ProgramFiles(x86)} "R"
    }

    if ($env:LOCALAPPDATA) {
        $SearchRoots += Join-Path $env:LOCALAPPDATA "Programs\R"
    }

    $VersionFolders = @()

    foreach ($SearchRoot in ($SearchRoots | Select-Object -Unique)) {
        if (-not (Test-Path -LiteralPath $SearchRoot)) {
            continue
        }

        foreach ($Folder in (Get-ChildItem -LiteralPath $SearchRoot -Directory `
                -Filter "R-*" -ErrorAction SilentlyContinue)) {
            $Version = try {
                [version]($Folder.Name -replace '^R-', '')
            } catch {
                [version]"0.0"
            }

            $VersionFolders += [pscustomobject]@{
                Folder = $Folder
                Version = $Version
            }
        }
    }

    $VersionFolders = $VersionFolders | Sort-Object `
        @{ Expression = { $_.Version }; Descending = $true }, `
        @{ Expression = { $_.Folder.FullName }; Descending = $false }

    foreach ($VersionFolder in $VersionFolders) {
        $Candidates = @(
            (Join-Path $VersionFolder.Folder.FullName "bin\x64\Rscript.exe"),
            (Join-Path $VersionFolder.Folder.FullName "bin\Rscript.exe")
        )

        foreach ($Candidate in $Candidates) {
            if (Test-Path -LiteralPath $Candidate) {
                return $Candidate
            }
        }
    }

    throw ("Rscript.exe was not found. Install R in a standard Windows " +
        "location or add Rscript to PATH, then run the dependency installer again.")
}

$RscriptPath = Find-Rscript
Write-Host "Using Rscript: $RscriptPath" -ForegroundColor DarkGray

$Python = Get-Command python.exe -ErrorAction SilentlyContinue
if ($null -eq $Python) {
    $Python = Get-Command python -ErrorAction SilentlyContinue
}
if ($null -eq $Python) {
    throw "Python was not found. Install Python 3.10 or newer and add it to PATH."
}

& $RscriptPath (Join-Path $RepositoryRoot "tools\install_dependencies.R")
if ($LASTEXITCODE -ne 0) {
    throw "R dependency installation failed."
}

& $Python.Source -m pip install -r (Join-Path $RepositoryRoot "requirements.txt")
if ($LASTEXITCODE -ne 0) {
    throw "Python dependency installation failed."
}

& $RscriptPath (Join-Path $RepositoryRoot "tools\verify_scientific_core.R")
if ($LASTEXITCODE -ne 0) {
    throw "Frozen scientific-core verification failed."
}

& $Python.Source (Join-Path $RepositoryRoot "tools\verify_release.py")
if ($LASTEXITCODE -ne 0) {
    throw "Release-integrity verification failed."
}

Write-Host ""
Write-Host "DEPENDENCY INSTALLATION AND RELEASE VERIFICATION: PASS" -ForegroundColor Green
