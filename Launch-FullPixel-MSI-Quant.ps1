param(
    [int]$Port = 3838
)

$ErrorActionPreference = "Stop"
$RepositoryRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

if ($Port -lt 1 -or $Port -gt 65535) {
    throw "Port must be between 1 and 65535."
}

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

    foreach ($SearchRoot in ($SearchRoots | Select-Object -Unique)) {
        if (-not (Test-Path -LiteralPath $SearchRoot)) {
            continue
        }

        $VersionFolders = Get-ChildItem `
            -LiteralPath $SearchRoot `
            -Directory `
            -Filter "R-*" `
            -ErrorAction SilentlyContinue |
            Sort-Object Name -Descending

        foreach ($VersionFolder in $VersionFolders) {
            $Candidates = @(
                (Join-Path $VersionFolder.FullName "bin\x64\Rscript.exe"),
                (Join-Path $VersionFolder.FullName "bin\Rscript.exe")
            )

            foreach ($Candidate in $Candidates) {
                if (Test-Path -LiteralPath $Candidate) {
                    return $Candidate
                }
            }
        }
    }

    throw "Rscript.exe was not found. Install R and restart the launcher."
}

$RscriptPath = Find-Rscript
$LaunchScript = Join-Path $RepositoryRoot "tools\launch_app.R"

if (-not (Test-Path -LiteralPath $LaunchScript)) {
    throw "Application launch script was not found: $LaunchScript"
}

$env:FULLPIXEL_MSI_QUANT_ROOT = $RepositoryRoot

Write-Host "Launching FullPixel MSI Quant V3 3.0.0-dev on port $Port..." -ForegroundColor Cyan
Write-Host "Using Rscript: $RscriptPath" -ForegroundColor DarkGray

& $RscriptPath `
    $LaunchScript `
    "--port=$Port" `
    "--launch-browser=true"

exit $LASTEXITCODE
