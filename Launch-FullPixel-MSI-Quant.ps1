param(
    [int]$Port = 3838
)

$ErrorActionPreference = "Stop"
$RepositoryRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

if ($Port -lt 1 -or $Port -gt 65535) {
    throw "Port must be between 1 and 65535."
}

$Rscript = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($null -eq $Rscript) {
    $Rscript = Get-Command Rscript -ErrorAction SilentlyContinue
}
if ($null -eq $Rscript) {
    throw "Rscript was not found. Install R and add Rscript to PATH."
}

$env:FULLPIXEL_MSI_QUANT_ROOT = $RepositoryRoot

Write-Host "Launching FullPixel MSI Quant 1.0.0 on port $Port..." -ForegroundColor Cyan
& $Rscript.Source (Join-Path $RepositoryRoot "tools\launch_app.R") "--port=$Port"
exit $LASTEXITCODE
