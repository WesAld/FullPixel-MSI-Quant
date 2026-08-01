param()

$ErrorActionPreference = "Stop"
$RepositoryRoot = Split-Path -Parent $MyInvocation.MyCommand.Path

Write-Host "FULLPIXEL MSI QUANT - INSTALL DEPENDENCIES" -ForegroundColor Cyan
Write-Host ""

$Rscript = Get-Command Rscript.exe -ErrorAction SilentlyContinue
if ($null -eq $Rscript) {
    $Rscript = Get-Command Rscript -ErrorAction SilentlyContinue
}
if ($null -eq $Rscript) {
    throw "Rscript was not found. Install R and add Rscript to PATH."
}

$Python = Get-Command python.exe -ErrorAction SilentlyContinue
if ($null -eq $Python) {
    $Python = Get-Command python -ErrorAction SilentlyContinue
}
if ($null -eq $Python) {
    throw "Python was not found. Install Python 3.10 or newer and add it to PATH."
}

& $Rscript.Source (Join-Path $RepositoryRoot "tools\install_dependencies.R")
if ($LASTEXITCODE -ne 0) {
    throw "R dependency installation failed."
}

& $Python.Source -m pip install -r (Join-Path $RepositoryRoot "requirements.txt")
if ($LASTEXITCODE -ne 0) {
    throw "Python dependency installation failed."
}

& $Rscript.Source (Join-Path $RepositoryRoot "tools\verify_scientific_core.R")
if ($LASTEXITCODE -ne 0) {
    throw "Frozen scientific-core verification failed."
}

& $Python.Source (Join-Path $RepositoryRoot "tools\verify_release.py")
if ($LASTEXITCODE -ne 0) {
    throw "Release-integrity verification failed."
}

Write-Host ""
Write-Host "DEPENDENCY INSTALLATION AND RELEASE VERIFICATION: PASS" -ForegroundColor Green
