$ErrorActionPreference = "Stop"
$PackagePath = "path/to/eyeprocess"

if (-not (Test-Path $PackagePath)) {
    throw "Package directory not found: $PackagePath"
}

$Rscript = (Get-Command Rscript.exe -ErrorAction SilentlyContinue).Source
if (-not $Rscript) {
    throw "Rscript.exe was not found on PATH. Install R and reopen PowerShell."
}

Write-Host "Installing eyeprocess from $PackagePath"
& $Rscript (Join-Path $PackagePath "install_eyeprocess.R")
if ($LASTEXITCODE -ne 0) { throw "eyeprocess installation failed." }

Write-Host "Running eyeprocess validation"
& $Rscript (Join-Path $PackagePath "validate_eyeprocess.R")
if ($LASTEXITCODE -ne 0) { throw "eyeprocess validation failed." }

Write-Host "eyeprocess installation and validation completed."
