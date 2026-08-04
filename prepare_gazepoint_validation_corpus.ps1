$ErrorActionPreference = "Stop"

$Source = "C:\Users\Stefanos-PC\Desktop\gp3_test_exports"
$Corpus = "C:\Users\Stefanos-PC\Documents\Rstudio\eyeprocess-validation-corpus"
$Case = Join-Path $Corpus "cases\gazepoint-analysis-v7.2.0-demo"
$Manifest = Join-Path $Corpus "validation-manifest.csv"

if (-not (Test-Path -LiteralPath $Source)) {
  throw "Gazepoint export folder not found: $Source"
}

New-Item -ItemType Directory -Force -Path $Case | Out-Null
Copy-Item -Path (Join-Path $Source "*") -Destination $Case -Recurse -Force

@'
case_id,path,vendor,format_family,software_version,device_model,expected_import,require_gaze,require_native_time,require_coordinate_space,require_provenance,require_raw_retention,run_roundtrip,notes
gazepoint-analysis-v7-2-0-six-users,cases/gazepoint-analysis-v7.2.0-demo,gazepoint,gazepoint_analysis,v7.2.0,GP3 with Gazepoint Biometrics,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,"Six paired all-gaze/fixation exports; two media per user; pupil for all users; GSR HR IBI and dial present for Users 3-5; four multi-section Data Summary reports."
'@ | Set-Content -LiteralPath $Manifest -Encoding UTF8

Write-Host "Gazepoint validation case copied to: $Case"
Write-Host "Manifest written to: $Manifest"
Write-Host "Next run: source('C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_real_exports.R')"
