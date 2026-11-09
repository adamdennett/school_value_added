# la_stay_awake.ps1 - Asks Windows not to idle-sleep while journey routing runs. Changes no
# settings; the request ends with this process. Stops when the stop file appears, or after 40 hours.
param([string]$StopFile)
Add-Type -Namespace Win32 -Name Power -MemberDefinition '[DllImport("kernel32.dll")] public static extern uint SetThreadExecutionState(uint esFlags);'
$ES_CONTINUOUS = [uint32]2147483648; $ES_SYSTEM_REQUIRED = [uint32]1
$t0 = Get-Date
while (-not (Test-Path $StopFile) -and ((Get-Date) - $t0).TotalHours -lt 40) {
  [void][Win32.Power]::SetThreadExecutionState($ES_CONTINUOUS -bor $ES_SYSTEM_REQUIRED)
  Start-Sleep -Seconds 60
}
[void][Win32.Power]::SetThreadExecutionState($ES_CONTINUOUS)
