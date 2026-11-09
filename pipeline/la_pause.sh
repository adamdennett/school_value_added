#!/bin/bash
# la_pause.sh - Stops the journey routing (R/27) and POI counts (R/28) at any moment.
# Nothing finished is lost: R/27 saves after each step of each network and R/28 after each
# network, so la_resume.sh repeats only the step that was under way. Touches no other R job.
# Usage: bash pipeline/la_pause.sh
cd "$(dirname "$0")/.." || exit 1
touch data/cache/journey_la/stay_awake.stop
powershell -NoProfile -Command "
  \$q = Get-CimInstance Win32_Process -Filter \"Name='bash.exe'\" | Where-Object { \$_.CommandLine -match 'la_resume\.sh|after_sy\.sh|q\.sh 5 18G|q\.sh 6 ' }
  \$q | ForEach-Object { Stop-Process -Id \$_.ProcessId -Force -ErrorAction SilentlyContinue }
  \$r = Get-CimInstance Win32_Process -Filter \"Name='Rscript.exe'\" | Where-Object { \$_.CommandLine -match '27_la_routing|28_la_pois' }
  \$r | ForEach-Object { Stop-Process -Id \$_.ProcessId -Force -ErrorAction SilentlyContinue }
  'Stopped ' + @(\$q).Count + ' queue and ' + @(\$r).Count + ' R processes'
"
