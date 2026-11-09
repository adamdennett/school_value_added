#!/bin/bash
# la_resume.sh - Restarts the journey work from wherever it stopped. Safe to run repeatedly:
# finished steps and networks are skipped. Pause at any time with pipeline/la_pause.sh.
#
#   bash pipeline/la_resume.sh          networks outside London, then POI counts, then R/29
#   bash pipeline/la_resume.sh london   London alone (long, needs nearly all the memory), then
#                                       its POI counts, then R/29. Nothing else should be running.
#
# Set RSCRIPT if R is somewhere else. Logs: data/cache/journey_la/logs/
cd "$(dirname "$0")/.." || exit 1
R="${RSCRIPT:-/c/Program Files/R/R-4.5.2/bin/Rscript.exe}"
J=data/cache/journey_la; L=$J/logs; NET=data-raw/r5/net_la; STOP=$J/stay_awake.stop
stamp() { date '+%H:%M'; }

if [ "$1" = "london" ]; then
  ALL="london"; MAX=1; HEAP=52G; TH=6; export SCENARIOS=walk_bus,walk
else
  ALL=$(ls "$NET" | grep -v '^london$'); MAX=3; HEAP=18G; TH=5
fi

rm -f "$STOP"; powershell -NoProfile -ExecutionPolicy Bypass -File pipeline/la_stay_awake.ps1 -StopFile "$STOP" &
trap 'touch "$STOP"' EXIT

route() { n=$1
  # A network stopped before its first checkpoint may hold half-written files, so it is rebuilt
  if [ ! -f "$J/flat_$n.rds" ]; then rm -f "$NET/$n/network.dat" "$NET/$n"/network.osm.pbf.mapdb*; fi
  NETS=$n R5R_HEAP=$HEAP R5R_THREADS=$TH "$R" R/27_la_routing.R 2>&1 | grep -v "Direct mode WALK" >> "$L/$n.log"
  if [ -f "$J/miss_$n.rds" ]; then echo "$(stamp) routed $n"; else echo "$(stamp) FAILED $n, see $L/$n.log"; fi
}
for n in $ALL; do
  [ -f "$J/miss_$n.rds" ] && continue
  while [ "$(jobs -r | wc -l)" -gt "$MAX" ]; do wait -n; done  # the keep-awake job is one of these
  route "$n" &
done
# Wait for the routing jobs alone, leaving the keep-awake job running
while [ "$(jobs -r | wc -l)" -gt 1 ]; do wait -n; done
echo "$(stamp) routing finished"

# POI counts for routed networks not yet counted, in up to three batches
TODO=""; for n in $ALL; do [ -f "$J/miss_$n.rds" ] && [ ! -f "$J/poi_$n.rds" ] && TODO="$TODO $n"; done
i=0; B0=""; B1=""; B2=""; for n in $TODO; do eval "B$((i % 3))=\"\$B$((i % 3)),$n\""; i=$((i + 1)); done
for b in "$B0" "$B1" "$B2"; do [ -n "$b" ] && { NETS=${b#,} STEP=count "$R" R/28_la_pois.R >> "$L/poi_resume.log" 2>&1 & }; done
while [ "$(jobs -r | wc -l)" -gt 1 ]; do wait -n; done
echo "$(stamp) POI counts finished"

"$R" R/29_la_analysis.R > "$L/analysis.log" 2>&1 && echo "$(stamp) analysis saved to data/journey_la.rds" || echo "$(stamp) analysis FAILED, see $L/analysis.log"
