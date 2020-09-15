#!/usr/bin/env bash
set -e
# ./orderly migrate
# ./orderly rebuild

TODAY=$(date "+%Y-%m-%d")
DATE=${1:-$TODAY}

DEFAULT_POP="2394000"
DAM_POP=${2:-$DEFAULT_POP}

DEFAULT_PHO="FALSE"
PHO=${3:-$DEFAULT_PHO}

DEFAULT_CA="same"
CA=${4:-$DEFAULT_CA}

DEFAULT_HOSP="4300"
HOSP=${5:-$DEFAULT_HOSP}

DEFAULT_HNU="0.55"
HNU=${6:-$DEFAULT_HNU}

DEFAULT_LS="FALSE"
LS=${7:-$DEFAULT_LS}

DEFAULT_DTF="reported"
DTF=${8:-$DEFAULT_DTF}

echo "*** Date: $DATE"
echo "*** Urban: $URBAN"
echo "*** Poorer Health Outcomes: $PHO"
echo "*** City Age: $CA"
echo "*** Hospital Beds: $HOSP"
echo "*** Hospital Normal Use: $HNU"
echo "*** Late Start: $LS"
echo "*** Data To Fit: $DTF"

# Default reporting looked at
parallel -k echo ::: "5e-04" "0.001" "0.005" "0.01" "0.0125" "0.015" "0.02" "0.03" "0.06" "0.1" "0.2" > rf.txt

# Certificate estimates - uncomment to run the certificate analysis
# parallel -k echo ::: "0.275" "0.35" "0.4" "0.55" "0.6" "0.65" "0.7" "1" > rf.txt

# Parallel
grep -E "*." rf.txt | \
parallel --progress -j 11 ./orderly run syria_under_reporting reporting_fraction={} date=$DATE dam_pop=$DAM_POP poorer_health_outcomes=$PHO city_age=$CA hosp_beds=$HOSP hospital_normal_use=$HNU late_start=$LS data_to_fit=$DTF
