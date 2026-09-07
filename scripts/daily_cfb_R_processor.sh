#!/bin/bash

# Commit + push, surviving a remote that moved while the build was running.
#
# Pulling BEFORE staging can only abort: the build has just rewritten tracked
# parquet/csv/json, so `git pull` refuses with "Your local changes would be
# overwritten by merge". The old form then committed anyway, pushed into a
# non-fast-forward rejection, and swallowed it -- a GREEN job that published
# nothing (wehoop-wnba-data 32192069433/32192069566, hoopR-nba-data 32204419012).
#
# Stage and commit FIRST so the tree is clean, then reconcile. `rebase --merge`
# rather than `pull --rebase`: the default am backend base64-encodes every blob
# it replays, which crawls on these binary-asset repos.
sdv_commit_push() {
  local msg="$1"; shift
  git add -- "$@" >/dev/null 2>&1 || true
  if git diff --cached --quiet; then
    echo "nothing to commit for: $msg"
    return 0
  fi
  git commit -m "$msg" >/dev/null || { echo "::warning ::commit failed: $msg"; return 1; }
  local attempt
  for attempt in 1 2 3; do
    if git push origin HEAD >/dev/null 2>&1; then
      echo "pushed: $msg (attempt $attempt)"
      return 0
    fi
    echo "push rejected (attempt $attempt); syncing with origin"
    git fetch --quiet origin main || true
    if ! git rebase --merge origin/main >/dev/null 2>&1; then
      git rebase --abort >/dev/null 2>&1 || true
      echo "::error ::cannot rebase onto origin/main for: $msg"
      return 1
    fi
  done
  echo "::error ::push still rejected after 3 attempts: $msg"
  return 1
}

while getopts s:e:r: flag
do
    case "${flag}" in
        s) START_YEAR=${OPTARG};;
        e) END_YEAR=${OPTARG};;
        r) RESCRAPE=${OPTARG};;
    esac
done
# The workflow checks out cfb/ sparsely (only cfb/schedules, which stage 02 reads
# back). Every other cfb/ output dir is write-only and absent from the runner, and
# saveRDS()/write_parquet() do NOT create parent dirs -- they fail with
# "cannot open the connection". That is what silently emptied cfbfastR_cfb_pbp
# from 2026-07-01 onward while the job stayed green.
mkdir -p \
  cfb/pbp/rds cfb/pbp/parquet \
  cfb/schedules/rds cfb/schedules/parquet \
  cfb/team_box/rds cfb/team_box/parquet \
  cfb/player_box/rds cfb/player_box/parquet \
  cfb/roster/rds cfb/roster/parquet cfb/roster/csv

# An R stage that dies prints "Execution halted" and exits non-zero, but the loop
# below used to ignore that and commit whatever happened to be on disk -- a green
# job publishing nothing. Record the failure and fail the run at the end.
# Stages are NOT skipped on an upstream failure: 02/03 read the schedules 01
# writes, so they fail too and each reports itself.
run_stage() {
  local label="$1"; shift
  local rc=0
  "$@" || rc=$?
  if [ "$rc" != "0" ]; then
    echo "::error ::${label} failed (exit ${rc}) -- output for this year is incomplete"
    STAGE_RC=1
  fi
  return "$rc"
}

for i in $(seq "${START_YEAR}" "${END_YEAR}")
do
    echo "$i"
    git pull  >> /dev/null
    git config --local user.email "action@github.com"
    git config --local user.name "Github Action"
    run_stage "week.R ($i)"            Rscript week.R -s $i -e $i
    run_stage "espn_cfb_01_pbp ($i)"   Rscript R/espn_cfb_01_pbp_creation.R -s $i -e $i
    run_stage "espn_cfb_02_team ($i)"  Rscript R/espn_cfb_02_team_box_creation.R -s $i -e $i
    run_stage "espn_cfb_03_player ($i)" Rscript R/espn_cfb_03_player_box_creation.R -s $i -e $i
    run_stage "espn_cfb_04_roster ($i)" Rscript R/espn_cfb_04_roster_creation.R -s $i -e $i
    sdv_commit_push "CFB Data Update (Start: $i End: $i)" . || PUSH_RC=1
done

# A rejected push is a FAILED run, not a green one. Release assets upload on a
# separate path and can succeed while the repo mirror is left stale.
if [ "${PUSH_RC:-0}" != "0" ]; then
  echo "::error ::At least one commit failed to reach origin; the repo mirror is stale."
  exit 1
fi

if [ "${STAGE_RC:-0}" != "0" ]; then
  echo "::error ::At least one R stage failed; released assets for the affected year are incomplete or stale."
  exit 1
fi
