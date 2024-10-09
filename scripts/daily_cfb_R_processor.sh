#!/bin/bash
while getopts s:e:r: flag
do
    case "${flag}" in
        s) START_YEAR=${OPTARG};;
        e) END_YEAR=${OPTARG};;
        r) RESCRAPE=${OPTARG};;
    esac
done
for i in $(seq "${START_YEAR}" "${END_YEAR}")
do
    echo "$i"
    git pull  >> /dev/null
    git config --local user.email "action@github.com"
    git config --local user.name "Github Action"
    Rscript week.R -s $i -e $i
    # Rscript R/espn_cfb_01_pbp_creation.R -s $i -e $i
    # Rscript R/espn_cfb_02_team_box_creation.R -s $i -e $i
    # Rscript R/espn_cfb_03_player_box_creation.R -s $i -e $i
    # Rscript R/espn_cfb_04_roster_creation.R -s $i -e $i
    git pull >> /dev/null
    git add . >> /dev/null
    git add cfb/* >> /dev/null
    git pull  >> /dev/null
    git commit -m "CFB Data Update (Start: $i End: $i)" >> /dev/null || echo "No changes to commit"
    git pull --rebase  >> /dev/null
    git push  >> /dev/null
done