#!/bin/zsh
# Remaining steps, run unattended. Logs in each RQ's results/.
cd "$(dirname "$0")"
Rscript RQ3_playoffs/scripts/01_playoff_models.R > RQ3_playoffs/results/run_log_01.txt 2>&1
while pgrep -f "02_download_euroleague" > /dev/null; do sleep 60; done
Rscript 00_data/scripts/04_build_shots.R > 00_data/build_shots_log.txt 2>&1
Rscript RQ3_playoffs/scripts/02_playoff_shot_zones.R > RQ3_playoffs/results/run_log_02.txt 2>&1
Rscript RQ3_playoffs/scripts/03_plot_rq3.R > RQ3_playoffs/results/run_log_03.txt 2>&1
Rscript RQ2_trends_leadlag/scripts/04_plot_rq2.R > RQ2_trends_leadlag/results/run_log_plot.txt 2>&1
quarto render report.qmd > report_render_log.txt 2>&1
echo "ALL DONE $(date)" > QUEUE_DONE.txt
