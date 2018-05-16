# timeliner nix library API:
#
#   timeliner.summary <path>:
#     Generate summary data from a timeline for further processing.
#
#   timeliner.summaryTarball <url>:
#     Like timeliner.summary but for remote tarball.
#
#   timeliner.evaluate <summary> <script>:
#     Evaluate R script on summary.
#
# The output of timeliner.summary is the input to timeliner.evaluate
# The processing is split into two parts so that the summary data can
# be archived separately from the timeline files (e.g. to save space
# by keeping only the summary data and discarding the full timeline.)

{ pkgs ? import ../../nix/pkgs.nix {} }:

with pkgs; with stdenv;

let buildInputs = with rPackages; [ R dplyr readr ggplot2 bit64 mgcv rlang ]; in

rec {
  # dir: path to a Snabb shm folder.
  summary = dir: runCommand "timeline-summary" { inherit buildInputs; } ''
      cd "${dir}"
      Rscript - <<EOF
        source("${./timeliner.R}")
        summarize_timeline("engine/timeline", "$out")
      EOF
    '';
  # url: path to a tarball containing a Snabb shm folder.
  summaryTarball = url: summary (fetchTarball url);
  # summaryData: Output from the summary derivation above.
  process = {summaryData, script}: runCommand "timeline-visualization" { inherit buildInputs; } ''
    mkdir $out
    Rscript - <<EOF
      source("${./timeliner.R}")
      load_timeline_summary("${summaryData}")
      export_graph_or_data(source("${script}"), "$out")
    '';
}

