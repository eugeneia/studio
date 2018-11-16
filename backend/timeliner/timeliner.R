# timerliner.R: Process Snabb timeline logs

library(dplyr)
library(stringr)
library(bit64)
library(readr)
library(ggplot2)
library(rlang)

# ------------------------------------------------------------
# High-level operational API functions
# ------------------------------------------------------------

# Load a binary timeline and save summary data for further processing.
summarize_timeline <- function(filename, outdir=".") {
  save_timeline_summaries(read_timeline(filename), outdir)
}

# Load timeline summaries into environment.
load_timeline_summary <- function (summarydir=".") {
  breath_summary <<- read_rds(file.path(summarydir, "breaths.rds.xz"))
  callback_summary <<- read_rds(file.path(summarydir, "callbacks.rds.xz"))
  event_summary <<- read_rds(file.path(summarydir, "events.rds.xz"))
}

# Export R data or a ggplot.
export_graph_or_data <- function (result, outdir) {
  if (is.data.frame(result$value)) {
    # TSV, unquoted and without row names: GTInspector likes it this way.
    write.table(result$value, file=file.path(outdir, "out.csv"),
                sep="\t", quote=F, row.names=F)
  } else {
    ggsave(file.path(outdir, "out.png"), result$value,
           width=20, height=16, units="cm", dpi=120)
  }
}

# ------------------------------------------------------------
# Reading and decoding timelines
# ------------------------------------------------------------

# Read a timeline file into a data frame.
read_timeline <- function(filename) {
  tl <- read_binary_timeline(filename)
  tl$numa <- as.factor(tl$numa)
  tl$core <- as.factor(tl$core)
  tl$unixtime <- calculate_unixtime(tl)
  tl$cycles <- calculate_cycles(tl)
  # Sort entries by unix time. Should roughly take care of log wrap-around.
  # See FIXME comment in unixtime() though.
  #tl <- arrange(tl, unixtime)
  tl
}

# Read a timeline file into a tibble.
read_binary_timeline <- function(filename) {
  f <- file(filename, "rb")
  # Read fields
  magic <- readBin(f, raw(), n=8, endian="little")
  version <- readBin(f, "integer", n=2, size=2, endian="little")
  log_bytes <- readBin(f, "integer", n=1, size=4, endian="little")
  strings_bytes <- readBin(f, "integer", n=1, size=4, endian="little")
  # Check compat
  if (!all(magic == c(0x01, 0x00, 0x1d, 0x44, 0x23, 0x72, 0xff, 0xa3))) {
    stop("bad magic number")
  }
  if (version[1] != 2 & version[1] != 3) {
    stop("unrecognized major version")
  }
  seek(f, 64)
  entries <- readBin(f, "double", n=log_bytes/8, size=8, endian="little")
  elem0 = seq(1, log_bytes/8, 64/8)
  # Tricky: Second element is integer on disk but double in R
  tmp <- entries[elem0+1]
  class(tmp) <- "integer64"
  entries[elem0+1] <- as.numeric(tmp)
  tl <- tibble(tsc = entries[elem0],
               msgid = bitwAnd(entries[elem0+1], 0xFFFF),
               core = bitwAnd(bitwShiftR(entries[elem0+1], 16), 0xF),
               numa = bitwShiftR(entries[elem0+1], 24),
               arg0 = entries[elem0+2],
               arg1 = entries[elem0+3],
               arg2 = entries[elem0+4],
               arg3 = entries[elem0+5],
               arg4 = entries[elem0+6],
               arg5 = entries[elem0+7])
  tl <- na.omit(tl)
  # Read strings
  stringtable <- character(strings_bytes/16) # dense array
  start <- 64+log_bytes
  seek(f, start)
  repeat {
    id <- 1+(seek(f)-start)/16
    s <- readBin(f, "character")
    if (s == "") break;
    stringtable[id] <- s
    seek(f, ceiling(seek(f)/16) * 16) # seek to 16-byte alignment
  }
  # Decode string messages
  messages <- tibble(msgid = 0:(length(stringtable)-1), message = stringtable) %>%
    filter(message != "") %>%
    mutate(summary = str_extract(message, "^[^\n]+"),
           level = as.integer(str_extract(summary, "^[0-9]")),
           event = gsub("^[0-9],[0-9]\\|([^:]+):.*", "\\1", summary))
  # Combine messages with events
  left_join(tl, messages, by="msgid")
}

# Calculate unix timestamps for each entry.
calculate_unixtime <- function(tl) {
  times <- filter(tl, grepl("got_monotonic_time", event))

  # FIXME: Make sure the delta is taken between two timestamps from
  # the _same CPU core_. If we take the delta between two timestamps
  # whose TSCs are not synchronized (e.g. different NUMA nodes) then
  # we will misestimate the clock speed (maybe even negative...)
  
  if (length(times) < 2) {
    stop("could not calculate unix time: need two timestamps to compare.")  
  } else {

    # Calculate GHz (cycles per nanosecond) from timestamp deltas.
    GHz <- (max(times$tsc)-min(times$tsc)) / (max(times$arg0)-min(times$arg0))
    # Pick an epoch (any will do)
    reftsc <- last(times$tsc)
    reftime <- last(times$arg0)
    # Function from cycles to unix nanoseconds
    unixtime <- function(tsc) {
      reftime + ((tsc - reftsc) / GHz)
    }
    mapply(unixtime, tl$tsc)
  }
}

# Calculate cycles since log entry of >= level ("lag") for each entry.
calculate_cycles <- function(tl) {
  # reference timestamp accumulator for update inside closure.
  # index is log level and value is reference timestamp for delta.
  ref <- as.numeric(rep(NA, 10))
  tscdelta <- function(level, time) {
    if (is.na(level)) { stop("level na") }
    if (is.na(time)) { stop("time na") }
    delta <- time - ref[level+1]
    ref[level+1:10] <<- time
    delta
  }
  mapply(tscdelta, tl$level, tl$tsc)
}

# ------------------------------------------------------------
# Saving CSV summaries of timelines
# ------------------------------------------------------------

# Save R object summaries of a timeline.
save_timeline_summaries <- function(tl, outdir=".") {
  if (!dir.exists(outdir)) { dir.create(outdir, recursive=T) }
  br <- breaths(tl)
  cb <- callbacks(tl)
  ev <- events(tl)
  save_data(br, file.path(outdir, "breaths.rds.xz"))
  save_data(cb, file.path(outdir, "callbacks.rds.xz"))
  save_data(ev, file.path(outdir, "events.rds.xz"))
}

save_data <- function(data, filename) {
  message("Saving ", filename)
  write_rds(data, filename, compress="xz")
}

# Create a data frame with one row for each breath.
breaths <- function(tl) {
  tl %>% 
    filter(grepl("breath_start|breath_end", event)) %>%
    mutate(breath = arg0,
           total_packets = lag(arg1), total_bytes = lag(arg2), total_ethbits = lag(arg3),
           packets = arg1, bytes = arg2) %>%
    filter(grepl("breath_end", event)) %>%
    na.omit() %>%
    select(unixtime, cycles, numa, core,
           breath, total_packets, total_bytes, total_ethbits, packets, bytes)
}

# Create a data frame with one row for each app callback.
callbacks <- function(tl) {
  tl %>% filter(grepl("^app.(pull|push)", event)) %>%
    mutate(inpackets = arg0 - lag(arg0), inbytes = arg1 - lag(arg1),
           outpackets = arg2 - lag(arg2), outbytes = arg3 - lag(arg3),
           dropped = arg4 - lag(arg4), dropbytes = arg5 - lag(arg5)) %>%
    mutate(packets = pmax(inpackets, outpackets), bytes = pmax(inbytes, outbytes)) %>%
    filter(grepl("^app.(pushed|pulled)", event)) %>%
    na.omit() %>%
    select(unixtime, cycles, numa, core,
           event, packets, bytes,
           inpackets, inbytes, outpackets, outbytes, dropped, dropbytes)
}

# Create a data frame with one row for each event (relative to breath_start.)
events <- function(tl) {
  tl %>%
    filter(cycles > 0) %>%
    select(unixtime, cycles, numa, core, event, level,
           arg0, arg1, arg2, arg3, arg4, arg5)
}

# ------------------------------------------------------------
# Visualizing the timeline summary (toolkit)
# ------------------------------------------------------------

# Show various metrics for recorded breaths over time from start to end (in
# seconds starting from zero), summarised to N points.
breath_history <- function(br=breath_summary, start=0, end=F, points=100) {
  hist <- br %>%
    mutate(t = (unixtime - first(unixtime)) / 1e9) %>%
    filter(t >= start & (!end | t <= end)) %>%
    mutate(Gbps = (total_ethbits - lag(total_ethbits)) / (t - lag(t)) / 1e9,
           Mpps = (total_packets - lag(total_packets)) / (t - lag(t)) / 1e6,
           usec = (unixtime - lag(unixtime)) / (breath - lag(breath)) * 1e-3) %>%
    na.omit() %>%
    quantize(points)
  nonzero <- hist %>%
    filter(packets>0) %>%
    mutate(cycles = pmin(cycles / packets, quantile(.$cycles / .$packets, .95)))
  metrics <- bind_rows(
    summarise_metric(hist, var="Gbps", metric="Gbps (freed)"),
    summarise_metric(hist, var="Mpps", metric="Mpps (freed)"),
    summarise_metric(nonzero, var="cycles",  metric="cpp (<=p95)"),
    summarise_metric(hist, var="packets", metric="packets/breath"),
    summarise_metric(nonzero, var="bytes", metric="bytes/packet"),
    summarise_metric(hist, var="usec", metric="usec/breath")
  )
  ggplot(metrics, aes(x = t, color = metric, fill = metric)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    facet_wrap(~ metric, scales="free_y", strip.position="right", ncol = 1) +
    labs(y = "", x = "time (s)") +
    geom_line(aes(y = mean, linetype="mean")) +
    geom_line(aes(y = median, linetype="median")) +
    geom_ribbon(aes(ymax = q95, ymin = q05, alpha = "5th-95th"), color = NA) +
    geom_ribbon(aes(ymax = q75, ymin = q25, alpha = "25th-75th"), color = NA) +
    theme(legend.position="top") +
    guides(color=FALSE, fill=FALSE) +
    scale_alpha_manual(name="Percentiles", values=c("5th-95th"=0.1, "25th-75th"=0.2)) +
    scale_linetype_manual(name="Averages", values=c(mean="solid", median="dotted"))
}

# Show various metrics for recorded callbacks that match a pattern over time
# from start to end (in seconds starting from zero), summarised to N points.
callback_history <- function(cb=callback_summary, pattern="",
                             start=0, end=F, points=100) {
  hist <- cb %>%
    mutate(t = (unixtime - first(unixtime)) / 1e9) %>%
    filter(t >= start & (!end | t <= end) & grepl(pattern, event)) %>%
    na.omit() %>%
    quantize(points) %>%
    group_by(t, event)
  nonzero <- hist %>%
    filter(packets>0) %>%
    group_by(event) %>%
    mutate(cycles = pmin(cycles / packets, quantile(.$cycles / .$packets, .95)),
           bytes = bytes / packets) %>%
    group_by(t, event)
  metrics <- bind_rows(
    summarise_metric(nonzero, var="cycles", metric="cycles/packet (<=p95)"),
    summarise_metric(hist, var="packets", metric = "packets/callback"),
    summarise_metric(nonzero, var="bytes", metric="bytes/packet"),
    summarise_metric(hist, var="dropped", metric = "drops/callback")
  )
  ggplot(metrics, aes(x = t, color = metric, fill = metric)) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(labels = scales::comma) +
    facet_grid(metric ~ event, scales="free_y") +
    labs(y = "", x = "time (s)") +
    geom_line(aes(y = mean, linetype="mean")) +
    geom_line(aes(y = median, linetype="median")) +
    geom_ribbon(aes(ymax = q95, ymin = q05, alpha = "5th-95th"), color = NA) +
    geom_ribbon(aes(ymax = q75, ymin = q25, alpha = "25th-75th"), color = NA) +
    theme(legend.position="top") +
    guides(color=FALSE, fill=FALSE) +
    scale_alpha_manual(name="Percentiles", values=c("5th-95th"=0.1, "25th-75th"=0.2)) +
    scale_linetype_manual(name="Averages", values=c(mean="solid", median="dotted"))
}

# Show breath duration in cycles relative to burst size for breaths recorded
# from start to end (in seconds starting from zero), excluding breaths outside
# the upper and lower percentiles phigh/plow.
breath_duration <- function (br=breath_summary,
                             start=0, end=F, phigh=.95, plow=0) {
  s <-  br %>%
    mutate(t = (unixtime - first(unixtime)) / 1e9) %>%
    filter(t >= start & (!end | t <= end))
  high <- quantile(s$cycles, phigh)
  low <- quantile(s$cycles, plow)
  d <- filter(s, cycles >= low & cycles <= high)
  ggplot(d, aes(y = cycles, x = packets)) +
    scale_y_continuous(labels = scales::comma) +
    geom_point(color="blue", alpha=0.25, shape=1) +
    labs(title = "Breath duration",
         subtitle = paste("Breaths that took between ", scales::comma(low),
                          " and ", scales::comma(high), " cycles ",
                          "(", scales::percent(nrow(d)/nrow(s)),
                          " of sampled breaths)", sep=""),
         y = "cycles",
         x = "packets processed in engine breath (burst size)")
}

# Show breath efficiency in cycles/packet relative to burst size for breaths
# recorded from start to end (in seconds starting from zero), excluding breaths
# outside the upper and lower percentiles phigh/plow.
breath_efficiency <- function (br=breath_summary, start=0,
                               end=F, phigh=.95, plow=0) {
  s <-  br %>%
    mutate(t = (unixtime - first(unixtime)) / 1e9, cpp = cycles / packets) %>%
    filter(packets > 0 & t >= start & (!end | t <= end))
  high <- quantile(s$cpp, phigh)
  low <- quantile(s$cpp, plow)
  d <- filter(s, cpp >= low & cpp <= high)
  ggplot(d, aes(y = cpp, x = packets)) +
    scale_y_continuous(labels = scales::comma) +
    geom_point(color="blue", alpha=0.25, shape=1) +
    geom_smooth(se=F, weight=1, alpha=0.1) +
    labs(subtitle = paste("Breaths that took between ", scales::comma(low),
                          " and ", scales::comma(high), " cycles/packet ",
                          "(", scales::percent(nrow(d)/nrow(s)),
                          " of sampled breaths)", sep=""),
         y = "cycles/packet",
         x = "packets processed in engine breath (burst size)")
}

# Show callback efficiency in cycles/packet relative to burst size for
# callbacks that match a pattern recorded from start to end (in seconds
# starting from zero), excluding callbacks outside the upper and lower
# percentiles phigh/plow.
callback_efficiency <- function (cb=callback_summary, pattern="",
                                 start=0, end=F, phigh=.95, plow=0) {
  s <-  cb %>%
    mutate(t = (unixtime - first(unixtime)) / 1e9, cpp = cycles / packets) %>%
    filter(grepl(pattern, event) & packets > 0 & t >= start & (!end | t <= end))
  high <- quantile(s$cpp, phigh)
  low <- quantile(s$cpp, plow)
  d <- filter(s, cpp >= low & cpp <= high)
  ggplot(d, aes(y = cpp, x = packets)) +
    scale_y_continuous(labels = scales::comma) +
    geom_point(color="blue", alpha=0.25, shape=1) +
    geom_smooth(se=F, weight=1, alpha=0.1) +
    facet_wrap(~ event) +
    labs(subtitle = paste("Callbacks that took between ", scales::comma(low),
                          " and ", scales::comma(high), " cycles/packet ",
                          "(", scales::percent(nrow(d)/nrow(s)),
                          " of sampled callbacks)", sep=""),
         y = "cycles/packet",
         x = "packets processed in callback (burst size)")
}

# Show event lag (cycles since log entry of >= level) for events that match a
# pattern recorded from start to end (in seconds starting from zero.)
# Only includes events between minl and maxl (lag in cycles.)
# Plots cycle latency on a log10 scale unless log=F.
event_lag <- function (ev=event_summary, pattern="",
                       start=0, end=F, minl=0, maxl=F, log=T) {
  d <- ev %>%
    mutate(t = (unixtime - first(unixtime)) / 1e9) %>%
    filter(grepl(pattern, event) &
             (is.na(t) | (t >= start & (!end | t <= end))) &
             cycles >= minl & (!maxl | cycles <= maxl))
  ggplot(d, aes(x = event, y = cycles, color = level)) +
    (if (log) { scale_y_log10(labels=scales::comma) }
         else { scale_y_continuous(labels=scales::comma)}) +
    geom_boxplot() +
    coord_flip() +
    theme(legend.position="bottom") +
    labs(subtitle = "Event lag (relative to last event >= level)",
         y = "cycles (lag)")
}

# ------------------------------------------------------------
# Utilities (toolkit)
# ------------------------------------------------------------

quantize <- function (df, steps) {
  step <- max(0.0001, (max(df$t) - min(df$t)) / steps)
  df %>% mutate(t = round(t / step) * step) %>% group_by(t)
}

summarise_metric <- function (df, var, metric) {
  df %>%
    summarise(metric = metric,
              mean = mean(!! sym(var)),
              median = median(!! sym(var)),
              q75 = quantile(!! sym(var), .75),
              q25 = quantile(!! sym(var), .25),
              q95 = quantile(!! sym(var), .95),
              q05 = quantile(!! sym(var), .05))
}
