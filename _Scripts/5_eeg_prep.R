############################################
### DEMI EEG processing & merging script ###
############################################

### Import required packages & functions ###

library(readr)
library(dplyr)
library(purrr)
library(data.table)

source("./_Scripts/_settings.R")
source("./_Scripts/_functions/eeg.R")
source("./_Scripts/_functions/emg.R")

options(dplyr.summarise.inform = FALSE)



### Load cached behavioural and EEG data ###

bdat <- readRDS("./_Scripts/_rds/bdat2.rds")
eeg_data <- readRDS("./_Scripts/_rds/eeg_imported.Rds")

# NOTE: should check and see how this differs from the MNE coords we use
eegcoords <- read_csv("./_Data/eeg/BESA-81.csv", col_types = cols())



### Perform EMG analysis for imagery participants ###

# Import, preprocess, and summarize epoch amplitudes per id/trial/deltoid

cat("\n### Preprocessing EMG data for imagery participants ###\n\n")
emg_activity <- map_df(names(eeg_data), function(id) {

  # If not an imagery participant (no saved EMG), skip to the next
  emg <- eeg_data[[id]]$emg
  if (all(is.na(emg))) {
    return(NULL)
  }
  id_num <- as.numeric(gsub("\\D", "", id))
  cat(" - Processing EMG signal for participant", id_num, "...\n")

  # Clean up epoch labels and append to signal
  cleaned_events <- emg$.events %>%
    filter(.description != "trace_start") %>%
    mutate(.description = gsub("real_", "", .description)) %>%
    filter(.description != "trace_end")
  emg$.signal <- append_epochs(emg$.signal, cleaned_events)

  # Wrangle, rectify, and smooth both EMG signals for each trial
  emg_cleaned <- emg$.signal %>%
    select(trial, .sample, epoch, EMG.L, EMG.A) %>%
    rename(Anterior = EMG.A, Lateral = EMG.L) %>%
    gather("deltoid", "signal", Anterior, Lateral) %>%
    group_by(trial, deltoid) %>%
    mutate(
      cleaned = emg_rectify(signal),
    ) %>%
    mutate(
      cleaned = channel_dbl(emg_smoothing(cleaned, 30, srate = 1000))
    )
  
  # Get median absolute deviation of muscle activity for each trial/epoch
  emg_summary <- emg_cleaned %>%
    group_by(trial, deltoid, epoch) %>%
    summarize(amplitude = mad(cleaned)) %>%
    mutate(
      trial_type = ifelse(trial >= 100, "physical", "imagery")
    ) %>%
    add_column(id = id_num, .before = "trial")

  emg_summary
})


# Calculate activity difference scores between rest and trace epochs

emg_diffs <- emg_activity %>%
  spread("epoch", "amplitude") %>%
  group_by(id, deltoid) %>%
  mutate(
    diff_score = trace_start - stim_on
  )


# Flag imagery trials w/ high activity relative to the id's physical trials

emg_bads <- emg_diffs %>%
  group_by(id, deltoid) %>%
  mutate(
    bad_threshold = quantile(diff_score[trial_type == "physical"], prob = 0.25)
  ) %>%
  group_by(id, trial) %>%
  summarize(
    bad = all(trial_type == "imagery" & diff_score >= bad_threshold)
  ) %>%
  filter(bad == TRUE)



### Perform preprocessing on EEG files to prepare for GAMs ###

# Create output path if it doesn't already exist

participants_rds_path <- "./_Scripts/_rds/participants/"
if (!dir.exists(participants_rds_path)) {
  dir.create(participants_rds_path)
}


# Set EEG baseline based on pipeline settings

if (use_pretrial_baseline) {
  baseline_window <- NULL
} else {
  baseline_window <- c(-0.5, -0.2)
}


# Perform wavelet decomposition and dB normalization on each id's data

subject_ids <- names(eeg_data)

for (id in subject_ids) {

  # Load in epoched and downsampled EEG data from list
  id_num <- as.numeric(gsub("\\D", "", id))
  cat("\n### Processing EEG signal for participant", id_num, "###\n")
  epoched <- eeg_data[[id]]$eeg

  # If enabled, drop imagery trials w/ excess EMG activity during trace
  if (exclude_bad_by_emg) {
    bad_img_trials <- emg_bads %>%
      filter(id == id_num) %>%
      pull(trial)
    bad_img_rows <- epoched$trial %in% bad_img_trials
    epoched <- epoched[!bad_img_rows, ]
  }

  # Convert "time" to seconds
  epoched$time <- as.double(epoched$time) / 1000

  # If using, perform wavelet decomposition on baseline data
  if (use_pretrial_baseline) {
    cat("\n# Performing wavelet decomposition on baseline epochs\n")
    is_baseline <- epoched$epoch == "baseline"
    baseline_wt <- wavelet_transform_id(
      eeg_signal = epoched[is_baseline, ],
      freqs = wt_frequencies,
      trim = c(1, 1),
      downsample = TRUE
    )
  }

  # Perform wavelet decomposition & dB normalization on tracing data
  cat("\n# Performing wavelet decomposition on tracing epochs\n")
  is_tracing <- epoched$epoch == "tracing"
  tracing_wt <- wavelet_transform_id(
    eeg_signal = epoched[is_tracing, ],
    freqs = wt_frequencies,
    trim = c(1, 1),
    baseline = baseline_window,
    downsample = TRUE
  )

  # Perform wavelet decomposition & dB normalization on post-tracing data
  cat("\n# Performing wavelet decomposition on post-tracing epochs\n")
  is_post_trace <- epoched$epoch == "post_trace"
  post_trace_wt <- wavelet_transform_id(
    eeg_signal = epoched[is_post_trace, ],
    freqs = wt_frequencies,
    trim = c(1, 1),
    baseline = baseline_window,
    downsample = TRUE
  )

  # Merge wavelet-decomposed data from the tracing and post-tracing epochs
  epoched_wt <- data.table::rbindlist(list(
    tracing = tracing_wt,
    post_trace = post_trace_wt
  ), idcol = "epoch")

  # Remove intermediate objects to free up memory
  rm(epoched, tracing_wt, post_trace_wt)

  # If using baseline epoch, use it to decibel-normalize the power data
  if (use_pretrial_baseline) {
    cat("\n# Decibel-normalizing power using mean baseline epoch power...\n")
    freq_key <- c("trial", "chan", "freq")
    baseline_pwr <- baseline_wt[, .(avg_pwr = mean(power)), by = freq_key]
    epoched_wt <- epoched_wt[
      baseline_pwr, on = freq_key, baseline_pwr := avg_pwr
    ]
    epoched_wt[, powerdb := 10 * (log10(power) - log10(baseline_pwr))]
    epoched_wt[, baseline_pwr := NULL]
    rm(baseline_wt)
  }

  # Save Rds of data for future modelling and clear data objects from memory
  cat("\n# Saving data to .Rds...\n")
  outfile <- paste0(participants_rds_path, id, "_eeg_processed.rds")
  setcolorder(epoched_wt, c("trial", "epoch", "chan", "freq"))
  saveRDS(epoched_wt, file = outfile)
  rm(epoched_wt)
  cat("\n### Participant", id_num, "successfully processed! ###\n\n")
}



### Combine full dataset ###

# Free up any memory taken up by wavelet transformation

rm(eeg_data)


# Get list of all wavelet-transformed EEG .Rds files

eeg_wt_files <- list.files(
  participants_rds_path, pattern = "*.rds",
  full.names = TRUE
)


# Merge wavelet-transformed EEG data from all participants

all_dat <- NULL

cat("\n### Combining wavelet-transformed EEG for all participants ###\n")
all_dat <- data.table::rbindlist(

  lapply(eeg_wt_files, function(f) {

    id_num <- as.integer(gsub("\\D", "", basename(f)))
    cat("\n# Loading EEG data from participant", id_num, "...\n")
    out <- readRDS(f)[, id := id_num]

    if (f == tail(eeg_wt_files, n = 1)) {
      cat("\n# Merging EEG data from all participants...\n")
    }

    out
  })
)



# Make some variable integers to reduce memory demands

all_dat[, trial := as.integer(trial)]
all_dat[, freq := as.integer(freq)]
all_dat[, epoch := ifelse(epoch == "tracing", 1L, 2L)]


# Merge behavioural data with EEG data

cols_to_merge <- c(
  "id", "group", "trial", "block_num", "condition", "rep", "complexity",
  "avg_velocity", "error", "vresp", "accuracy_rating"
)

bdat_merge <- bdat %>%
  mutate(
    id = as.integer(as.character(participant)),
    trial = as.integer(trial_num + (block_num - 1) * 20),
    rep = as.integer(rep),
    accuracy_rating = as.integer(accuracy_rating)
  ) %>%
  select(all_of(cols_to_merge)) %>%
  arrange(id) %>%
  as.data.table()

all_dat <- all_dat[bdat_merge,
  on = c("id", "trial"),
  `:=`(
    group = group,
    block_num = block_num,
    condition = condition,
    rep = rep,
    complexity = complexity,
    avg_velocity = avg_velocity,
    error = error,
    vresp = vresp,
    accuracy_rating = accuracy_rating
  )
]


# Add spatial electrode coordinate info for each channel

# NOTE: Shouldn't do this here: different scripts use coords in different
# formats, so would be better to join coords in the correct format in
# scripts before they're needed

eegcoords <- eegcoords %>%
  mutate(
    lat = 90 - (asin(z) * (180 / pi)),
    long = atan2(y, x) * (180 / pi)
  ) %>%
  as.data.table()

all_dat <- all_dat[eegcoords, on = c("chan"), `:=`(lat = lat, long = long)]


# Clean up column names & order

setcolorder(all_dat, c("id", "group", "block_num", "trial", "condition", "rep", "epoch"))
setnames(all_dat, "id", "participant")


# Write out giant merged data frame to Rds for modelling and plotting

saveRDS(all_dat, file = "./_Scripts/_rds/all_dat.rds")
