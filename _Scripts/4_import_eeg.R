##############################
### DEMI EEG import script ###
##############################


### Import required packages & functions ###

library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(eeguana)

source("./_Scripts/_settings.R")
source("./_Scripts/_functions/eeg.R")
source("./_Scripts/_functions/emg.R")



### Import relevant task & pipeline data for EEG import ###

# Import relevant task data

bdat <- readRDS("./_Scripts/_rds/bdat2.rds")
tracings <- readRDS("./_Scripts/_rds/tracings.rds")
trace_filter_info <- readRDS("./_Scripts/_rds/trace_filter_info.rds")


# Import PREP pipeline info

info_path <- "./_Data/eeg/prep_info.csv"
prep_info <- read_csv(info_path, col_types = cols(id = col_integer()))



### Import preprocessed EEG data ###

# Get list of all processed EDF files

eeg_files <- list.files(
  "./_Data/eeg/edfs", pattern = "*.edf",
  full.names = TRUE
)


# Get names of interpolated & excessively noisy channels from PREP pipeline info
# Note: If data is CSD-processed, bad channels have already been dropped

bad_chans_by_id <- prep_info %>%
  select(c(id, remaining_bad)) %>%
  separate_rows(remaining_bad, sep = " ") %>%
  filter(!is.na(remaining_bad)) %>%
  rename(bad_ch = remaining_bad)

if (using_csd) {
  bad_chans_by_id <- bad_chans_by_id %>%
    filter(is.na(id))
}

if (drop_interpolated) {
  interpolated_chans_by_id <- prep_info %>%
    select(c(id, interpolated)) %>%
    separate_rows(interpolated, sep = " ") %>%
    filter(!is.na(interpolated)) %>%
    rename(bad_ch = interpolated)

  bad_chans_by_id <- bad_chans_by_id %>%
    bind_rows(interpolated_chans_by_id) %>%
    arrange(id)
}


# Calculate the actual tracing start and end times for each physical trial,
# relative to the trial's 'trace_start' trigger

tracing_times_filtered <- bdat %>%
  mutate(
    id = as.numeric(as.character(participant)),
    physical = condition == "physical",
    mt = ifelse(physical, mt_clip, mt)
  ) %>%
  rename(session = session_num, block = block_num, trial = trial_num) %>%
  select(c(id, session, block, trial, physical, it, mt, stimulus_mt)) %>%
  mutate(trial_count = trial + (block - 1) * 20)

tracing_times_raw <- tracings %>%
  group_by(id, session, block, trial) %>%
  summarize(trace_onset = time[1], trace_end = max(time))

epoch_offsets <- tracing_times_filtered %>%
  left_join(tracing_times_raw) %>%
  left_join(trace_filter_info$false_start) %>%
  mutate(
    start_shift = ifelse(is.na(start_shift), 0, start_shift),
    real_start = ifelse(physical, it + trace_onset + start_shift, 0),
    real_end = ifelse(physical, real_start + mt, mt),
    end_trigger = ifelse(physical, it + trace_end, mt)
  ) %>%
  mutate(
    real_start = ifelse(is.na(mt), NA, real_start)
  ) %>%
  select(-c(samples, flagged, session, block, trial))


# Get the subject IDs for all imagery participants

imagery_ids <- bdat %>%
  group_by(participant) %>%
  summarize(group = group[1]) %>%
  filter(group != "physical") %>%
  pull(participant)


# Ignore EDF files that have no corresponding ID in the behavioural data

eeg_ids <- as.numeric(gsub("\\D", "", basename(eeg_files)))
task_ids <- unique(epoch_offsets$id)
eeg_files <- eeg_files[eeg_ids %in% task_ids]


# Import and epoch EEG data (or load cached Rdata if it exists)

cached_eeg_path <- "./_Scripts/_rds/eeg_imported.Rds"

if (file.exists(cached_eeg_path)) {

  eeg_data <- readRDS(cached_eeg_path)

} else {

  eeg_data <- lapply(eeg_files, function(f) {

    # Actually read in EDF data
    cat(paste0("\nImporting and epoching ", basename(f), "...\n\n"))
    eeg <- read_edf(f)
    id_num <- as.numeric(gsub("\\D", "", basename(f)))

    # Update EEG triggers using behavioural data, dropping practice trials and
    # adding new triggers for actual tracing starts/ends
    offsets_for_id <- subset(epoch_offsets, id == id_num)
    new_events <- update_events(eeg$.events, offsets_for_id)
    eeg$.events <- new_events

    # Separate EEG and EMG signals from full set of channels
    emg <- eeg %>% select(EMG.L, EMG.A)
    eeg <- eeg %>% select(-HEO, -VEO, -EMG.L, -EMG.A)

    # Drop any channels flagged as bad by the PREP pipeline
    bad_chans <- subset(bad_chans_by_id, id == id_num)$bad_ch
    eeg <- eeg %>% select(-all_of(bad_chans))

    # Extract baseline, tracing, and post-tracing epochs from EEG
    eeg_baseline <- eeg %>%
      eeg_segment(
        # one second is trimmed off each end, and the rest is averaged for the baseline
        .description == "red_on", .lim = c(-1500, 800), .unit = "ms"
      )
    eeg_tracing <- eeg %>%
      eeg_segment(
        # note that we want a bit before time=0 to plot this properly in 6_plot_raw.r
        .description == "real_trace_start", .lim = c(-1501, 2500), .unit = "ms"
      )
    eeg_post_trace <- eeg %>%
      eeg_segment(
        # note that we want a bit before time=0 to plot this properly in 6_plot_raw.r
        .description == "real_trace_end", .lim = c(-1501, 2500), .unit = "ms"
      )

    # Join trial numbers to epochs
    trial_key <- data.table(select(eeg_baseline$.segments, c(.id, trial)))
    eeg_baseline$.signal[trial_key, on = ".id", trial := trial]
    eeg_tracing$.signal[trial_key, on = ".id", trial := trial]
    eeg_post_trace$.signal[trial_key, on = ".id", trial := trial]

    # If participant is in the imagery condition, filter and epoch their EMG data
    if (id_num %in% imagery_ids) {
      epoched_emg <- emg %>%
        mutate(
          EMG.L = channel_dbl(emg_bandpass(EMG.L, 20, 450, srate = 1000)),
          EMG.A = channel_dbl(emg_bandpass(EMG.A, 20, 450, srate = 1000))
        ) %>%
        eeg_segment(
          .description == "stim_on", .end = .description == "real_trace_end"
        )
      epoched_emg$.signal[trial_key, on = ".id", trial := trial]
    } else {
      epoched_emg <- NA
    }

    # Downsample EEG to 100 Hz and merge data frames
    eeg_list <- list(
      baseline = eeg_baseline$.signal[.sample %% 10 == 0],
      tracing = eeg_tracing$.signal[.sample %% 10 == 0],
      post_trace = eeg_post_trace$.signal[.sample %% 10 == 0]
    )
    eeg_merged <- data.table::rbindlist(eeg_list, idcol = "epoch")

    # Clean up EEG data table
    setnames(eeg_merged, ".sample", "time")  # rename '.sample' to 'time'
    eeg_merged[, .id := NULL]  # drop '.id' column
    eeg_merged[, trial := as.integer(trial)]  # make trial int to save space
    setcolorder(eeg_merged, c("trial", "epoch", "time"))  # reorder columns

    list(eeg = eeg_merged, emg = epoched_emg)
  })

  # Cache epoched EEG and EMG data for next time
  names(eeg_data) <- gsub("_eeg_prepped.edf", "", basename(eeg_files))
  saveRDS(eeg_data, file = cached_eeg_path)
}
