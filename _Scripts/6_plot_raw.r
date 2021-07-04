##########################
### Plot raw EEG data  ###
##########################


library(tidyverse)
options(clustermq.scheduler = "multicore")

scale_to_0range = function(x,range=1){
	x = x - min(x)
	x = x/max(x)
	x = x-.5
	x = x * range
	return(x)
}

(
	'_Scripts/_rds/all_dat.rds'
	#'_Scripts/_rds/tmp_mike.rds'
	%>% readRDS()
	%>% as_tibble()
	%>% dplyr::filter(
		!(group == 'imagery' & condition == 'physical') # for imagery participants, leave out the physical block
		, !(chan == 'M1')
		, !(chan == 'M2')
	)
	# %>% mutate(
	# 	time = time + (2 * (epoch - 1))
	# )
) -> dat

(
	dat
	%>% group_keys(chan,lat,long)
	%>% ungroup()
	%>% mutate(
		#polar to cartesian:
		x = lat*cos(long*(pi/180))
		, y = lat*sin(long*(pi/180))
		, x_scaled = scale_to_0range(x,8)
		, y_scaled = scale_to_0range(y,8.5)
	)
) -> chan_locs

#add handedness & flip lefties
(
	readRDS('_Scripts/_rds/bdat2.rds')
	%>% mutate(participant=as.character(participant))
	%>% group_by(
		participant
	)
	%>% summarize(
		handedness = handedness[1]
		, .groups = 'drop'
	)
	%>% right_join(
		dat	%>% mutate(participant=as.character(participant))
		, by = 'participant'
	)
	%>% mutate(

		#polar to cartesian:
		x = lat*cos(long*(pi/180))
		, y = lat*sin(long*(pi/180))

		#flip lefties
		, x = case_when(
			handedness=='l' ~ -x
			, T ~ x
		)
	)
) -> dat


#for each participant, compute mean across trials
(
	dat
	%>% group_by(group,participant,chan,x,y,time,freq)
	%>% summarize(
		powerdb = mean(powerdb)
		, .groups = 'drop'
	)
) -> dat

#for each group, compute mean across participants and append as 0th participant
(
	dat
	%>% group_by(group,chan,x,y,time,freq)
	%>% summarize(
		powerdb = mean(powerdb)
		, .groups = 'drop'
	)
	%>% mutate(participant='0')
	%>% dplyr::bind_rows(
		(
			dat
			%>% select(group,participant,chan,x,y,time,freq,powerdb)
		)
	)
	%>% mutate(participant=as.numeric(participant))
) -> dat



#determine freqs to NA for band boundaries
unique_freqs = unique(dat$freq)
band_boundaries = c(
	unique_freqs[which.min(abs(unique_freqs-3.5))]
	, unique_freqs[which.min(abs(unique_freqs-8.5))]
	, unique_freqs[which.min(abs(unique_freqs-12.5))]
	, unique_freqs[which.min(abs(unique_freqs-30.5))]
)
unique_times = unique(dat$time)
# zero_time = unique_times[which.min(abs(unique_times))]
time_boundaries = c(
	0, 1.5, 2
)

#add stuff
(
	dat
	%>% ungroup()
	%>% mutate(

		#NA powerdb on band boundaries
		powerdb = case_when(
			!(freq %in% band_boundaries) ~ powerdb
		)

		#NA powerdb at time==0
		, powerdb = case_when(
			# time!=zero_time ~ powerdb
			!(time %in% time_boundaries) ~ powerdb
		)

		#rescaling
		, x_scaled = scale_to_0range(x,8)
		, y_scaled = scale_to_0range(y,8.5)
		, time_scaled = scale_to_0range(time,1)
		, freq_scaled = scale_to_0range(freq,1)

		#sum to get final positions
		, to_plot_x = x_scaled + time_scaled
		, to_plot_y = y_scaled + freq_scaled

	)
) -> dat

get_raster_for_channel = function(this_channel_data,all_channels_powerdb_range){
	# (
	# 	dat
	# 	%>% filter(
	# 		participant==participant[1]
	# 		, chan==chan[1]
	# 		, trial==0
	# 	)
	# )->this_channel_data
	# all_channels_powerdb_range = range(this_channel_data$powerdb)
	(
		this_channel_data
		%>% ggplot()
		+ geom_raster(
			mapping = aes(
				x = to_plot_x
				, y = to_plot_y
				, fill = powerdb
			)
		)
		+ scale_fill_viridis_c(
			option='C'
			, limits = all_channels_powerdb_range
			, na.value = 'transparent'
		)
		+ theme(
			legend.position = "top"
			, legend.title = element_blank()
		)
	) -> temp
	tmp = ggplot_gtable(ggplot_build(temp))
	return(list(
		legend = tmp$grobs[[which(sapply(tmp$grobs, function(x) x$name) == "guide-box") ]]
		, topo_raster = layer_grob(temp)$`1`$raster
		, xmin = min(this_channel_data$to_plot_x)
		, xmax = max(this_channel_data$to_plot_x)
		, ymin = min(this_channel_data$to_plot_y)
		, ymax = max(this_channel_data$to_plot_y)
	))
}

plot_participant = function(this_participant_data,all_participants_powerdb_range){
	# (
	# 	dat
	# 	%>% filter(
	# 		participant==participant[1]
	# 		# , chan==chan[1]
	# 	)
	# ) -> this_participant_data

	#determine which range to use
	if(this_participant_data$participant[1]==0){
		this_participant_powerdb_range = range(this_participant_data$powerdb,na.rm=T)
	}else{
		this_participant_powerdb_range = all_participants_powerdb_range
	}
	#get the rasters per channel
	(
		this_participant_data
		%>% group_by(x,y)
		%>% group_split()
		%>% purrr::map(
			.f = get_raster_for_channel
			, all_channels_powerdb_range = this_participant_powerdb_range
		)
	) -> channel_rasters

	#construct the canvas with limits & theme stuff
	(
		ggplot()
		+ scale_x_continuous(limits = range(this_participant_data$to_plot_x))
		+ scale_y_continuous(limits = range(this_participant_data$to_plot_y))
		+ coord_equal()
		+ labs(
			title = this_participant_data$participant[1]
		)
		+ theme(
			axis.title = element_blank()
			, axis.ticks = element_blank()
			, axis.text = element_blank()
			, panel.grid = element_blank()
			, panel.background = element_rect(fill='transparent',colour='grey90')
		)
	) -> p

	#add each channel's raster
	for(chan in channel_rasters){
		(
			p
			+ annotation_raster(
				raster = chan$topo_raster
				, xmin = chan$xmin
				, xmax = chan$xmax
				, ymin = chan$ymin
				, ymax = chan$ymax
			)
		)->p
	}

	#add legend
	(
		p
		+ annotation_custom(
			chan$legend
			, xmin = max(this_participant_data$to_plot_x)-.2*diff(range(this_participant_data$to_plot_x))
			, xmax = max(this_participant_data$to_plot_x)
			, ymin = min(this_participant_data$to_plot_y)-.1*diff(range(this_participant_data$to_plot_y))
			, ymax = min(this_participant_data$to_plot_y)+.2*diff(range(this_participant_data$to_plot_y))
		)
	) -> p

	#add channel labels
	(
		p
		+ geom_text(
			data = chan_locs
			, mapping = aes(
				x = x_scaled*1.25 -.5
				, y = y_scaled*1.175 +.5
				, label = chan
			)
			, colour = 'grey70'
			, size = 2
			, hjust = 'right'
			, vjust = 'top'
		)
	) ->
		p


	#add panel legend

	#x-axis ticks are located at the locations where all are NA

	(
		this_participant_data
		%>% group_by(
			time
		)
		%>% filter(
			all(is.na(powerdb))
		)
		%>% pull(time_scaled)
		%>% unique()
		%>% sort()
	) -> x_tick_locs


	#y-axis labels are located BETWEEN the locations where all are NA
	(
		this_participant_data
		%>% group_by(
			freq
		)
		%>% filter(
			all(is.na(powerdb))
		)
		%>% pull(freq_scaled)
		%>% unique()
		%>% sort()
	) -> y_tick_locs
	y_tick_locs = c(-.5,y_tick_locs,.5)
	y_label_locs = (
		y_tick_locs[1:(length(y_tick_locs)-1)]
		+ diff(y_tick_locs)/2
	)


	y_axis_y_offset = -4.5
	y_axis_x_offset = -5
	y_axis_dat_ticks = tibble(
		y_scaled = y_tick_locs
		, to_plot_y = y_scaled + y_axis_y_offset
		, to_plot_x = rep(0,length(y_scaled)) + y_axis_x_offset
		, label = 1:length(y_scaled)
	)
	y_axis_dat_labels = tibble(
		label = c('delta','theta','alpha','beta','gamma')
		# label = c('δ','θ','α','β','γ')
		, y_scaled = y_label_locs
		, to_plot_y = y_scaled + y_axis_y_offset
		, to_plot_x = rep(0,length(y_scaled)) + y_axis_x_offset
	)

	x_axis_y_offset = y_axis_y_offset-.5
	x_axis_x_offset = y_axis_x_offset+.5

	x_axis_dat_small_ticks = tibble(
		label = c('0','0')
		, x_scaled = x_tick_locs[c(1,3)]
		, to_plot_x = x_scaled + x_axis_x_offset
		, to_plot_y = rep(0,length(label)) + x_axis_y_offset
	)
	x_axis_dat_big_ticks = tibble(
		x_scaled = c(-.5,x_tick_locs[2],.5)
		, to_plot_x = x_scaled + x_axis_x_offset
		, to_plot_y = rep(0,length(x_scaled)) + x_axis_y_offset
		, label = 1:length(x_scaled)
	)
	x_axis_dat_big_labels = tibble(
		label = c('During','After')
		, x_scaled = c(-.25,.25)
		, to_plot_x = x_scaled + x_axis_x_offset
		, to_plot_y = rep(0,length(label)) + x_axis_y_offset -.05
	)


	axis_title_dat = tibble(
		label = c('Band','Time')
		, x = c(y_axis_x_offset-.25,x_axis_x_offset)
		, y = c(y_axis_y_offset,x_axis_y_offset-.4)
		, angle = c(90,0)
		, hjust = c('center','center')
		, vjust = c('bottom','top')
	)

	(
		p
		# y-axis line
		+ geom_line(
			data = y_axis_dat_ticks
			, aes(
				x = to_plot_x
				, y = to_plot_y
			)
		)
		# y-axis ticks
		+ geom_line(
			data = (
				y_axis_dat_ticks
				%>% mutate(
					xmin = to_plot_x-.05
					, xmax = to_plot_x
				)
				%>% select(-to_plot_x)
				%>% pivot_longer(
					cols = c(xmin,xmax)
					, values_to = 'to_plot_x'
				)
			)
			, aes(
				x = to_plot_x
				, y = to_plot_y
				, group = label
			)
			, size = .25
		)
		# y-axis labels
		+ geom_text(
			data = y_axis_dat_labels
			, aes(
				x = to_plot_x-.1
				, y = to_plot_y
				, label = label
			)
			, hjust = 'right'
			, size = 1
			, parse = T
		)
		# x-axis line
		+ geom_line(
			data = x_axis_dat_big_ticks
			, aes(
				x = to_plot_x
				, y = to_plot_y
			)
		)
		# x-axis big ticks
		+ geom_line(
			data = (
				x_axis_dat_big_ticks
				%>% mutate(
					ymin = to_plot_y-.1
					, ymax = to_plot_y
				)
				%>% select(-to_plot_y)
				%>% pivot_longer(
					cols = c(ymin,ymax)
					, values_to = 'to_plot_y'
				)
			)
			, aes(
				x = to_plot_x
				, y = to_plot_y
				, group = interaction(label,x_scaled)
			)
			, size = .25
		)
		# x-axis small ticks
		+ geom_line(
			data = (
				x_axis_dat_small_ticks
				%>% mutate(
					ymin = to_plot_y-.05
					, ymax = to_plot_y
				)
				%>% select(-to_plot_y)
				%>% pivot_longer(
					cols = c(ymin,ymax)
					, values_to = 'to_plot_y'
				)
			)
			, aes(
				x = to_plot_x
				, y = to_plot_y
				, group = interaction(label,x_scaled)
			)
			, size = .25
		)
		#x-axis small labels
		+ geom_text(
			data = x_axis_dat_small_ticks
			, aes(
				x = to_plot_x
				, y = to_plot_y - .08
				, label = label
			)
			, vjust = 'top'
			, size = 1
			#, parse = TRUE
		)
		#extra x-axis big labels
		+ geom_text(
			data = x_axis_dat_big_labels
			, aes(
				x = to_plot_x
				, y = to_plot_y-.15
				, label = label
			)
			, vjust = 'top'
			, size = 2
			, parse = TRUE
		)
		# axis titles
		+ geom_text(
			data = axis_title_dat
			, aes(
				x = x
				, y = y
				, label = label
				, angle = angle
				, hjust = hjust
				, vjust = vjust
			)
			, size = 3
		)

		#example panel
		+ annotation_raster(
			raster = channel_rasters[[1]]$topo_raster
			, xmin = y_axis_x_offset
			, xmax = y_axis_x_offset+1
			, ymin = y_axis_y_offset-.5
			, ymax = y_axis_y_offset+.5
		)

	) -> p
	#plot
	print(p)
	return(NULL)
}

plot_group_participants = function(this_group_data,path){
	# (
	# 	dat
	# 	%>% filter(
	# 		group==group[1]
	# 		# , trial==0
	# 		# , chan==chan[1]
	# 	)
	# ) -> this_group_data

	#prepare the directory structure
	path_list = c('plots','raw_power',this_group_data$group[1])
	for(i in 1:length(path_list)){
		path = paste(path_list[1:i],collapse='/')
		if(!dir.exists(path)){
			dir.create(path)
		}
	}

	pdf(
		file = paste0(path,'/group.pdf')
		, height = 8
		, width = 8
	)
	(
		this_group_data
		%>% group_by(participant)
		%>% group_split()
		%>% purrr::map(
			.f = plot_participant
			, all_participants_powerdb_range = range(this_group_data$powerdb,na.rm=T)
		)
	)
	dev.off()
	return(NULL)
}


# (
# 	dat
# 	%>% group_by(group)
# 	%>% group_split()
# 	%>% purrr::map(
# 		.f = plot_group_participants
# 	)
# )

