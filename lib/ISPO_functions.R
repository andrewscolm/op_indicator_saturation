fast_timeframe = 3
slow_timeframe = 9

library(viridis)
library(RColorBrewer)
library(forcats)
library(cowplot)

trace_inset_layout <-
    "AAA#
BBBB
BBBB"

trace_inset_layout_multiple <-
    "AAA#
BBBB
BBBB
BBBB
BBBB"

generate_random_data = function(label, date, these_values) {
    indicator_def = defData(
        varname = "value",
        dist = "normal",
        formula = these_values["mean"],
        variance = these_values["sd"]
    )

    tmp_dat = tibble(genData(these_values["n_total"], indicator_def)) %>%
        mutate(
            indicator = label,
            date = date
        ) %>%
        ### ADD MISSING VALUES
        mutate(value = replace(
            value,
            sample(row_number(), size = these_values["n_missing"], replace = FALSE),
            NA
        ))

    tmp_dat = tmp_dat %>%
        mutate(value = ifelse(value <  0, runif(1,0     ,0.05), value)) %>%
        mutate(value = ifelse(value >= 1, runif(1,1-0.05,1   ), value))

    return( tmp_dat )
}


generate_new_values_using_profile = function(base, profile, this_indicator, reference) {
    new_data = tibble()

    all_dates = profile %>%
        filter(!is.na(change_magnitude)) %>%
        pull(date) %>%
        unique()

    for (this_date in all_dates) {
        this_change_magnitude <- profile %>%
            filter(date == this_date) %>%
            pull(change_magnitude)

        this_change_type <- profile %>%
            filter(date == this_date) %>%
            pull(change_type)

        these_values <- reference %>%
            filter(date == this_date) %>%
            filter(indicator == this_indicator) %>%
            select(-indicator, -date) %>%
            unlist()

        cat("----------------------------\n")
        cat(glue("Date: {this_date}\n\n"))
        cat(glue("Starting mean: {these_values['mean']}\n\n"))
        cat(glue("Change magnitude: {this_change_magnitude}\n\n"))
        cat(glue("Change type: {this_change_type}\n\n"))

        old_mean <- these_values["mean"]

        if (!is.na(old_mean)) {
            # if (this_change_type == "sd") {
            #     these_values["mean"] <- these_values["mean"] + (this_change_magnitude * these_values["sd"])
            #     cat(sprintf("Updating the mean by a factor of %f * SD\n", this_change_magnitude))
            #     cat(sprintf("> from %.2f to %.2f\n", old_mean, these_values["mean"]))
            # } else if (this_change_type == "perc") {
            #     these_values["mean"] <- these_values["mean"] + this_change_magnitude
            #     cat(sprintf("Updating the mean by an absolute value of %.2f\n", this_change_magnitude))
            #     cat(sprintf("> from %.2f to %.2f\n", old_mean, these_values["mean"]))
            # }

            tmp_data = base %>%
                filter(date == this_date) %>%
                mutate(value_updated = value + this_change_magnitude) %>%
                select( -value )
            
            # tmp_data <- generate_random_data(this_indicator, this_date, these_values) %>%
            #     rename(value_updated = value)

            new_data <- new_data %>% bind_rows(tmp_data)
        }
    }

    out <- base %>%
        full_join(new_data, by = c("id", "indicator", "date")) %>%
        mutate(
            updated_flag = ifelse(is.na(value_updated), FALSE, TRUE),
            value_updated = ifelse(updated_flag, value_updated, value)
        )

    return(out)
}

get_spike_profiles = function( spike_specification, month_range = c(ymd("2019-09-01",ymd("2021-09-01")))){
    
    spike_profiles = tibble()

    for ( i in 1:nrow( spike_specification )) {

        this_load = ( spike_specification[i,] %>% pull( size )) [[1]]
        this_date = ( spike_specification[i,] %>% pull( date )) [[1]]

        this_profile = add_spike( load = this_load, date = this_date )
            
        ### This is required for adjacent spikes - the values are summed
        this_profile_consolidated = this_profile %>%
            group_by( date ) %>%
            mutate( change_magnitude = sum(change_magnitude)) %>% 
            unique()

        spike_profiles = spike_profiles %>% 
            bind_rows( this_profile_consolidated )

    }

    spike_profiles = spike_profiles %>%
            complete(
                date = seq(month_range[1], month_range[2], by = "1 month"),
                profile_type,
                fill = list(
                    change_magnitude = 0,
                    change_type = "perc"
                )
            ) %>%
            group_by( profile_type, date ) %>%
            mutate( date = as.character(date) )

    return( spike_profiles ) 

}

add_spike = function( load = 0.25,
    date = ymd( "2020-04-01" ) ) {

        profile_name = sprintf("%s=%.2f",format(date,"%y%m"),load) %>% paste( collapse="+" )

        spike = tibble(
            profile_type = glue("SPIKE_{profile_name}"),
            date = c(date,date+months(1)),
            change_magnitude = c(load,-1*load),
            change_type = "perc"
    )

}

get_example_COVID_profiles = function( start_date = ymd("2020-04-01"), month_range=c(ymd("2019-09-01",ymd("2021-09-01"))) ) {

    ###################################################################
    ### COVID profiles                                              ###
    ### - fast (3mo) increase post-COVID, fast (3mo) recovery       ###
    ### - fast (3mo) increase post-COVID, slow (9mo) recovery       ###
    ### - slow (9mo) increase post-COVID, fast (3mo) recovery       ###
    ### - fast (9mo) increase post-COVID, slow (9mo) recovery       ###
    ### - fast (3mo) increase post-COVID, no recovery               ###
    ### - slow (9mo) increase post-COVID, no recovery               ###
    ###################################################################

    total_increase = 0.5

    ### - fast (3mo) increase post-COVID, fast (3mo) recovery       ###
    PC_value       = total_increase / fast_timeframe
    recovery_value = total_increase / fast_timeframe

    fast_PC_fast_recovery = tibble(
        profile_type = "fast_PC_fast_recovery",
        date = c(
            seq(start_date, start_date + months(fast_timeframe - 1), by = "1 month"),
            seq(start_date + months(fast_timeframe), start_date + months(fast_timeframe) + months(fast_timeframe - 1), by = "1 month")
        ),
        change_magnitude = c(
            rep.int(PC_value, fast_timeframe),
            rep.int(-1 * recovery_value, fast_timeframe)
        ),
        change_type = "perc",
        break_group = c(
            rep.int("PC", fast_timeframe),
            rep.int("recovery", fast_timeframe)
        )
    )



    ### - fast (3mo) increase post-COVID, slow (9mo) recovery       ###
    PC_value = total_increase / fast_timeframe
    recovery_value = total_increase / slow_timeframe

    fast_PC_slow_recovery = tibble(
        profile_type = "fast_PC_slow_recovery",
        date = c(
            seq(start_date, start_date + months(fast_timeframe - 1), by = "1 month"),
            seq(start_date + months(fast_timeframe), start_date + months(fast_timeframe) + months(slow_timeframe - 1), by = "1 month")
        ),
        change_magnitude = c(
            rep.int(PC_value, fast_timeframe),
            rep.int(-1*recovery_value, slow_timeframe)
        ),
        change_type = "perc",
        break_group = c(
            rep.int("PC", fast_timeframe),
            rep.int("recovery", slow_timeframe)
        )
    )


    ### - slow (9mo) increase post-COVID, fast (3mo) recovery       ###
    PC_value = total_increase / slow_timeframe
    recovery_value = total_increase / fast_timeframe

    slow_PC_fast_recovery = tibble(
        profile_type = "slow_PC_fast_recovery",
        date = c(
            seq(start_date, start_date + months(slow_timeframe - 1), by = "1 month"),
            seq(start_date + months(slow_timeframe), start_date + months(slow_timeframe) + months(fast_timeframe - 1), by = "1 month")
        ),
        change_magnitude = c(
            rep.int(PC_value, slow_timeframe),
            rep.int(-1 * recovery_value, fast_timeframe)
        ),
        change_type = "perc",
        break_group = c(
            rep.int("PC", slow_timeframe),
            rep.int("recovery", fast_timeframe)
        )
    )

    ### - slow (9mo) increase post-COVID, slow (9mo) recovery       ###
    PC_value = total_increase / slow_timeframe
    recovery_value = total_increase / slow_timeframe

    slow_PC_slow_recovery = tibble(
        profile_type = "slow_PC_slow_recovery",
        date = c(
            seq(start_date, start_date + months(slow_timeframe - 1), by = "1 month"),
            seq(start_date + months(slow_timeframe), start_date + months(slow_timeframe) + months(slow_timeframe - 1), by = "1 month")
        ),
        change_magnitude = c(rep.int(PC_value, slow_timeframe),
        rep.int(-1*recovery_value, slow_timeframe)),
        change_type = "perc",
        break_group = c(
            rep.int("PC", slow_timeframe),
            rep.int("recovery", slow_timeframe)
        )
    )

    ### - fast (3mo) increase post-COVID, no recovery               ###
    PC_value = total_increase / fast_timeframe

    fast_PC_NO_recovery = tibble(
        profile_type = "fast_PC_NO_recovery",
        date = c(
            seq(start_date, start_date + months(fast_timeframe - 1), by = "1 month")
        ),
        change_magnitude = c(
            rep.int(PC_value, fast_timeframe)
        ),
        change_type = "perc",
        break_group = c(
            rep.int("PC", fast_timeframe)
        )
    )

    ### - slow (9mo) increase post-COVID, no recovery               ###
    PC_value = total_increase / slow_timeframe

    slow_PC_NO_recovery = tibble(
        profile_type = "slow_PC_NO_recovery",
        date = c(
            seq(start_date, start_date + months(slow_timeframe - 1), by = "1 month")
        ),
        change_magnitude = c(
            rep.int(PC_value, slow_timeframe)
        ),
        change_type = "perc",
        break_group = c(
            rep.int("PC", slow_timeframe)
        )
    )

    profiles_grouped = fast_PC_fast_recovery %>%
        bind_rows(fast_PC_slow_recovery) %>%
        bind_rows(slow_PC_fast_recovery) %>%
        bind_rows(slow_PC_slow_recovery) %>%
        bind_rows(fast_PC_NO_recovery) %>%
        bind_rows(slow_PC_NO_recovery)

    profiles_breaks = profiles_grouped %>%
        group_by(profile_type, break_group) %>%
        summarise(
            start = min(date),
            end = max(date),
            magnitude = sum(change_magnitude)
        )

    profiles = profiles_grouped %>% 
        select(-break_group) %>% 
        complete(
            date = seq(month_range[1], month_range[2], by = "1 month"),
            profile_type,
            fill = list(
                # profile_type = "fast_PC_fast_recovery",
                change_magnitude = 0,
                change_type = "perc"
            )
        ) %>%
        mutate( date = as.character(date) ) %>% 
        mutate( profile_type = glue( "COVID_{profile_type}" ))

    return(list(profiles = profiles, profiles_breaks = profiles_breaks))

}

convert_profile_name_to_plot_titles = function(plot_string) {
    plot_title_string = ""
    plot_subtitle_string = ""

    if ( plot_string %>% str_detect("^SPIKE") ) {
        spike_list = plot_string %>% str_remove( "SPIKE_") %>% str_split("\\+")  %>% unlist()
        plot_subtitle_list = c()

        for ( s in spike_list ) {
            split_list = s %>% str_split("=") %>% unlist
            this_date = format(as.Date(glue("{split_list[1]}01"),"%y%m%d"),"%b %y")
            this_size = split_list[2]

            plot_subtitle_list = c( plot_subtitle_list,
            sprintf( "%s (%s)", this_date, this_size ) )
        }

        plot_title_string = glue("Spiked ({length(plot_subtitle_list)})")
        plot_subtitle_string = paste(plot_subtitle_list,collapse=" | ")

    } else if ( plot_string %>% str_detect("^COVID") ) {
        postcovid_string = plot_string %>% str_extract("([^_]*)_PC") %>% str_remove("_PC$")
        postcovid_months = get( glue("{postcovid_string}_timeframe") )
        postcovid_months = glue("{postcovid_months} mo")
        recovery_string = plot_string %>% str_extract("([^_]*)_recovery") %>% str_remove("_recovery$")
        recovery_months = "no recovery"
        if ( exists(glue("{recovery_string}_timeframe")) ) {
            recovery_months = get( glue("{recovery_string}_timeframe") )
            recovery_months = glue("{recovery_months} mo")
        }

        plot_title_string = glue("COVID profile ({toupper(postcovid_string)}/{toupper(recovery_string)})")
        plot_subtitle_string = c(
            glue("{toupper(postcovid_string)} change ({postcovid_months}) post-covid" ),
            glue("{toupper(recovery_string)} ({postcovid_months}) recovery" ) )  %>% 
            paste( collapse="\n")
    }

    return( list(   title=plot_title_string,
                    subtitle=plot_subtitle_string ) )
}

convert_yymm_to_yyyymmdd = function( yymm) {
    yyyy = as.numeric( substr( yymm, 1, 2 ) ) + 2000
    mm = sprintf( "%02d", as.numeric( substr( yymm, 3, 4 ) ) )
    this_date = glue("{yyyy}-{mm}-01")
}

build_spike_mapper = function(spike_strings) {
    spike_mapper = tibble()

    for( this_spike_string in spike_strings ) {
        yymm = find_spike_locations( this_spike_string )
        yyyy = as.numeric( substr( yymm, 1, 2 ) ) + 2000
        mm = sprintf( "%02d", as.numeric( substr( yymm, 3, 4 ) ) )
        this_date = glue("{yyyy}-{mm}-01")
        
        coef = find_spike_sizes( this_spike_string )

        tmp_spike = tibble(
            n=1:length(coef),
            location = this_date,
            size = coef,
            dataset = this_spike_string
        )

        spike_mapper = spike_mapper %>% 
            bind_rows( tmp_spike )
    }

    return( spike_mapper )
}


find_spike_locations = function( s ) {
    spike_info = s %>%
        str_remove( "^SPIKE_") %>% 
        str_split( "\\+")  %>% 
        unlist %>%
        str_split("=") %>% unlist

    location_mask = seq(1,length(spike_info),by=2)
    
    return( spike_info[location_mask] )
}

find_spike_sizes = function( s ) {
    spike_info = s %>%
        str_remove( "^SPIKE_") %>% 
        str_split( "\\+")  %>% 
        unlist %>%
        str_split("=") %>% unlist

    location_mask = seq(1,length(spike_info),by=2)
    size_mask = location_mask + 1
    
    return( spike_info[size_mask] )
}

expand_list_from_character = function(c) {
    l = 0

    if ( !is.na(l) ) {
        l = list(eval(parse(text=c)))
        if ( length(l) == 0 ){
            l=0
        }
    }
    return(l)
}

assess_SPIKE_performance = function( truth, model, 
                                    file_pre_stump = "", 
                                    file_post_stump = "_break-predictions",
                                    out_dir = "fig/evaluation" ) {

    if ( !dir.exists(out_dir) ) {
        cat( glue("creating output directory: {out_dir}\n\n") )
        dir.create(out_dir)
    }

    n_practices = model %>% pull( name ) %>% unique %>% length

    merged = truth %>% left_join(
        model,
        by="dataset",
        suffix = c(".truth",".model") )  %>% 
        mutate( location.model = ymd( location.model ))  %>% 
        mutate( location.truth = ymd( location.truth ))

    performance_numbers = merged %>% 
        select( dataset, name, p_threshold, r_threshold, location.truth, location.model, name )  %>% 
        mutate( break_offset = interval(location.truth,location.model) %/% months(1) )  %>% 
        group_by( dataset, name, p_threshold, r_threshold, location.truth )  %>% 
        summarise(  breaks_identified_boolean = ifelse( all(is.na(location)), FALSE, TRUE ),
                    breaks_identified_count = ifelse( breaks_identified_boolean, length( unique( location.model ) ), 0 ),
                    break_identified_correctly = ifelse( breaks_identified_boolean, any( break_offset == 0 ), NA ),
                    closest_break = ifelse( break_identified_correctly, 0, min( break_offset, na.rm=TRUE ) ) )

    performance_numbers_breaks_located = performance_numbers %>% 
        filter( breaks_identified_boolean )  %>% select(-breaks_identified_boolean)
    
    calculated_numbers = performance_numbers_breaks_located %>% 
        summarise( num_identified_correctly = sum(break_identified_correctly,na.rm=TRUE),
                    num_true_breaks = length(unique(location.truth) ) ) %>%
        ungroup() %>%
        rowwise() %>%
        mutate( spike_profile = convert_profile_name_to_plot_titles( dataset )$subtitle )  %>% 
        mutate( identified_breaks_perc = num_identified_correctly/num_true_breaks * 100 )

    return( list( raw=merged,
                    summary = performance_numbers,
                    final = calculated_numbers ) )
}


plot_SPIKE_performance = function( raw_numbers,
                                    summary_numbers,
                                    final_numbers,
                                    n_practices,
                                    file_pre_stump = "", 
                                    file_post_stump = "_break-predictions",
                                    out_dir = "fig/evaluation" ) {

    SPIKE_OUTPUTS = list()

    #################################################################
    #################################################################
    ### GENERATING SOME SUMMARY TABLES/FIGURES                    ###
    #################################################################
    #################################################################

    ### Table 1 - for each combination of dataset/p_threshold/r_threshold,
    ###           how many analyses found at least one break?
    t1 = summary_numbers %>% group_by( dataset, p_threshold, r_threshold )  %>% 
        summarise( analyses_total = n(),
                    analyses_breaks = sum( breaks_identified_boolean ) )  %>%
        ungroup() %>%
        mutate( analyses_breaks_perc = analyses_breaks/analyses_total * 100 )
    write_csv(t1, glue("{out_dir}/{file_pre_stump}Table1_CSV{file_post_stump}.csv" ) )

    ### Figure 1 - Percentages from Table 1, shown as a facetted geom_tile

    # cat( sprintf("Starting t1...\n") )

    t1_extra = t1 %>%
        # mutate(
            # p_threshold_cat = as.character(p_threshold),
            # r_threshold_cat = as.character(r_threshold)
        # ) %>%
        mutate(success_label = case_when(
            analyses_breaks_perc == 100 ~ "ALL",
            analyses_breaks_perc > 0 ~ "SOME",
            TRUE ~ "NONE"
        )) %>% mutate( spike_count = str_count(dataset,"\\+") + 1) %>% 
        mutate( p_threshold = sprintf("%.5f", p_threshold) )  %>% 
        mutate( r_threshold = sprintf("%.2f", r_threshold) )  %>% 
        ungroup() %>%
        rowwise() %>%
        mutate(spike_locations = list(find_spike_locations(dataset))) %>%
        mutate(num_spikes = spike_locations %>% length()) %>%
        mutate(spike_size = list(find_spike_sizes(dataset))) %>%
        mutate(facet_location = ifelse(length(spike_locations) > 1, NA, spike_locations)) %>%
        mutate(facet_size = ifelse(length(spike_size) > 1, NA, spike_size)) %>%
        mutate(title = convert_profile_name_to_plot_titles(dataset)$subtitle) %>%
        mutate(full_date = convert_yymm_to_yyyymmdd(facet_location))
    
    # t12 %>% filter( spike_count == 1 )  %>% 
    #     ggplot( aes(x=p_threshold_cat,
    #                 y=r_threshold_cat,
    #                 fill=success_label
    #                 )) +
    #     geom_tile(colour = "white") +
    #         scale_fill_manual( values = c(ALL = "black", SOME = "grey", NONE = "white")) +
    #         facet_wrap(~dataset) +
    #         theme_bw() +
    #         # geom_label( fill="white", size=3 ) +
    #         theme(legend.position = "bottom" ) +
    #         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    #         labs(title = glue("% practices where 1+ break is found (1 SPIKE, n={n_practices})"))
        
    # t1_filename = glue("{out_dir}/{file_pre_stump}Table1_PNG{file_post_stump}.png" )
    # ggsave(t1_filename, height=11, width = 8)
    # # cat( sprintf("Finished t1.\n") )

    # SPIKE_OUTPUTS$T1 = t1_filename

    # cat( sprintf("Starting t2a...\n") )

    ### Table 2a - for each combination of dataset/p_threshold/r_threshold,
    ###           how many analyses found the true break?

    t2a = final_numbers
    # write_csv(t2a, glue("{out_dir}/{file_pre_stump}Table2a_CSV{file_post_stump}.csv" ) )

    # t2a %>%
    #     mutate( row_label = glue("p={p_threshold}/r={r_threshold}"))  %>%  
    #     ggplot( aes( x = dataset, y=identified_breaks_perc ) ) +
    #     geom_boxplot() +
    #     theme_bw() +
    #     facet_grid(row_label~.) +
    #     theme( axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5) )
    # t2a_filename = glue("{out_dir}/{file_pre_stump}Table2a_PNG{file_post_stump}.png" )
    # ggsave(t2a_filename, height=11, width=8 )

    # cat( sprintf("Finished t2a.\n") )

    # SPIKE_OUTPUTS$T2a = t2a_filename
    
    cat( sprintf("Starting preprocessing for the t2bs...\n") )

    t2b = t2a %>%
        ungroup() %>% 
        group_by( p_threshold, r_threshold, dataset ) %>%
        summarise( average_performance = mean( identified_breaks_perc ) ) %>% 
        mutate( p_threshold = sprintf("%.5f", p_threshold) )  %>% 
        mutate( r_threshold = sprintf("%.2f", r_threshold) )  %>% 
        ungroup() %>%
        rowwise() %>%
        mutate( spike_locations = list(find_spike_locations(dataset)) ) %>% 
        mutate( num_spikes = spike_locations %>% length() )  %>% 
        mutate( spike_size = list(find_spike_sizes(dataset)) ) %>% 
        mutate( facet_location = ifelse( length(spike_locations) >1, NA, spike_locations ) ) %>% 
        mutate( facet_size = ifelse( length(spike_size) >1, NA, spike_size ) ) %>% 
        mutate( title = convert_profile_name_to_plot_titles(dataset)$subtitle )  %>% 
        mutate( full_date = convert_yymm_to_yyyymmdd(facet_location) )

    cat( sprintf("Finishing preprocessing for the t2bs.\n") )

    for ( this_group in t2b %>% pull( num_spikes ) %>% unique ) {

        t = t1_extra %>% filter( num_spikes == this_group )  %>% 
            ggplot( aes(x=p_threshold,
                        y=r_threshold,
                        fill=success_label
                        )) +
            geom_tile(colour = "white") +
                scale_fill_manual(values = c(ALL = "black", SOME = "grey", NONE = "white")) +
                facet_wrap(~dataset) +
                theme_bw() +
                coord_equal() +
                # geom_label( fill="white", size=3 ) +
                theme(legend.position = "bottom") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
                labs(title = glue("% practices where 1+ break is found ({this_group}xSPIKE, n={n_practices})"))

        if (this_group == 1) {
            t = t + facet_grid(facet_size ~ full_date)
        } else {
            t = t + facet_wrap(~dataset)
        }

        t1_filename = glue("{out_dir}/{file_pre_stump}Table1_SPIKES={this_group}_PNG{file_post_stump}.png")
        ggsave(t1_filename, height=6, width = 11)
        # cat( sprintf("Finished t1.\n") )

        SPIKE_OUTPUTS[glue("T1_{this_group}")] <- t1_filename

        cat( sprintf("Starting t/%s...\n", this_group) )

        t = t2b %>% 
            filter( num_spikes == this_group )  %>% 
            ggplot( aes( x=p_threshold, y=r_threshold, fill=average_performance)) +
            geom_tile( colour = "white", size = 1) +
            theme_bw() +
            scale_fill_viridis( ) +
            coord_equal() +
            xlab( "t.pval" ) +
            ylab( "ratio_threshold" ) +
            theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5) )

        if ( this_group == 1 ) {
            t = t + facet_grid( facet_size~full_date )
        } else {
            t = t + facet_wrap( ~dataset )
        }
        t2b_filename = glue("{out_dir}/{file_pre_stump}Table2b_SPIKES={this_group}_PNG{file_post_stump}.png" )
        ggsave(t2b_filename, height = 6, width = 11)

        cat( sprintf("Finished t/%s.\n", this_group) )

        cat( sprintf("Starting t/%s with labels...\n", this_group) )

        SPIKE_OUTPUTS[glue("T2b_{this_group}")] = t2b_filename

        t + geom_label( aes(label=round(average_performance) ), fill="white", size=3 )

        t2b_filename = glue("{out_dir}/{file_pre_stump}Table2b_SPIKES={this_group}+mean_PNG{file_post_stump}.png" )
        ggsave(t2b_filename, height = 6*1.5, width = 11*1.5)

        SPIKE_OUTPUTS[glue("T2b_labelled_{this_group}")] = t2b_filename

        cat( sprintf("Finished t/%s with labels.\n", this_group) )

    }

    for ( this_modification in raw_numbers %>% pull( dataset ) %>% unique ) {
        
        this_raw_numbers = raw_numbers %>%
        filter( dataset == this_modification )  %>% 
        filter( !is.na(location.model) )  %>% 
        mutate( change_direction = ifelse( size.truth > 0, "increase", "decrease"))

        #############################################################
        ### Where has the algorithm found breaks?
        #############################################################

        plot_titles = convert_profile_name_to_plot_titles( this_modification )
        max_y = this_raw_numbers$size.model %>% max()
        min_y = this_raw_numbers$size.model %>% min()

        this_plot_height = 2 * ( this_raw_numbers$p_threshold  %>% unique() %>% length() )
        this_plot_width = 2 * ( this_raw_numbers$r_threshold  %>% unique() %>% length() )

        ggplot( this_raw_numbers,
                aes(x=location.model,y=size.model,group=location.model) ) +
            scale_x_date( date_labels = "%b %y", breaks = xbreaks, limits=range(xbreaks) )  +
            annotate("segment",
                x = this_raw_numbers %>% filter( change_direction == "increase" ) %>% pull(location.truth)  %>% unique(),
                xend = this_raw_numbers %>% filter( change_direction == "increase" ) %>% pull(location.truth) %>% unique(),
                y = min_y, yend = max_y,
                colour = increase_colour,
                alpha = 0.5,
                size = 1,
                arrow = arrow(length=unit(0.30,"cm"),type="closed") 
            ) +
            annotate("segment",
                x = this_raw_numbers %>% filter( change_direction == "decrease" ) %>% pull(location.truth) %>% unique(),
                xend = this_raw_numbers%>% filter( change_direction == "decrease" ) %>% pull(location.truth) %>% unique(),
                y = max_y, yend = min_y,
                colour = decrease_colour,
                alpha = 0.5,
                size = 1,
                arrow = arrow(length=unit(0.30,"cm"),type="closed") 
            ) +
            geom_boxplot( outlier.size=1 ) +
            theme_bw() +
            theme(axis.text.x = element_text(angle=90, hjust = 1, vjust=0.5),
            axis.title = element_text( face="bold")) +
            labs(
                title=plot_titles$title,
                subtitle=plot_titles$subtitle ) +
            xlab( "Month" ) +
            ylab( "Model size prediction" ) +
            facet_grid( p_threshold ~ r_threshold )

        fig_out = glue("{out_dir}/{file_pre_stump}{this_modification}{file_post_stump}.png" )
        ggsave( fig_out, width=this_plot_width, height=this_plot_height )

        SPIKE_OUTPUTS[glue("T3_{this_modification}")] = fig_out

    }

    return( SPIKE_OUTPUTS )
}

generate_COVID_profile_name <- function(s) {
    PC_speed_0 <- s %>% str_remove("_PC_.*")
    PC_speed_0 <- ifelse(PC_speed_0 == "NO", 0, PC_speed_0)
    PC_speed_1 <- ifelse(PC_speed_0 == 0, 0, get(paste(PC_speed_0, "timeframe", sep = "_")))
    PC_speed_1 <- get(paste(PC_speed_0, "timeframe", sep = "_"))
    PC_speed_0 <- ifelse(PC_speed_0 == 0, "NONE", PC_speed_0)
    PC_speed_2 <- glue("Post-COVID: {PC_speed_0} ({PC_speed_1} months)")

    recovery_speed_0 <- s %>%
        str_remove(".*_PC_") %>%
        str_remove("_recovery")
    recovery_speed_0 <- ifelse(recovery_speed_0 == "NO", 0, recovery_speed_0)
    recovery_speed_1 <- ifelse(recovery_speed_0 == 0, 0, get(paste(recovery_speed_0, "timeframe", sep = "_")))
    recovery_speed_0 <- ifelse(recovery_speed_0 == 0, "NONE", recovery_speed_0)
    recovery_speed_2 <- glue("Recovery: {recovery_speed_0} ({recovery_speed_1} months)")

    return(paste(PC_speed_2, recovery_speed_2, sep = " / "))
}

generate_COVID_profile_description <- function(s, start_date = ymd("2020-04-01")) {
    PC_date_text = format(start_date, "%b %y")

    PC_speed_0 <- s %>% str_remove("_PC_.*")
    PC_speed_0 <- ifelse(PC_speed_0 == "NO", 0, PC_speed_0)
    PC_speed_1 <- ifelse(PC_speed_0 == 0, 0, get(paste(PC_speed_0, "timeframe", sep = "_")))
    PC_speed_1 <- get(paste(PC_speed_0, "timeframe", sep = "_"))
    PC_speed_num = as.numeric(PC_speed_1)
    PC_speed_0 <- ifelse(PC_speed_0 == 0, "NO", PC_speed_0)
    PC_speed_2 <- glue("{toupper(PC_speed_0)} post-covid effect ({PC_speed_1} months) starting in {PC_date_text}")

    # recovery_date_text = format(start_date + months(PC_speed_num), "%b %y")
    recovery_speed_0 <- s %>%
        str_remove(".*_PC_") %>%
        str_remove("_recovery")
    recovery_speed_0 <- ifelse(recovery_speed_0 == "NO", 0, recovery_speed_0)
    recovery_speed_1 <- ifelse(recovery_speed_0 == 0, 0, get(paste(recovery_speed_0, "timeframe", sep = "_")))
    recovery_speed_0 <- ifelse(recovery_speed_0 == 0, "NO", recovery_speed_0)
    recovery_speed_2 <- glue("{toupper(recovery_speed_0)} recovery period ({recovery_speed_1} months)") #" starting in {recovery_date_text}")

    return(list(PC_speed_2, recovery_speed_2) %>% unlist)
}

assess_COVID_performance = function( truth,
                                    trace,
                                    model, 
                                    start_date,
                                    file_pre_stump = "", 
                                    file_post_stump = "_trend-predictions",
                                    out_dir = "fig/evaluation" ) {

    if ( !dir.exists(out_dir) ) {
        cat( glue("creating output directory: {out_dir}\n\n") )
        dir.create(out_dir)
    }

    list_of_outputs = tibble()

    model = model %>%
        rowwise() %>%
        mutate(start = ymd(start)) %>%
        mutate(end = ymd(end))

    n_practices = model %>% pull( name ) %>% unique %>% length
    all_profiles = truth %>% pull( profile_type ) %>% unique 
    all_p = model %>% pull(p_threshold) %>% unique()
    all_r = model %>% pull(r_threshold) %>% unique()

    for ( this_profile in all_profiles ) {

        # trace_plots = list()
        # hist_plots = list()
        up_plots = list()
        down_plots = list()

        covid_profile_descriptive = generate_COVID_profile_name(this_profile)
        
        cat(glue("Analysing {this_profile} ({covid_profile_descriptive})\n\n"))

        this_truth = truth %>% filter(profile_type == this_profile)  %>% 
            mutate( change_direction = ifelse( magnitude > 0, "increase", "decrease" ))
        
        this_trace = trace %>%
            filter(profile_type == glue("COVID_{this_profile}")) %>%
            mutate(trace = cumsum(change_magnitude)) %>%
            mutate(date = ymd(date))
            
        trace_plot = ggplot(this_trace, aes(x = date, y = trace, group = 1)) +
            annotate("rect",
                xmin = this_truth %>% filter( change_direction == "increase" ) %>% pull(start),
                xmax = this_truth %>% filter( change_direction == "increase" ) %>% pull(end),
                ymin = -Inf,
                ymax = Inf,
                fill = increase_colour,
                alpha = 0.1
            ) +
            annotate("rect",
                xmin = this_truth %>% filter( change_direction == "decrease" ) %>% pull(start),
                xmax = this_truth %>% filter( change_direction == "decrease" ) %>% pull(end),
                ymin = -Inf,
                ymax = Inf,
                fill = decrease_colour,
                alpha = 0.1
            ) +
            geom_point() +
            geom_path() +
            theme_bw() +
            scale_x_date(date_labels = "%b %y", breaks = xbreaks) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) 
        
        # p_out = glue("{out_dir}/{file_pre_stump}{this_profile}_TRACE{file_post_stump}.png")

        # ggsave(filename=p_out, height = 4, width = 4)

        # list_of_outputs = list_of_outputs %>%
        #     bind_rows(tibble(profile = this_profile, p = NA_real_, r = NA_real_, type = "trace", file = p_out)) # , plot = trace_plot))

        # trace_plots[[this_profile]] = trace_plot

        this_model = model %>% filter(dataset == glue("COVID_{this_profile}"))

        hist_plot = ggplot(this_model, aes(x = start, fill = direction))  +
            annotate("rect",
                xmin = this_truth %>% filter( change_direction == "increase" ) %>% pull(start),
                xmax = this_truth %>% filter( change_direction == "increase" ) %>% pull(end),
                ymin = -Inf,
                ymax = Inf,
                fill = increase_colour,
                alpha = 0.1
            ) +
            annotate("rect",
                xmin = this_truth %>% filter( change_direction == "decrease" ) %>% pull(start),
                xmax = this_truth %>% filter( change_direction == "decrease" ) %>% pull(end),
                ymin = -Inf,
                ymax = Inf,
                fill = decrease_colour,
                alpha = 0.1
            ) +
            scale_fill_manual( values = c(up = increase_colour, down = decrease_colour )) +

            geom_histogram(stat = "count", position = position_dodge(preserve = "single")) +
            theme_bw() +
            scale_x_date(date_labels = "%b %y", breaks = xbreaks, limits = range(xbreaks)) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
            facet_grid(p_threshold ~ r_threshold, scales="free")

        # p_out = glue("{out_dir}/{file_pre_stump}{this_profile}_REGION-STARTS{file_post_stump}.png")

        # list_of_outputs <- list_of_outputs %>%
            # bind_rows(tibble(profile = this_profile, p = NA_real_, r = NA_real_, type = "histogram", file = p_out)) # , plot = hist_plot))

        # ggsave(p_out, height = 8, width = 8)

        break_plot_i = 0


        for (this_p in all_p) {

            for ( this_r in all_r ) {

                break_plot_i = break_plot_i + 1

                cat(glue("> [{break_plot_i}] p={this_p} r={this_r}\n\n"))


                this_tag = glue("{this_profile}|{this_p}|{this_r}")
                    
                this_pr_model = this_model %>%
                    filter(p_threshold == this_p & r_threshold == this_r) 
                
                for( this_direction in c("up", "down") ) {

                    if ( this_direction == "up" ) {
                        this_xmin = this_truth %>% filter( change_direction == "increase" ) %>% pull(start)
                        this_xmax = this_truth %>% filter( change_direction == "increase" ) %>% pull(end)
                        this_colour = increase_colour
                    } else {
                        this_xmin = this_truth %>%
                            filter(change_direction == "decrease") %>%
                            pull(start)
                        this_xmax = this_truth %>%
                            filter(change_direction == "decrease") %>%
                            pull(end)
                        this_colour = decrease_colour
                    }

                    plot_list = get(glue("{this_direction}_plots"))

                    this_model_density = this_pr_model %>%
                        filter(direction == this_direction) %>%
                        arrange(start) %>%
                        mutate( name = fct_reorder( name, start ))

                    this_break_plot = NULL
                    
                    if ( nrow( this_model_density ) > 0 ) {

                    this_break_plot = 
                            ggplot(this_model_density,
                                aes(x = start, xend = end,
                                    y = name, yend = name,
                                    group=name))  +
                                annotate("rect",
                                    xmin = this_xmin,
                                    xmax = this_xmax,
                                    ymin = -Inf,
                                    ymax = Inf,
                                    fill = this_colour,
                                    alpha = 0.1
                                ) +
                                geom_segment() +
                                    theme_bw() +
                                    scale_y_discrete(limits = rev) +
                                    scale_x_date(date_labels = "%b %y",
                                                breaks = xbreaks,
                                                limits=range(xbreaks)) +
                                    theme(
                                        # axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
                                        axis.text.x = element_blank(),
                                        axis.text.y = element_blank(),
                                        axis.ticks.y = element_blank(),
                                        panel.grid.major.y = element_blank(),
                                        panel.grid.minor.y = element_blank()
                                    ) +
                                xlab("") +
                                ylab("") +
                                labs(title = glue("p={this_p} / r={this_r}"))

                    }
                                
                    plot_list[[this_tag]] = this_break_plot

                    assign(glue("{this_direction}_plots"), plot_list)
                    # assign(glue("break_plot_{break_plot_i}"), this_break_plot )

                }

                
            }
        }

        profile_plot_list = names(up_plots) %>%
            keep(~str_detect(.x,this_profile))

        # parameter_list_tmp = str_remove(profile_plot_list, ".*?\\|") %>%
        #     str_split("\\|")
        
        # parameter_list = do.call(rbind, parameter_list_tmp) %>%
        #     as_tibble() %>%
        #     mutate(parameter_pair = glue("p={V1}, r={V2}")) %>%
        #     pull( parameter_pair )
        
        ### UP PLOT

        # up_title = ggdraw() + draw_label(
        #     glue("{this_profile} (positive breaks)"),
        #     fontface = "bold", x = 0, hjust = 0
        # ) + theme(plot.margin = margin(0, 0, 0, 7))


        up_matrix = plot_grid(
            plotlist = up_plots[profile_plot_list],
            ncol = length(all_r),
            nrow = length(all_p) )
            
        # up_matrix_final = plot_grid( up_title, up_matrix,
        #     ncol = 1,
        #     rel_heights = c(0.05, 1))

        # p_out = glue("{out_dir}/{file_pre_stump}{this_profile}_UP-REGIONS{file_post_stump}.png")

        # list_of_outputs = list_of_outputs %>%
        #     bind_rows(tibble(profile = this_profile, p = NA, r = NA, type = "up-regions", file = p_out)) # , plot = up_matrix_final))

        # ggsave(p_out, height = 8, width=8 )

        ### DOWN PLOT
        
        # down_title <- ggdraw() + draw_label(
        #     glue("{this_profile} (negative breaks)"),
        #     fontface = "bold", x = 0, hjust = 0
        # ) + theme(plot.margin = margin(0, 0, 0, 7))


        down_matrix <- plot_grid(
            plotlist = down_plots[profile_plot_list],
            ncol = length(all_r),
            nrow = length(all_p)
        )

        # down_matrix_final = plot_grid(down_title, down_matrix,
        #     ncol = 1,
        #     rel_heights = c(0.05, 1)
        # )

        # p_out = glue("{out_dir}/{file_pre_stump}{this_profile}_DOWN-REGIONS{file_post_stump}.png")

        # list_of_outputs = list_of_outputs %>%
        # bind_rows(tibble(profile = this_profile, p = NA_real_, r = NA_real_, type = "down-regions", file = p_out)) # , plot = down_matrix_final))

        # ggsave(p_out, height = 8, width = 8)


        ### SAVE THE TRACE + HISTOGRAM OF START LOCATIONS
        histogram_tosave = (trace_plot + plot_spacer()) / hist_plot + theme(legend.position = "none") +
            plot_layout(design = trace_inset_layout) +
            plot_annotation(
                title = this_profile,
                subtitle = "Histogram of start locations of breaks"
            )
        
        histogram_tosave_file = glue("{out_dir}/{file_pre_stump}{this_profile}_HISTOGRAM{file_post_stump}.png")
        ggsave(histogram_tosave_file, histogram_tosave, height = 12, width = 8)

        ### SAVE THE UP MATRIX
        # upmatrix_tosave <- (trace_plot + plot_spacer()) / up_matrix +
        #     plot_layout(design = trace_inset_layout_multiple) +
        #     ggtitle("") +
        #     plot_annotation(
        #         title = this_profile,
        #         subtitle = "Locations of positive break regions"
        #     )
        upmatrix_tosave <- up_matrix +
            ggtitle("") +
            plot_annotation(
                title = this_profile,
                subtitle = "Locations of positive break regions"
            )
        
        upmatrix_tosave_file <- glue("{out_dir}/{file_pre_stump}{this_profile}_UP-REGIONS{file_post_stump}.png")
        ggsave(upmatrix_tosave_file, upmatrix_tosave, height = 12, width = 8)

        ### SAVE THE DOWN MATRIX
        # downmatrix_tosave <- (trace_plot + plot_spacer()) / down_matrix +
        #     plot_layout(design = trace_inset_layout_multiple) +
        #     plot_annotation(
        #             title = this_profile,
        #             subtitle = "Locations of negative break regions"
        #         )
        downmatrix_tosave <- down_matrix +
            ggtitle("") +
            plot_annotation(
                title = this_profile,
                subtitle = "Locations of negative break regions"
            )
        downmatrix_tosave_file <- glue("{out_dir}/{file_pre_stump}{this_profile}_DOWN-REGIONS{file_post_stump}.png")
        ggsave(downmatrix_tosave_file, downmatrix_tosave, height = 12, width = 8)

        list_of_outputs = list_of_outputs %>%
            bind_rows(tibble(
                profile = this_profile,
                type = "histogram",
                file = histogram_tosave_file
            )) %>%
            bind_rows(tibble(
                profile = this_profile,
                type = "upmatrix",
                file = upmatrix_tosave_file
            )) %>%
            bind_rows(tibble(
                profile = this_profile,
                type = "downmatrix",
                file = downmatrix_tosave_file
            )) 


    }

    return( list_of_outputs )

}

