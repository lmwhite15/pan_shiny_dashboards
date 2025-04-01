
plot_function <- function(game = "All"){
  if(game == "All"){
    ggplot(overall_attempted_table, aes(x = reorder(game, percent_missing), 
                                        y = percent_missing, color = site)) +
      geom_point(size = 2) +
      geom_line(aes(group = site), linewidth = 1) +
      labs(x = "Games", y = "% Missing", color = "Site") +
      theme(legend.position = "top",
            axis.text.x = element_text(angle = 90,
                                       hjust = 0.9,
                                       vjust = 0.1),
            text = element_text(size = 18))
  }else{ # Look at a specific game for all sites
    if(game == "attention"){ # Look at Attention game
      files_list[["attention"]] %>%
        rbind(files_list[["attention"]] %>% mutate(site = "All")) %>%
        filter(attention_game_status == "Completed",
               attnRT_median != "") %>%
        mutate(site = str_to_title(site),
               site = factor(site,
                             levels = c("Atlanta", "Baltimore", "Miami", "Tucson", "All"))) %>%
        ggplot(aes(x = site, y = as.numeric(attnRT_median))) +
        geom_boxplot(outlier.shape = NA, lwd = 1) +
        geom_jitter(width = 0.25, height = 0, alpha = 0.5, color = "#224A7B", size = 2) +
        labs(x = "Site", y = "Median Reaction Time") +
        theme(legend.position = "none",
              text = element_text(size = 18))
    }else{ # Look at any other game
      files_list[[game]] %>%
        rbind(files_list[[game]] %>% mutate(site = "All")) %>%
        filter(!!as.name(paste0(game, "_game_status")) == "Completed") %>%
        mutate(site = str_to_title(site),
               site = factor(site,
                             levels = c("Atlanta", "Baltimore", "Miami", "Tucson", "All"))) %>%
        ggplot(aes(x = site, y = totalcorrect)) +
        geom_boxplot(outlier.shape = NA, lwd = 1) +
        geom_jitter(width = 0.25, height = 0, alpha = 0.5, color = "#224A7B", size = 2) +
        labs(x = "Site", y = "Total Correct Answers Across Non-Practice Rounds") +
        theme(legend.position = "none",
              text = element_text(size = 18))
    }
  }
}

table_function <- function(picked_site = "All", game = "All", picked_id = "All"){
  if(picked_site == "All"){
    if(game == "All"){ # Look at all games for all sites
      overall_attempted_table %>%
        mutate(site = factor(site,
                             levels = c("Atlanta", "Baltimore", "Miami", "Tucson", "All"))) %>%
        arrange(site) %>%
        mutate(counts = paste0(number_missing, " (", round(percent_missing, 1), "%)"),
               site = paste0(str_to_title(site), " (n = ", total_games, ")")) %>%
        pivot_wider(id_cols = game,
                    names_from = site,
                    values_from = counts) %>%
        rename(Game = game) %>%
        kable(caption = "% Missing") %>%
        kable_styling(full_width = F)
    }else{ # Look at a specific game for all sites
      if(game == "attention"){
        files_list[["attention"]] %>%
          rbind(files_list[["attention"]] %>% mutate(site = "All")) %>%
          filter(attention_game_status == "Completed",
                 attnRT_median != "") %>%
          mutate(site = str_to_title(site),
                 site = factor(site,
                               levels = c("Atlanta", "Baltimore", "Miami", "Tucson", "All")),
                 attnRT_median = as.numeric(attnRT_median)) %>%
          select(site, attnRT_median) %>%
          group_by(site) %>%
          summarise(mean_sd = paste0(round(mean(attnRT_median, na.rm = T), 1), " (", 
                                     round(sd(attnRT_median, na.rm = T), 1), ")")) %>%
          pivot_wider(names_from = site, values_from = mean_sd) %>%
          data.frame() %>%
          `row.names<-`("Mean of Median Reaction Times (SD)") %>%
          kable(row.names = T) %>%
          kable_styling(full_width = F)
      }else{
        files_list[[game]] %>%
          rbind(files_list[[game]] %>% mutate(site = "All")) %>%
          filter(!!as.name(paste0(game, "_game_status")) == "Completed") %>%
          mutate(site = str_to_title(site),
                 site = factor(site,
                               levels = c("Atlanta", "Baltimore", "Miami", "Tucson", "All"))) %>%
          select(site, totalcorrect) %>%
          group_by(site) %>%
          summarise(mean_sd = paste0(round(mean(totalcorrect, na.rm = T), 1), " (", 
                                     round(sd(totalcorrect, na.rm = T), 1), ")")) %>%
          pivot_wider(names_from = site, values_from = mean_sd) %>%
          data.frame() %>%
          `row.names<-`("Mean Total Correct (SD)") %>%
          kable() %>%
          kable_styling(full_width = F)
      }
    }
  }else{ # Looking at specific site
    if(game == "All"){
      if(picked_id == "All"){ # Look at list of completed games for specific site
        overall_attempted_table %>%
          filter(site == picked_site) %>%
          mutate(Missing = paste0(number_missing, " (", round(percent_missing, 1), "%)")) %>%
          select(Game = game, Missing) %>%
          kable() %>%
          kable_styling(full_width = F)
      }else{ # Look at list of completed games for a participant
        overall_attempted %>%
          filter(hml_id == picked_id) %>%
          select(-c(hml_id, participant_id_parent, site)) %>%
          rename_all(~str_remove(., "_game")) %>%
          pivot_longer(cols =  everything(),
                       names_to = c("game", ".value"),
                       names_pattern = "(.*)_(.*)") %>%
          mutate(game = str_replace(game, "_game_status", ""),
                 game = case_when(game == "faces_names" ~ "Faces & Names",
                                  game == "objects_spatial" ~ "Objects & Spaces",
                                  game == "objects_temporal" ~ "Objects & Time",
                                  TRUE ~ str_to_title(str_replace(game, "_", " "))),
                 timestamp = as.character(timestamp),
                 timestamp = ifelse(is.na(timestamp), "Not Attempted", timestamp)) %>%
          select(Game = game, `Date Attempted` = timestamp, status) %>%
          kable(col.names = c("Game", "Date Attempted", "")) %>%
          kable_styling(full_width = F)
      }
    }else{
      # Look at specific answers for a participant
      files_list[[game]] %>%
        filter(hml_id == picked_id) %>%
        select(-c(hml_id, participant_id, study_start_date_before, study_start_date_after, site, game_result, game_name, browser_useragent,
                  user_agent, latitude, longitude, ip_address, language, tz_offset,
                  zip_code, user_device)) %>%
        mutate_all(as.character) %>%
        pivot_longer(cols = everything(),
                     names_to = "Variable",
                     values_to = "record") %>%
        mutate(record = ifelse(is.na(record), "Missing", record)) %>%
        kable(col.names = c("Variable", "Participant Record")) %>%
        kable_styling(full_width = F)
    }
  }
}