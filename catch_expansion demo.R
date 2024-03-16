
# Length Expansion Script
# Levi Lewis 3-7-2024


# ------------- PREP. WORKSPACE ----------

library(tidyverse)


# -------- (1) GENERATE LENGTH AND PLUS-COUNT TABLES ----------------

# ...plus-count dataframe(s) ----

demo_plus <- read.csv("Data/demo_plus.csv") # plus-count data frame (generated from length and total catch tables)
demo_lengths <- read.csv("Data/demo_lengths.csv") # original lengths datafame (filtered here only to tows in demo_plus)

plus_count_df_o <- 
  demo_plus %>%
  mutate(sl = 0, count = n_plus, type = "plus") %>%
  select(id, code, sl, count, type) %>% 
  print

plus_count_errors <- plus_count_df_o %>% filter(count < 0) %>% print
write.csv(plus_count_errors,"demo_plus_count_errors.csv")

plus_count_df <- plus_count_df_o %>% filter(count > 0) # removing errors; need to check/fix

# ...length dataframe

lengths_df <- 
  demo_lengths %>% 
  mutate(sl = round(sl,0), count = 1, type = "length") %>% 
  select(id, code, sl, count, type) %>% 
  print

merged_df <- lengths_df %>% rbind(plus_count_df) %>% arrange(id,code,type,sl) %>% print



# ---  (2)  LENGTH EXPANSION TABLE -------------------------------



# ...expansion reference table ----

# calculate mean, sd, n of sl x spp & tow (for expanding lenghths)

exp_table <- 
  # lengths_df  %>%                       # formatted raw catch data
  demo_lengths %>%
  group_by(id,code) %>%                 # group x taxon & tow 
  summarize(m_sl = mean(sl, na.rm=T),            # generate mean sl (for rnorm expansion)
            sd_sl = sd(sl, na.rm=T),             # generate sd (for rnorm expansion)
            n_sl = length(sl)) %>%      # generate n of length measurements (as a check)
  full_join(plus_count_df, c("id", "code")) %>%  # join with plus counts (number of lengths that require expansion x tow x spp)
  filter(sl == 0) %>%                   # keep only the plus count data (remove length data)
  mutate(sd_sl = if_else(count==1, 0.2*m_sl, sd_sl)) %>%  # if only 1 length, assume sd = 0.2*SL (if no lengths, then expanded lengths will be NA)
  data.frame %>% print


# generate unique "sp_id" vector for looping

exp_table$id_sp <- paste0(exp_table$id, "_", exp_table$code) 
id_sp <- unique(exp_table$id_sp)

# ...expanded lengths list/table ----

exp_sl_list <- list()   # establish empty expanded lengths list
for(i in id_sp){        # add expanded lengths to list
  # i = id_sp[1]
  temp <- exp_table %>% filter(id_sp ==i)
  set.seed(1)
  df <- data.frame(
    id = rep(temp$id, temp$count),
    code =  rep(temp$code, temp$count),
    count = rep(1, temp$count),
    sl = round(rnorm(temp$count, mean=temp$m_sl, sd=temp$sd_sl)),
    type = "length")
  df_list <- list(df)  # convert to list element
  names(df_list) <- i  # name list element based on id_sp value
  exp_sl_list <- append(exp_sl_list, df_list) # add new element to master list
  }

# ...convert expanded lengths list to data.frame ----

exp_sl_df <- bind_rows(exp_sl_list) %>% print # bind list elements into df

# ...add expanded lengths to orig. lengths_df ----

lengths_df_exp <-                      
  bind_rows(lengths_df,exp_sl_df) %>%
  arrange(id,code,sl) %>% print

unique(lengths_df_exp$type) # check table for lengths only (no plus)

# ...check New Counts In Expanded Length Table ----
# diff_check compares total catch in catch table ("count") to number of length measurements (n) in new expanded length table (should be zero)
# number of plus counts should match difference bt the original (count_len) and expanded (n) number of lengths 
lengths_df_exp %>% 
  group_by(id,code) %>% 
  summarise(n = length(sl), 
            m_sl = mean(sl, na.rm=T), 
            sd_sl = sd(sl, na.rm=T))%>%
  left_join(demo_plus %>% select(id,code, count, count_len, n_plus), by = c("id", "code")) %>%
  mutate(diff_check = n - count) %>% 
  filter(!is.na(count)) %>%
  data.frame %>% print

# ...save joined lengths table to cvs ----

#write.csv(lengths_df_exp, "Data/demo_lengths_df_exp.csv") 

