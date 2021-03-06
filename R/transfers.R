library(stringr)

#' Adds columns identifying bart transactions, including lags and leads
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions with columns: from_bart, to_bart, from_not_bart, to_not_bart
bart_identify <- function(tr_df) {
    tr_df <- tr_df %>%
      dplyr::group_by(cardid_anony) %>%
      dplyr::arrange(transaction_time) %>%
      dplyr::mutate(is_bart = operatorid==4,
             from_bart = lag(operatorid)==4,
             to_bart = lead(operatorid)==4,
             exit_to_not_bart = (is_bart & from_bart & !to_bart),
             entrance_from_not_bart = (is_bart & to_bart & !from_bart))
    return(tr_df)
}

#' Adds columns describing the route, time, and operator name of previous and later transactions
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions
bart_lag_and_lead_metadata <- function(tr_df) {
    tr_df <- tr_df %>%
      dplyr::group_by(cardid_anony) %>%
      dplyr::arrange(transaction_time) %>%
      dplyr::mutate(timediff = abs(difftime(transaction_time, lag(transaction_time),units="mins")),
             time_of_previous = lag(transaction_time),
             transfer_from_time = round(timediff,2),
             transfer_to_operator_time = round(lead(timediff),2),
             transfer_to_not_bart = (exit_to_not_bart & transfer_to_operator_time<180),
             transfer_from_not_bart = (entrance_from_not_bart & transfer_from_time<180),
             transfer_from_operator = case_when(transfer_from_not_bart ~ lag(participantname)),
             transfer_to_operator = case_when(transfer_to_not_bart ~ lead(participantname)),
             transfer_from_route = case_when(transfer_from_not_bart ~ lag(routename)),
             transfer_to_route = case_when(transfer_to_not_bart ~ lead(routename)))

    #this is confusing, but you have to pull routes and transfers from
    #operators from two-transactions back
    #so here we effectively reach the lag back 2 transactions
    tr_df <- tr_df %>%
      dplyr::group_by(cardid_anony) %>%
      dplyr::arrange(transaction_time) %>%
      dplyr::mutate(transfer_from_operator=case_when(from_bart ~ lag(transfer_from_operator)),
             transfer_from_route=case_when(from_bart ~ lag(transfer_from_route)),
             transfer_from_operator_time = case_when(from_bart ~ lag(transfer_from_time)))
    return(tr_df)
}

#' Adds a column with a counts of the number of transactions per user
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions with a transaction_count column
transactions_per_user <- function(tr_df) {
    tr_df <- tr_df %>%
      dplyr::group_by(cardid_anony,yday) %>%
      dplyr::mutate(transaction_count = n())
    return(tr_df)
}


#' Adds a column with a counts of the number of bart transactions per user and diff with all transactions
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions with a bart_tr_count and tr_count_diff column
bart_transactions_per_user <- function(tr_df) {
    tr_df <- tr_df %>%
      dplyr::group_by(cardid_anony,yday,is_bart) %>%
      dplyr::mutate(bart_tr_count = n(),
             tr_count_diff = transaction_count-bart_tr_count)
    return(tr_df)
}


nicetime <- function(df1){
  out_time_df <- spread_time_column(df1$transaction_time, prefix="tag_on_")
  in_time_df <- spread_time_column(df1$time_of_previous, prefix="tag_out_")

  bart_od3 <- cbind(df1,in_time_df,out_time_df)
}


#' Spreads multiple transactions across columns into one-row-per-bart-trip (with a focus on transfers in and out)
#'
#' @param tr_df a sample of transactions, effectively from the raw sfofaretransactions table, joined to other tables
#' @returns bart_xfer_df table in which transfers in and out of bart are captured with metadata
#' @importFrom dplyr group_by mutate case_when lag arrange filter select
bart_transactions_as_transfers <- function(tr_df){
  bart_rider_ids <- tr_df %>%
    dplyr::filter(operatorid==4) %>%
    dplyr::pull(cardid_anony)

  tr_df <- tr_df %>%
    dplyr::filter(cardid_anony %in% bart_rider_ids)

  tr_df <- bart_identify(tr_df)
  tr_df <- bart_lag_and_lead_metadata(tr_df)

  #here we drop the first in the series of bart transactions
  #the relevant metadata for the first has been pulled
  #onto the final transaction in bart_lag_and_lead_metadata()
  bart_xfer_df <- tr_df[tr_df$is_bart &
                            !is.na(tr_df$locationname.destination),]
  bart_xfer_df <- bart_xfer_df %>% ungroup() %>% as_tibble()
  return(bart_xfer_df)
}


#'Build a database of transfers
#'
#'The anonymous Clipper data is potentially an excellent source for quantifying operator-to-operator transit movements.
#'Here, we seek to understand transfers in the travel model sense,
#'i.e. a movement between an origin and destination that requires
#'a tranfer between or within transit operators. This definition differs from transfers
#'as defined by certain transit agencies, which generally allow multiple boardings at no or reduced cost
#'during a narrow time window.
#'Here, we implement a set of rules to build a database of transfers.
#'This script uses data derived from the anonymous Clipper data
#'see Extract Transfer for Random Weekday to CSV.R.
#'@param df_row a row from transfer_filenames_df, which is included in the package
#'@param data_dir the directory in which the transfer and transaction data resides
#'@param transfer_rules_df a set of travel model rules on what constituted a transfer
#'@param sampling_rate_float
#'@param output_file_string
#'@param append_boolean
#'@returns the path to which the output was written
Build_Database <- function(df_row,data_dir,
                           transfer_rules_df, sampling_rate_float,
                           output_file_string, append_boolean){
  load(paste0(data_dir,"/",df_row[['Transfers']]))
  transfers_df <- working.output

  load(paste0(data_dir,"/",df_row[['Transactions']]))
  transactions_df <- operator_counts

  # Summarise the transfers for typical weekdays
  typical.weekday <- transfers_df %>%
    dplyr::mutate(from_AgencyName = str_trim(from_AgencyName)) %>%
    dplyr::mutate(to_AgencyName   = str_trim(to_AgencyName)) %>%
    dplyr::filter(CircadianDayOfWeek > 2) %>%
    dplyr::filter(CircadianDayOfWeek < 6) %>%
    dplyr::mutate(key_time = ifelse(!is.na(Diff_Min_TagOff_to_TagOn), Diff_Min_TagOff_to_TagOn, Diff_Min_TagOn_to_TagOn)) %>%
    dplyr::select(Year, Month, CircadianDayOfWeek, RandomWeekID, from_AgencyName, to_AgencyName, key_time)

  transfer.data <- inner_join(typical.weekday, transfer_rules_df, by = c("from_AgencyName", "to_AgencyName"))

  transfer.sum <- transfer.data %>%
    dplyr::filter(key_time <= max_time) %>%
    dplyr::select(-key_time, -max_time) %>%
    dplyr::group_by(Year, Month, CircadianDayOfWeek, RandomWeekID, from_AgencyName, to_AgencyName) %>%
    summarise(sampled_transfers = n())

  # Join the transactions
  working.transactions <- transactions_df %>%
    dplyr::mutate(AgencyName = str_trim(AgencyName))

  from_transactions <- working.transactions %>%
    dplyr::select(from_AgencyName = AgencyName, from_Agency_Sampled_Transactions = Sampled_Transactions, Year, Month, CircadianDayOfWeek, RandomWeekID)

  to_transactions <- working.transactions %>%
    dplyr::select(  to_AgencyName = AgencyName,   to_Agency_Sampled_Transactions = Sampled_Transactions, Year, Month, CircadianDayOfWeek, RandomWeekID)

  transfer.write <- left_join(transfer.sum,   from_transactions, by = c("Year", "Month", "CircadianDayOfWeek", "RandomWeekID", "from_AgencyName"))
  transfer.write <- left_join(transfer.write, to_transactions,   by = c("Year", "Month", "CircadianDayOfWeek", "RandomWeekID", "to_AgencyName"))

  # Estimate transfers & tranactions using the sample rate
  transfer.write <- transfer.write %>%
    dplyr::mutate(estimated_transfers = sampled_transfers / SAMPLING_RATE) %>%
    dplyr::mutate(estimated_from_agency_transactions = from_Agency_Sampled_Transactions / SAMPLING_RATE) %>%
    dplyr::mutate(estimated_to_agency_transactions   = to_Agency_Sampled_Transactions   / SAMPLING_RATE)

  # Write to disk
  if (append_boolean) {
    existing <- read.table(file = output_file_string, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    transfer.write <- rbind(existing, transfer.write)

  }

  output_filename <- paste0(data_dir,"/",output_file_string)
  write.csv(transfer.write, output_filename, row.names = FALSE, quote = T)
  return(output_filename)
}


Make_to_from_plot <- function(transfer_df, a_string, b_string){

  # A to B
  a_to_b <- transfer_df %>%
    dplyr::mutate(from_AgencyName = str_trim(from_AgencyName)) %>%
    dplyr::mutate(to_AgencyName   = str_trim(to_AgencyName)) %>%
    dplyr::filter(from_AgencyName == a_string) %>%
    dplyr::filter(to_AgencyName   == b_string)

  a_to_b <- a_to_b %>%
    dplyr::mutate(key_time = ifelse(!is.na(Diff_Min_TagOff_to_TagOn), Diff_Min_TagOff_to_TagOn, Diff_Min_TagOn_to_TagOn)) %>%
    dplyr::group_by(Year, Month, CircadianDayOfWeek, RandomWeekID, key_time) %>%
    summarise(transfers = n()) %>%
    dplyr::group_by(Year, Month, key_time) %>%
    summarise(typical_weekdays = n(), median_transfers = median(transfers)) %>%
    dplyr::mutate(cumulative_median_transfers = cumsum(median_transfers)) %>%
    dplyr::select(Year, Month, key_time, cumulative_median_transfers) %>%
    dplyr::mutate(movement = paste(a_string," to ", b_string)) %>%
    dplyr::filter(key_time > 0) %>%
    dplyr::filter(key_time < 130)

  # B to A
  b_to_a <- transfer_df %>%
    dplyr::mutate(from_AgencyName = str_trim(from_AgencyName)) %>%
    dplyr::mutate(to_AgencyName   = str_trim(to_AgencyName)) %>%
    dplyr::filter(from_AgencyName == b_string) %>%
    dplyr::filter(to_AgencyName   == a_string)

  b_to_a <- b_to_a %>%
    dplyr::mutate(key_time = ifelse(!is.na(Diff_Min_TagOff_to_TagOn), Diff_Min_TagOff_to_TagOn, Diff_Min_TagOn_to_TagOn)) %>%
    dplyr::group_by(Year, Month, CircadianDayOfWeek, RandomWeekID, key_time) %>%
    summarise(transfers = n()) %>%
    dplyr::group_by(Year, Month, key_time) %>%
    summarise(typical_weekdays = n(), median_transfers = median(transfers)) %>%
    dplyr::mutate(cumulative_median_transfers = cumsum(median_transfers)) %>%
    dplyr::select(Year, Month, key_time, cumulative_median_transfers) %>%
    dplyr::mutate(movement = paste(b_string," to ", a_string)) %>%
    dplyr::filter(key_time > 0) %>%
    dplyr::filter(key_time < 130)

  # Merge and plot
  data.to_plot <- rbind(a_to_b, b_to_a)

  # Plot
  plot <- ggplot(data.to_plot, aes(x = key_time, y = cumulative_median_transfers, colour = movement, group = movement)) +
    xlab("Time between Tags") +
    ylab("Cumulative Sampled Transfers") +
    theme(text = element_text(size = 16), axis.title.x = element_text(vjust = -0.5), axis.title.y = element_text(vjust = 2.0), panel.grid.major.x = element_line(colour = "black")) +
    geom_line(size = 2) +
    ggtitle(paste(a_string,"to/from", b_string))

  return(plot)

}

