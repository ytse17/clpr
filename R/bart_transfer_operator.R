#' Show relevant BART-related transactions using transfer operator method
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions
filter_bart_transfers <- function(tr_df) {
  tr_df <- tr_df %>%
    drop_tagons() %>%
    dplyr::group_by(cardid_anony) %>%
    dplyr::arrange(transaction_time) %>%
    dplyr::mutate(is_bart = operatorid == 4) %>%
    dplyr::mutate(from_bart = transferoperator == 4) %>%
    dplyr::mutate(to_bart = (lead(operatorid) == 4 & lead(transferoperator) == operatorid)) %>%
    dplyr::filter(is_bart | from_bart | to_bart)
  return(tr_df)
}

#' Create time metadata using spread_time_column
#' @param df1 a dataframe of transactions
#' @returns times_df a dataframe of spread times
nice_time <- function(df1) {
  out_time_df <- spread_time_column(df1$transaction_time, prefix="tag_on_")
  in_time_df <- spread_time_column(df1$time_of_previous, prefix="tag_out_")
  times_df <- as_tibble(cbind(in_time_df, out_time_df))
  return(times_df)
}

#' Combine from BART and to BART transactions to create rows of BART journeys
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions
as_bart_journeys <- function(tr_df) {
  tr_df <- tr_df %>%
    filter_bart_transfers() %>%
    dplyr::group_by(cardid_anony) %>%
    dplyr::arrange(transaction_time) %>%
    dplyr::mutate(time_of_previous = lag(transaction_time)) %>%
    dplyr::mutate(transfer_to = is_bart & lag(to_bart)) %>%
    dplyr::mutate(transfer_from = is_bart & lag(from_bart)) %>%
    dplyr::mutate(transfer_to_operator_time = dplyr::case_when(transfer_to ~ abs(difftime(transaction_time, lag(transaction_time), units = "mins")))) %>%
    dplyr::mutate(transfer_from_operator_time = dplyr::case_when(transfer_from ~ abs(difftime(transaction_time, lead(transaction_time), units = "mins")))) %>%
    dplyr::mutate(transfer_to_operator = dplyr::case_when(transfer_to ~ lag(participantname))) %>%
    dplyr::mutate(transfer_from_operator = dplyr::case_when(transfer_from ~ lead(participantname))) %>%
    dplyr::mutate(transfer_to_route = dplyr::case_when(transfer_to ~ lag(routename))) %>%
    dplyr::mutate(transfer_from_route = dplyr::case_when(transfer_from ~ lead(routename))) %>%
    dplyr::filter(transfer_to | transfer_from) %>%
    dplyr::ungroup() %>%
    dplyr::select(cardid_anony, transaction_time, time_of_previous, is_bart, transfer_from_route, transfer_to_route,
                  transfer_to_route, locationname.origin, locationname.destination, transfer_to_operator, transfer_from_operator,
                  transfer_from_operator_time, transfer_to_operator_time) %>%
    cbind(nice_time(.))
  return(tr_df)
}




