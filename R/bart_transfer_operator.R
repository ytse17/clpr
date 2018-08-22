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

#' Combine from BART and to BART transactions to create rows of BART journeys
#' @param tr_df a dataframe of transactions
#' @returns tr_df a dataframe of transactions
bart_journeys <- function(tr_df) {
  tr_df <- tr_df %>%
    dplyr::group_by(cardid_anony) %>%
    dplyr::arrange(transaction_time) %>%
    dplyr::mutate(time_of_previous = lag(transaction_time)) %>%
    dplyr::mutate(transfer_to = is_bart & lag(to_bart)) %>%
    dplyr::mutate(transfer_from = is_bart & lag(from_bart)) %>%
    dplyr::mutate(transfer_to_time = dplyr::case_when(transfer_to ~ abs(difftime(transaction_time, lag(transaction_time), units = "mins")))) %>%
    dplyr::mutate(transfer_from_time = dplyr::case_when(transfer_from ~ abs(difftime(transaction_time, lead(transaction_time), units = "mins")))) %>%
    dplyr::mutate(transfer_to_operator = dplyr::case_when(transfer_to ~ lag(participantname))) %>%
    dplyr::mutate(transfer_from_operator = dplyr::case_when(transfer_from ~ lead(participantname))) %>%
    dplyr::mutate(transfer_to_route = dplyr::case_when(transfer_to ~ lag(routename))) %>%
    dplyr::mutate(transfer_from_route = dplyr::case_when(transfer_from ~ lead(routename))) %>%
    dplyr::filter(transfer_to | transfer_from)
  return(tr_df)
}




