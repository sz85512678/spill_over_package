#' Compute group-wise data on top of the raw data
#'
#' For each individual i belonging to group g,
#'   compute the total size of the group g,
#'   compute the fraction (excluding i) of treated individual in group g,
#'   compute the fraction (excluding i) of compilers in group g,
#' Assume data does not have NA.
#' Assume that in each non-zero saturation group there are at least 2 offered.
#' @param data_in, containing columns "group_id", "offered", "treated", "outcome",
#'   where data$group_id are positive integers, other entries in data all (0,1)
#' @return data_out, sorted by group_id,
#'   with additional columns: "group_size", "frac_treated_exclude_i", "frac_compilers_exclude_i"
#'   The value of "frac_compilers_exclude_i" will be NaN for zero-saturation groups
#' @importFrom plyr join
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise_at
#' @importFrom dplyr group_by
#' @importFrom data.table setnames
#' @importFrom rlang .data
compute_auxilliary_data <- function(data_in){
  stopifnot("data should not have NA" = any(is.na(data_in)) == 0)
  stopifnot("data should contains group_id, offered, treated, outcome columns"
            = "group_id" %in% names(data_in) &
              "offered" %in% names(data_in) &
              "treated" %in% names(data_in) &
              "outcome" %in% names(data_in)
  )
  arrange(data_in, "group_id")

  data_by_group <- data_in %>%
    mutate(ones = 1L) %>%
    group_by(.data$group_id) %>%
    summarise_at(c("offered", "treated","ones") ,sum) %>%
    setnames(old = c('offered','treated','ones'),
             new = c('group_total_offered','group_total_treated','group_size')
    )
  # The algorithm tolerates zero saturation (none offered), but not groups with non-zero saturation
  # while in the actual data only one person in the group offered.
  stopifnot("each non-zero saturation group should have at least two people offered treatment" =
              all(data_by_group$group_total_offered >= 2 | data_by_group$group_total_offered == 0))

  data_out = join(data_in,
                  data_by_group[,c('group_id','group_total_offered',
                                   'group_total_treated','group_size')],
                  by = "group_id"
                  ) %>%
    mutate(frac_treated_exclude_i =
             (.data$group_total_treated - .data$treated) / (.data$group_size - 1),
           frac_compilers_exclude_i =
             (.data$group_total_treated - .data$treated) / (.data$group_total_offered - .data$offered)
           )
  return(data_out)
}
