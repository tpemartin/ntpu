get_aspectTable <- function(tb, aspect){
  require(dplyr)
  tb %>%
    dplyr::filter(
      面向==aspect
    ) %>%
    mutate(
      分數區間 = cut(分數,
                   breaks=c(60, 70, 80, 90, 100), right=F)
    ) -> tb_standardised_details

  tb_standardised_details %>%
    group_by(
      評分老師, 分數區間
    ) %>%
    summarise(
      count=n()
    ) %>%
    tidyr::pivot_wider(
      names_from = "分數區間",
      values_from = count
    )
}
