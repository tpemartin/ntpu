library(readxl)
sheet_aggregate <- read_excel("~/Downloads/1100319個人申請模擬書審成績試算-6位委員.xlsx",
                              col_types = c("numeric", "text", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric", "numeric", "numeric",
                                            "numeric"), skip = 2)

names(sheet_aggregate) %>%
  {which(stringr::str_length(.)==1)} -> whichIsCommittees
committeeNames <- names(sheet_aggregate)[whichIsCommittees]

tbLong_grades <- {
  tidyr::pivot_longer(
    sheet_aggregate,
    cols = c(committeeNames),
    names_to = "committee",
    values_to = "grade"
  )
}
