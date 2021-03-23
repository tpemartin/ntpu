Analysis <- function(){
  analysis <- new.env()
  analysis$info$committeeNames <- committeeNames
  analysis$sheets <- list()
  analysis$sheets$append_sheet <- function(sheet, name){
    analysis$sheets <- append(
      analysis$sheets,
      setNames(
        list(
          sheet
        ),
        name
      )
    )
  }


  analysis$standardise <- list()

  analysis$standardise_sheet <- function(sheet_name, colnames4standardise){

    colname_committee = colnames4standardise$colname_committee
    colname_aspects = colnames4standardise$colname_aspects
    colname_total = colnames4standardise$colname_total
    colname_ID = colnames4standardise$colname_ID

    sheet = analysis$sheets[[sheet_name]]

    standardise_columnNames(sheet, colname_committee, colname_aspects, colname_total, colname_ID) -> sheetX
    analysis$sheets[[paste0("standardised_", sheet_name)]] <- sheetX
  }

  analysis$columns2standardise_template = function(){
    list(
      colname_ID="",
      colname_committee="",
      colname_aspects="",
      colname_total=""
    )
  }

  return(analysis)
}



# helpers ------------------------------------------------------------------


standardise_columnNames <-
  function(sheet, colname_committee, colname_aspects, colname_total, colname_ID){
    require(dplyr)
    names(sheet) %>%
      {case_when(
        .==colname_committee ~ "評分老師",
        TRUE ~ .
      )} -> names(sheet)
    whichAreAspects <- which(names(sheet) %in% colname_aspects)
    names(sheet)[whichAreAspects] <- LETTERS[seq_along(whichAreAspects)]
    whichIsTotal <- which(names(sheet) %in% colname_total)
    names(sheet)[whichIsTotal] <- "total"
    whichIsID <- which(names(sheet) %in% colname_ID)
    names(sheet)[whichIsID] <- "ID"

    sheet
}

# addEvaluator <- function(df_chu,evaluator){
#   df_chu %>%
#     mutate(
#       評分老師=evaluator
#     ) %>%
#     rename(
#       "A"="A:在校成績35%",
#       "B"="B:自傳及讀書計畫(含申請動機)35%",
#       "C"="C:多元表現(其他有助於審查之資料)30%",
#       "total"="總分(A*35%+B*35%+C*30%)"
#     ) -> df_chu2
#   return(df_chu2)
# }

