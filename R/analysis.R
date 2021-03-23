#' Create analysis instance
#'
#' @return
#' @export
#'
#' @examples None
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

  analysis$get_df_exclusionByColumn <- get_df_exclusionByColumn

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

    sheet %>%
      mutate(
        評分老師 = as.factor(評分老師),
        ID = as.factor(ID)
      ) -> sheet

    sheet
}

prepare_rankDF_byColumnX <- function(df_all, columnX){
  columnX_quo <- rlang::enquo(columnX)
  # browser()
  df_all %>%
    group_by(ID) %>%
    summarise(
      averageAll=mean(!!columnX_quo) # mean(total),
    ) %>%
    mutate(
      rankAll=rank(averageAll)
    )

}
get_dfExcludes <- function(df_all, columnX){
  quo_columnX <- rlang::enquo(columnX)
  # ntpu:::prepare_rankDF_byColumnX(df_all, total) -> dfx
  levels(df_all$評分老師)-> listEvalutators
  for(i in seq_along(listEvalutators)){
    df_name <- paste0("df_exclude_",i)

    df_exclude <- df_all %>%
      dplyr::filter(
        評分老師!=listEvalutators[[i]]
      ) %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(
        average=mean(!!quo_columnX),
      ) %>%
      dplyr::mutate(
        rank=rank(average),
        exclude=listEvalutators[[i]]
      )
    assign(df_name,df_exclude)
  }
  bind_rows(
    df_exclude_1,df_exclude_2,df_exclude_3,
    df_exclude_4,df_exclude_5,df_exclude_6
  ) %>%
    mutate(
      exclude=as.factor(exclude)
    )-> df_exclude
  return(df_exclude)
}
get_df_rankSelf <- function(df_all, df_exclude, columnX){
  quo_columnX <- rlang::enquo(columnX)
  # 將學生名字依名次排序
  df_all$ID -> allNames
  df_all$rankAll -> rankAll
  allNames[rankAll] <- allNames

  # df_exclude
  # assessList$df_exclude

  # 排除委員自己後的比序
  df_exclude %>%
    dplyr::select(-average) %>%
    tidyr::pivot_wider(
      names_from = "ID", values_from = "rank"
    ) -> df_exclude_spread

  # df_exclude_spread[,c("exclude",as.character(allNames))]

  df_all %>%
    dplyr::group_by(評分老師) %>%
    dplyr::mutate(
      rank_self=as.integer(rank(!!quo_columnX))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      ID, 評分老師, rank_self
    ) %>%
    dplyr::rename(
      "name_self"="ID"
    )-> df_rankSelf

  return(df_rankSelf)
}
prepare_df_exclusion <- function(df_exclude, df_rankSelf, df_all_totalRanked){
  df_exclude  %>%
    rename(
      "rank_exclude"="rank",
      "student"="ID"
    ) %>%
    select(
      exclude, rank_exclude, student
    ) %>%
    left_join(
      df_rankSelf,
      by=c("exclude"="評分老師", "student"="name_self")
    ) -> df_rankComparison #%>% View("rankExam")

  # df_rankComparison # rank_self: 1委員排序  rank_exclude: 排1委員排序

  df_rankComparison %>%
    left_join(
      df_all_totalRanked %>%
        select(ID,rankAll),
      by=c("student"="ID")
    ) %>%
    rename(
      "排序:排除一位委員"="rank_exclude",
      "排序:單一委員"="rank_self",
      "排序:全部委員"="rankAll",
      "委員"="exclude"
    ) -> df_exclusion
  return(df_exclusion)
}
get_df_exclusionByColumn <- function(df_all, columnX){
  quo_columnX <- rlang::enquo(columnX)

  df_all_totalRanked <- prepare_rankDF_byColumnX(df_all, !!quo_columnX)

  df_exclude <- get_dfExcludes(df_all, !!quo_columnX)

  df_rankSelf <- get_df_rankSelf(df_all, df_exclude, !!quo_columnX)

  df_exclusion <- prepare_df_exclusion(df_exclude = df_exclude, df_rankSelf = df_rankSelf,
                                       df_all_totalRanked = df_all_totalRanked)
  return(df_exclusion)
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

