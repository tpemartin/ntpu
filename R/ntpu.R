NTPU <- function(){
  require(dplyr)
  ntpu <- list()
  ntpu$admission <- list()
  ntpu$admission$assessment <- new.env()

  ntpu$admission$assessment$download_pdfs <- function(googleDriveURL, pdfdir){
    admission_assessment = ntpu$admission$assessment
    admission_assessment$pdfdir <- pdfdir
    admission_assessment$googleDriveURL <- googleDriveURL
    download_pdfs(googleDriveURL, pdfdir)
    admission_assessment$getTable_autobiographyStudyplan <- function(){
      # admissionX=ntpu$admission$assessment
      admission_assessment$pdfdir %>%
        get_dividedPDFs() %>%
        listPDFs_as_tibble() %>%
        flatten_columnX("自傳(學生自述)") %>%
        flatten_columnX("讀書計畫(含申請動機)") -> admission_assessment$tb_autobiographyStudyplan
    }

  }

  ntpu$admission$googleSheets <- new.env()
  ntpu$admission$googleSheets$sheets <- list()
  ntpu$admission$googleSheets$googleSheetURL_setup <- function(googleSheetURL, name){
    ntpu$admission$googleSheets$sheets$googleSheetInfo <-
      append(ntpu$admission$googleSheets$sheets,
             setNames(
               list(googleSheetURL_setup(googleSheetURL, name)),
               name))

    ntpu$admission$googleSheets$sheets[[name]]$upload <- function(){
      upload_googleSheet(
        tb_pdfs = ntpu$admission$assessment$tb_autobiographyStudyplan,
        sheet_name = ntpu$admission$googleSheets$sheets$googleSheetInfo[[name]]$name,
        ss = ntpu$admission$googleSheets$sheets$googleSheetInfo[[name]]$ss
        )
    }
  }

  return(ntpu)
}


# pdf module

#' Extract PR in category give the dataframe of grade page
#'
#' @param page_grade a dataframe of the grade page
#'
#' @return a numeric vector of PR rank in category
#' @export
#'
#' @examples None.
pdf_extractPR_cat <- function(page_grade){
  # 類排
  require(dplyr)
  page_grade$text %>%
    stringr::str_which(
      "類組?排名?"
    ) -> which_catRank

  page_grade$y[[which_catRank]] -> y_catRank
  page_grade %>%
    filter(
      y == y_catRank
    ) -> catRank_data

  catRank_data %>%
    filter(
      stringr::str_detect(text, "^[0-9]")
    ) %>%
    mutate(
      PR_cat = purrr::map_dbl(parse(text=text), eval)
    ) %>%
    pull(PR_cat)

}

#' Get list of pdf that is divided into 5 parts
#'
#' @param pdfdir A character of pdfs path
#'
#' @return
#' @export
#'
#' @examples None
get_dividedPDFs <- function(pdfdir){
  pdffiles <- list.files(pdfdir, pattern="\\.pdf$", full.names = T)
  list_pdfs <- vector("list", length(pdffiles))
  divisionPattern = "高中\\(職\\)在校成績證明|自傳\\(學生自述\\)|讀書計畫\\(含申請動機\\)|其他\\(有助於審查之資料\\)"

  purrr::map(
    pdffiles,
    pdftools::pdf_data
  ) -> list_PDFs
  list_pdfDivided <- vector("list", length(list_PDFs))
  for(i in seq_along(list_PDFs)){
    list_PDF_i <- list_PDFs[[i]]
    which(purrr::map_lgl(
      seq_along(list_PDF_i),
      ~{
        length(list_PDF_i[[.x]]$text)==0L
      }
    )) -> whichIsEmptyPage
    if(length(whichIsEmptyPage)!=0){
      list_PDF_i[whichIsEmptyPage] <- NULL
    }
    whichAreDivisions <- {
      purrr::map_lgl(
        seq_along(list_PDF_i),
        ~{
          if(length(list_PDF_i[[.x]]$text)==0L){
            FALSE
          } else {
            stringr::str_detect(
              list_PDF_i[[.x]]$text[[1]],
              divisionPattern)
          }
        }
      ) -> pick_divisions
      which(pick_divisions)
    }
    titleDivisions <- {
      purrr::map_chr(
        seq_along(list_PDF_i),
        ~{

          stringr::str_extract(
            list_PDF_i[[.x]]$text[[1]],
            "高中\\(職\\)在校成績證明|自傳\\(學生自述\\)|讀書計畫\\(含申請動機\\)|其他\\(有助於審查之資料\\)")
        }
      )
    }
    titleDivisions <- c("首頁", titleDivisions[whichAreDivisions])
    cut(
      seq_along(list_PDF_i),
      c(0, whichAreDivisions, Inf), right=F) -> cut_divisions
    levels_division <- levels(cut_divisions)
    purrr::map(
      seq_along(levels_division),
      ~{
        level_division_j =
          levels_division[[which(levels_division == levels_division[[.x]])]]
        part_j = list_PDF_i[which(cut_divisions == level_division_j)]
        part_j
      }
    ) -> pdfDivided_i
    setNames(pdfDivided_i, titleDivisions) -> pdfDivided_i
    list_pdfDivided[[i]] <- pdfDivided_i
  }
  purrr::map_chr(
    list_pdfDivided,
    ~{.x$首頁[[1]]$text %>%
        stringr::str_extract("(?<=學測應試號碼：)[0-9]+") %>%
        na.omit()}
  ) %>%
    setNames(
      list_pdfDivided, .
    ) -> list_pdfDivided
  return(list_pdfDivided)
}
#' Flatten tb_pdf$colnameX to a character vector
#'
#' @param tb_pdf A list of columns
#' @param colnameX A character of column name
#'
#' @return
#' @export
#'
#' @examples None
flatten_columnX = function(tb_pdfs, colnameX){
  purrr::map_chr(
    seq_along(tb_pdfs[[colnameX]]),
    ~{
      partX <- tb_pdfs[[colnameX]][[.x]]
      partX %>%
        purrr::map(
          ~purrr::pluck(.x, "text")) -> partX_text
      unlist(partX_text) -> partX_textFlattened
      paste0(partX_textFlattened, collapse = "")
    }
  ) -> tb_pdfs[[colnameX]]
  tb_pdfs
}
#' Turn list_pdfs into a tibble
#'
#' @param list_pdfs
#'
#' @return
#' @export
#'
#' @examples None
listPDFs_as_tibble <- function(list_pdfs){
  purrr::transpose(list_pdfs) -> tb_pdfs
  tb_pdfs$ID <- names(list_pdfs)
  tb_pdfs %>%
    as_tibble() -> tb_pdfs
  tb_pdfs %>%
    relocate(ID) -> tb_pdfs
  return(tb_pdfs)
}
#' Download pdfs from googleDriveURL to pdfdir
#'
#' @param googleDriveURL A character of google drive folder URL
#' @param pdfdir A character of path to local folder
#'
#' @return
#' @export
#'
#' @examples None
download_pdfs <- function(googleDriveURL, pdfdir){
  googledrive::as_dribble(googleDriveURL) -> pdf_dribble
  tb_pdfs <- googledrive::drive_ls(pdf_dribble)

  if(!dir.exists(pdfdir)) dir.create(pdfdir)
  for(i in 1:nrow(tb_pdfs)){
    pdffilename = file.path(pdfdir,paste0("pdf", i,".pdf"))
    if(!file.exists(pdffilename)) googledrive::drive_download(
      googledrive::as_id(tb_pdfs[i,]), path=pdffilename, overwrite = T)
  }
}
googleSheetURL_setup <- function(googleSheetURL, name){
  list(
    ss = googlesheets4::as_sheets_id(googleSheetURL),
    name = name
  )
}

upload_googleSheet <- function(tb_pdfs,
                               sheet_name,
                               ss,
                               columns_selected = c("自傳(學生自述)", "讀書計畫(含申請動機)")){

  googlesheets4::write_sheet(
    dplyr::select(tb_pdfs, ID, columns_selected),
    ss = ss,
    sheet = sheet_name
  )

}

