crosstable <- function(x, format, sums=T, expected=F, chisq=F, varnames=T, row.order=NULL, col.order=NULL, caption=NULL, df.only=F){
  sink("nul")
  xt <- gmodels::CrossTable(x[,1],x[,2], expected = T)
  sink()
  ts <- ""
  chisq.contribs <- c()
  if(is.null(row.order)){row.order <- rownames(xt$t)}
  if(is.null(col.order)){col.order <- colnames(xt$t)}
  first.col <- row.order
  if(format=="latex") {
    nbsp <- "~"
  } else {
    nbsp <- "&nbsp;"
  }
  col.names <- c(ifelse(varnames, paste0(colnames(x)[1], nbsp, "$\\downarrow$"), ""), col.order)
  if (format == "latex"){
    building <- list(
        begin_cell = "\\makecell[tr]{",
        end_cell = "}",
        begin_expected = "(",
        end_expected = ")",
        begin_chisq = "\\textcolor{goethe_blue}{",
        end_chisq = "}",
        linebreak = "\\\\"
      )
  } else if (format == "html") {
    if(is.numeric(expected)) {
      begin_expected <- sprintf('<span class="fragment" data-fragment-index=%s>(', expected)
    } else {
      begin_expected <- "<span>("
    }
    if(is.numeric(chisq)) {
       begin_chisq = sprintf('<span class="blue-text fragment" data-fragment-index=%s>', chisq)
    } else {
      begin_chisq = "<span class='blue-text'>"
    }
    building = list(
      begin_cell = "",
      end_cell = "",
      begin_expected = begin_expected,
      end_expected = ")<span>",
      begin_chisq = begin_chisq,
      end_chisq = "</span>",
      linebreak = "<br>"
    )
  } else { stop("Invalid format.") }
  kable.out <- function(df){
    kable.params <- list(
      x = df,
      format = format,
      align = "r",
      escape = F)
    if(format=="latex"){
      kable.params$booktabs = T
      kable.params$longtable = F
    }
    if(!is.null(caption)) kable.params$caption=caption
    k <- do.call(kable, kable.params)
    if(varnames){
      header.above <- c(1,2)
      if(sums) header.above <- c(header.above, 1)
      header.names <- c(" ", colnames(x)[2])
      if(sums) header.names <- c(header.names, " ")
      names(header.above) <- header.names
      k <- k %>%
               add_header_above(header = header.above, line = F) %>%
               column_spec(1, border_right = T)
    }
    if(format == "html"){
      return(k %>%
             column_spec(1, extra_css = "background:var(--sand_gray); color:var(--goethe_blue);font-weight:600") %>%
             row_spec(nrow(xt$t)+1, bold = T) %>%
             column_spec(ncol(xt$t)+2, bold=T))
    }
    if(sums){
      k <- k %>%
        column_spec(1 + ncol(xt$t), border_right = T) %>%
        row_spec(nrow(xt$t), hline_after = T)
    }
    return(k %>%
             column_spec(1, border_right=T) %>%
             kable_styling())
  }
  for(row in row.order){
    for(col in col.order){
      obs <- xt$t[row,col]
      exp <- round(xt$chisq$expected[row,col],2)
      chisq.contrib <- round((obs-exp)^2/exp,3)
      chisq.contribs <- c(chisq.contribs, chisq.contrib)
      ts <- paste0(ts, building$begin_cell, obs )
      if(expected){ ts <- paste0(ts, building$linebreak, building$begin_expected, format(exp, nsmall=2), building$end_expected)}
      if(chisq){ ts <- paste0(ts, building$linebreak, building$begin_chisq, format(chisq.contrib,nsmall=3), building$end_chisq)}
      ts <- paste0(ts, building$end_cell, "|")
    }
    if(sums){ts <- paste0(ts, sum(xt$t[row,]))}
    ts <- paste0(ts, "
")
  }
  if(sums){
    for(col in col.order){
      ts <- paste0(ts, sum(xt$t[,col]),"|")
    }
    ts <- paste0(ts, sum(xt$t))
    col.names <-c(col.names, "")
    first.col <- c(first.col, "")
  }
  df <- cbind(data.frame(first.col), read.table(text = ts, stringsAsFactors = F, quote = "", sep="|"))
  colnames(df) <- col.names
  if(df.only){
    return(df)
  } else {
    return(list(df=kable.out(df), contrib=chisq.contribs))
  }
}
