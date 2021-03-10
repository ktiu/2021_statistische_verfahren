############
## Colors ##
############

goethe_blue <- "#00618F"
light_gray <- "#f8f6f5"
sand_gray <- "#e4e3dd"
dark_gray <- "#4d4b46"
purple <- "#860047"
emo_red <- "#b3062c"
mustard_yellow <- "#e3ba0f"
green <- "#737c45"
magenta <- "#ad3b76"
orange <- "#c96215"
sun_yellow <- "#f7d926"
light_green <- "#a5ab52"
light_blue <- "#48a9da"

th_css <- "background-color: var(--sand_gray);
  color: var(--goethe_blue);
  font-weight: 600"

###########
## Setup ##
###########

setup <- function() {
  library(pubIHG)
  knitr::opts_chunk$set(
      echo = FALSE,
      warning = FALSE,
      message = FALSE,
      cache = T,
      comment = NA,
      fig.align = "center"
  )
  options(OutDec = ",")
  target <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  if (!is.null(target)) {
    if (target == "revealjs") {
      knitr::opts_chunk$set(fig.height = 5, fig.width = 9)
    }
    if (target == "latex") {
      knitr::opts_chunk$set(
        out.width = ".49\\linewidth",
        fig.width = 5,
        fig.height = 3,
        fig.pos = "h")
    }
  }
}

#####################################
## Render wrapper with target file ##
#####################################

render <- function(input_file) {
  yaml_params <- rmarkdown::yaml_front_matter(input_file)
  if (is.null(yaml_params$output)) {
    output_format <- "pubIHG::pdf_document"
  } else {
    output_format <- "all"
  }
  lang <- ifelse(!is.null(yaml_params$lang), yaml_params$lang, "de")
  stopifnot(lang %in% c("en", "de"))
  defaults <- paste0("~/.pubIHG/defaults/", lang, ".yaml")
  output_options <- list(defaults = defaults,
                         lang = lang)
  if (is.null(yaml_params$softcopy)) yaml_params$softcopy <- FALSE
  if (yaml_params$softcopy) {
    output_options <- c(output_options,
                        logo = "color",
                        watermark = TRUE,
                        signature = TRUE)
  }
  output_dir <- NULL
  output_file <- NULL
  if (!is.null(yaml_params$target)) {
    if (stringr::str_sub(yaml_params$target, -1) == "/") {
      output_dir <- yaml_params$target
    } else {
      output_file <- yaml_params$target
    }
  }
  auto_filename <- rmarkdown::render(
    input_file,
    output_format = output_format,
    output_options = output_options,
    output_dir = output_dir,
    output_file = output_file
  )
}

###############################
## Build resources in reveal ##
###############################

html_dependency_pub_ihg <- function() {
  htmltools::htmlDependency(
    name = "pubIHG",
    version = "0.1",
    src = system.file("render", package = "pubIHG")
  )
}

reveal <- function(defaults = NULL) {
  before_body <- system.file("before_body.html", package = "pubIHG")
  after_body  <- system.file("after_body.html", package = "pubIHG")
  template    <- system.file("template.html", package = "pubIHG")
  pandoc_args <- c("--html-q-tags")
  if (!is.null(defaults)) {
    pandoc_args <- c(pandoc_args, "--metadata-file", defaults)
  }
  revealjs::revealjs_presentation(
    template = template,
    includes = rmarkdown::includes(
                 before_body = before_body,
                 after_body = after_body
               ),
    transition         = "slide",
    reveal_plugins     = c("notes", "chalkboard"),
    self_contained     = FALSE,
    slide_level        = 2,
    extra_dependencies = list(
        rmarkdown::html_dependency_jquery(),
        html_dependency_pub_ihg()),
    fig_caption = TRUE,
    reveal_options = list(chalkboard      = list(toggleNotesButton = FALSE,
                                                 toggleChalkboardButton = FALSE),
                          slideNumber     = TRUE,
                          showSlideNumber = "all",
                          progress        = FALSE,
                          center          = TRUE,
                          controls        = FALSE),
    pandoc_args = pandoc_args)
}

rtf_document <- function(defaults = NULL) {
  rtf_template <- system.file("template.rtf", package = "pubIHG")
  pandoc_args <- c("--template", rtf_template)
  if (!is.null(defaults)) {
    pandoc_args <- c(pandoc_args, "--metadata-file", defaults)
  }
  rmarkdown::rtf_document(pandoc_args = pandoc_args)
}

word_document <- function(lang = "de") {
  docx_template <- system.file(
    sprintf("template_%s.docx", lang),
    package = "pubIHG"
  )
  rmarkdown::word_document(
    reference_docx = docx_template
  )
}

pdf_document <- function(lang            = "de",
                         defaults        = NULL,
                         number_sections = F,
                         keep_tex        = F) {
  texfile <- system.file("template.tex", package = "pubIHG")
  pandoc_args <- c(#"--filter", "pandoc-eqnos",
                  "--variable", "csquotes=true")
  if (!is.null(defaults)) {
    pandoc_args <- c(pandoc_args, "--metadata-file", defaults)
  }
  rmarkdown::pdf_document(
    number_sections = number_sections,
    keep_tex = keep_tex,
    fig_width = 5,
    fig_height = 3,
    template = texfile,
    pandoc_args = pandoc_args)
}

letter <- function(lang      = "de",
                   defaults  = NULL,
                   signature = F) {
  texfile     <- system.file("letter.tex", package = "pubIHG")
  pandoc_args <- c(#"--filter", "pandoc-eqnos",
                   "--variable", "csquotes=true")
  if (!is.null(defaults)) {
    pandoc_args <- c(pandoc_args, "--metadata-file", defaults)
  }
  if (signature) {
    signature_file <- "~/.pubIHG/signature.png"
    pandoc_args <- c(pandoc_args,
                     rmarkdown::pandoc_variable_arg("signature_file",
                                                    signature_file))
  }
  rmarkdown::pdf_document(
    number_sections = F,
    fig_width       = 5,
    fig_height      = 3,
    template        = texfile,
    pandoc_args     = pandoc_args)
}

vote <- function(text, tick=T) {
  text    <- gsub("(\\n[0-9])\\.", "\\1`", text)
  choices <- read.table(text=text, sep="`")
  choices <- cbind(LETTERS[1:nrow(choices)],choices)
  choices <- cbind(
    choices,
    ifelse(
      (choices[,2]==1 & tick),
      "<span class='checkmark fragment'>✔</span>",
      ""))
  html.rows <- paste0("
  <div>
    <div class='votespace'>
      <div class='voteletter'>", choices[, 1], ")</div>
      <div class='votewrapper'>
        <div class='votebar'></div>
        <div class='voteprop'></div>
      </div>
    </div>
    <div>", choices[, 3], choices[, 4], "</div>
  </div>", collapse = "")
  paste0("
<div class='votecontainer'>
<div class='votemsg'></div>
<div class='votable'>",
    html.rows, "
</div>
</div>") %>% htmltools::HTML()
}

frag <- function(x, order=NA) {
  if (is.na(order)) {
    dfi <- ""
  } else {
    dfi <- sprintf(" data-fragment-index=%s ", order)
  }
  sprintf("<div class='fragment'%s>%s</div>", dfi, x)
}


overview <- function(file, show, highlight = NA, fragment = NA, dfi = 1) {
  read.table(file, header = T, sep = "|") %>%
    mutate(
      display = sprintf("%s&nbsp;(%s)", trimws(Titel), Sitzung),
      nodisplay = sprintf(
        "<span style='color:rgba(0,0,0,0)'>%s</span>",
        display),
      display = ifelse(
        Sitzung %in% highlight,
        sprintf("<span style='font-weight:600'>%s</span>", display),
        display),
      display = ifelse(
        Sitzung %in% fragment,
        sprintf("<span class='fragment' data-fragment-index=%s style='color:var(--purple); font-weight:600'>%s</span>",
                dfi,
                display),
        display),
      display = ifelse(Sitzung %in% c(show, highlight, fragment),
        display,
        nodisplay)) %>%
    group_by(Zeile, Spalte) %>%
    mutate(content = paste(unique(display), collapse = "<br>")) %>%
    select(Zeile, Spalte, content) %>%
    summarize(content = first(content)) %>%
    spread(Spalte, content, sep = "_") %>%
    ungroup() %>%
    add_row(Zeile = 3, Spalte_1 = "&nbsp;", Spalte_2 = "&nbsp;", Spalte_3 = "&nbsp;") %>%
    mutate(Zeile = c("Univariat", "Bivariat", "Multivariat")) %>%
    column_to_rownames("Zeile") %>%
    kable(escape = F, col.names = c("Deskriptiv", "Schätzend", "Testend")) %>%
    kableExtra::add_header_above(c(" " = 2, "Schließend" = 2)) %>%
    column_spec(1, extra_css = "background-color: var(--sand_gray); font-weight:600; color:var(--goethe_blue)")
}

draw.curve <- function(interval, colorf, xlabel, curve, n) {
  mode <- optimize(curve, interval, maximum = T)
  ggplot(data = data.frame(x = interval), aes(x)) +
    stat_function(fun = curve,
                  n = n,
                  xlim =  colorf,
                  geom = "area",
                  fill = light_blue) +
    geom_vline(xintercept = colorf[2],
               color = goethe_blue,
               linetype = "dashed") +
    stat_function(fun = curve,
                  n = n,
                  color = goethe_blue) +
    scale_y_continuous(breaks = NULL, expand=c(0,0), limits=c(0,mode$objective*1.05)) +
    scale_x_continuous(breaks = c(0), expand=c(0,0)) +
    annotate(geom="blank", x=interval[1], y=mode$objective*1.05) +
    xlab("") +
    ylab("") +
    theme(text = element_text(size=10),
          axis.text = element_text(size=10))
}

sum_costs <- function(cost_tibble) {
prep <- dplyr::mutate(cost_tibble, 
                      tex = sprintf("%s & %s €\\\\",
                                      posten,
                                      format(betrag, nsmall = 2)))
  knitr::asis_output(
    paste(
      c("\\begin{tabularx}{\\linewidth}{Xr}",
        prep$tex,
        "\\hline",
        sprintf("\\textbf{Summe} & \\textbf{%s €}\\\\",
                format(sum(prep$betrag), nsmall = 2)),
        "\\hline\\hline",
        "\\end{tabularx}"),
      collapse = "\n")
  )
}

generate_numbers <- function(n, mean, sd, min, max, precision) {
  result <- rnorm(n, mean, sd) %>% round(precision)
  if(min(result) >= min & max(result) <= max) {
    return(result)
  } else {
    return(generate_numbers(n, mean, sd, min, max, precision))
  }
}
