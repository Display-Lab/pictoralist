#' OnLoad
#' @export
#' @importFrom showtext showtext_auto
#' @import sysfonts
.onLoad <- function(libname, pkgname) {
  font_add_cache_google("Montserrat","Montserrat")
  showtext::showtext_auto()
}

#' Get Variant Font File
#' @description Load the font variant from cache, download, or warn on failure
#' @param url pass through to sysfont::download_font_file
#' @param repo pass through to sysfont::download_font_file
#' @param handle pass through to sysfont::download_font_file
#' @param variant name of the font face variant
#' @param name name of the font family
#' @param local_cache path to the sysfont app cache
#' @import sysfonts
get_variant_file <- function(url, repo, handle, variant, name, local_cache){
  # file path from cache
  filename <- paste(name, variant, sep="_")
  filepath <- file.path(local_cache, filename)
  # return file from cache exists
  if(!file.exists(filepath)){
    # UNSAFE call to non-exported function in sysfonts
    dl_file <- sysfonts:::download_font_file(url, repo, handle)
    file.rename(dl_file, filepath)
  }

  return(filepath)
}

#' Google Font Add from Local Cache
#' @description  Hacked out of the sysfont package. Locally caching version of
#'   font_add_google function.
#' @import sysfonts
#' @importFrom rappdirs user_cache_dir
font_add_cache_google <- function(name, family = name, regular.wt = 400,
                           bold.wt = 700, repo = "http://fonts.gstatic.com/",
                           db_cache = TRUE, handle = curl::new_handle())
{
  name   = as.character(name)[1]
  family = as.character(family)[1]
  repo   = as.character(repo)[1]

  # UNSAFE access non-exported functions from sysfonts
  db = sysfonts:::google_font_db(db_cache, handle)
  ind = sysfonts:::search_db(name, db_cache, handle)
  font = db[[2]][[ind]]

  ## Names of type variants to search in the db
  ## e.g., "regular", "700", "italic", 700italic", etc.
  regular = as.character(regular.wt)
  bold = as.character(bold.wt)
  italic = paste(regular, "italic", sep = "")
  bolditalic = paste(bold, "italic", sep = "")
  if(regular.wt == 400) {
    regular = "regular"
    italic = "italic"
  }

  ## Create cache dir if it doesn't exist or die
  local_cache_dir = file.path(rappdirs::user_cache_dir(), "pictoralist")
  dir.create(local_cache_dir, showWarnings = F, mode="2755")

  ## Download regular font face
  r.url = font$files[[regular]]
  if(is.null(r.url))
    stop(sprintf("regular (weight=%d) variant of '%s' font not found", regular.wt, name))
  r.file = get_variant_file(r.url, repo, handle, regular, name, local_cache_dir)

  ## Download bold font face
  b.url = font$files[[bold]]
  if(is.null(b.url)){
    b.file <- NULL
  }else{
    b.file <- get_variant_file(b.url, repo, handle, bold, name, local_cache_dir)
  }

  ## Download italic font face
  i.url = font$files[[italic]]
  if(is.null(b.url)){
    i.file <- NULL
  }else{
    i.file <- get_variant_file(i.url, repo, handle, italic, name, local_cache_dir)
  }

  ## Download bold-italic font face
  bi.url = font$files[[bolditalic]]
  if(is.null(b.url)){
    bi.file <- NULL
  }else{
    bi.file <- get_variant_file(bi.url, repo, handle, bolditalic, name, local_cache_dir)
  }

  font_add(family, r.file, b.file, i.file, bi.file)
}

