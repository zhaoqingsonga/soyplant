# =============================================================================
# Patch: Fix insert_ck_rows bug where is_ck column mismatch causes rbind error
# Bug: insert_ck_rows adds is_ck=1 to inserted ck rows but original rows lack
#      is_ck column, causing "numbers of columns of arguments do not match" error
# Fix: Ensure sub_df has is_ck=0 column before rbind
# =============================================================================

# Fixed version of insert_ck_rows
.insert_ck_rows_fixed <- function(df, ck) {
  lapply(df, function(sub_df) {
    n_insert <- length(ck)
    if (n_insert == 0)
      return(sub_df)
    insert_rows <- lapply(ck, function(iname) {
      mdf <- sub_df[1, , drop = FALSE]
      mdf[] <- NA
      if ("id" %in% names(mdf))
        mdf$id <- NA
      if ("stageid" %in% names(mdf))
        mdf$stageid <- NA
      if ("name" %in% names(mdf))
        mdf$name <- iname
      mdf$is_ck <- 1L
      mdf
    })
    # PATCH: ensure sub_df also has is_ck column
    if (!"is_ck" %in% names(sub_df))
      sub_df$is_ck <- 0L
    do.call(rbind, c(list(sub_df), insert_rows))
  })
}

# Apply patch when package loads
.onLoad <- function(libname, pkgname) {
  assignInNamespace("insert_ck_rows", .insert_ck_rows_fixed, ns = pkgname)
}
