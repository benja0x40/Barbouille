# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onAttach <- function(...) {

  # Initialize global options of the Barbouille package
  Barbouille::ResetOptions()

}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onDetach <- function(...) {

  # Remove global options of the Barbouille package from the R environment
  Barbouille::RemoveOptions()

}
