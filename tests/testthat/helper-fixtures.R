preexisting_rounds <- function(n = 4L) {
  passcodes <- paste0("p", seq_len(n))
  list(
    react1.r01 = data.frame(
      U_PASSCODE = passcodes,
      HEALTHA05 = rep(c(0L, 1L), length.out = n),
      HEALTHA06 = rep(c(1L, 0L), length.out = n),
      check.names = FALSE
    ),
    react2.r06 = data.frame(
      U_PASSCODE = passcodes,
      HEALTHA_05 = rep(c(1L, 0L), length.out = n),
      check.names = FALSE
    )
  )
}

fixture_crosswalk <- function(n = 4L) {
  data.frame(
    REACT_ID = paste0("p", seq_len(n)),
    SUBJECT_ID = paste0("s", seq_len(n)),
    check.names = FALSE
  )
}
