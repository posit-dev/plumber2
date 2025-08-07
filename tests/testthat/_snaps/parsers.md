# register_parser registers a parser correctly

    Code
      register_parser("mock/invalid", mock_parser, "text/mock")
    Condition
      Error in `register_parser()`:
      ! `name` must not contain the forward slash character (/)

---

    Code
      register_parser("...", mock_parser, "text/mock")
    Condition
      Error in `register_parser()`:
      ! `name` must not be "..." and "none"

---

    Code
      register_parser("none", mock_parser, "text/mock")
    Condition
      Error in `register_parser()`:
      ! `name` must not be "..." and "none"

# get_parsers returns correct parsers

    Code
      get_parsers("nonexistent")
    Condition
      Error in `FUN()`:
      ! No parser registered as nonexistent

---

    Code
      get_parsers(list(c("...", "...")))
    Condition
      Error in `get_parsers()`:
      ! "..." can only be used once in `parsers`

