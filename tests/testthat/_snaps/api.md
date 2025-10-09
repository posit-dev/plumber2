# api function handles constructor errors correctly

    Code
      api(server_yml)
    Condition
      Error:
      ! object 'constructor' not found

# dots_to_plumber_files handles errors correctly

    Code
      dots_to_plumber_files("non_existent_file.R")
    Condition
      Error:
      ! `...` must point to existing files

---

    Code
      dots_to_plumber_files("fixtures/server1/_server.yml", prefer_yml = FALSE)
    Condition
      Warning:
      Ignoring '_server.yml' files in `...`
    Output
      character(0)

---

    Code
      dots_to_plumber_files("fixtures")
    Condition
      Error:
      ! You can at most use one '_server.yml' file to specify your API

---

    Code
      dots_to_plumber_files("fixtures/server1/_server.yml", "fixtures/minimal_api.R")
    Condition
      Warning:
      '_server.yml' found. Ignoring all other files provided in `...`
    Output
      [1] "fixtures/server1/_server.yml"

