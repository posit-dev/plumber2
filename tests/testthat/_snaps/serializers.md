# register_serializer registers a serializer correctly

    Code
      register_serializer("mock/invalid", mock_serializer, "text/mock")
    Condition
      Error in `register_serializer()`:
      ! `name` must not contain the forward slash character (/)

---

    Code
      register_serializer("...", mock_serializer, "text/mock")
    Condition
      Error in `register_serializer()`:
      ! `name` must not be "..." and "none"

---

    Code
      register_serializer("none", mock_serializer, "text/mock")
    Condition
      Error in `register_serializer()`:
      ! `name` must not be "..." and "none"

# get_serializers returns correct serializers

    Code
      get_serializers("nonexistent")
    Condition
      Error in `FUN()`:
      ! No serializer registered as nonexistent

---

    Code
      get_serializers(list(c("...", "...")))
    Condition
      Error in `get_serializers()`:
      ! "..." can only be used once in `serializers`

# device_formatter creates valid formatter

    Code
      device_formatter(bad_device)
    Condition
      Error in `device_formatter()`:
      ! `dev_open` must be a function with a `filename` or `file` argument

