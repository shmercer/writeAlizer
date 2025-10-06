# offline error message is helpful (snapshot)

    Code
      .wa_ensure_file("models/x/y.bin", "file:///nowhere.bin")
    Condition
      Error in `.wa_ensure_file()`:
      ! Cannot download 'y.bin' while offline. Set options(writeAlizer.offline = FALSE) to enable.

