# offline error message is helpful (snapshot)

    Code
      .wa_ensure_file("models/x/y.bin", "file:///nowhere.bin")
    Condition
      Error in `do_fetch()`:
      ! Missing file for URL 'file:///nowhere.bin'.

