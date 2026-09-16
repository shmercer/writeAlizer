# Isolate every test from the real user cache, including tests that use R_user_dir.
withr::local_envvar(R_USER_CACHE_DIR = withr::local_tempdir(), .local_envir = teardown_env())
withr::local_options(writeAlizer.cache_dir = NULL, writeAlizer.mock_dir = NULL,
                     .local_envir = teardown_env())
