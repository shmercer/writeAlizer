# tests/testthat/setup-cache.R
# Use the default per-user cache (R_user_dir). Do NOT override writeAlizer.cache_dir
# so cache API tests can control cache roots via options/env. We just ensure we’re
# not accidentally in mock mode when running integration tests.
options(writeAlizer.mock_dir = NULL)
