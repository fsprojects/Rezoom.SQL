# Gem manifest for the GitHub-Pages Jekyll build of the Rezoom.SQL docs.
#
# The github-pages metagem pins every Jekyll dependency to the exact
# version GitHub's Pages builder runs in production, so what you see
# locally with `bundle exec jekyll serve` matches what gets published.
#
# Just-the-Docs is consumed as a remote theme (configured in _config.yml),
# not as a gem dependency here — jekyll-remote-theme is included in
# github-pages and fetches it at build time.

source "https://rubygems.org"

gem "github-pages", group: :jekyll_plugins

# Local-preview prerequisites. Harmless on the GitHub builder; needed on
# Windows / macOS dev machines.
gem "wdm", ">= 0.1.0" if Gem.win_platform?
gem "tzinfo-data", platforms: [:mingw, :x64_mingw, :mswin, :jruby]
