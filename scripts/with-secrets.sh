#!/bin/sh
# Fogpipe Cloud hands an app its credentials as mounted files and puts none of
# them in the environment, while the apps read theirs from env variables. This
# runs as the images' entrypoint and turns the one into the other: every file
# under /secrets/env becomes the variable it is named after, so mounting the
# database's owner secret at /secrets/env/DATABASE_URL is how the app gets
# DATABASE_URL.
#
# Running it twice is harmless, which the release command relies on.
set -eu

for f in /secrets/env/*; do
  [ -f "$f" ] || continue
  export "$(basename "$f")=$(cat "$f")"
done

exec "$@"
