#!/usr/bin/env bash
set -euo pipefail

# Authenticate docker against the fpcloud registry.
#
# Not `fpcloud registry login`: that registers fpcloud as a docker credential
# helper and symlinks the binary next to itself, which cannot work when the
# binary lives in the nix store. It reports success, leaves a `credHelpers`
# entry naming a helper that is not on PATH, and the next push fails with
# "error getting credentials" rather than anything about the helper.
#
# The entry is removed here before logging in, because docker consults it ahead
# of the credential this writes — including one left behind by an earlier
# `fpcloud registry login` on this machine.

host=registry.cloud.fogpipe.com
config="${DOCKER_CONFIG:-$HOME/.docker}/config.json"

if [ -f "$config" ] && jq -e --arg h "$host" '.credHelpers[$h]?' "$config" >/dev/null 2>&1; then
    tmp=$(mktemp)
    jq --arg h "$host" 'del(.credHelpers[$h])' "$config" >"$tmp"
    mv "$tmp" "$config"
fi

fpcloud registry get-login-password \
    | docker login --username fogpipe --password-stdin "$host"
