#!/usr/bin/env bash
set -euo pipefail

# Print the Fogpipe organization id to deploy into.
#
# It is the org's opaque id, not its readable name: image paths are built from
# it, and the readable name is renameable so nothing derives from it. With one
# organization there is nothing to choose, so this answers it. With several,
# it refuses and lists them rather than picking.

if ! orgs=$(fpcloud org list -o json 2>/dev/null); then
    echo "could not reach fpcloud — run 'fpcloud login' first" >&2
    exit 1
fi

ids=$(echo "$orgs" | grep -o '"short_id": *"[^"]*"' | sed 's/.*: *"//; s/"$//')
count=$(echo "$ids" | grep -c . || true)

case "$count" in
    0)
        echo "no organizations — ask an operator to grant you one" >&2
        exit 1
        ;;
    1)
        echo "$ids"
        ;;
    *)
        echo "several organizations, name one with ORG=<org-id>:" >&2
        echo "$ids" | sed 's/^/  /' >&2
        exit 1
        ;;
esac
