#!/bin/bash
set -euo pipefail

# Run the sqitch migrations against the managed database on Fogpipe Cloud.
#
# The database is cluster-internal and never internet-exposed, so this opens a
# tunnel through the API, reads the connection URL it prints, and deploys
# through that. The tunnel is closed on the way out, including on failure.

ORG=${ORG:?set ORG=<org-id>}
PROJECT=${PROJECT:-organizeparty}
DATABASE=${DATABASE:-events}

tunnel_log=$(mktemp)
cleanup() {
    [[ -n "${tunnel_pid:-}" ]] && kill "$tunnel_pid" 2>/dev/null
    rm -f "$tunnel_log"
}
trap cleanup EXIT

fpcloud db connect "$DATABASE" --org "$ORG" --project "$PROJECT" >"$tunnel_log" 2>&1 &
tunnel_pid=$!

# The URL carries live credentials the platform rotates out of band, so it is
# read from the tunnel rather than assembled here.
url=""
for _ in $(seq 1 60); do
    url=$(grep -o 'postgres://[^[:space:]]*' "$tunnel_log" | head -1 || true)
    [[ -n "$url" ]] && break
    kill -0 "$tunnel_pid" 2>/dev/null || { cat "$tunnel_log" >&2; echo "the tunnel exited before printing a connection url" >&2; exit 1; }
    sleep 1
done

if [[ -z "$url" ]]; then
    cat "$tunnel_log" >&2
    echo "no connection url after 60s" >&2
    exit 1
fi

sqitch --chdir db deploy "db:pg://${url#postgres://}" --mode change --verify
