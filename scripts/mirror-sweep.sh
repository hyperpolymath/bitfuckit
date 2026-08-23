#!/usr/bin/env bash
# SPDX-License-Identifier: MPL-2.0
# Mirror-reconcile / deletion-prune sweep (WS3)
# Diffs GitHub repo list against downstream forges to find orphans.

set -euo pipefail

# Dependencies
if ! command -v jq &> /dev/null; then
    echo "ERROR: jq is required" >&2
    exit 1
fi
if ! command -v curl &> /dev/null; then
    echo "ERROR: curl is required" >&2
    exit 1
fi

PRUNE_CONFIRMED=false
if [[ "${1:-}" == "--prune-confirmed" ]]; then
    PRUNE_CONFIRMED=true
fi

OWNER="hyperpolymath"
echo "=> Fetching baseline repositories from GitHub for ${OWNER}..."

# Fetch GitHub repos
# Fail-closed: if curl fails, script aborts due to set -e
GITHUB_REPOS=$(curl -fsSL -H "Accept: application/vnd.github.v3+json" \
    ${GITHUB_TOKEN:+-H "Authorization: token $GITHUB_TOKEN"} \
    "https://api.github.com/users/${OWNER}/repos?per_page=100" | \
    jq -r '.[].name' | sort)

if [[ -z "$GITHUB_REPOS" ]]; then
    echo "ERROR: No repositories found on GitHub or API error. Aborting to prevent mass deletion (fail-closed)." >&2
    exit 1
fi

echo "=> Found $(echo "$GITHUB_REPOS" | wc -w) repositories on GitHub."

declare -A FORGES=(
    ["gitlab"]="https://gitlab.com/api/v4/users/${OWNER}/projects"
    ["bitbucket"]="https://api.bitbucket.org/2.0/repositories/${OWNER}"
    ["codeberg"]="https://codeberg.org/api/v1/users/${OWNER}/repos"
    ["disroot"]="https://git.disroot.org/api/v1/users/${OWNER}/repos"
    ["gitea"]="https://gitea.com/api/v1/users/${OWNER}/repos"
    ["sourcehut"]="https://git.sr.ht/api/repos"
    ["radicle"]="http://127.0.0.1:8080/api/v1/projects"
)

# Parse different API responses to get just repo names
parse_repo_names() {
    local forge="$1"
    case "$forge" in
        gitlab)       jq -r '.[].path' ;;
        bitbucket)    jq -r '.values[].slug' ;;
        codeberg|disroot|gitea) jq -r '.[].name' ;;
        sourcehut)    jq -r '.results[].name' ;;
        radicle)      jq -r '.[].name' ;;
        *)            echo "ERROR: Unknown forge $forge" >&2; exit 1 ;;
    esac
}

check_forge() {
    local forge="$1"
    local url="$2"
    local token_var="$(echo "$forge" | tr 'a-z' 'A-Z')_TOKEN"
    local token="${!token_var:-}"
    
    echo "=> Checking ${forge}..."
    
    # Custom headers based on forge
    local curl_opts=(-fsSL)
    case "$forge" in
        sourcehut) [[ -n "$token" ]] && curl_opts+=(-H "Authorization: Bearer $token") ;;
        gitlab)    [[ -n "$token" ]] && curl_opts+=(-H "PRIVATE-TOKEN: $token") ;;
        bitbucket) [[ -n "$token" ]] && curl_opts+=(-H "Authorization: Bearer $token") ;;
        codeberg|disroot|gitea) [[ -n "$token" ]] && curl_opts+=(-H "Authorization: token $token") ;;
    esac
    
    local forge_repos
    forge_repos=$(curl "${curl_opts[@]}" "$url" | parse_repo_names "$forge" | sort) || {
        echo "WARNING: Failed to fetch from $forge. Skipping." >&2
        return 0
    }
    
    # Diff logic
    local orphans
    # Only keep lines unique to forge_repos (orphans)
    orphans=$(comm -13 <(echo "$GITHUB_REPOS") <(echo "$forge_repos") || true)
    
    # Remove empty lines
    orphans=$(echo "$orphans" | sed '/^\s*$/d')
    
    if [[ -n "$orphans" ]]; then
        for orphan in $orphans; do
            echo "[ORPHAN] ${forge} : ${orphan}"
            if [[ "$PRUNE_CONFIRMED" == "true" ]]; then
                echo "         -> Pruning ${orphan} on ${forge}... (Dry run - actual API deletes not yet implemented)"
                # To be implemented with DELETE API calls per forge
            fi
        done
    else
        echo "   No orphans found."
    fi
}

for forge in "${!FORGES[@]}"; do
    check_forge "$forge" "${FORGES[$forge]}"
done

echo "=> Sweep complete."
