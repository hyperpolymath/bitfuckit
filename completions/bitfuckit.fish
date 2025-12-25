# SPDX-License-Identifier: AGPL-3.0-or-later
# Fish completion for bitfuckit

# Main commands
complete -c bitfuckit -n __fish_use_subcommand -a auth -d 'Manage authentication'
complete -c bitfuckit -n __fish_use_subcommand -a repo -d 'Repository operations'
complete -c bitfuckit -n __fish_use_subcommand -a mirror -d 'Mirror repository to Bitbucket'
complete -c bitfuckit -n __fish_use_subcommand -a tui -d 'Launch interactive TUI'
complete -c bitfuckit -n __fish_use_subcommand -a help -d 'Show help'

# Auth subcommands
complete -c bitfuckit -n '__fish_seen_subcommand_from auth' -a login -d 'Authenticate with Bitbucket'
complete -c bitfuckit -n '__fish_seen_subcommand_from auth' -a status -d 'Show authentication status'

# Repo subcommands
complete -c bitfuckit -n '__fish_seen_subcommand_from repo' -a create -d 'Create a new repository'
complete -c bitfuckit -n '__fish_seen_subcommand_from repo' -a list -d 'List all repositories'
complete -c bitfuckit -n '__fish_seen_subcommand_from repo' -a delete -d 'Delete a repository'
complete -c bitfuckit -n '__fish_seen_subcommand_from repo' -a exists -d 'Check if repository exists'

# Repo create options
complete -c bitfuckit -n '__fish_seen_subcommand_from repo; and __fish_seen_subcommand_from create' -l private -d 'Make repository private'
complete -c bitfuckit -n '__fish_seen_subcommand_from repo; and __fish_seen_subcommand_from create' -l description -d 'Set repository description' -r
