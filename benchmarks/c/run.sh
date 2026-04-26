#!/usr/bin/env bash
set -eu

script_dir=$(cd "$(dirname "$0")" && pwd)
bin="$("$script_dir/build.sh")"
exec "$bin" "$@"
