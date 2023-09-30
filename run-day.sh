#! /usr/bin/env bash

while getopts "d:fp" opt; do
    case "$opt" in
        d)
            day="$OPTARG"
            ;;
        f)
            full='--ta "full"'
            ;;
        p)
            profile='--profile'
            ;;
    esac
done
if [[ -z "$day" ]]; then
    echo "No day supplied!"
    exit 1
fi
day=$(printf ':day%02d-test' "$day")

stack test $day $profile $full