#!/usr/bin/env bash
# We keep the parts of speech and noun inflections that fit in one identifier word.
set -euo pipefail
export LC_ALL=C
archive=${1:?expected a WordNet 3.0 wordnet.zip archive}
for category in noun verb adj adv; do
  unzip -p "$archive" "wordnet/index.$category" |
    awk -v category="$category" '/^[a-z]/ && $1 ~ /^[a-z][a-z0-9]*$/ {print category, $1}'
done
unzip -p "$archive" wordnet/noun.exc |
  awk '$1 ~ /^[a-z][a-z0-9]*$/ { for (field = 2; field <= NF; field++)
    if ($field ~ /^[a-z][a-z0-9]*$/ && $1 != $field) print "plural", $1, $field }'
