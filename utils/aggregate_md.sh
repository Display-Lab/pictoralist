#!/usr/bin/env bash

# WHEREAMI

# Handle resolving symlinks to this script.
# Using ls instead of readlink, because bsd and gnu flavors
# have different behavior.

# Start by assuming it was the path invoked.
THIS_SCRIPT="$0"

while [ -h "$THIS_SCRIPT" ] ; do
  ls=`ls -ld "$THIS_SCRIPT"`
  # Drop everything prior to ->
  link=`expr "$ls" : '.*-> \(.*\)$'`
  if expr "$link" : '/.*' > /dev/null; then
    THIS_SCRIPT="$link"
  else
    THIS_SCRIPT=`dirname "$THIS_SCRIPT"`/"$link"
  fi
done

# Get path to the scripts directory.
SCRIPT_DIR=$(dirname "${THIS_SCRIPT}")

TEMPLATES_DIR=${SCRIPT_DIR}/../inst/templates

JQ_FILTER=$(printf "%s" \
  '{ "@context": (reduce .[]["@context"] as $ctx ({}; . * $ctx)),' \
  '"@graph":   [(.[]["@graph"]   | add)] }' )

# Combine all template json files. 
find ${TEMPLATES_DIR} -type f -name '*.json' |\
  xargs -I {} cat {} |\
  jq --slurp "${JQ_FILTER}"
