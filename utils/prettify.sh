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

# Create .pretty files 
find ${TEMPLATES_DIR} -type f -name '*.json' |\
  xargs -I {} sh -c 'jq . "$1" > "$1".pretty' -- {}

# Rename pretty files over originals
OLD_DIR=$(pwd)
cd ${TEMPLATES_DIR}
rename -f -e 's/.pretty//' *.pretty
cd ${OLD_DIR}

