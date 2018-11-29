#!/usr/bin/env bash

# Check if R is installed
command -v Rscript 1> /dev/null 2>&1 || \
  { echo >&2 "Rscript required but it's not installed.  Aborting."; exit 1; }

# Usage message
read -r -d '' USE_MSG <<'HEREDOC'
Usage:
  pictoralist.sh -h
  pictoralist.sh [options]
  pictoralist.sh [options] spek.json  

Bitstomach reads a spek from stdin or provided file path.  Unless output dir 
is specified, it prints updated spek to stdout.

Options:
  -h | --help   print help and exit
  -s | --spek   path to spek file (default to stdin)
  -d | --data   path to data file
  --version     print package version
HEREDOC

# Parse args
PARAMS=()
while (( "$#" )); do
  case "$1" in
    -h|--help)
      echo "${USE_MSG}"
      exit 0
      ;;
    -c|--config)
      CONFIG_FILE="'${2}'"
      shift 2
      ;;
    -d|--data)
      DATA_FILE="'${2}'"
      shift 2
      ;;
    --version)
      VER_EXPR='cat(as.character(packageVersion("pictoralist")))'
      VER_STRING=$(Rscript --vanilla --default-packages=utils -e "${VER_EXPR}")
      echo "pictoralist package version: ${VER_STRING}"
      exit 0
      ;;
    --) # end argument parsing
      shift
      break
      ;;
    -*|--*=) # unsupported flags
      echo "Aborting: Unsupported flag $1" >&2
      exit 1
      ;;
    *) # preserve positional arguments
      PARAMS+=("${1}")
      shift
      ;;
  esac
done

INPUT_ARGS="spek_path=${SPEK_FILE:-NULL}, data_path=${DATA_FILE:-NULL}"

EXPR="pictoralist::main(${INPUT_ARGS})"
Rscript --vanilla --default-packages=pictoralist -e "${EXPR}"
