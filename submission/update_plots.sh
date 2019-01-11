#!/usr/bin/env bash

# Created: 2017-10-12
# Author: jongbin

function copy_figures {
  local PUB="$1"
  if [[ -z "$PUB" ]]; then
    echo "No publication target specified"
    exit 1
  fi

  if [[ -d "$PUB" ]]; then
    #statements
    echo Copying figures to "$PWD/$PUB"
    shift
    for tgt in "$@"
    do
      cp ../figs/"$tgt".pdf "$PUB"/.
      # cp ../fig/"$tgt".log "$PUB/$tgt".txt
    done
  else
   echo "Publication target $PWD/$PUB does not exist."
   exit 1
  fi
}

copy_figures \
  "$1" \
  attack_results
