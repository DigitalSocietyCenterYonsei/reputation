#!/bin/bash

# Created: 2017-02-13
# Author: jongbin

./update_plots.sh $1 && \
  cd "$1" && \
  git pull && \
  git commit -am "Update plots and plot logs" && \
  git push
