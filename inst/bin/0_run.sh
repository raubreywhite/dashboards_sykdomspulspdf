#!/bin/bash

COMPUTER=$(cat /tmp/computer)

(
  flock -n 200 || exit 1

  source /etc/environment

  echo
  echo
  echo
  echo
  echo "****START****sykdomspulspdf****"

  /usr/local/bin/Rscript /r/sykdomspulspdf/src/RunProcess.R

  echo "****END****sykdomspulspdf****"

) 200>/var/lock/.sykdomspulspdf.exclusivelock
