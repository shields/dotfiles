#!/bin/sh
export LANG=en_US.UTF-8
curl --silent "https://tgftp.nws.noaa.gov/data/observations/metar/stations/$1.TXT" \
    | tr -c 'A-Z0-9 /-+$\n' '�' \
    | /usr/local/bin/gsed -e '1d' -e 's/ RMK.*//' -e 's/\([0-9]SM \)/\1\n/'
