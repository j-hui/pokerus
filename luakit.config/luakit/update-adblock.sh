#!/usr/bin/env bash

set -euf

# if [[ -z "$XDG_DATA_HOME" ]]; then
DATADIR="$HOME/.local/share"
# else
#   DATADIR="$XDG_DATA_HOME"
# fi

mkdir -p "$DATADIR/luakit/adblock/"
cd "$DATADIR/luakit/adblock/"

updatelist () {
  local listname
  local listurl
  listurl="$1"
  listname="$(basename "${listurl}")"

  if [[ -f ${listname} ]]; then
    cp -p "${listname}" "${listname}.b"
  fi

  wget -N --connect-timeout=10 --tries=20 --retry-connrefused --waitretry=5 "${listurl}"

  local ret
  ret=$?

  if (( ret == 0 )); then
    rm -f "${listname}.b"
    echo "Successfully downloaded $listname from $listurl"
  else
    echo "Error: failed to download $listname from $listurl"
    if [[ -f ${listname}.b ]]; then
      cp -p "${listname}.b" "${listname}"
      rm -f "${listname}.b"
    fi
    exit "$ret"
  fi
}

updatelist "https://easylist-downloads.adblockplus.org/easylist.txt"
updatelist "https://easylist.to/easylist/easyprivacy.txt"
#https://secure.fanboy.co.nz/fanboy-cookiemonster.txt
#https://easylist.to/easylist/fanboy-social.txt
#https://easylist-downloads.adblockplus.org/easylist_noelemhide.txt
updatelist "https://secure.fanboy.co.nz/fanboy-annoyance.txt"
updatelist "https://easylist-downloads.adblockplus.org/antiadblockfilters.txt"
