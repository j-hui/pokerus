#!/usr/bin/env zsh

sl () {
  echo "
                 _-====-__-======-__-========-_____-============-__
               _(                                                 _)
            OO(           _/_ _  _  _/_   _/_ _  _  _/_           )_
           0  (_          (__(_)(_) (__   (__(_)(_) (__            _)
         o0     (_                                                _)
        o         -=-___-===-_____-========-___________-===-dwb-=-
      .o                                _________
     . ______          ______________  |         |      _____
   _()_||__|| ________ |            |  |_________|   __||___||__
  (BNSF 1995| |      | |            | __Y______00_| |_         _|
 /-OO----OO^^=^OO--OO^=^OO--------OO^=^OO-------OO^=^OO-------OO^=P
#####################################################################"
}

path_add () {
    if [ -n "$1" ]; then
        case ":$PATH:" in
            *":$1:"*) :;;           # already there
            *) PATH="$1:$PATH";;    # or PATH="$PATH:$new_entry"
        esac
    fi
}

path_add ~/.local/tms
path_add ~/.cargo/bin
path_add ~/go/bin
path_add ~/.local/bin
path_add ~/.cabal/bin
path_add ~/.ghcup/bin
path_add ~/.yarn/bin
path_add ~/.config/yarn/global/node_modules/.bin
path_add /snap/bin
path_add ~/.local/go/bin
path_add ~/.platformio/penv/bin

export GEM_HOME=$HOME/gems
path_add ~/gems/bin
