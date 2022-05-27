#!/usr/bin/env zsh

autoload -U add-zsh-hook

# From https://github.com/felixgravila/zsh-abbr-path/blob/master/.abbr_pwd
function felix_pwd_abbr {
  base_pwd=$PWD
  tilda_notation=${base_pwd//$HOME/\~}
  pwd_list=(${(s:/:)tilda_notation})
  list_len=${#pwd_list}

  if [[ $list_len -le 1 ]]; then
    echo $tilda_notation
    return
  fi

  if [[ ${pwd_list[1]} != '~' ]]; then
    formed_pwd='/'
  fi

  firstchar=$(echo ${pwd_list[1]} | cut -c1)
  if [[ $firstchar == '.' ]] ; then
    firstchar=$(echo ${pwd_list[1]} | cut -c1,2)
  fi

  formed_pwd=${formed_pwd}$firstchar

  for ((i=2; i <= $list_len; i++)) do
    if [[ $i != ${list_len} ]]; then

      firstchar=$(echo ${pwd_list[$i]} | cut -c1)
      if [[ $firstchar == '.' ]] ; then
        firstchar=$(echo ${pwd_list[$i]} | cut -c1,2)
      fi

      formed_pwd=${formed_pwd}/$firstchar
    else
      formed_pwd=${formed_pwd}/${pwd_list[$i]}
    fi
  done

  echo $formed_pwd
  return
}

autoload -Uz vcs_info
vcs_precmd () { vcs_info }
add-zsh-hook precmd vcs_precmd

zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:*' unstagedstr '%F{red}*'   # display this when there are unstaged changes
zstyle ':vcs_info:*' stagedstr '%F{yellow}+'  # display this when there are staged changes
zstyle ':vcs_info:*' actionformats \
    '%F{5}%F{5}[%F{2}%b%F{3}|%F{1}%a%c%u%F{5}]%f '
zstyle ':vcs_info:*' formats       \
    '%F{5}%F{5}[%F{2}%b%c%u%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' enable git svn

timeinfo='%F{blue}[%T]%f'

userinfo='%B%F{cyan}${USER}%f%b'
# show user in bold red if it is root
if [[ $USER == 'root' ]]; then
  userinfo='%B%F{red}${USER}%f%b'
fi

at='%F{blue}@%f'

hostinfo='%F{cyan}$HOST%f'

colon='%F{blue}:%f'

pathinfo='%F{yellow}$(felix_pwd_abbr)%f'

vcsinfo='${vcs_info_msg_0_}'

# only show return code when non-zero
returninfo='%B%(?..%F{red}[%?] )%b'

setopt prompt_subst

prompt_precmd () {
    print -rP "${timeinfo} ${userinfo}${at}${hostinfo}${colon}${pathinfo} ${vcsinfo} ${returninfo}"
}
add-zsh-hook precmd prompt_precmd

PROMPT_ICON=▶

PS1='%B%F{blue}${PROMPT_ICON}%f%b '
