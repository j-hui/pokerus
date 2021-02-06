function fish_prompt --description 'Write out fancy fishy prompt'
    set -l last_pipestatus $pipestatus    # Color the prompt differently when we're root

    set -l pipestatus_string (__fish_print_pipestatus "$fish_prompt_return [" "]" "|" (set_color $fish_color_status) (set_color --bold $fish_color_status) $last_pipestatus)

    if test -n $pipestatus_string
        # tput el clears the line to ensure we write to a clean line
        echo (tput el)$pipestatus_string
    else
        echo
    end

    set -l color_cwd $fish_color_cwd
    set -l suffix $fish_prompt_suffix
    if functions -q fish_is_root_user; and fish_is_root_user
        if set -q fish_color_cwd_root
            set color_cwd $fish_color_cwd_root
        end
        set suffix $fish_prompt_suffix_root
    end

    # Date
    # set_color brblack
    set_color $fish_color_date
    echo -n "["(date "+%H:%M")"] "
    set_color $fish_color_normal

    # User
    set_color $fish_color_user
    echo -n $USER
    set_color $fish_color_normal

    set_color $fish_color_ps1
    echo -n '@'
    set_color $fish_color_normal

    # Host
    # set_color ff8700
    set_color $fish_color_host
    echo -n (prompt_hostname)
    set_color $fish_color_normal

    set_color $fish_color_ps1
    echo -n ':'
    set_color $fish_color_normal

    # PWD
    # set_color d8fa3b
    set_color $color_cwd
    echo -n (prompt_pwd)
    # echo -n (basename $PWD)
    set_color $fish_color_normal

    # VCS
    __terlar_git_prompt
    fish_hg_prompt
    set_color $fish_color_normal

    # Keep prompt symbol on its own line
    echo

    set_color $fish_color_ps1
    echo -n "$suffix "
    set_color $fish_color_normal
end

