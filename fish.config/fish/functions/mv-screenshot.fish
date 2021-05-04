function mv-screenshot --description 'Move most recent screenshot'
    set img (find ~/Pictures/screenshots -type f | sort -r | fzf --layout=reverse)
    # TODO: preview image?

    if ! test -e $img
        return
    end

    kitty +kitten icat --align left $img

    # TODO: confirm
    if not set -q argv[1]
        mv $img .
    else
        mv $img $argv[1]
    end
end
