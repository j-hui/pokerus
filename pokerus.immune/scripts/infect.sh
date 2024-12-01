#!/usr/bin/env sh

set -e

TAG="[POKERUS]"
pecho() { echo "$TAG           " "$@" >&2 ; }
wecho() { echo "$TAG  WARN     " "$@" >&2 ; }
eecho() { echo "$TAG  ERROR    " "$@" >&2 ; }

# Sort out this realpath issue
if [ -x "/opt/homebrew/opt/coreutils/libexec/gnubin/realpath" ]; then
  # this is probably Apple Silicon macOS; prefer GNU version of realpath
  export PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
elif [ -x "/usr/local/homebrew/opt/coreutils/libexec/gnubin/realpath" ]; then
  # this is probably x86 macOS; prefer GNU version of realpath
  export PATH="/usr/local/homebrew/opt/coreutils/libexec/gnubin:$PATH"
fi
# else, hope for the best and use whatever is already on PATH.
# note that this does fail with non-GNU versions of realpath

pecho "Using realpath: $(which realpath)"
pecho "Using readlink: $(which readlink)"

if ! realpath --relative-to=/bin /etc >/dev/null 2>/dev/null ; then
  eecho "realpath implementation not supported.\nIf on macOS, please run 'brew install coreutils'"
  exit 1
fi

# Echoes "." if we want to ignore it
ignores() {
    case "$1" in
        .DS_STORE)
            echo .
            ;;
        *~|*.swp)
            echo .
            ;;
        ..)
            echo .
            ;;
        README.md)
            echo .
            ;;
        *)
            echo "$1"
            ;;
    esac
}

infect() {
    src="$1"
    dst="$2"

    if ! [ -d "$src" ]; then
        wecho "Source $src is not a directory"
        return
    fi

    pecho "Infecting $dst with contents of $src..."

    if ! [ -d "$dst" ]; then
        pecho "Destination $dst does not yet existing, creating now..."
        mkdir -p "$dst"
    fi

    ls -1 -a "$src" | while read -r f ; do
        # Skip ignored items
        case "$(ignores "$f")" in .|..|"") continue ;; esac

        if [ -h "$dst/$f" ]; then
            # Some symbolic link already exists at dst
            if [ "$(readlink -e "$dst/$f")" = "$(readlink -e "$src/$f")" ]; then
                wecho "Already symlinked: $dst/$f -> $src/$f. Skipping."
            else
                eecho "A symlink already exists at $dst/$f. Skipping."
                eecho "  $dst/$f -> $(readlink "$dst/$f")"
            fi
        elif [ -e "$dst/$f" ]; then
            eecho "A file already exists at $dst/$f. Skipping."
        else
            # We're good to go, symlink away
            pecho "Symlinking $src/$f..."
            ln -s "$(realpath --relative-to="$dst" "$src/$f")" "$dst/$f"
        fi
    done
}

if [ "$#" -lt 1 ]; then
    echo "usage: $0 [<boxes> ...]"
    exit 1
fi

for dir in "$@"; do
    dir="$(realpath "$dir")"

    pecho "Infecting $dir..."

    case "$dir" in
        *.immune)
            pecho "ignoring $dir"
            ;;
        *.config)
            infect "$dir" "$HOME/.config"
            ;;
        *.local)
            infect "$dir" "$HOME/.local"
            ;;
        *.local-share)
            infect "$dir" "$HOME/.local/share"
            ;;
        *)
            infect "$dir" "$HOME"
            ;;
    esac
done
