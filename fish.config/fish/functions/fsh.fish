function fsh -d "ssh using fish"
    if not set -q argv[1]
        echo "No arguments given."
        return 1
    end
    ssh $argv fish
end
