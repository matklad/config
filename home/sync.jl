#!/usr/bin/env julia

config_home = "/home/matklad/config/home"
for (root, _dirs, files) in walkdir(config_home)
    for file in files
        file == "sync.jl" && continue

        abs_path = joinpath(root, file)
        rel_path = relpath(abs_path, config_home)
        dest = joinpath("/home/matklad/", rel_path)

        println("$abs_path -> $dest")

        rm(dest, force=true)
        mkpath(dirname(dest))
        symlink(abs_path, dest)
    end
end
