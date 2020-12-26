#!/usr/bin/env julia

cd(mkpath("/home/matklad/downloads"))
rm(".old/", force = true, recursive = true)
entries = readdir(".")
if !isempty(entries)
   mkpath(".old/")
   for e in entries
      mv(e, ".old/$e")
   end
end
