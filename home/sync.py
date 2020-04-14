#!/usr/bin/env not-bash

home = Path("/home/matklad")
home_cfg = Path('/home/matklad/config/home')
for dir, _dirs, files in os.walk(home_cfg):
    for file in files:
        if file == "sync.py":
            continue

        abs_path = Path(dir) / file
        rel_path = abs_path.relative_to(home_cfg)
        dest = home / rel_path

        print(abs_path, '->', dest)

        try:
            dest.unlink()
        except:
            pass
        mkdir(dest.parent)
        os.symlink(abs_path, dest)
