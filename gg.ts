```typescript
#!/usr/bin/env -S deno run --allow-run --allow-read --allow-write

import $ from "https://deno.land/x/dax@0.35.0/mod.ts";

type Flags = {
  _: string[];
  remote?: boolean;
  offline?: boolean;
  review?: boolean;
  branch?: string;
  from?: string;
  to?: string;
};

const main = async () => {
  const flags = parseFlags(Deno.args);
  const cmd = flags._[0];
  const args = flags._.slice(1);

  const mainBranch = await getMainBranch();
  const remote = await getRemote();

  switch (cmd) {
    case "clone":
      await clone(args[0]);
      break;
    case "worktree":
      if (args[0] === "add") await worktreeAdd(args[1]);
      if (args[0] === "init") await worktreeInit(args[1]); 
      break;
    case "amend":
      await amend();
      break;
    case "switch":
      await switchBranch(args[0], flags.remote);
      break;
    case "branch":
      await createBranch(args[0], flags.offline, mainBranch, remote);
      break;
    case "prune":
      await prune();
      break;
    case "commit":
      await commit(args[0], flags.branch);
      break;
    case "uncommit":
      await uncommit();
      break;
    case "sync":
      await sync(flags.to, remote);
      break;
    case "refresh":
      await refresh(flags.from, flags.to, mainBranch, remote);
      break;
    case "pr":
      await pr(args[0], flags.review, mainBranch, remote);
      break;
  }
};

async function getMainBranch() {
  const branches = await $`git branch`.text();
  return branches.includes(" master") ? "master" : "main";
}

async function getRemote() {
  const remotes = await $`git remote`.text();
  return remotes.includes("upstream") ? "upstream" : "origin";
}

async function clone(userRepo: string) {
  const [user, repo] = userRepo.split("/");
  if (!repo) throw new Error("Invalid user/repo format");

  await $`git clone git@github.com:matklad/${repo}`;
  
  if (user === "matklad") return;

  await $`cd ${repo}`;
  await $`git remote add upstream git@github.com:${user}/${repo}.git`;
  await $`git fetch upstream`;
  
  const branch = await $`git rev-parse --abbrev-ref HEAD`.text();
  await $`git switch ${branch}`;
  await $`git reset --hard upstream/${branch}`;
  await $`git branch --set-upstream-to=upstream/${branch}`;
}

// Add other functions similarly...

function parseFlags(args: string[]): Flags {
  // Basic flag parsing - in real code use proper arg parser
  const flags: Flags = { _: [] };
  for (let i = 0; i < args.length; i++) {
    const arg = args[i];
    if (arg.startsWith("-")) {
      const key = arg.replace(/^-+/, "");
      flags[key as keyof Flags] = true;
    } else {
      flags._.push(arg);
    }
  }
  return flags;
}

if (import.meta.main) {
  main();
}
```

This is a basic skeleton - you'll need to implement the remaining functions following similar patterns. The dax library provides a nicer interface for shell commands than raw Deno.

Key differences from the Rust version:
- Uses dax for shell commands
- Simpler flag parsing
- More async/await based
- Doesn't need explicit error handling like Rust
- Directory operations are simplified

Let me know if you want me to implement any specific functions.
