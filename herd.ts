import { writeAll } from "https://deno.land/std@0.173.0/streams/mod.ts";

export async function rm(path: string) {
  try {
    await Deno.remove(path, { recursive: true });
  } catch {
    // ignore
  }
}

export async function run(strings: TemplateStringsArray, ...expr: string[]) {
  const { command, display } = cmd(strings, ...expr);
  await out(display);
  const child = await command.spawn();
  child.ref();
  const status = await child.status;
  if (status.code != 0) {
    Deno.exit(status.code)
  }
}

function cmd(
  strings: TemplateStringsArray,
  ...expr: string[]
): { command: Deno.Command; display: string } {
  const args: string[] = [];
  strings.forEach((part, i) => {
    if (i > 0) {
      args.push(expr[i - 1]);
    }
    for (const word of part.split(" ")) {
      const trimmed = word.trim();
      if (trimmed != "") {
        args.push(trimmed);
      }
    }
  });
  const display = `$ ${args.join(" ")}\n`;

  const command = new Deno.Command(args[0], { args: args.slice(1) });
  return { command, display };
}

export async function out(text: string) {
  await writeAll(Deno.stdout, new TextEncoder().encode(text));
}

export async function par(...futures: Promise<any>[]) {
  await Promise.all(futures)
}
