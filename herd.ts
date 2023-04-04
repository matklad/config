import { writeAll } from "https://deno.land/std@0.173.0/streams/mod.ts";

export async function rm(path: string) {
  try {
    await Deno.remove(path, { recursive: true });
  } catch {
    // ignore
  }
}

export async function $(strings: TemplateStringsArray, ...expr: string[]) {
  const { command, display } = cmd(strings, expr);
  await out(display);
  const child = command.spawn();
  child.ref();
  const status = await child.status;
  if (status.code != 0) {
    Deno.exit(status.code);
  }
}

$.read = async (strings: TemplateStringsArray, ...expr: string[]) => {
  const { command, display } = cmd(strings, expr, true);
  const output = await command.output();
  if (!output.success) {
    Deno.exit(output.code);
  }
  const result = new TextDecoder().decode(output.stdout);
  return result.trimEnd()
};

function cmd(
  strings: TemplateStringsArray,
  expr: string[],
  pipe_stdout: boolean = false,
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

  const options: Deno.CommandOptions = { args: args.slice(1) };
  if (pipe_stdout) {
    options.stdout = "piped";
    options.stderr = "inherit";
  };
  const command = new Deno.Command(args[0], options);
  return { command, display };
}

export async function out(text: string) {
  await writeAll(Deno.stdout, new TextEncoder().encode(text));
}

export async function par<T>(...futures: Promise<T>[]): Promise<Awaited<T>[]>{
  return await Promise.all(futures);
}
