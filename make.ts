#!/usr/bin/env -S deno run --allow-all
import $ from "jsr:@david/dax";

switch (Deno.args[0]) {
    case "gg":
        await $`cargo install --path tools/gg`;
        break;
    case "config":
        await $`cargo install --path tools/config`;
        break;
}
