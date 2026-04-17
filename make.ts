#!/usr/bin/env -S deno run --allow-all
import $ from "jsr:@david/dax";

async function main() {
    if (Deno.args.length === 0) return help();
    switch (Deno.args[0]) {
        case "-h":
        case "help":
            return help();
        case "gg":
            await $`cargo install --path tools/gg`;
            break;
        case "config":
            await $`cargo install --path tools/config`;
            break;
        case "code":
            await $`cd ./tools/my-code && npm i && npm run install-extension`
            break;
    }
}

function help() {
    console.log(`./make.ts

gg
config
code
`);
}

await main();
