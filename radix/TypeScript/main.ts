import process from "node:process";
import * as readline from "node:readline";

const USAGE = "USAGE: deno run --allow-env main.ts BASE [STDIN]";

const die = (msg: string) => {
  console.error(msg);
  process.exit(1);
};

export const toBase10 = (c: string) => {
  if (c >= "0" && c <= "9") {
    return c.charCodeAt(0) - "0".charCodeAt(0);
  } else if (c >= "a" && c <= "z") {
    return c.charCodeAt(0) - "a".charCodeAt(0) + 10;
  } else if (c >= "A" && c <= "Z") {
    return c.charCodeAt(0) - "A".charCodeAt(0) + 10;
  } else {
    return -1;
  }
};


const convertToBase10 = (base: number, chars: string[]) => {
  const good: number[] = [];
  const bad: string[] = [];
  for (const c of chars) {
    const n = toBase10(c);
    if (n < 0 || n >= base || base < 2 || base > 36) {
      bad.push(c);
    } else {
      good.push(n);
    }
  }
  return { good, bad };
};

const horner = (base: number, digits: number[]) =>
  digits.reduce((acc, d) => acc * base + d, 0);

function handleLine(base: number, line: string) {
  const chars = line.split("");
  const { good, bad } = convertToBase10(base, chars);
  if (bad.length > 0) {
    console.log(" -> Bad chars: ", bad);
  } else {
    console.log(good);

    console.log(" ->", horner(base, good));
  }
  console.log();

}

const startProgram = (base: number) => {
  console.log("Base: ", base, "\n");

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
  });

  rl.on("line", (line) => {
    handleLine(base, line);
  });
};

const main = (args: string[]) => {
  const baseStr = args[0] || die(USAGE);
  const base = Number(baseStr) || die("Base must be a number");
  startProgram(base);
};

/*
Run with one of:
deno run --allow-env main.ts 16
deno task dev 16

import.meta.main is necessary to make deno test work smoothly
*/
if (import.meta.main) {
  main(process.argv.slice(2));
}

