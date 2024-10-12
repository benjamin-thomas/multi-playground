import * as readline from "node:readline";

const USAGE = "Usage: ./Main BASE [STDIN]";

const die = (msg) => {
    console.error(msg);
    process.exit(1);
};

export const mainLoop = (handleLine) => () => {
    const args = process.argv.slice(2);
    const baseStr = args[0] || die(USAGE);
    const base = parseInt(baseStr) || die("Base must be an integer");

    console.log("Base: " + baseStr, "\n");

    const rl = readline.createInterface({
        input: process.stdin,
        output: process.stdout
    });

    rl.on("line", function (line) {
        handleLine(base)(line)();
    });
};

export const myLog = (s) => () => console.log(s);
