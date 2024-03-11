
// bun --watch ./solution.ts

const red = (str: string) => "\x1b[31m" + str + "\x1b[0m";
const green = (str: string) => "\x1b[32m" + str + "\x1b[0m";


function assertEqual<T>(a: T, b: T) {
    if (a !== b) {
        console.log(red(`🔴 FAIL: ${a} !== ${b}`));
    } else {
        console.log(green(`🟢 PASS: ${a}`));
    }
}

function mergeStr(str1: string, str2: string) {
    const chars1 = str1.split('');
    const chars2 = str2.split('');
    const result: string[] = [];
    let i = 0;
    while (i < chars1.length || i < chars2.length) {
        if (i < chars1.length) {
            result.push(chars1[i]);
        }
        if (i < chars2.length) {
            result.push(chars2[i]);
        }
        i++;
    }

    return result.join('');
}

assertEqual(2, 1 + 1);
assertEqual("AaBbCc", mergeStr("ABC", "abc"));
assertEqual("AaBb_CDE", mergeStr("AB_CDE", "ab"));
assertEqual("AaBb_cde", mergeStr("AB", "ab_cde"));
assertEqual("ABC", mergeStr("ABC", ""));
assertEqual("abc", mergeStr("", "abc"));
assertEqual("", mergeStr("", ""));
