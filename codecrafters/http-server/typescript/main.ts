import * as net from "node:net";
import * as fs from "node:fs";
import * as path from "node:path";

/*
  bun run --watch ./main.ts --directory /tmp
*/

function getArg(args: string[], name: string) {
    const index = args.indexOf(name);
    if (index == -1) {
        return undefined;
    } else {
        return args[index + 1];
    }
}

const argv: string[] = process.argv.slice(2);
const filesDir = getArg(argv, "--directory") || "/dev/null";
log("Using files dir:", filesDir);

const server = net.createServer((socket) => {
    log("Client connected");

    socket.setEncoding("utf-8");

    socket.on("data", (buf: Buffer) => {
        const [preBody, body] = buf.toString().split("\r\n\r\n");
        // log("preBody");
        // console.table(preBody);

        // log("body");
        // console.table(body);

        const [requestLine, ...headers] = preBody.split("\r\n");
        console.debug("Request line: >>%s<<", requestLine);
        console.debug("Headers:");
        console.table(headers);

        const [method, pathFull] = requestLine.split(" ");
        const [path_, ...frags] = pathFull.split("/").slice(1);
        const writeln = (s: string) => socket.write(s + "\r\n");
        const crlf = () => socket.write("\r\n");
        const getHeader = (p: string) => headers
            .find((line) => line.startsWith(p))?.split(":").at(1)?.trim();

        switch (method) {
            case "GET":
                handleGets(path_, frags, filesDir, getHeader, writeln, crlf);
                break;
            case "POST":
                handlePosts(path_, frags, body, filesDir, getHeader, writeln, crlf);
                break;
        }

        socket.end();
    });

    socket.on("error", (err) => {
        console.error("Socket error", err);
    });
});


function log(...args: any) {
    console.log("[%s]", new Date().toISOString(), ...args);
}

const port = 4221;
server.listen(port, "localhost", () => {
    log("Server listening on port: %d", port);
});

server.on("error", (err) => {
    console.error("Server error", err);
});
function handleGets(path_: string, frags: string[], filesDir: string,
    getHeader: (p: string) => string | undefined,
    writeln: (s: string) => boolean, crlf: () => boolean) {
    switch (path_) {
        case "":
            writeln("HTTP/1.1 200 OK"); // status line
            crlf(); // end headers
            break;
        case "echo":
            const echoed = frags.join("");
            writeln("HTTP/1.1 200 OK"); // status line
            writeln("Content-Type: text/plain");
            writeln("Content-Length: " + echoed.length);
            crlf(); // end headers
            writeln(echoed); // body
            break;

        case "user-agent":
            const userAgent = getHeader("User-Agent");
            if (!userAgent) {
                writeln("HTTP/1.1 500 Internal Server Error"); // status line
                crlf(); // end headers
            } else {
                writeln("HTTP/1.1 200 OK"); // status line
                writeln("Content-Type: text/plain");
                writeln("Content-Length: " + userAgent.length);
                crlf(); // end headers
                writeln(userAgent);
            }
            break;
        case "files":
            const fileName = frags[0];
            if (!fileName) {
                writeln("HTTP/1.1 400 Bad Request"); // status line
                crlf();
            } else {
                const filePath = path.join(filesDir, fileName);
                if (!fs.existsSync(filePath)) {
                    writeln("HTTP/1.1 404 Not Found"); // status line
                    crlf();
                } else {
                    writeln("HTTP/1.1 200 OK"); // status line
                    writeln("Content-Type: application/octet-stream");
                    writeln("Content-Length: " + fs.statSync(filePath).size);
                    crlf(); // end headers
                    writeln(fs.readFileSync(filePath).toString()); // body
                }
            }
            break;
        case "hello":
            writeln("HTTP/1.1 200 OK"); // status line
            crlf(); // end headers
            writeln("Hello World!"); // body
            break;
        default:
            writeln("HTTP/1.1 404 Not Found"); // status line
            writeln("X-Error: Not Found");
            crlf(); // end headers
            writeln("WAT?!?!");
            break;
    }
}

function handlePosts(path_: string, frags: string[], body: string, filesDir: string,
    getHeader: (p: string) => string | undefined, writeln: (s: string) => boolean, crlf: () => boolean) {
    switch (path_) {
        case "files":
            const fileName = frags[0];
            if (!fileName) {
                writeln("HTTP/1.1 400 Bad Request"); // status line
                crlf();
            } else {
                const filePath = path.join(filesDir, fileName);
                fs.writeFileSync(filePath, body);
                writeln("HTTP/1.1 201 Created"); // status line
                crlf(); // end headers
            }
            break;
        default:
            writeln("HTTP/1.1 404 Not Found"); // status line
            writeln("X-Error: Not Found");
            crlf(); // end headers
            writeln("WAT?!?!");
            break;
    }
}

