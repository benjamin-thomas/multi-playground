#!/usr/bin/env node

import { readFileSync } from 'fs';
import { execSync } from 'child_process';
import { basename } from 'path';
import { createRequire } from 'module';

const USAGE = 'Usage: ./run.mjs ELM_FILEPATH INPUT_FILEPATH';
const die = (msg) => { console.error(msg); console.error(USAGE); process.exit(1); };

const require = createRequire(import.meta.url);
const args = process.argv.slice(2);

const elmFile = args[0] || die('Missing mandatory arg: ELM_FILEPATH');
const inputFile = args[1] || die('Missing mandatory arg: INPUT_FILEPATH');

const moduleName = basename(elmFile, '.elm');
const jsFile = `/tmp/${moduleName}.js`;

execSync(`elm make ${elmFile} --output=${jsFile}`, { stdio: 'inherit' });

const input = readFileSync(inputFile, 'utf8');
const { Elm } = require(jsFile);

Elm[moduleName].init({ flags: input });
