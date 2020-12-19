#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const regexCombiner = require('regex-combiner');

// read list file as array of lines
const listContents = fs.readFileSync(path.resolve(__dirname, './asm-instructions.txt'))
                     .toString().split('\n');

const combined = regexCombiner(listContents);
const regexPattern = combined.toString();

const modAsmFile = path.resolve(__dirname, '../../source/WebApp/components/internal/codemirror/mode-asm.ts');
let asmScriptContents = fs.readFileSync(modAsmFile).toString();

console.log('size before: ', fs.statSync(modAsmFile).size, ' bytes');

asmScriptContents = asmScriptContents.replace(/^const generatedRegex =.*$/mg, `const generatedRegex = new RegExp(${regexPattern});`);

fs.writeFileSync(modAsmFile, asmScriptContents);

console.log('size after: ', fs.statSync(modAsmFile).size, ' bytes');
