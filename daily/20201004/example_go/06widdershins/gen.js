'use strict';

const inputName = "00doc/openapi.json";
const outputName = "widershins.output.md";

const converter = require('widdershins');
const fs = require('fs');

let options = {};
options.templateCallback = myCallBackFunction;

function myCallBackFunction(templateName, stage, data) {
  let statusString = "Template name: " + templateName + "\n";
  statusString += "Stage: " + stage + "\n";
  data.append = statusString;
  return data;
}

const apiObj = JSON.parse(fs.readFileSync(inputName));

converter.convert(apiObj, options)
.then(str => {
  fs.writeFileSync(outputName, str, 'utf8');
});
