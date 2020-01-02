var fs = require('fs');
var glob = require('glob');
const path = require('path');
const { spawn } = require('child_process');

const { Elm } = require('./elm.js');
const app = Elm.Top.init();

//---=== Find and run elm-format.

const formatLocations = [
  // Running locally during dev of this package
  path.join(__dirname, '..', 'node_modules', '.bin', 'elm-format'),

  // Installed in the npm .bin folder.
  // webpack-dev-server should be along side it
  path.join(__dirname, 'elm-format'),
];

const findFormat = () => formatLocations
  .filter(fs.existsSync)[0];

const formatFile = (filename) => {
  const format = findFormat();

  if (!format) {
    console.error(`Unable to find elm-format among:
${formatLocations.join('\n')}`);
    return;
  }

  spawn(format, [filename, '--yes'], {
    cwd: process.cwd(),
    env: process.env,
    stdio: ['ignore', 'ignore', process.stderr],
  });
};

//---=== Find API specs and process them.

// glob("api/*.normal.json", function(er, files) {
//   files.forEach(function(file) {
//       var filename = file.slice('api/'.length);
//
//       fs.readFile(file, 'utf8', function(err, contents) {
//         app.ports.modelInPort.send([filename, contents]);
//       });
//     });
// });

const specs = [
  // "athena-2017-05-18.normal.json",
  // "batch-2016-08-10.normal.json",
  // "cloudformation-2010-05-15.normal.json",
  // "cloudtrail-2013-11-01.normal.json",
  "cognito-identity-2014-06-30.normal.json",
  "cognito-idp-2016-04-18.normal.json"
  // "dynamodb-2012-08-10.normal.json",
  // "ec2-2016-11-15.normal.json",
  // "iam-2010-05-08.normal.json",
  // "lambda-2015-03-31.normal.json",
  // "rds-2014-10-31.normal.json",
  // "route53-2013-04-01.normal.json",
  // "s3-2006-03-01.normal.json"
]

specs.forEach(function(item, index) {
  var filename = 'api/' + item;

  fs.readFile(filename, 'utf8', function(err, contents) {
    app.ports.modelInPort.send([filename, contents]);
  });
});

app.ports.codeOutPort.subscribe(request => {

  console.log("=== Processed ===: " + request[0] + "\n");

  request[2].forEach(function(item, index) {
    console.log(item + "\n");
  });

  var filename = 'stubs/AWS/' + request[0];

  fs.writeFile(filename, request[1], (err) => {
    if (err) throw err;
  })

  formatFile(filename);
});
