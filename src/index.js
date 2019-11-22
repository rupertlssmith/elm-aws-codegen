var fs = require('fs');
var glob = require('glob');

const { Elm } = require('./elm.js');
const app = Elm.Top.init();

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
  "athena-2017-05-18.normal.json",
  "batch-2016-08-10.normal.json",
  "cloudformation-2010-05-15.normal.json",
  "cloudtrail-2013-11-01.normal.json",
  "cognito-identity-2014-06-30.normal.json",
  "dynamodb-2012-08-10.normal.json",
  "iam-2010-05-08.normal.json",
  "lambda-2015-03-31.normal.json",
  "rds-2014-10-31.normal.json",
  "route53-2013-04-01.normal.json",
  "s3-2006-03-01.normal.json"
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

  fs.writeFile('stubs/AWS/' + request[0], request[1], (err) => {
    if (err) throw err;
  })
});
