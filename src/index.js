var fs = require('fs');
var glob = require('glob');

const { Elm } = require('./elm.js');
const app = Elm.Top.init();

// glob("api/*.normal.json", function(er, files) {
//   files.forEach(function(file) {
//       fs.readFile(file, 'utf8', function(err, contents) {
//         app.ports.modelInPort.send([file, contents]);
//       });
//     });
// });

const specs = [
  "athena-2017-05-18.normal.json",
  "dynamodb-2012-08-10.normal.json",
  "iam-2010-05-08.normal.json",
  "lambda-2015-03-31.normal.json"
]

specs.forEach(function(item, index) {
  var filename = 'api/' + item;

  fs.readFile(filename, 'utf8', function(err, contents) {
    app.ports.modelInPort.send([filename, contents]);
  });
});

app.ports.codeOutPort.subscribe(request => {
  fs.writeFile('stubs/AWS/' + request[0], request[1], (err) => {
    if (err) throw err;
  })
});
