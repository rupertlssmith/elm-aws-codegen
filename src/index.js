var fs = require('fs');
var glob = require('glob');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
  Elm
} = require('./elm.js');

const app = Elm.Top.init();

// glob("api/*.normal.json", function(er, files) {
//   files.forEach(function(file) {
//       fs.readFile(file, 'utf8', function(err, contents) {
//         app.ports.modelInPort.send([file, contents]);
//       });
//     });
// });

// fs.readFile('api/dynamodb-2012-08-10.normal.json', 'utf8', function(err, contents) {
//   app.ports.modelInPort.send(['api/dynamodb-2012-08-10.normal.json', contents]);
// });
// fs.readFile('api/athena-2017-05-18.normal.json', 'utf8', function(err, contents) {
//   app.ports.modelInPort.send(['api/athena-2017-05-18.normal.json', contents]);
// });

const specs = [
  "athena-2017-05-18.normal.json",
  "dynamodb-2012-08-10.normal.json"
]

specs.forEach(function(item, index) {
  var filename = 'api/' + item;

  console.log(filename);

  fs.readFile(filename, 'utf8', function(err, contents) {
    app.ports.modelInPort.send([filename, contents]);
  });
});

app.ports.codeOutPort.subscribe(request => {
  fs.writeFile('stubs/AWS/' + request[0], request[1], (err) => {
    if (err) throw err;
  })
});
