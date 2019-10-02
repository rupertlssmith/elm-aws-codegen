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

fs.readFile('api/iam-2010-05-08.normal.json', 'utf8', function(err, contents) {
  app.ports.modelInPort.send(['api/iam-2010-05-08.normal.json', contents]);
});

app.ports.codeOutPort.subscribe(request => {
  fs.writeFile('example.txt', request, (err) => {
    if (err) throw err;
  })
});
