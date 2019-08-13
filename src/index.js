var fs = require('fs');
var glob = require('glob');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
  Elm
} = require('./elm.js');

const app = Elm.Top.init();

// options is optional
glob("api/*.normal.json", function(er, files) {
  files.forEach(function(file) {
      fs.readFile(file, 'utf8', function(err, contents) {
        app.ports.modelInPort.send([file, contents]);
      });
    });
});

app.ports.codeOutPort.subscribe(request => {
  fs.writeFile('example.txt', request, (err) => {
    if (err) throw err;
  })
});
