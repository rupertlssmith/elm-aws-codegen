var fs = require('fs');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
  Elm
} = require('./elm.js');

const app = Elm.Top.init();

fs.readFile('api/acm-2015-12-08.normal.json', 'utf8', function(err, contents) {
  app.ports.modelInPort.send(contents);
});

app.ports.codeOutPort.subscribe(request => {
  console.log(request);
  fs.writeFile('example.txt', request, (err) => {
    if (err) throw err;
  })
});
