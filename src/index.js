var fs = require('fs');

//const { Elm }  = require('./Top.elm'); -- do it this way if using webpack-loader
const {
  Elm
} = require('./elm.js');

const app = Elm.Top.init();

app.ports.modelInPort.send("model");

app.ports.codeOutPort.subscribe(request => {
  console.log(request);
  fs.writeFile('example.txt', request, (err) => {
    if (err) throw err;
  })
});
