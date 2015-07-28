// module Node.SimpleRequest

var http = require('http');

var isOptionPrimFn = function isOptionPrimFn(k, v) {
  return [[k, v]];
}

var requestImpl = function requestImpl(opts, msg, onErr, onSucc) {
  return function() {
    var req = http.request(opts, function(res) {
      var body = "";
      res.on("data", function(chunk) {
        body += chunk;
      });
      res.on("end", function() {
        res.body = body;
        onSucc(res)();
      });
      return {};
    });
    req.on("error", onErr);
    req.write(msg);
    req.end();
    return {};
  };
}

module.exports = {
  isOptionPrimFn: isOptionPrimFn,
  requestImpl: requestImpl
}
