// module Node.SimpleRequest.Foreign

var isOptionPrimFn = function isOptionPrimFn(k, v) {
  return [[k, v]];
}

var requestImpl = function requestImpl(secure, opts, msg, onErr, onSucc) {
  return function() {
    if (secure) {
      lib = require('https');
    } else {
      lib = require('http');
    }
    var req = lib.request(opts, function(res) {
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
