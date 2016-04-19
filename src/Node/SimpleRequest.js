// module Node.SimpleRequest

exports.collapseStream = function (stream) {
  return function (onErr) {
    return function (onSucc) {
      return function () {
        var body = "";
        stream.on("data", function (chunk) {
          body += chunk;
        });
        stream.on("end", function () {
          onSucc(body)();
        });
        stream.on("error", function (err) {
          onErr(err)();
        });
        return {};
      }
    }
  }
}
