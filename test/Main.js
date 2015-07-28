// module Test.Main

exports.logAnything = function logAnything(str) {
  return function () {
    console.log(str);
    return {};
  }
}
