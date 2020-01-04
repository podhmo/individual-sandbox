exports.handler = async function(event, context) {
  console.log("ENVIRONMENT VARIABLES\n" + JSON.stringify(process.env, null, 2)["PWD"])
  console.log("EVENT\n" + JSON.stringify(event, null, 2))
  return context.logStreamName
}
