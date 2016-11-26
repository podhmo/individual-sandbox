const SwaggerParser = require('swagger-parser');
const parser = new SwaggerParser();


parser.parse("main.yaml")
  .then(function(api){
    return parser.bundle(api);
  })
  .then(function(api){
    console.log(JSON.stringify(api, null, 2));
  })
  .catch(function(err){
    console.log(err);
  })
;
