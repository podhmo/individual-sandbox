import React from "react";
import ReactDomServer from "react-dom/server";

// see: https://deno.land/manual@v1.25.3/node/cdns
// see: https://esm.sh/#use-cli-script

/* setup
$ deno run -A -r https://esm.sh init
$ deno task npm:add react react-dom
# $ deno task npm:update
*/

export const View = (props: { msg: string }) => (
  <div className="deno">{props.msg}</div>
);

const s = ReactDomServer.renderToString(<View msg="hello world"></View>);
console.log(s);


