// Copyright 2019 Yusuke Sakurai. All rights reserved. MIT license.
import { createRouter } from "https://servestjs.org/@v0.24.2/router.ts";
const router = createRouter();
router.handle("/", async req => {
  await req.respond({
    status: 200,
    headers: new Headers({
      "content-type": "text/plain"
    }),
    body: "Hello, Servest!"
  });
});
router.listen(":8899");
