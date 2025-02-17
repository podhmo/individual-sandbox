(() => {
  // code.module.css
  var normal = "code_normal";

  // hello.js
  console.log("hello");
  var el = document.createElement("pre");
  el.className = normal;
  el.textContent = "console.log(`hello`);";
  document.body.appendChild(el);
})();
