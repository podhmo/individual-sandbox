import "./base.css"
import { normal } from "./code.module.css"

console.log("hello")


const el = document.createElement("pre");
el.className = normal; // css modules (as .code_normal)
el.textContent = "console.log(`hello`);"
document.body.appendChild(el)
