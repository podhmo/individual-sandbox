
document.querySelector("body").insertAdjacentHTML("afterbegin", `
<style>
#ext-message {
  position: sticky;
  display: inline-block;
  z-index: 2147483647;
  top: 10px;
  left: 10px;
  background-color: #aaffaa;
  opacity: 0.7;
  padding: 10px;
}
</style>
<div id="ext-message">${window.location.hostname}</div>
`);
