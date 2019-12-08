const Button = TableauUI.Button;

function App(){
  return <div>
    <section><h2>enabled</h2>
    <Button>I am using tableau-ui</Button>
    <Button kind="primary">I am using tableau-ui</Button>
    <Button kind="outline">I am using tableau-ui</Button>
    <Button kind="destructive">I am using tableau-ui</Button>
    </section>
    <section><h2>disabled</h2>
    <Button disabled>I am using tableau-ui</Button>
    <Button disabled kind="primary">I am using tableau-ui</Button>
    <Button disabled kind="outline">I am using tableau-ui</Button>
    <Button disabled kind="destructive">I am using tableau-ui</Button>
    </section>
</div>
}

ReactDOM.render(
  <App/>,
  document.getElementById('root')
)
