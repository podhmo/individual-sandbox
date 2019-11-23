import React, {Component} from 'react';
import ReactDOM from "react-dom";
import '@material/react-button/dist/button.css';
import Button from '@material/react-button';

import '@material/react-card/dist/card.css';
import Card, {
  CardPrimaryContent,
  CardMedia,
  CardActions,
  CardActionButtons,
  CardActionIcons
} from "@material/react-card";

class App extends Component {
  render() {
    return (
      <div>
      <section>
        <h2>Button</h2>
        <Button
          raised
          className='button-alternate'
          onClick={() => console.log('clicked!')}
        >
        Click Me!
      </Button>
     </section>
     <section>
      <h2>Card</h2>
      <Card className="my-card">
        <CardPrimaryContent>
        <h1>Header</h1>
        <CardMedia square imageUrl='./my/fancy/image.png' />
        </CardPrimaryContent>

        <CardActions>
        <CardActionButtons>
        <button>Click Me</button>
        </CardActionButtons>

        <CardActionIcons>
        <i>Click Me Too!</i>
        </CardActionIcons>
        </CardActions>
        </Card>
      </section>
     </div>
    );
  }
}

var mountNode = document.getElementById("root");
ReactDOM.render(<App />, mountNode);

// export default App;
