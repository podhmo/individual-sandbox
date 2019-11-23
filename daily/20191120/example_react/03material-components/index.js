import React, {Component} from 'react';
import ReactDOM from "react-dom";
import '@material/react-button/dist/button.css';
import Button from '@material/react-button';

class App extends Component {
  render() {
    return (
      <div>
        <Button
          raised
          className='button-alternate'
          onClick={() => console.log('clicked!')}
        >
          Click Me!
        </Button>
      </div>
    );
  }
}

var mountNode = document.getElementById("root");
ReactDOM.render(<App />, mountNode);

// export default App;
