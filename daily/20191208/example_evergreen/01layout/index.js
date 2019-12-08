import React from 'react'
import ReactDOM from 'react-dom'
import { Button } from 'evergreen-ui'
import { Pane,  Text} from 'evergreen-ui'

function App(){
  return (
    <Pane
      height={120}
      width={240}
      display="flex"
      alignmentItems="center"
      justifyContent="ceonter"
      border="default"
    >
    <Text>Pane</Text>
  </Pane>
  );
}

ReactDOM.render(
    <App/>,
    document.getElementById('root')
)
