import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = 0, view = view, update = update }

type Msg = Increment | Decrement
type alias Model = Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      model + 1

    Decrement ->
      model - 1


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , div [] [ text (toString model) ]
    , button [ onClick Increment ] [ text "+" ]
    ]