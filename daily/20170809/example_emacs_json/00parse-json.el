(require 'json)

(json-read-file "./person.json"); => ((name . "foo") (age . 20) (skills . ["x" "y" "z"]))
