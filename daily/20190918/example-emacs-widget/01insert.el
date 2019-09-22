(require 'widget)

(widget-insert "hello \n")hello 

(widget-create 'editable-field
              :size 13
              :format "Name: %v "
              "My Name")Name: hello hall    

(use-local-map widget-keymap)
(widget-setup)

