val = "G"

define _echo3
   $(eval val = $1)
   echo $(val)
   echo $(val)
   echo $(val)
endef

default:
	$(call _echo3,x)
	$(call _echo3,y)
