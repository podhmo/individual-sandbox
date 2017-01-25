<%!
import os
name = os.getenv("name", "world")
%>
hello ${name}