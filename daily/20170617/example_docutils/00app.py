from docutils.writers.null import Writer
from docutils.core import publish_cmdline

publish_cmdline(writer=Writer())
