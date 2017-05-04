__requires__ = 'zenmai'
import cProfile
profile = cProfile.Profile()
profile.enable()
import pstats
import re
import sys
from pkg_resources import load_entry_point

sys.argv[0] = re.sub(r'(-script\.pyw?|\.exe)?$', '', sys.argv[0])
status = load_entry_point('zenmai', 'console_scripts', 'zenmai')()
profile.disable()
profile.dump_stats("run.prof")
sys.exit(status)
