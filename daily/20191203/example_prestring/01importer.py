import sys
from _prestring_importer import PrestringImporter

sys.meta_path.append(PrestringImporter)
import person
print(person, dir(person))
