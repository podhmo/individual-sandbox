import argparse
import json
import os.path


class MkdirpFileType(argparse.FileType):
    def __call__(self, string):
        if string and "w" in self._mode:  # privateな紳士協定を破っているので行儀は良くない
            if os.path.dirname(string):
                os.makedirs(os.path.dirname(string), exist_ok=True)
        return super().__call__(string)


parser = argparse.ArgumentParser()
parser.add_argument("--output-file", type=MkdirpFileType("w"), default="-")
args = parser.parse_args()

data = {"msg": "hello"}
json.dump(data, args.output_file, indent=2)
