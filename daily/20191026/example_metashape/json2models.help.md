```console
usage: json2models [-h] [-m <Model name> [<JSON files> ...]]
                   [-l <Model name> <JSON key> <JSON file>] [-o FILE]
                   [-f {base,attrs,dataclasses,custom}] [-s {nested,flat}]
                   [--datetime] [--strings-converters]
                   [--disable-unicode-conversion] [--merge MERGE [MERGE ...]]
                   [--dict-keys-regex RegEx [RegEx ...]]
                   [--dict-keys-fields FIELD NAME [FIELD NAME ...]]
                   [--code-generator CODE_GENERATOR]
                   [--code-generator-kwargs [NAME=VALUE [NAME=VALUE ...]]]

Convert given json files into Python models.

optional arguments:
  -h, --help            show this help message and exit
  -m <Model name> [<JSON files> ...], --model <Model name> [<JSON files> ...]
                        Model name and its JSON data as path or unix-like path pattern.
                        '*',  '**' or '?' patterns symbols are supported.
                        
  -l <Model name> <JSON key> <JSON file>, --list <Model name> <JSON key> <JSON file>
                        Like -m but given json file should contain list of model data.
                        If this file contains dict with nested list than you can pass
                        <JSON key> to lookup. Deep lookups are supported by dot-separated path.
                        If no lookup needed pass '-' as <JSON key>
                        
                        I.e. for file that contains dict {"a": {"b": [model_data, ...]}} you should
                        pass 'a.b' as <JSON key>.
                        
  -o FILE, --output FILE
                        Path to output file
                        
  -f {base,attrs,dataclasses,custom}, --framework {base,attrs,dataclasses,custom}
                        Model framework for which python code is generated.
                        'base' (default) mean no framework so code will be generated without any decorators
                        and additional meta-data.
                        If you pass 'custom' you should specify --code-generator argument
                        
  -s {nested,flat}, --structure {nested,flat}
                        Models composition style. By default nested models become nested Python classes.
                        
  --datetime            Enable datetime/date/time strings parsing.
                        Warn.: This can lead to 6-7 times slowdown on large datasets.
                               Be sure that you really need this option.
                        
  --strings-converters  Enable generation of string types converters (i.e. IsoDatetimeString or BooleanString).
                        
  --disable-unicode-conversion, --no-unidecode
                        Disabling unicode conversion in fields and class names.
                        
  --merge MERGE [MERGE ...]
                        Merge policy settings. Default is 'percent_70 number_10' (percent of field match
                        or number of fields match).
                        Possible values are:
                        'percent[_<percent>]' - two models had a certain percentage of matched field names.
                                                Default percent is 70%. Custom value could be i.e. 'percent_95'.
                        'number[_<number>]'   - two models had a certain number of matched field names.
                                                Default number of fields is 10.
                        'exact'               - two models should have exact same field names to merge.
                        
  --dict-keys-regex RegEx [RegEx ...], --dkr RegEx [RegEx ...]
                        List of regular expressions (Python syntax).
                        If all keys of some dict are match one of them
                        then this dict will be marked as dict field but not nested model.
                        Note: ^ and $ tokens will be added automatically but you have to
                        escape other special characters manually.
  --dict-keys-fields FIELD NAME [FIELD NAME ...], --dkf FIELD NAME [FIELD NAME ...]
                        List of model fields names that will be marked as dict fields
                        
  --code-generator CODE_GENERATOR
                        Absolute import path to GenericModelCodeGenerator subclass.
                        Works in pair with '-f custom'
                        
  --code-generator-kwargs [NAME=VALUE [NAME=VALUE ...]]
                        List of code generator arguments (for __init__ method).
                        Each argument should be in following format:
                            argument_name=value or "argument_name=value with space"
                        Boolean values should be passed in JS style: true | false
                        

```
