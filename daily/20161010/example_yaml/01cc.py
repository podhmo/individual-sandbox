SafeConstructor.add_constructor(
        'tag:yaml.org,2002:null',
        SafeConstructor.construct_yaml_null)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:bool',
        SafeConstructor.construct_yaml_bool)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:int',
        SafeConstructor.construct_yaml_int)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:float',
        SafeConstructor.construct_yaml_float)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:binary',
        SafeConstructor.construct_yaml_binary)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:timestamp',
        SafeConstructor.construct_yaml_timestamp)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:omap',
        SafeConstructor.construct_yaml_omap)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:pairs',
        SafeConstructor.construct_yaml_pairs)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:set',
        SafeConstructor.construct_yaml_set)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:str',
        SafeConstructor.construct_yaml_str)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:seq',
        SafeConstructor.construct_yaml_seq)

SafeConstructor.add_constructor(
        'tag:yaml.org,2002:map',
        SafeConstructor.construct_yaml_map)

SafeConstructor.add_constructor(None,
        SafeConstructor.construct_undefined)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/none',
    Constructor.construct_yaml_null)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/bool',
    Constructor.construct_yaml_bool)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/str',
    Constructor.construct_python_str)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/unicode',
    Constructor.construct_python_unicode)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/bytes',
    Constructor.construct_python_bytes)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/int',
    Constructor.construct_yaml_int)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/long',
    Constructor.construct_python_long)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/float',
    Constructor.construct_yaml_float)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/complex',
    Constructor.construct_python_complex)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/list',
    Constructor.construct_yaml_seq)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/tuple',
    Constructor.construct_python_tuple)

Constructor.add_constructor(
    'tag:yaml.org,2002:python/dict',
    Constructor.construct_yaml_map)
