// ../../../../with-help/build-help.ts
var _negatable_padding = 3;
function formatBooleanOptions(booleans, negatable, required, flagDescription, envvar, maxLength) {
  return booleans.map((name) => {
    const output = [];
    if (negatable.includes(name)) {
      const paddedName = name.padEnd(maxLength - _negatable_padding, " ");
      if (flagDescription[name] || flagDescription[`no-${name}`]) {
        output.push(`  --no-${paddedName} ${flagDescription[name]}`);
      } else {
        output.push(`  --no-${paddedName}${required.includes(name) ? " (required)" : ""} (default: ${name}=true)`);
      }
    } else {
      const paddedName = name.padEnd(maxLength, " ");
      if (flagDescription[name]) {
        output.push(`  --${paddedName} ${flagDescription[name]}`);
      } else {
        output.push(`  --${paddedName}${required.includes(name) ? " (required)" : ""} (default: ${name}=false)`);
      }
    }
    if (envvar[name]) {
      output.push(`    (env: ${envvar[name]})`);
    }
    return output.join("");
  });
}
function formatStringOptions(strings, collectable, defaults, required, flagDescription, envvar, maxLength) {
  return strings.map((name) => {
    const output = [];
    const paddedName = name.padEnd(maxLength, " ");
    if (flagDescription[name]) {
      output.push(
        `  --${paddedName} <string${collectable.includes(name) ? "[]" : ""}> ${flagDescription[name]}`
      );
    } else if (defaults[name] !== void 0) {
      output.push(
        `  --${paddedName} <string${collectable.includes(name) ? "[]" : ""}>${required.includes(name) ? " (required)" : ""} (default: ${name}=${JSON.stringify(defaults[name])})`
      );
    } else {
      output.push(
        `  --${paddedName} <string${collectable.includes(name) ? "[]" : ""}>${required.includes(name) ? " (required)" : ""}`
      );
    }
    if (envvar[name]) {
      output.push(`    (env: ${envvar[name]})`);
    }
    return output.join("");
  });
}
function buildUsage({ name, usageText }) {
  if (usageText) {
    return usageText;
  }
  return `Usage: ${name || "cli"} [options]`;
}
function buildHelp(options) {
  const {
    boolean,
    string,
    negatable,
    collect,
    default: defaults,
    required,
    description,
    flagDescription,
    envvar
  } = options;
  if (options.helpText) {
    return options.helpText;
  }
  const maxLength = Math.max(
    ...(boolean || []).map((name) => name.length + ((negatable || []).includes(name) ? _negatable_padding : 0)),
    ...(string || []).map((name) => name.length)
  ) + 3;
  const help = [
    buildUsage(options),
    description ? `
Description: ${description}
` : "",
    "Options:",
    ...formatStringOptions(
      string || [],
      collect || [],
      defaults || {},
      required || [],
      flagDescription || {},
      envvar || {},
      maxLength
    ),
    ...formatBooleanOptions(
      boolean || [],
      negatable || [],
      required || [],
      flagDescription || {},
      envvar || {},
      maxLength
    )
  ];
  return help.join("\n");
}

// https://jsr.io/@std/cli/1.0.6/parse_args.ts
var FLAG_REGEXP = /^(?:-(?:(?<doubleDash>-)(?<negated>no-)?)?)(?<key>.+?)(?:=(?<value>.+?))?$/s;
var LETTER_REGEXP = /[A-Za-z]/;
var NUMBER_REGEXP = /-?\d+(\.\d*)?(e-?\d+)?$/;
var HYPHEN_REGEXP = /^(-|--)[^-]/;
var VALUE_REGEXP = /=(?<value>.+)/;
var FLAG_NAME_REGEXP = /^--[^=]+$/;
var SPECIAL_CHAR_REGEXP = /\W/;
var NON_WHITESPACE_REGEXP = /\S/;
function isNumber(string) {
  return NON_WHITESPACE_REGEXP.test(string) && Number.isFinite(Number(string));
}
function setNested(object, keys, value, collect = false) {
  keys = [...keys];
  const key = keys.pop();
  keys.forEach((key2) => object = object[key2] ??= {});
  if (collect) {
    const v = object[key];
    if (Array.isArray(v)) {
      v.push(value);
      return;
    }
    value = v ? [v, value] : [value];
  }
  object[key] = value;
}
function hasNested(object, keys) {
  for (const key of keys) {
    const value = object[key];
    if (!Object.hasOwn(object, key)) return false;
    object = value;
  }
  return true;
}
function aliasIsBoolean(aliasMap, booleanSet, key) {
  const set = aliasMap.get(key);
  if (set === void 0) return false;
  for (const alias of set) if (booleanSet.has(alias)) return true;
  return false;
}
function isBooleanString(value) {
  return value === "true" || value === "false";
}
function parseBooleanString(value) {
  return value !== "false";
}
function parseArgs(args, options) {
  const {
    "--": doubleDash = false,
    alias = {},
    boolean = false,
    default: defaults = {},
    stopEarly = false,
    string = [],
    collect = [],
    negatable = [],
    unknown: unknownFn = (i) => i
  } = options ?? {};
  const aliasMap = /* @__PURE__ */ new Map();
  const booleanSet = /* @__PURE__ */ new Set();
  const stringSet = /* @__PURE__ */ new Set();
  const collectSet = /* @__PURE__ */ new Set();
  const negatableSet = /* @__PURE__ */ new Set();
  let allBools = false;
  if (alias) {
    for (const [key, value] of Object.entries(alias)) {
      if (value === void 0) {
        throw new TypeError("Alias value must be defined");
      }
      const aliases = Array.isArray(value) ? value : [value];
      aliasMap.set(key, new Set(aliases));
      aliases.forEach(
        (alias2) => aliasMap.set(
          alias2,
          /* @__PURE__ */ new Set([key, ...aliases.filter((it) => it !== alias2)])
        )
      );
    }
  }
  if (boolean) {
    if (typeof boolean === "boolean") {
      allBools = boolean;
    } else {
      const booleanArgs = Array.isArray(boolean) ? boolean : [boolean];
      for (const key of booleanArgs.filter(Boolean)) {
        booleanSet.add(key);
        aliasMap.get(key)?.forEach((al) => {
          booleanSet.add(al);
        });
      }
    }
  }
  if (string) {
    const stringArgs = Array.isArray(string) ? string : [string];
    for (const key of stringArgs.filter(Boolean)) {
      stringSet.add(key);
      aliasMap.get(key)?.forEach((al) => stringSet.add(al));
    }
  }
  if (collect) {
    const collectArgs = Array.isArray(collect) ? collect : [collect];
    for (const key of collectArgs.filter(Boolean)) {
      collectSet.add(key);
      aliasMap.get(key)?.forEach((al) => collectSet.add(al));
    }
  }
  if (negatable) {
    const negatableArgs = Array.isArray(negatable) ? negatable : [negatable];
    for (const key of negatableArgs.filter(Boolean)) {
      negatableSet.add(key);
      aliasMap.get(key)?.forEach((alias2) => negatableSet.add(alias2));
    }
  }
  const argv = { _: [] };
  function setArgument(key, value, arg, collect2) {
    if (!booleanSet.has(key) && !stringSet.has(key) && !aliasMap.has(key) && !(allBools && FLAG_NAME_REGEXP.test(arg)) && unknownFn?.(arg, key, value) === false) {
      return;
    }
    if (typeof value === "string" && !stringSet.has(key)) {
      value = isNumber(value) ? Number(value) : value;
    }
    const collectable = collect2 && collectSet.has(key);
    setNested(argv, key.split("."), value, collectable);
    aliasMap.get(key)?.forEach((key2) => {
      setNested(argv, key2.split("."), value, collectable);
    });
  }
  let notFlags = [];
  const index = args.indexOf("--");
  if (index !== -1) {
    notFlags = args.slice(index + 1);
    args = args.slice(0, index);
  }
  argsLoop:
    for (let i = 0; i < args.length; i++) {
      const arg = args[i];
      const groups = arg.match(FLAG_REGEXP)?.groups;
      if (groups) {
        const { doubleDash: doubleDash2, negated } = groups;
        let key = groups.key;
        let value = groups.value;
        if (doubleDash2) {
          if (value) {
            if (booleanSet.has(key)) value = parseBooleanString(value);
            setArgument(key, value, arg, true);
            continue;
          }
          if (negated) {
            if (negatableSet.has(key)) {
              setArgument(key, false, arg, false);
              continue;
            }
            key = `no-${key}`;
          }
          const next = args[i + 1];
          if (next) {
            if (!booleanSet.has(key) && !allBools && !next.startsWith("-") && (!aliasMap.has(key) || !aliasIsBoolean(aliasMap, booleanSet, key))) {
              value = next;
              i++;
              setArgument(key, value, arg, true);
              continue;
            }
            if (isBooleanString(next)) {
              value = parseBooleanString(next);
              i++;
              setArgument(key, value, arg, true);
              continue;
            }
          }
          value = stringSet.has(key) ? "" : true;
          setArgument(key, value, arg, true);
          continue;
        }
        const letters = arg.slice(1, -1).split("");
        for (const [j, letter] of letters.entries()) {
          const next = arg.slice(j + 2);
          if (next === "-") {
            setArgument(letter, next, arg, true);
            continue;
          }
          if (LETTER_REGEXP.test(letter)) {
            const groups2 = VALUE_REGEXP.exec(next)?.groups;
            if (groups2) {
              setArgument(letter, groups2.value, arg, true);
              continue argsLoop;
            }
            if (NUMBER_REGEXP.test(next)) {
              setArgument(letter, next, arg, true);
              continue argsLoop;
            }
          }
          if (letters[j + 1]?.match(SPECIAL_CHAR_REGEXP)) {
            setArgument(letter, arg.slice(j + 2), arg, true);
            continue argsLoop;
          }
          setArgument(letter, stringSet.has(letter) ? "" : true, arg, true);
        }
        key = arg.slice(-1);
        if (key === "-") continue;
        const nextArg = args[i + 1];
        if (nextArg) {
          if (!HYPHEN_REGEXP.test(nextArg) && !booleanSet.has(key) && (!aliasMap.has(key) || !aliasIsBoolean(aliasMap, booleanSet, key))) {
            setArgument(key, nextArg, arg, true);
            i++;
            continue;
          }
          if (isBooleanString(nextArg)) {
            const value2 = parseBooleanString(nextArg);
            setArgument(key, value2, arg, true);
            i++;
            continue;
          }
        }
        setArgument(key, stringSet.has(key) ? "" : true, arg, true);
        continue;
      }
      if (unknownFn?.(arg) !== false) {
        argv._.push(
          stringSet.has("_") || !isNumber(arg) ? arg : Number(arg)
        );
      }
      if (stopEarly) {
        argv._.push(...args.slice(i + 1));
        break;
      }
    }
  for (const [key, value] of Object.entries(defaults)) {
    const keys = key.split(".");
    if (!hasNested(argv, keys)) {
      setNested(argv, keys, value);
      aliasMap.get(key)?.forEach(
        (key2) => setNested(argv, key2.split("."), value)
      );
    }
  }
  for (const key of booleanSet.keys()) {
    const keys = key.split(".");
    if (!hasNested(argv, keys)) {
      const value = collectSet.has(key) ? [] : false;
      setNested(argv, keys, value);
    }
  }
  for (const key of stringSet.keys()) {
    const keys = key.split(".");
    if (!hasNested(argv, keys) && collectSet.has(key)) {
      setNested(argv, keys, []);
    }
  }
  if (doubleDash) {
    argv["--"] = notFlags;
  } else {
    argv._.push(...notFlags);
  }
  return argv;
}

// ../../../../with-help/parse-args.ts
var denoHandler = {
  getEnvVar(name) {
    return Deno.env.get(name);
  },
  showHelp(options) {
    console.log(buildHelp(options));
    console.log("");
  },
  terminate(options) {
    console.error(options.message);
    Deno.exit(options.code);
  }
};
function parseArgs2(args, options, handler) {
  handler = handler ?? denoHandler;
  const envvar = options.envvar ?? {};
  if (options?.unknown === void 0) {
    options = {
      ...options,
      unknown: (name) => {
        if (!name.startsWith("-")) {
          return;
        }
        if (!options.supressHelp) {
          handler.showHelp({ ...options, envvar });
        }
        handler.terminate({ message: `Unknown option: ${name}`, code: 1 });
      }
    };
  }
  const booleans = options.boolean ?? [];
  if (!booleans.includes("help")) {
    booleans.push("help");
    const flagDescription = options.flagDescription ?? {};
    flagDescription["help"] = "show help";
    options = { ...options, flagDescription, boolean: booleans };
  }
  if (options.boolean !== void 0) {
    const defaults = options.default ?? {};
    const negatable = options.negatable ?? [];
    options.boolean.forEach((name) => {
      if (defaults[name] === void 0) {
        defaults[name] = negatable.includes(name);
      }
    });
    options = { ...options, default: defaults };
  }
  const parsed = parseArgs(args, options);
  if (parsed.help) {
    handler.showHelp({ ...options, envvar });
    handler.terminate({ message: "", code: 0 });
  }
  if (options.envvar !== void 0) {
    for (const [name, envname] of Object.entries(envvar)) {
      if (envname === void 0) {
        continue;
      }
      const data = parsed;
      const value = handler.getEnvVar(envname) ?? "";
      if (value !== "") {
        if (booleans.includes(name)) {
          if (value === "1" || value.toUpperCase() === "TRUE") {
            data[name] = true;
          } else if (value === "0" || value.toUpperCase() === "FALSE") {
            data[name] = false;
          } else {
            console.debug(`envvar ${envname}=${value} is not boolean value, ignored`);
          }
        } else {
          if (options.collect?.includes(name)) {
            data[name] = [value];
          } else {
            data[name] = value;
          }
        }
      }
    }
  }
  options?.required?.forEach((name) => {
    if (parsed[name] === void 0) {
      if (!options.supressHelp) {
        handler.showHelp({ ...options, envvar });
      }
      handler.terminate({ message: `Missing required option: --${name}`, code: 1 });
    }
  });
  return parsed;
}
var Restriction = class {
  constructor(options, supressHelp = false, _handler = denoHandler) {
    this.options = options;
    this.supressHelp = supressHelp;
    this._handler = _handler;
  }
  choices(value, candidates) {
    if (!candidates.includes(value)) {
      if (!this.supressHelp) {
        this._handler.showHelp(this.options);
      }
      this._handler.terminate({ message: `"${value}" is not one of ${JSON.stringify(candidates)}`, code: 1 });
    }
    return value;
  }
};
export {
  Restriction,
  buildHelp,
  buildUsage,
  parseArgs2 as parseArgs
};
