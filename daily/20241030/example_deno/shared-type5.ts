import { parseArgs as originalParseArgs } from "jsr:@std/cli/parse-args";

// Define the structure of command-line options
interface CommandLineOptions<StringKeys extends readonly string[], BooleanKeys extends readonly string[], RequiredKeys extends readonly string[]> {
    string: StringKeys;
    boolean: BooleanKeys;
    required: RequiredKeys;
}

// Define the structure of parsed arguments
type ParsedArguments<T extends CommandLineOptions<any, any, any>> = {
    [K in T['string'][number]]: K extends T["required"][number] ? string : string | undefined
} & {
    [K in T['boolean'][number]]: K extends T["required"][number] ? boolean : boolean | undefined
};

// Parse command-line arguments
function parseCommandLineArgs<StringKeys extends readonly string[], BooleanKeys extends readonly string[], RequiredKeys extends readonly string[]>(
    args: string[],
    options: CommandLineOptions<StringKeys, BooleanKeys, RequiredKeys>
): ParsedArguments<CommandLineOptions<StringKeys, BooleanKeys, RequiredKeys>> {
    // Display help if --help is present
    if (args.includes("--help")) {
        console.log(generateHelpMessage(options));
        Deno.exit(0);
    }

    const parsedArgs = originalParseArgs(args, options) as ParsedArguments<CommandLineOptions<StringKeys, BooleanKeys, RequiredKeys>>;

    // Check for required arguments
    for (const key of options.required) {
        if (parsedArgs[key] === undefined) {
            console.error(`--${key} is required`);
            Deno.exit(1);
        }
    }
    return parsedArgs;
}

// Generate help message
function generateHelpMessage<StringKeys extends readonly string[], BooleanKeys extends readonly string[], RequiredKeys extends readonly string[]>(
    options: CommandLineOptions<StringKeys, BooleanKeys, RequiredKeys>
): string {
    const helpLines = [];
    for (const key of options.string) {
        helpLines.push(`--${key} <string>${options.required.includes(key) ? " (required)" : ""}`);
    }
    for (const key of options.boolean) {
        helpLines.push(`--${key}${options.required.includes(key) ? " (required)" : ""}`);
    }
    return helpLines.join("\n");
}

// Define command-line options
const commandLineOptions: CommandLineOptions<["name", "version"], ["color"], ["name", "color"]> = {
    string: ["name", "version"],
    boolean: ["color"],
    required: ["name", "color"]
} as const;

// Parse the command-line arguments
const parsedArgs = parseCommandLineArgs(Deno.args, commandLineOptions);

console.dir(parsedArgs, { depth: null });

// Output parsed arguments
console.log(parsedArgs.name, parsedArgs.version, parsedArgs.color);