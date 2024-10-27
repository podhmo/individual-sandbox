// Copyright 2018-2024 the Deno authors. All rights reserved. MIT license.
// This module is browser compatible.

/**
 * Command line arguments parser based on
 * {@link https://github.com/minimistjs/minimist | minimist}.
 *
 * @example
 * ```ts
 * import { parseArgs } from "@std/cli/parse-args";
 *
 * console.dir(parseArgs(Deno.args));
 * ```
 *
 * @module
 */

/** Combines recursively all intersection types and returns a new single type.
 * @internal
 */
type Id<TRecord> = TRecord extends Record<string, unknown>
  ? TRecord extends infer InferredRecord
    ? { [Key in keyof InferredRecord]: Id<InferredRecord[Key]> }
  : never
  : TRecord;

/** Converts a union type `A | B | C` into an intersection type `A & B & C`.
 * @internal
 */
type UnionToIntersection<TValue> =
  (TValue extends unknown ? (args: TValue) => unknown : never) extends
    (args: infer R) => unknown ? R extends Record<string, unknown> ? R : never
    : never;

/** @internal */
type BooleanType = boolean | string | undefined;
/** @internal */
type StringType = string | undefined;
/** @internal */
type ArgType = StringType | BooleanType;

/** @internal */
type Collectable = string | undefined;
/** @internal */
type Negatable = string | undefined;

type UseTypes<
  TBooleans extends BooleanType,
  TStrings extends StringType,
  TCollectable extends Collectable,
> = undefined extends (
  & (false extends TBooleans ? undefined : TBooleans)
  & TCollectable
  & TStrings
) ? false
  : true;

/**
 * Creates a record with all available flags with the corresponding type and
 * default type.
 * @internal
 */
type Values<
  TBooleans extends BooleanType,
  TStrings extends StringType,
  TCollectable extends Collectable,
  TNegatable extends Negatable,
  TDefault extends Record<string, unknown> | undefined,
  TAliases extends Aliases | undefined,
> = UseTypes<TBooleans, TStrings, TCollectable> extends true ?
    & Record<string, unknown>
    & AddAliases<
      SpreadDefaults<
        & CollectValues<TStrings, string, TCollectable, TNegatable>
        & RecursiveRequired<CollectValues<TBooleans, boolean, TCollectable>>
        & CollectUnknownValues<
          TBooleans,
          TStrings,
          TCollectable,
          TNegatable
        >,
        DedotRecord<TDefault>
      >,
      TAliases
    >
  // deno-lint-ignore no-explicit-any
  : Record<string, any>;

/** @internal */
type Aliases<TArgNames = string, TAliasNames extends string = string> = Partial<
  Record<Extract<TArgNames, string>, TAliasNames | ReadonlyArray<TAliasNames>>
>;

type AddAliases<
  TArgs,
  TAliases extends Aliases | undefined,
> = {
  [TArgName in keyof TArgs as AliasNames<TArgName, TAliases>]: TArgs[TArgName];
};

type AliasNames<
  TArgName,
  TAliases extends Aliases | undefined,
> = TArgName extends keyof TAliases
  ? string extends TAliases[TArgName] ? TArgName
  : TAliases[TArgName] extends string ? TArgName | TAliases[TArgName]
  : TAliases[TArgName] extends Array<string>
    ? TArgName | TAliases[TArgName][number]
  : TArgName
  : TArgName;

/**
 * Spreads all default values of Record `TDefaults` into Record `TArgs`
 * and makes default values required.
 *
 * **Example:**
 * `SpreadValues<{ foo?: boolean, bar?: number }, { foo: number }>`
 *
 * **Result:** `{ foo: boolean | number, bar?: number }`
 */
type SpreadDefaults<TArgs, TDefaults> = TDefaults extends undefined ? TArgs
  : TArgs extends Record<string, unknown> ?
      & Omit<TArgs, keyof TDefaults>
      & {
        [Default in keyof TDefaults]: Default extends keyof TArgs
          ? (TArgs[Default] & TDefaults[Default] | TDefaults[Default]) extends
            Record<string, unknown>
            ? NonNullable<SpreadDefaults<TArgs[Default], TDefaults[Default]>>
          : TDefaults[Default] | NonNullable<TArgs[Default]>
          : unknown;
      }
  : never;

/**
 * Defines the Record for the `default` option to add
 * auto-suggestion support for IDE's.
 * @internal
 */
type Defaults<TBooleans extends BooleanType, TStrings extends StringType> = Id<
  UnionToIntersection<
    & Record<string, unknown>
    // Dedotted auto suggestions: { foo: { bar: unknown } }
    & MapTypes<TStrings, unknown>
    & MapTypes<TBooleans, unknown>
    // Flat auto suggestions: { "foo.bar": unknown }
    & MapDefaults<TBooleans>
    & MapDefaults<TStrings>
  >
>;

type MapDefaults<TArgNames extends ArgType> = Partial<
  Record<TArgNames extends string ? TArgNames : string, unknown>
>;

type RecursiveRequired<TRecord> = TRecord extends Record<string, unknown> ? {
    [Key in keyof TRecord]-?: RecursiveRequired<TRecord[Key]>;
  }
  : TRecord;

/** Same as `MapTypes` but also supports collectable options. */
type CollectValues<
  TArgNames extends ArgType,
  TType,
  TCollectable extends Collectable,
  TNegatable extends Negatable = undefined,
> = UnionToIntersection<
  Extract<TArgNames, TCollectable> extends string ?
      & (Exclude<TArgNames, TCollectable> extends never ? Record<never, never>
        : MapTypes<Exclude<TArgNames, TCollectable>, TType, TNegatable>)
      & (Extract<TArgNames, TCollectable> extends never ? Record<never, never>
        : RecursiveRequired<
          MapTypes<Extract<TArgNames, TCollectable>, Array<TType>, TNegatable>
        >)
    : MapTypes<TArgNames, TType, TNegatable>
>;

/** Same as `Record` but also supports dotted and negatable options. */
type MapTypes<
  TArgNames extends ArgType,
  TType,
  TNegatable extends Negatable = undefined,
> = undefined extends TArgNames ? Record<never, never>
  : TArgNames extends `${infer Name}.${infer Rest}` ? {
      [Key in Name]?: MapTypes<
        Rest,
        TType,
        TNegatable extends `${Name}.${infer Negate}` ? Negate : undefined
      >;
    }
  : TArgNames extends string ? Partial<
      Record<TArgNames, TNegatable extends TArgNames ? TType | false : TType>
    >
  : Record<never, never>;

type CollectUnknownValues<
  TBooleans extends BooleanType,
  TStrings extends StringType,
  TCollectable extends Collectable,
  TNegatable extends Negatable,
> = UnionToIntersection<
  TCollectable extends TBooleans & TStrings ? Record<never, never>
    : DedotRecord<
      // Unknown collectable & non-negatable args.
      & Record<
        Exclude<
          Extract<Exclude<TCollectable, TNegatable>, string>,
          Extract<TStrings | TBooleans, string>
        >,
        Array<unknown>
      >
      // Unknown collectable & negatable args.
      & Record<
        Exclude<
          Extract<Extract<TCollectable, TNegatable>, string>,
          Extract<TStrings | TBooleans, string>
        >,
        Array<unknown> | false
      >
    >
>;

/** Converts `{ "foo.bar.baz": unknown }` into `{ foo: { bar: { baz: unknown } } }`. */
type DedotRecord<TRecord> = Record<string, unknown> extends TRecord ? TRecord
  : TRecord extends Record<string, unknown> ? UnionToIntersection<
      ValueOf<
        {
          [Key in keyof TRecord]: Key extends string ? Dedot<Key, TRecord[Key]>
            : never;
        }
      >
    >
  : TRecord;

type Dedot<TKey extends string, TValue> = TKey extends
  `${infer Name}.${infer Rest}` ? { [Key in Name]: Dedot<Rest, TValue> }
  : { [Key in TKey]: TValue };

type ValueOf<TValue> = TValue[keyof TValue];

/** The value returned from {@linkcode parseArgs}. */
export type Args<
  // deno-lint-ignore no-explicit-any
  TArgs extends Record<string, unknown> = Record<string, any>,
  TDoubleDash extends boolean | undefined = undefined,
> = Id<
  & TArgs
  & {
    /** Contains all the arguments that didn't have an option associated with
     * them. */
    _: Array<string | number>;
  }
  & (boolean extends TDoubleDash ? DoubleDash
    : true extends TDoubleDash ? Required<DoubleDash>
    : Record<never, never>)
>;

/** @internal */
type DoubleDash = {
  /** Contains all the arguments that appear after the double dash: "--". */
  "--"?: Array<string>;
};

/** Options for {@linkcode parseArgs}. */
export interface ParseOptions<
  TBooleans extends BooleanType = BooleanType,
  TStrings extends StringType = StringType,
  TCollectable extends Collectable = Collectable,
  TNegatable extends Negatable = Negatable,
  TDefault extends Record<string, unknown> | undefined =
    | Record<string, unknown>
    | undefined,
  TAliases extends Aliases | undefined = Aliases | undefined,
  TDoubleDash extends boolean | undefined = boolean | undefined,
> {
  "--"?: TDoubleDash;
  alias?: TAliases;
  boolean?: TBooleans | ReadonlyArray<Extract<TBooleans, string>>;
  default?: TDefault & Defaults<TBooleans, TStrings>;
  stopEarly?: boolean;
  string?: TStrings | ReadonlyArray<Extract<TStrings, string>>;
  collect?: TCollectable | ReadonlyArray<Extract<TCollectable, string>>;
  negatable?: TNegatable | ReadonlyArray<Extract<TNegatable, string>>;
  unknown?: (arg: string, key?: string, value?: unknown) => unknown;
}

export function parseArgs<
  TArgs extends Values<
    TBooleans,
    TStrings,
    TCollectable,
    TNegatable,
    TDefaults,
    TAliases
  >,
  TDoubleDash extends boolean | undefined = undefined,
  TBooleans extends BooleanType = undefined,
  TStrings extends StringType = undefined,
  TCollectable extends Collectable = undefined,
  TNegatable extends Negatable = undefined,
  TDefaults extends Record<string, unknown> | undefined = undefined,
  TAliases extends Aliases<TAliasArgNames, TAliasNames> | undefined = undefined,
  TAliasArgNames extends string = string,
  TAliasNames extends string = string,
>(
  args: string[],
  options?: ParseOptions<
    TBooleans,
    TStrings,
    TCollectable,
    TNegatable,
    TDefaults,
    TAliases,
    TDoubleDash
  >,
): Args<TArgs, TDoubleDash> {
    const argv = {}
    return argv as Args<TArgs, TDoubleDash>;
}

