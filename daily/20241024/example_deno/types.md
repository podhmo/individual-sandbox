# types

型の呼び出し関係

parseArgs()の型パズルを把握したい。Args,Valuesが本体かも？


```
- function parseArgs<>:
    - args:
        - args []string
        - options? ParseOptions<>:
            - TBooleans     extends boolean | undefined
            - TStrings      extends StringType
            - TCollectable  extends Collectable
            - TNegatable    extends Negatable
            - TDefaults     extends Record<string, unknown>
            - TAliases      extends Aliases<TAliasArgNames, TAliasNames>   | undefined
                - TAliasArgNames  extends string
                - TAliasNames     extends string
    - returns:
        - Args<>:
            - TArgs  extends Values<TBooleans, TStrings, TCollectable, TNegatable, TDefaults, TAliases>
            - TDoubleDash   extends boolean | undefined
```

```
- interface ParseOptions<>:
    - "--"?        TDoubleDash   extends boolean | undeined
    - alias?:      TAliases     extends Aliases | undefined
    - boolean?:    Tbooleans | ReadonlyArray<Extract<TBooleans, string
        - TBooleans extends BooleanType
    - default?:    TDefault & Deafults<TBooleans, TStrings>
        - TDeault   extends Record<string, unknown> | undefined
        - TBooleans extends BooleanType
        - TStrings  extends StringType
    - stopEarly?:   boolean
    - string?:     TStrings | ReadonlyArray<Extract<TStrings, string>>
        - TStrings  extends StringType
    - collect?:    TCollectable | ReadonlyArray<Extract<TNegatable, string>>
        - TCollectable  extends Collectable
        - TNegatable    extends Negatable
    - negatable?:  TNegatable | ReadonlyArray<Extract<TNegatable, string>>
        - TNegatable    extends Negatable
    - unknown?:    (arg: string, key?: string, value?: unknown) => unknown


- type Extract<T,U> = T extends U ? : never
    - // Extract from T those types that are assignable to U
- type Partial<T>   = { [P in keyof T]?: T[P]; };
    - // Make all properties in T optional
- interface ReadonlyArray<T> :
    - // Array<T> with readonly


- type Aliases<>   = Partial<Record<Extract<TArgNames, string>, TAliasNames>> | TreadonlyArray<TAliasNames>>>
    - TArgNames string
    - TaliasNames extends string


- type StringType  = string  | undefined
- type BooleanType = boolean | undefined
- type Collectable = string  | undefined
- type Negatable:  = string  | undefined
```

```
- type Values<> = UseTypes<TBooleans, TStrings, TCollectable> extends true ?
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
    - TBooleans    extends BooleanType
    - TStrings     extends StringType
    - TCollectable extends Collectable
    - TNegatable   extends Negatable
    - TDefault     extends Record<string, unknown> | undefined
    - TAliases     extends Aliases | undefined

- UseTypes
- AddAliases
- SpreadDefaults
- CollectValues
- RecursiveRequired
- CollectUnknownValues
- DedotRecord
```


```
- type Args<> = Args = Id<
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
    - Targs       extends Record<string, unknown> = Record<string, any>
    - TDoubleDash extends boolean | undefined     = undefined

- Id
- Requored

```
