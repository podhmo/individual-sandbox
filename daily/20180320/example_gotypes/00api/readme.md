## type and underlying

|name|type|underlying|
|:--|:--|:--|
|S1|S1|string|
|S2|S2|string|
|S3|string|string|

## go/types's api (for type)

|T0|T1|ConvertibleTo(T0,T1)|ConvertibleTo(T1,T0)|AssignableTo(T0,T1)|AssignableTo(T1,T0)|Identical(T0,t1)
|:--|:--|:--|:--|:--|:--|:--|
|S1|S2|true|true|false|false|false|
|S1|string|true|true|false|false|false|
|S1|*S1|false|false|false|false|false|
|S1|*S2|false|false|false|false|false|
|S1|*string|false|false|false|false|false|
|S1|string|true|true|false|false|false|
|S1|int|false|true|false|false|false|
|S1|*string|false|false|false|false|false|
|S2|string|true|true|false|false|false|
|S2|*S1|false|false|false|false|false|
|S2|*S2|false|false|false|false|false|
|S2|*string|false|false|false|false|false|
|S2|string|true|true|false|false|false|
|S2|int|false|true|false|false|false|
|S2|*string|false|false|false|false|false|
|string|*S1|false|false|false|false|false|
|string|*S2|false|false|false|false|false|
|string|*string|false|false|false|false|false|
|string|string|true|true|true|true|true|
|string|int|false|true|false|false|false|
|string|*string|false|false|false|false|false|
|*S1|*S2|true|true|false|false|false|
|*S1|*string|true|true|false|false|false|
|*S1|string|false|false|false|false|false|
|*S1|int|false|false|false|false|false|
|*S1|*string|true|true|false|false|false|
|*S2|*string|true|true|false|false|false|
|*S2|string|false|false|false|false|false|
|*S2|int|false|false|false|false|false|
|*S2|*string|true|true|false|false|false|
|*string|string|false|false|false|false|false|
|*string|int|false|false|false|false|false|
|*string|*string|true|true|true|true|true|
|string|int|false|true|false|false|false|
|string|*string|false|false|false|false|false|
|int|*string|false|false|false|false|false|

## go/types's api (for interface)

|iface|T0|AssertableTo(iface,T0)|Implements(T0,iface)|
|:--|:--|:--|:--|
|fmt.Stringer|S1|true|true|
|fmt.Stringer|S2|false|false|
|fmt.Stringer|string|false|false|
|fmt.Stringer|*S1|true|true|
|fmt.Stringer|*S2|true|true|
|fmt.Stringer|*string|false|false|
|fmt.Stringer|string|false|false|
|fmt.Stringer|int|false|false|
|fmt.Stringer|*string|false|false|
