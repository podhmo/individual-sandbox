type Type = PrimitiveType | ArrayType | MapType | OneOfType | StructType | Named
type PrimitiveType = { tag: "primitive", name: string }
type ArrayType = { tag: "array", value: Type }
type MapType = { tag: "map", key: Type, value: Type }
type OneOfType = { tag: "oneof", values: Type[] }
type StructType = { tag: "struct", fields: Field[] }
type Named = { tag: "named", name: string, type: Type, comment?: string }

type Field = { name: string, required: boolean, type: Type, comment?: string }

function EmitType(ob: Type, buf: string[], spacer: string, indent: string) {
    switch (ob.tag) {
        case 'named':
            buf.push(ob.name);
            break;
        case 'primitive':
            buf.push(ob.name);
            break;
        case 'array':
            buf.push('[]')
            EmitType(ob.value, buf, spacer, indent)
            break;
        case 'map':
            buf.push('map[')
            EmitType(ob.key, buf, spacer, indent)
            buf.push(']')
            EmitType(ob.value, buf, spacer, indent)
            break;
        case 'oneof':
            buf.push('(')
            for (const x of ob.values) {
                EmitType(x, buf, spacer, indent)
                buf.push(" | ")
            }
            buf.pop()
            buf.push(")")
            break;
        case 'struct':
            buf.push('struct {\n')
            for (const f of ob.fields) {
                if (f.comment) {
                    for (const line of f.comment.split("\n")) {
                        buf.push(indent + spacer)
                        buf.push("// ")
                        buf.push(line)
                        buf.push("\n")
                    }
                }

                buf.push(indent + spacer)
                buf.push(f.name)
                buf.push(" ")
                EmitType(f.type, buf, spacer, indent + spacer)
                if (f.required) {
                    buf.push("!")
                }
                buf.push("\n")
            }
            buf.push(indent)
            buf.push("}")
            break;
        default:
            const _: never = ob;
            break;
    }
}

function EmitDecl(ob: Type, buf: string[]) {
    switch (ob.tag) {
        case "named":
            buf.push("\n")
            if (ob.comment) {
                for (const line of ob.comment.split("\n")) {
                    buf.push("// ")
                    buf.push(line)
                    buf.push("\n")
                }
            }

            buf.push("type ")
            buf.push(ob.name)
            buf.push(" ")
            EmitType(ob.type, buf, "\t", "")
            break;
        default:
            throw new Error("unexpected type")
    }
}

function ToPrimitive(name: string): PrimitiveType {
    return { tag: "primitive", name }
}
function ToArray(value: Type): ArrayType {
    return { tag: "array", value }
}
function ToMap(key: Type, value: Type): MapType {
    return { tag: "map", key, value }
}
function ToOneOf(x: Type, ...xs: Type[]): OneOfType {
    return { tag: "oneof", values: [x, ...xs] }
}
function ToNamed(name: string, type: Type): Named {
    return { tag: "named", name, type }
}
function ToStruct(...fields: Field[]): StructType {
    return { tag: "struct", fields: fields }
}



function P(t: Type) {
    const buf = [];
    EmitType(t, buf, "\t", "")
    console.log(buf.join(""))
}
function PD(t: Type) {
    const buf = [];
    EmitDecl(t, buf)
    console.log(buf.join(""))
}

const string = ToPrimitive("string");
const int = ToPrimitive("int");
{

    P(string)
    P(ToArray(string))
    P(ToMap(ToPrimitive("string"), ToArray(ToOneOf(int, string))))
}
{
    const User = ToNamed("User", ToStruct(
        { name: "Name", type: string, required: true, comment: "name of user" },
        { name: "Age", type: int, required: false, comment: "age of user" },
    ))
    User.comment = "User is user object"
    PD(User)

    const Account = ToNamed("Account", ToStruct(
        { name: "ID", type: string, required: true },
        { name: "User", type: User, required: true },
    ))
    PD(Account)

    const Account2 = ToNamed("Account2", ToStruct(
        { name: "ID", type: string, required: true },
        { name: "User", type: User.type, required: true, comment: "user\nuser,user\nuser,user,user" },
    ))
    PD(Account2)
}