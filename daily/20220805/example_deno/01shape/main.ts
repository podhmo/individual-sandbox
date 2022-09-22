type Type = PrimitiveType | ArrayType | MapType | OneOfType | StructType | Named
type PrimitiveType = { tag: "primitive", name: string }
type ArrayType = { tag: "array", value: Type }
type MapType = { tag: "map", key: Type, value: Type }
type OneOfType = { tag: "oneof", values: Type[] }
type StructType = { tag: "struct", fields: Field[] }
type Named = { tag: "named", name: string, type: Type, comment?: string }

type Field = { name: string, required: boolean, type: Type, comment?: string }

function emitType(ob: Type, buf: string[], spacer: string, indent: string) {
    switch (ob.tag) {
        case 'named':
            buf.push(ob.name);
            break;
        case 'primitive':
            buf.push(ob.name);
            break;
        case 'array':
            buf.push('[]')
            emitType(ob.value, buf, spacer, indent)
            break;
        case 'map':
            buf.push('map[')
            emitType(ob.key, buf, spacer, indent)
            buf.push(']')
            emitType(ob.value, buf, spacer, indent)
            break;
        case 'oneof':
            buf.push('(')
            for (const x of ob.values) {
                emitType(x, buf, spacer, indent)
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
                emitType(f.type, buf, spacer, indent + spacer)
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

function emitDecl(ob: Type, buf: string[]) {
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
            emitType(ob.type, buf, "\t", "")
            break;
        default:
            throw new Error("unexpected type")
    }
}

function toPrimitive(name: string): PrimitiveType {
    return { tag: "primitive", name }
}
function toArray(value: Type): ArrayType {
    return { tag: "array", value }
}
function toMap(key: Type, value: Type): MapType {
    return { tag: "map", key, value }
}
function toOneOf(x: Type, ...xs: Type[]): OneOfType {
    return { tag: "oneof", values: [x, ...xs] }
}
function toNamed(name: string, type: Type): Named {
    return { tag: "named", name, type }
}
function toStruct(...fields: Field[]): StructType {
    return { tag: "struct", fields: fields }
}



function ptype(t: Type) {
    const buf = [];
    emitType(t, buf, "\t", "")
    console.log(buf.join(""))
}
function pdecl(t: Type) {
    const buf = [];
    emitDecl(t, buf)
    console.log(buf.join(""))
}

const string = toPrimitive("string");
const int = toPrimitive("int");
{

    ptype(string)
    ptype(toArray(string))
    ptype(toMap(toPrimitive("string"), toArray(toOneOf(int, string))))
}
{
    const User = toNamed("User", toStruct(
        { name: "Name", type: string, required: true, comment: "name of user" },
        { name: "Age", type: int, required: false, comment: "age of user" },
    ))
    User.comment = "User is user object"
    pdecl(User)

    const Account = toNamed("Account", toStruct(
        { name: "ID", type: string, required: true },
        { name: "User", type: User, required: true },
    ))
    pdecl(Account)

    const Account2 = toNamed("Account2", toStruct(
        { name: "ID", type: string, required: true },
        { name: "User", type: User.type, required: true, comment: "user\nuser,user\nuser,user,user" },
    ))
    pdecl(Account2)
}