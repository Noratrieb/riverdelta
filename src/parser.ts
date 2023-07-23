import { FunctionDef, Item } from "./ast";
import { CompilerError, todo } from "./error";
import { Token } from "./lexer";

export function parse(t: Token[]): Item[] {
    const items: Item[] = [];

    while (t.length > 0) {
        let item;
        [t, item] = parseItem(t);
        items.push(item);
    }

    return items;
}

function parseItem(t: Token[]): [Token[], Item] {
    let next;
    [t, next] = nextT(t);
    if (next.kind === "kw_function") {

        const def: FunctionDef = {
            name: "",
            args: [],
            body: todo("todo", next.span)
        }

        return [t, {kind: "function", node: def, span: next.span}]
    } else {
        unexpectedToken(next);
    }
}

function nextT(t: Token[]): [Token[], Token] {
    const next = t[0];
    if (!next) {
        throw new CompilerError("unexpected end of file", {start: Number.MAX_SAFE_INTEGER, end: Number.MAX_SAFE_INTEGER})
    }
    const rest = t.slice(1);
    return [rest, next];
}

function unexpectedToken(token: Token): never {
    throw new CompilerError("unexpected token", token.span);
}
