import { Ident, Item, ItemId, Resolved, Type, Typecked } from "../ast";
import { CompilerError, Span } from "../error";
import { printTy } from "../printer";
import { TYS, Ty, createIdentityGenericArgs, substituteTy } from "../types";
import { TypeckCtx, tyError, tyErrorFrom } from "./base";

function builtinAsTy(cx: TypeckCtx, name: string, span: Span): Ty {
  switch (name) {
    case "String": {
      return TYS.STRING;
    }
    case "Int": {
      return TYS.INT;
    }
    case "I32": {
      return TYS.I32;
    }
    case "Bool": {
      return TYS.BOOL;
    }
    default: {
      return tyError(cx, new CompilerError(`\`${name}\` is not a type`, span));
    }
  }
}

/**
 * Lowers the AST representation of a type into its resolved Ty representation.
 * Will also validate the type, for example ensuring that generic arguments match up.
 */
export function lowerAstTy(cx: TypeckCtx, type: Type<Resolved>): Ty {
  // This function is called for every syntactical type in the program.
  // Could be a function argument, but also a struct field or a local variable annotation.
  switch (type.kind) {
    case "ident": {
      const ident = type.value;
      const res = ident.res;

      const genericArgs = type.genericArgs.map((type) => lowerAstTy(cx, type));
      let ty: Ty;
      let generics: Generics;
      // We only actually substitute anything when "peeking" behind a type into its
      // internals, where the params are used. This is only the case for aliases today.
      let isAlias = false;
      switch (res.kind) {
        case "local": {
          throw new Error("Item type cannot refer to local variable");
        }
        case "item": {
          ty = typeOfItem(cx, res.id, type.span);
          const item = cx.gcx.findItem<Resolved>(res.id, cx.ast);
          if (item.kind === "type" && item.type.kind === "alias") {
            isAlias = true;
          }
          generics = itemGenerics(item);
          break;
        }
        case "builtin": {
          ty = builtinAsTy(cx, res.name, ident.span);
          generics = { kind: "none" };
          break;
        }
        case "tyParam": {
          ty = { kind: "param", idx: res.index, name: res.name };
          generics = { kind: "none" };
          break;
        }
        case "error": {
          // Skip generics validation, it's fine!
          return tyErrorFrom(res);
        }
      }

      if (
        (generics.kind === "none" || generics.params.length === 0) &&
        genericArgs.length > 0
      ) {
        return tyError(
          cx,
          new CompilerError(
            `type ${printTy(ty)} does not take any generic arguments but ${
              genericArgs.length
            } were passed`,
            type.span,
          ),
        );
      }
      if (
        generics.kind === "some" &&
        generics.params.length > genericArgs.length
      ) {
        return tyError(
          cx,
          new CompilerError(
            `missing generics for type ${printTy(ty)}, expected ${
              generics.params.length
            }, but only ${genericArgs.length} were passed`,
            type.span,
          ),
        );
      }
      if (isAlias) {
        return substituteTy(genericArgs, ty);
      } else {
        if (ty.kind === "struct") {
          return { ...ty, genericArgs };
        }
        return ty;
      }
    }
    case "tuple": {
      return {
        kind: "tuple",
        elems: type.elems.map((type) => lowerAstTy(cx, type)),
      };
    }
    case "rawptr": {
      const inner = lowerAstTy(cx, type.inner);
      if (inner.kind !== "struct") {
        return tyError(
          cx,
          new CompilerError("raw pointers must point to structs", type.span),
        );
      }

      return { kind: "rawptr", inner };
    }
    case "never": {
      return TYS.NEVER;
    }
    case "error": {
      return tyErrorFrom(type);
    }
  }
}

type Generics =
  | {
      kind: "none";
    }
  | {
      kind: "some";
      params: Ident[];
    };

function itemGenerics(item: Item<Typecked> | Item<Resolved>): Generics {
  const none: Generics = { kind: "none" };
  switch (item.kind) {
    case "function":
    case "extern":
    case "error":
    case "global":
    case "mod":
    case "import":
      return none;
    case "type":
      return { kind: "some", params: item.genericParams };
  }
}

export function typeOfItem(cx: TypeckCtx, itemId: ItemId, cause: Span): Ty {
  if (itemId.pkgId !== cx.ast.id) {
    // Look up foreign items in the foreign pkgs, we don't need to lower those
    // ourselves.
    const item = cx.gcx.findItem(itemId);

    switch (item.kind) {
      case "function":
      case "import":
      case "type":
      case "global":
        return item.ty!;
      case "mod": {
        return tyError(
          cx,
          new CompilerError(
            `module ${item.name} cannot be used as a type or value`,
            cause,
          ),
        );
      }
      case "extern": {
        return tyError(
          cx,
          new CompilerError(
            `extern declaration ${item.name} cannot be used as a type or value`,
            cause,
          ),
        );
      }
    }
  }

  const item = cx.gcx.findItem(itemId, cx.ast);
  const cachedTy = cx.itemTys.get(itemId);
  if (cachedTy) {
    return cachedTy;
  }
  if (cachedTy === null) {
    return tyError(
      cx,
      new CompilerError(
        `cycle computing type of #G${itemId.toString()}`,
        item.span,
      ),
    );
  }
  cx.itemTys.set(itemId, null);

  let ty: Ty;

  switch (item.kind) {
    case "function":
    case "import": {
      const args = item.params.map((arg) => lowerAstTy(cx, arg.type));
      const returnTy: Ty = item.returnType
        ? lowerAstTy(cx, item.returnType)
        : TYS.UNIT;

      ty = { kind: "fn", params: args, returnTy };
      break;
    }
    case "type": {
      switch (item.type.kind) {
        case "struct": {
          ty = {
            kind: "struct",
            genericArgs: createIdentityGenericArgs(item.genericParams),
            itemId: item.id,
            _name: item.name,
            fields_no_subst: [
              /*dummy*/
            ],
          };
          // Set it here already to allow for recursive types.
          cx.itemTys.set(item.id, ty);

          const fields = item.type.fields.map<[string, Ty]>(
            ({ name, type }) => [name.name, lowerAstTy(cx, type)],
          );

          ty.fields_no_subst = fields;
          break;
        }
        case "alias": {
          const actual = lowerAstTy(cx, item.type.type);

          ty = actual;
          break;
        }
      }
      break;
    }
    case "mod": {
      ty = tyError(
        cx,
        new CompilerError(
          `module ${item.name} cannot be used as a type or value`,
          cause,
        ),
      );
      break;
    }
    case "extern": {
      ty = tyError(
        cx,
        new CompilerError(
          `extern declaration ${item.name} cannot be used as a type or value`,
          cause,
        ),
      );
      break;
    }
    case "global": {
      ty = lowerAstTy(cx, item.type);
      break;
    }
    case "error": {
      ty = tyErrorFrom(item);
    }
  }

  cx.itemTys.set(item.id, ty);
  return ty;
}
