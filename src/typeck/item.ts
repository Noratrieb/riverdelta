import {
  ItemId,
  Resolved,
  Type,
} from "../ast";
import { CompilerError, Span } from "../error";
import { printTy } from "../printer";
import { TY_BOOL, TY_I32, TY_INT, TY_NEVER, TY_STRING, TY_UNIT, Ty, substituteTy } from "../types";
import { TypeckCtx, tyError, tyErrorFrom } from "./base";

function builtinAsTy(cx: TypeckCtx, name: string, span: Span): Ty {
  switch (name) {
    case "String": {
      return TY_STRING;
    }
    case "Int": {
      return TY_INT;
    }
    case "I32": {
      return TY_I32;
    }
    case "Bool": {
      return TY_BOOL;
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
// TODO: Cleanup, maybe get the ident switch into this function because typeOfItem is unused.
export function lowerAstTy(cx: TypeckCtx, type: Type<Resolved>): Ty {
  switch (type.kind) {
    case "ident": {
      const ident = type.value;
      const res = ident.res;

      const generics = type.generics.map((type) => lowerAstTy(cx, type));
      let ty: Ty;
      switch (res.kind) {
        case "local": {
          throw new Error("Item type cannot refer to local variable");
        }
        case "item": {
          ty = typeOfItem(cx, res.id, generics, type.span);
          break;
        }
        case "builtin": {
          ty = builtinAsTy(cx, res.name, ident.span);
          break;
        }
        case "tyParam": {
          ty = { kind: "param", idx: res.index, name: res.name };
          break;
        }
        case "error": {
          ty = tyErrorFrom(res);
          break;
        }
      }

      if (ty.kind === "struct" || ty.kind === "alias") {
        if (generics.length === ty.params.length) {
          if (ty.kind === "alias") {
            return substituteTy(ty.genericArgs, ty.actual);
          }
          return { ...ty, genericArgs: generics };
        } else {
          return tyError(
            cx,
            new CompilerError(
              `expected ${ty.params.length} generic arguments, found ${generics.length}`,
              type.span,
            ),
          );
        }
      } else if (ty.kind !== "error") {
        if (generics.length > 0) {
          return tyError(
            cx,
            new CompilerError(
              `type ${printTy(ty)} does not take generic arguments`,
              type.span,
            ),
          );
        }
      }

      return ty;
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
      return TY_NEVER;
    }
    case "error": {
      return tyErrorFrom(type);
    }
  }
}

export function typeOfItem(
  cx: TypeckCtx,
  itemId: ItemId,
  genericArgs: Ty[],
  cause: Span,
): Ty {
  if (itemId.pkgId !== cx.ast.id) {
    // Look up foreign items in the foreign pkgs, we don't need to lower those
    // ourselves.
    const item = cx.gcx.findItem(itemId);

    switch (item.kind) {
      case "function":
      case "import":
      case "type":
      case "global":
        return substituteTy(genericArgs, item.ty!);
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
        : TY_UNIT;

      ty = { kind: "fn", params: args, returnTy };
      break;
    }
    case "type": {
      switch (item.type.kind) {
        case "struct": {
          ty = {
            kind: "struct",
            genericArgs: item.generics.map(
              ({ name }, idx): Ty => ({
                kind: "param",
                name,
                idx,
              }),
            ),
            params: item.generics.map((ident) => ident.name),
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

          ty = {
            kind: "alias",
            actual,
            genericArgs: item.generics.map(
              ({ name }, idx): Ty => ({
                kind: "param",
                name,
                idx,
              }),
            ),
            params: item.generics.map((ident) => ident.name),
          };
          break;
        }
      }
      break;
    }
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
    case "global": {
      ty = lowerAstTy(cx, item.type);
      break;
    }
    case "error": {
      return tyErrorFrom(item);
    }
  }

  ty = substituteTy(genericArgs, ty);

  cx.itemTys.set(item.id, ty);
  return ty;
}
