import {
  Pkg,
  Expr,
  Folder,
  Item,
  ItemId,
  Resolved,
  Typecked,
  foldAst,
  mkDefaultFolder,
} from "../ast";
import { GlobalContext } from "../context";
import { CompilerError, ErrorEmitted, Span } from "../error";
import { TYS, Ty, TyFn, tyIsUnit } from "../types";
import { ComplexMap } from "../utils";
import { emitError } from "./base";
import { checkBody, exprError } from "./expr";
import { InferContext } from "./infer";
import { typeOfItem } from "./item";
import { lintProgram } from "./lint";

export function typeck(gcx: GlobalContext, ast: Pkg<Resolved>): Pkg<Typecked> {
  const cx = {
    gcx,
    itemTys: new ComplexMap<ItemId, Ty | null>(),
    ast,
  };

  const checker: Folder<Resolved, Typecked> = {
    ...mkDefaultFolder(),
    itemInner(item: Item<Resolved>): Item<Typecked> {
      switch (item.kind) {
        case "function": {
          // Functions do not have generic arguments right now.
          const fnTy = typeOfItem(cx, item.id, item.span) as TyFn;
          const body = checkBody(cx, ast, item.body, fnTy);

          return {
            ...item,
            name: item.name,
            params: item.params.map((arg) => ({ ...arg })),
            body,
            ty: fnTy,
          };
        }
        case "import": {
          const fnTy = typeOfItem(cx, item.id, item.span) as TyFn;

          fnTy.params.forEach((param, i) => {
            switch (param.kind) {
              case "int":
              case "i32":
                break;
              default: {
                emitError(
                  cx,
                  new CompilerError(
                    `import parameters must be I32 or Int`,
                    item.params[i].ident.span,
                  ),
                );
              }
            }
          });

          if (!tyIsUnit(fnTy.returnTy)) {
            switch (fnTy.returnTy.kind) {
              case "int":
              case "i32":
                break;
              default: {
                emitError(
                  cx,
                  new CompilerError(
                    `import return must be I32, Int or ()`,
                    item.returnType!.span,
                  ),
                );
              }
            }
          }

          return {
            ...item,
            kind: "import",
            params: item.params.map((arg) => ({ ...arg })),
            ty: fnTy,
          };
        }
        case "type": {
          const ty = typeOfItem(cx, item.id, item.span);

          switch (item.type.kind) {
            case "struct": {
              const fieldNames = new Set();
              item.type.fields.forEach(({ name }) => {
                if (fieldNames.has(name)) {
                  emitError(
                    cx,
                    new CompilerError(
                      `type ${item.name} has a duplicate field: ${name.name}`,
                      name.span,
                    ),
                  );
                } else {
                  fieldNames.add(name);
                }
              });

              return {
                ...item,
                type: {
                  kind: "struct",
                  fields: item.type.fields.map((field) => ({ ...field })),
                },
                ty,
              };
            }
            case "alias": {
              return {
                ...item,
                type: { ...item.type },
                ty,
              };
            }
          }
        }
        case "mod": {
          return {
            ...item,
            contents: item.contents.map((item) => this.item(item)),
          };
        }
        case "extern": {
          // Nothing to check.
          return item;
        }
        case "global": {
          const ty = typeOfItem(cx, item.id, item.span);
          const { init } = item;

          let initChecked: Expr<Typecked>;
          if (init.kind !== "literal" || init.value.kind !== "int") {
            const err: ErrorEmitted = emitError(
              cx,
              new CompilerError(
                "globals must be initialized with an integer literal",
                init.span,
              ),
            );
            initChecked = exprError(err, init.span);
          } else {
            const initTy = init.value.type === "I32" ? TYS.I32 : TYS.INT;
            const infcx = new InferContext(cx.gcx.error);
            infcx.assign(ty, initTy, init.span);
            initChecked = { ...init, ty };
          }

          return {
            ...item,
            ty,
            init: initChecked,
          };
        }
        case "use": {
          return { ...item };
        }
        case "error": {
          return { ...item };
        }
      }
    },
    expr(_expr) {
      throw new Error("expressions need to be handled in checkBody");
    },
    ident(ident) {
      return ident;
    },
    type(_type) {
      throw new Error("all types should be typechecked manually");
    },
  };

  const typecked = foldAst(ast, checker);

  const main = typecked.rootItems.find((item) => {
    if (item.kind === "function" && item.name === "main") {
      if (!tyIsUnit(item.ty!.returnTy)) {
        emitError(
          cx,
          new CompilerError(
            `\`main\` has an invalid signature. main takes no arguments and returns nothing`,
            item.span,
          ),
        );
      }

      return true;
    }
    return false;
  });

  if (ast.id === 0) {
    // Only the final id=0 pkg needs and cares about main.
    if (!main) {
      emitError(
        cx,
        new CompilerError(
          `\`main\` function not found`,
          Span.startOfFile(ast.rootFile),
        ),
      );
    }

    typecked.typeckResults = { main: undefined };
    if (main) {
      typecked.typeckResults.main = { kind: "item", id: main.id };
    }
  }

  lintProgram(gcx, typecked);

  return typecked;
}
