import {
  Crate,
  BuiltinName,
  COMPARISON_KINDS,
  mkDefaultFolder,
  EQUALITY_KINDS,
  Expr,
  ExprBinary,
  ExprUnary,
  foldAst,
  Folder,
  IdentWithRes,
  ItemId,
  LOGICAL_KINDS,
  LoopId,
  Resolution,
  Resolved,
  Ty,
  TY_BOOL,
  TY_I32,
  TY_INT,
  TY_NEVER,
  TY_STRING,
  TY_UNIT,
  TyFn,
  tyIsUnit,
  Type,
  Typecked,
  TyStruct,
  Item,
  StructLiteralField,
  superFoldExpr,
} from "./ast";
import { GlobalContext } from "./context";
import { CompilerError, Span } from "./error";
import { printTy } from "./printer";
import { ComplexMap } from "./utils";

function mkTyFn(params: Ty[], returnTy: Ty): Ty {
  return { kind: "fn", params, returnTy };
}

function builtinAsTy(name: string, span: Span): Ty {
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
      throw new CompilerError(`\`${name}\` is not a type`, span);
    }
  }
}

function typeOfBuiltinValue(name: BuiltinName, span: Span): Ty {
  switch (name) {
    case "false":
    case "true":
      return TY_BOOL;
    case "print":
      return mkTyFn([TY_STRING], TY_UNIT);
    case "trap":
      return mkTyFn([], TY_NEVER);
    case "__i32_store":
      return mkTyFn([TY_I32, TY_I32], TY_UNIT);
    case "__i64_store":
      return mkTyFn([TY_I32, TY_INT], TY_UNIT);
    case "__i32_load":
      return mkTyFn([TY_I32], TY_I32);
    case "__i64_load":
      return mkTyFn([TY_I32], TY_INT);
    case "__memory_size":
      return mkTyFn([], TY_I32);
    case "__memory_grow":
      return mkTyFn([TY_I32], TY_I32);
    case "__i32_extend_to_i64_u":
      return mkTyFn([TY_I32], TY_INT);
    default: {
      throw new CompilerError(`\`${name}\` cannot be used as a value`, span);
    }
  }
}

// TODO: Cleanup, maybe get the ident switch into this function because typeOfItem is unused.
function lowerAstTyBase(
  type: Type<Resolved>,
  lowerIdentTy: (ident: IdentWithRes<Resolved>) => Ty,
  typeOfItem: (itemId: ItemId, cause: Span) => Ty,
): Ty {
  switch (type.kind) {
    case "ident": {
      return lowerIdentTy(type.value);
    }
    case "list": {
      return {
        kind: "list",
        elem: lowerAstTyBase(type.elem, lowerIdentTy, typeOfItem),
      };
    }
    case "tuple": {
      return {
        kind: "tuple",
        elems: type.elems.map((type) =>
          lowerAstTyBase(type, lowerIdentTy, typeOfItem),
        ),
      };
    }
    case "never": {
      return TY_NEVER;
    }
  }
}

export function typeck(
  gcx: GlobalContext,
  ast: Crate<Resolved>,
): Crate<Typecked> {
  const itemTys = new ComplexMap<ItemId, Ty | null>();

  function typeOfItem(itemId: ItemId, cause: Span): Ty {
    if (itemId.crateId !== ast.id) {
      const item = gcx.findItem(itemId, ast);

      switch (item.kind) {
        case "function":
        case "import":
        case "type":
        case "global":
          return item.ty!;
        case "mod": {
          throw new CompilerError(
            `module ${item.name} cannot be used as a type or value`,
            cause,
          );
        }
        case "extern": {
          throw new CompilerError(
            `extern declaration ${item.name} cannot be used as a type or value`,
            cause,
          );
        }
      }
    }

    const item = gcx.findItem(itemId, ast);
    const ty = itemTys.get(itemId);
    if (ty) {
      return ty;
    }
    if (ty === null) {
      throw new CompilerError(
        `cycle computing type of #G${itemId.toString()}`,
        item.span,
      );
    }
    itemTys.set(itemId, null);
    switch (item.kind) {
      case "function":
      case "import": {
        const args = item.params.map((arg) => lowerAstTy(arg.type));
        const returnTy: Ty = item.returnType
          ? lowerAstTy(item.returnType)
          : TY_UNIT;

        const ty: Ty = { kind: "fn", params: args, returnTy };
        itemTys.set(item.id, ty);
        return ty;
      }
      case "type": {
        switch (item.type.kind) {
          case "struct": {
            const ty: Ty = {
              kind: "struct",
              name: item.name,
              fields: [
                /*dummy*/
              ],
            };

            itemTys.set(item.id, ty);

            const fields = item.type.fields.map<[string, Ty]>(
              ({ name, type }) => [name.name, lowerAstTy(type)],
            );

            ty.fields = fields;
            return ty;
          }
          case "alias": {
            return lowerAstTy(item.type.type);
          }
        }
      }
      case "mod": {
        throw new CompilerError(
          `module ${item.name} cannot be used as a type or value`,
          cause,
        );
      }
      case "extern": {
        throw new CompilerError(
          `extern declaration ${item.name} cannot be used as a type or value`,
          cause,
        );
      }
      case "global": {
        const ty = lowerAstTy(item.type);
        itemTys.set(item.id, ty);
        return ty;
      }
    }
  }

  function lowerAstTy(type: Type<Resolved>): Ty {
    return lowerAstTyBase(
      type,
      (ident) => {
        const res = ident.res;
        switch (res.kind) {
          case "local": {
            throw new Error("Item type cannot refer to local variable");
          }
          case "item": {
            return typeOfItem(res.id, type.span);
          }
          case "builtin": {
            return builtinAsTy(res.name, ident.span);
          }
        }
      },
      typeOfItem,
    );
  }

  const checker: Folder<Resolved, Typecked> = {
    ...mkDefaultFolder(),
    itemInner(item: Item<Resolved>): Item<Typecked> {
      switch (item.kind) {
        case "function": {
          const fnTy = typeOfItem(item.id, item.span) as TyFn;
          const body = checkBody(gcx, ast, item.body, fnTy, typeOfItem);

          const returnType = item.returnType && {
            ...item.returnType,
            ty: fnTy.returnTy,
          };
          return {
            ...item,
            name: item.name,
            params: item.params.map((arg, i) => ({
              ...arg,
              type: { ...arg.type, ty: fnTy.params[i] },
            })),
            body,
            returnType,
            ty: fnTy,
          };
        }
        case "import": {
          const fnTy = typeOfItem(item.id, item.span) as TyFn;

          fnTy.params.forEach((param, i) => {
            switch (param.kind) {
              case "int":
              case "i32":
                break;
              default: {
                throw new CompilerError(
                  `import parameters must be I32 or Int`,
                  item.params[i].span,
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
                throw new CompilerError(
                  `import return must be I32 or Int`,
                  item.returnType!.span,
                );
              }
            }
          }

          const returnType = item.returnType && {
            ...item.returnType,
            ty: fnTy.returnTy,
          };

          return {
            ...item,
            kind: "import",
            module: item.module,
            func: item.func,
            name: item.name,
            params: item.params.map((arg, i) => ({
              ...arg,
              type: { ...arg.type, ty: fnTy.params[i] },
            })),
            returnType,
            ty: fnTy,
          };
        }
        case "type": {
          switch (item.type.kind) {
            case "struct": {
              const fieldNames = new Set();
              item.type.fields.forEach(({ name }) => {
                if (fieldNames.has(name)) {
                  throw new CompilerError(
                    `type ${item.name} has a duplicate field: ${name.name}`,
                    name.span,
                  );
                }
                fieldNames.add(name);
              });

              const ty = typeOfItem(item.id, item.span) as TyStruct;

              return {
                ...item,
                name: item.name,
                type: {
                  kind: "struct",
                  fields: item.type.fields.map((field, i) => ({
                    name: field.name,
                    type: {
                      ...field.type,
                      ty: ty.fields[i][1],
                    },
                  })),
                },
              };
            }
            case "alias": {
              return {
                ...item,
                name: item.name,
                type: {
                  kind: "alias",
                  type: item.type.type,
                },
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
          const ty = typeOfItem(item.id, item.span);
          const { init } = item;

          if (init.kind !== "literal" || init.value.kind !== "int") {
            throw new CompilerError(
              "globals must be initialized with an integer literal",
              init.span,
            );
          }

          const initTy = init.value.type === "I32" ? TY_I32 : TY_INT;
          const infcx = new InferContext();
          infcx.assign(ty, initTy, init.span);

          return {
            ...item,
            ty,
            init: { ...init, ty },
          };
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
      if (item.returnType !== undefined) {
        const ty = item.body.ty;
        if (ty.kind !== "tuple" || ty.elems.length !== 0) {
          throw new CompilerError(
            `\`main\` has an invalid signature. main takes no arguments and returns nothing`,
            item.span,
          );
        }
      }

      return true;
    }
    return false;
  });

  if (ast.id === 0) {
    // Only the final id=0 crate needs and cares about main.
    if (!main) {
      throw new CompilerError(
        `\`main\` function not found`,
        Span.startOfFile(ast.rootFile),
      );
    }

    typecked.typeckResults = {
      main: { kind: "item", id: main.id },
    };
  }

  return typecked;
}

type TyVarRes =
  | {
      kind: "final";
      ty: Ty;
    }
  | {
      kind: "unified";
      index: number;
    }
  | {
      kind: "unknown";
    };

export class InferContext {
  tyVars: TyVarRes[] = [];

  public newVar(): Ty {
    const index = this.tyVars.length;
    this.tyVars.push({ kind: "unknown" });
    return { kind: "var", index };
  }

  private tryResolveVar(variable: number): Ty | undefined {
    const varRes = this.tyVars[variable];
    switch (varRes.kind) {
      case "final": {
        return varRes.ty;
      }
      case "unified": {
        const ty = this.tryResolveVar(varRes.index);
        if (ty) {
          this.tyVars[variable] = { kind: "final", ty };
          return ty;
        } else {
          return undefined;
        }
      }
      case "unknown": {
        return undefined;
      }
    }
  }

  private findRoot(variable: number): number {
    let root = variable;
    let nextVar;
    while ((nextVar = this.tyVars[root]).kind === "unified") {
      root = nextVar.index;
    }
    return root;
  }

  /**
   * Try to constrain a type variable to be of a specific type.
   * INVARIANT: Both sides must not be of res "final", use resolveIfPossible
   * before calling this.
   */
  private constrainVar(variable: number, ty: Ty) {
    const root = this.findRoot(variable);

    if (ty.kind === "var") {
      // Now we point our root to the other root to unify the two graphs.
      const otherRoot = this.findRoot(ty.index);

      // If both types have the same root, we don't need to do anything
      // as they're already part of the same graph.
      if (root != otherRoot) {
        this.tyVars[root] = { kind: "unified", index: otherRoot };
      }
    } else {
      this.tyVars[root] = { kind: "final", ty };
    }
  }

  public resolveIfPossible(ty: Ty): Ty {
    // TODO: dont be shallow resolve
    // note that fixing this will cause cycles. fix those cycles instead using
    // he fancy occurs check as errs called it.
    if (ty.kind === "var") {
      return this.tryResolveVar(ty.index) ?? ty;
    } else {
      return ty;
    }
  }

  public assign(lhs_: Ty, rhs_: Ty, span: Span): void {
    const lhs = this.resolveIfPossible(lhs_);
    const rhs = this.resolveIfPossible(rhs_);

    if (lhs.kind === "var") {
      this.constrainVar(lhs.index, rhs);
      return;
    }
    if (rhs.kind === "var") {
      this.constrainVar(rhs.index, lhs);
      return;
    }

    if (rhs.kind === "never") {
      return;
    }

    switch (lhs.kind) {
      case "string": {
        if (rhs.kind === "string") return;
        break;
      }
      case "int": {
        if (rhs.kind === "int") return;
        break;
      }
      case "i32": {
        if (rhs.kind === "i32") return;
        break;
      }
      case "bool": {
        if (rhs.kind === "bool") return;
        break;
      }
      case "list": {
        if (rhs.kind === "list") {
          this.assign(lhs.elem, rhs.elem, span);
          return;
        }
        break;
      }
      case "tuple": {
        if (rhs.kind === "tuple" && lhs.elems.length === rhs.elems.length) {
          lhs.elems.forEach((lhs, i) => this.assign(lhs, rhs.elems[i], span));
          return;
        }
        break;
      }
      case "fn": {
        if (rhs.kind === "fn" && lhs.params.length === rhs.params.length) {
          // swapping because of contravariance in the future maybe
          lhs.params.forEach((lhs, i) => this.assign(rhs.params[i], lhs, span));

          this.assign(lhs.returnTy, rhs.returnTy, span);

          return;
        }
        break;
      }
      case "struct": {
        if (rhs.kind === "struct" && lhs.name === rhs.name) {
          return;
        }
      }
    }

    throw new CompilerError(
      `cannot assign ${printTy(rhs)} to ${printTy(lhs)}`,
      span,
    );
  }
}

export function checkBody(
  gcx: GlobalContext,
  ast: Crate<Resolved>,
  body: Expr<Resolved>,
  fnTy: TyFn,
  typeOfItem: (itemId: ItemId, cause: Span) => Ty,
): Expr<Typecked> {
  const localTys = [...fnTy.params];
  const loopState: { hasBreak: boolean; loopId: LoopId }[] = [];

  const infcx = new InferContext();

  function typeOf(res: Resolution, span: Span): Ty {
    switch (res.kind) {
      case "local": {
        const idx = localTys.length - 1 - res.index;
        return localTys[idx];
      }
      case "item": {
        return typeOfItem(res.id, span);
      }
      case "builtin":
        return typeOfBuiltinValue(res.name, span);
    }
  }

  function lowerAstTy(type: Type<Resolved>): Ty {
    return lowerAstTyBase(
      type,
      (ident) => {
        const res = ident.res;
        switch (res.kind) {
          case "local": {
            const idx = localTys.length - 1 - res.index;
            return localTys[idx];
          }
          case "item": {
            return typeOfItem(res.id, type.span);
          }
          case "builtin":
            return builtinAsTy(res.name, ident.span);
        }
      },
      typeOfItem,
    );
  }

  const checker: Folder<Resolved, Typecked> = {
    ...mkDefaultFolder(),
    expr(expr): Expr<Typecked> {
      switch (expr.kind) {
        case "empty": {
          return { ...expr, ty: TY_UNIT };
        }
        case "let": {
          const loweredBindingTy = expr.type && lowerAstTy(expr.type);
          const bindingTy = loweredBindingTy
            ? loweredBindingTy
            : infcx.newVar();

          const rhs = this.expr(expr.rhs);
          infcx.assign(bindingTy, rhs.ty, expr.span);

          // AST validation ensures that lets can only be in blocks, where
          // the types will be popped.
          localTys.push(bindingTy);

          expr.local!.ty = bindingTy;

          const type: Type<Typecked> | undefined = loweredBindingTy && {
            ...expr.type!,
          };

          return {
            kind: "let",
            name: expr.name,
            type,
            rhs,
            ty: TY_UNIT,
            span: expr.span,
          };
        }
        case "assign": {
          const lhs = this.expr(expr.lhs);
          const rhs = this.expr(expr.rhs);

          infcx.assign(lhs.ty, rhs.ty, expr.span);

          switch (lhs.kind) {
            case "ident": {
              const { res } = lhs.value;
              switch (res.kind) {
                case "local":
                  break;
                case "item": {
                  const item = gcx.findItem(res.id, ast);
                  if (item.kind !== "global") {
                    throw new CompilerError("cannot assign to item", expr.span);
                  }
                  break;
                }
                case "builtin":
                  throw new CompilerError(
                    "cannot assign to builtins",
                    expr.span,
                  );
              }
              break;
            }
            default: {
              throw new CompilerError(
                "invalid left-hand side of assignment",
                lhs.span,
              );
            }
          }

          return {
            ...expr,
            kind: "assign",
            lhs,
            rhs,
            ty: TY_UNIT,
          };
        }
        case "block": {
          const prevLocalTysLen = localTys.length;

          const exprs = expr.exprs.map((expr) => this.expr(expr));

          const ty = exprs.length > 0 ? exprs[exprs.length - 1].ty : TY_UNIT;

          localTys.length = prevLocalTysLen;

          return {
            ...expr,
            exprs,
            ty,
          };
        }
        case "literal": {
          let ty;
          switch (expr.value.kind) {
            case "str": {
              ty = TY_STRING;
              break;
            }
            case "int": {
              switch (expr.value.type) {
                case "Int":
                  ty = TY_INT;
                  break;
                case "I32":
                  ty = TY_I32;
                  break;
              }
              break;
            }
          }

          return { ...expr, ty };
        }
        case "ident": {
          const ty = typeOf(expr.value.res, expr.value.span);

          return { ...expr, ty };
        }
        case "path": {
          const ty = typeOf(expr.res, expr.span);
          return { ...expr, ty };
        }
        case "binary": {
          const lhs = this.expr(expr.lhs);
          const rhs = this.expr(expr.rhs);

          lhs.ty = infcx.resolveIfPossible(lhs.ty);
          rhs.ty = infcx.resolveIfPossible(rhs.ty);

          return checkBinary(expr, lhs, rhs);
        }
        case "unary": {
          const rhs = this.expr(expr.rhs);
          rhs.ty = infcx.resolveIfPossible(rhs.ty);
          return checkUnary(expr, rhs);
        }
        case "call": {
          if (
            expr.lhs.kind === "ident" &&
            expr.lhs.value.res.kind === "builtin" &&
            expr.lhs.value.res.name === "___transmute"
          ) {
            const ty = infcx.newVar();
            const args = expr.args.map((arg) => this.expr(arg));
            const ret: Expr<Typecked> = {
              ...expr,
              lhs: { ...expr.lhs, ty: TY_UNIT },
              args,
              ty,
            };

            return ret;
          }

          const lhs = this.expr(expr.lhs);
          lhs.ty = infcx.resolveIfPossible(lhs.ty);
          const lhsTy = lhs.ty;
          if (lhsTy.kind !== "fn") {
            throw new CompilerError(
              `expression of type ${printTy(lhsTy)} is not callable`,
              lhs.span,
            );
          }

          const args = expr.args.map((arg) => this.expr(arg));

          lhsTy.params.forEach((param, i) => {
            if (args.length <= i) {
              throw new CompilerError(
                `missing argument of type ${printTy(param)}`,
                expr.span,
              );
            }
            const arg = checker.expr(args[i]);

            infcx.assign(param, arg.ty, args[i].span);
          });

          if (args.length > lhsTy.params.length) {
            throw new CompilerError(
              `too many arguments passed, expected ${lhsTy.params.length}, found ${args.length}`,
              expr.span,
            );
          }

          return { ...expr, lhs, args, ty: lhsTy.returnTy };
        }
        case "fieldAccess": {
          const lhs = this.expr(expr.lhs);
          lhs.ty = infcx.resolveIfPossible(lhs.ty);

          const { field } = expr;
          let ty: Ty;
          let fieldIdx: number;
          switch (lhs.ty.kind) {
            case "tuple": {
              const { elems } = lhs.ty;
              if (typeof field.value === "number") {
                if (elems.length > field.value) {
                  ty = elems[field.value];
                  fieldIdx = field.value;
                } else {
                  throw new CompilerError(
                    `tuple with ${elems.length} elements cannot be indexed with ${field.value}`,
                    field.span,
                  );
                }
              } else {
                throw new CompilerError(
                  "tuple fields must be accessed with numbers",
                  field.span,
                );
              }
              break;
            }
            case "struct": {
              if (typeof field.value === "string") {
                const idx = lhs.ty.fields.findIndex(
                  ([name]) => name === field.value,
                );
                if (idx === -1) {
                  throw new CompilerError(
                    `field \`${field.value}\` does not exist on ${printTy(
                      lhs.ty,
                    )}`,
                    field.span,
                  );
                }

                ty = lhs.ty.fields[idx][1];
                fieldIdx = idx;
              } else {
                throw new CompilerError(
                  "struct fields must be accessed with their name",
                  field.span,
                );
              }
              break;
            }
            default: {
              throw new CompilerError(
                `cannot access field \`${field.value}\` on type \`${printTy(
                  lhs.ty,
                )}\``,
                expr.span,
              );
            }
          }

          return {
            ...expr,
            lhs,
            field: {
              ...expr.field,
              fieldIdx,
            },
            ty,
          };
        }
        case "if": {
          const cond = this.expr(expr.cond);
          const then = this.expr(expr.then);
          const elsePart = expr.else && this.expr(expr.else);

          infcx.assign(TY_BOOL, cond.ty, cond.span);

          let ty: Ty;
          if (elsePart) {
            infcx.assign(then.ty, elsePart.ty, elsePart.span);
            ty = then.ty!;
          } else {
            infcx.assign(TY_UNIT, then.ty, then.span);
            ty = TY_UNIT;
          }

          return { ...expr, cond, then, else: elsePart, ty };
        }
        case "loop": {
          loopState.push({
            hasBreak: false,
            loopId: expr.loopId,
          });

          const body = this.expr(expr.body);
          infcx.assign(TY_UNIT, body.ty, body.span);

          const hadBreak = loopState.pop();
          const ty = hadBreak ? TY_UNIT : TY_NEVER;

          return {
            ...expr,
            body,
            ty,
          };
        }
        case "break": {
          if (loopState.length === 0) {
            throw new CompilerError("break outside loop", expr.span);
          }
          const target = loopState[loopState.length - 1].loopId;
          loopState[loopState.length - 1].hasBreak = true;

          return {
            ...expr,
            ty: TY_NEVER,
            target,
          };
        }
        case "structLiteral": {
          const fields = expr.fields.map<StructLiteralField<Typecked>>(
            ({ name, expr }) => ({ name, expr: this.expr(expr) }),
          );

          const structTy = typeOf(expr.name.res, expr.name.span);

          if (structTy.kind !== "struct") {
            throw new CompilerError(
              `struct literal is only allowed for struct types`,
              expr.span,
            );
          }

          const assignedFields = new Set();

          fields.forEach(({ name, expr: field }, i) => {
            const fieldIdx = structTy.fields.findIndex(
              (def) => def[0] === name.name,
            );
            if (fieldIdx == -1) {
              throw new CompilerError(
                `field ${name.name} doesn't exist on type ${expr.name.name}`,
                name.span,
              );
            }
            const fieldTy = structTy.fields[fieldIdx];
            infcx.assign(fieldTy[1], field.ty, field.span);
            assignedFields.add(name.name);
            fields[i].fieldIdx = fieldIdx;
          });

          const missing: string[] = [];
          structTy.fields.forEach(([name]) => {
            if (!assignedFields.has(name)) {
              missing.push(name);
            }
          });
          if (missing.length > 0) {
            throw new CompilerError(
              `missing fields in literal: ${missing.join(", ")}`,
              expr.span,
            );
          }

          return { ...expr, fields, ty: structTy };
        }
        case "tupleLiteral": {
          const fields = expr.fields.map((expr) => this.expr(expr));

          const ty: Ty = {
            kind: "tuple",
            elems: fields.map((field) => field.ty),
          };

          return { ...expr, fields, ty };
        }
      }
    },
    itemInner(_item) {
      throw new Error("cannot deal with items inside body");
    },
    ident(ident) {
      return ident;
    },
    type(_type) {
      throw new Error("all types in the body should be handled elsewhere");
    },
  };

  const checked = checker.expr(body);

  infcx.assign(fnTy.returnTy, checked.ty, body.span);

  const resolveTy = (ty: Ty, span: Span) => {
    const resTy = infcx.resolveIfPossible(ty);
    // TODO: When doing deep resolution, we need to check for _any_ vars.
    if (resTy.kind === "var") {
      throw new CompilerError("cannot infer type", span);
    }
    return resTy;
  };

  const resolver: Folder<Typecked, Typecked> = {
    ...mkDefaultFolder(),
    expr(expr) {
      const ty = resolveTy(expr.ty, expr.span);

      if (expr.kind === "block") {
        expr.locals!.forEach((local) => {
          local.ty = resolveTy(local.ty!, local.span);
        });
      }

      const innerExpr = superFoldExpr(expr, this);

      return { ...innerExpr, ty };
    },
    type(type) {
      return type;
    },
    ident(ident) {
      return ident;
    },
  };

  const resolved = resolver.expr(checked);

  return resolved;
}

function checkBinary(
  expr: Expr<Resolved> & ExprBinary<Resolved>,
  lhs: Expr<Typecked>,
  rhs: Expr<Typecked>,
): Expr<Typecked> {
  const lhsTy = lhs.ty;
  const rhsTy = rhs.ty;

  if (COMPARISON_KINDS.includes(expr.binaryKind)) {
    if (lhsTy.kind === "int" && rhsTy.kind === "int") {
      return { ...expr, lhs, rhs, ty: TY_BOOL };
    }

    if (lhsTy.kind === "i32" && rhsTy.kind === "i32") {
      return { ...expr, lhs, rhs, ty: TY_BOOL };
    }

    if (lhsTy.kind === "string" && rhsTy.kind === "string") {
      return { ...expr, lhs, rhs, ty: TY_BOOL };
    }

    if (EQUALITY_KINDS.includes(expr.binaryKind)) {
      if (lhsTy.kind === "bool" && rhsTy.kind === "bool") {
        return { ...expr, lhs, rhs, ty: TY_BOOL };
      }
    }
  }

  if (lhsTy.kind === "int" && rhsTy.kind === "int") {
    return { ...expr, lhs, rhs, ty: TY_INT };
  }
  if (lhsTy.kind === "i32" && rhsTy.kind === "i32") {
    return { ...expr, lhs, rhs, ty: TY_I32 };
  }

  if (LOGICAL_KINDS.includes(expr.binaryKind)) {
    if (lhsTy.kind === "bool" && rhsTy.kind === "bool") {
      return { ...expr, lhs, rhs, ty: TY_BOOL };
    }
  }

  throw new CompilerError(
    `invalid types for binary operation: ${printTy(lhs.ty)} ${
      expr.binaryKind
    } ${printTy(rhs.ty)}`,
    expr.span,
  );
}

function checkUnary(
  expr: Expr<Resolved> & ExprUnary<Resolved>,
  rhs: Expr<Typecked>,
): Expr<Typecked> {
  const rhsTy = rhs.ty;

  if (
    expr.unaryKind === "!" &&
    (rhsTy.kind === "int" || rhsTy.kind === "i32" || rhsTy.kind === "bool")
  ) {
    return { ...expr, rhs, ty: rhsTy };
  }

  if (expr.unaryKind === "-" && rhsTy.kind == "int") {
    // Negating an unsigned integer is a bad idea.
  }

  throw new CompilerError(
    `invalid types for unary operation: ${expr.unaryKind} ${printTy(rhs.ty)}`,
    expr.span,
  );
}
