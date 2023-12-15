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
  Item,
  StructLiteralField,
  superFoldExpr,
  ExprCall,
  substituteTy,
} from "./ast";
import { GlobalContext } from "./context";
import {
  CompilerError,
  ErrorEmitted,
  ErrorHandler,
  Span,
  unreachable,
} from "./error";
import { printTy } from "./printer";
import { ComplexMap } from "./utils";

type TypeckCtx = {
  gcx: GlobalContext;
  /**
   * A cache of all item types.
   * Starts off as undefined, then gets set to null
   * while computing the type (for cycle detection) and
   * afterwards, we get the ty.
   */
  itemTys: ComplexMap<ItemId, Ty | null>;
  ast: Crate<Resolved>;
};

function mkTyFn(params: Ty[], returnTy: Ty): Ty {
  return { kind: "fn", params, returnTy };
}

function tyError(cx: TypeckCtx, err: CompilerError): Ty {
  return {
    kind: "error",
    err: emitError(cx, err),
  };
}

function tyErrorFrom(prev: { err: ErrorEmitted }): Ty {
  return { kind: "error", err: prev.err };
}

function emitError(cx: TypeckCtx, err: CompilerError): ErrorEmitted {
  return cx.gcx.error.emit(err);
}

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

function typeOfBuiltinValue(fcx: FuncCtx, name: BuiltinName, span: Span): Ty {
  switch (name) {
    case "false":
    case "true":
      return TY_BOOL;
    case "print":
      return mkTyFn([TY_STRING], TY_UNIT);
    case "trap":
      return mkTyFn([], TY_NEVER);
    case "__NULL":
      return { kind: "rawptr", inner: fcx.infcx.newVar() };
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
      return tyError(
        fcx.cx,
        new CompilerError(`\`${name}\` cannot be used as a value`, span),
      );
    }
  }
}

// TODO: Cleanup, maybe get the ident switch into this function because typeOfItem is unused.
function lowerAstTy(cx: TypeckCtx, type: Type<Resolved>): Ty {
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

function typeOfItem(
  cx: TypeckCtx,
  itemId: ItemId,
  genericArgs: Ty[],
  cause: Span,
): Ty {
  if (itemId.crateId !== cx.ast.id) {
    // Look up foreign items in the foreign crates, we don't need to lower those
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

export function typeck(
  gcx: GlobalContext,
  ast: Crate<Resolved>,
): Crate<Typecked> {
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
          const fnTy = typeOfItem(cx, item.id, [], item.span) as TyFn;
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
          const fnTy = typeOfItem(cx, item.id, [], item.span) as TyFn;

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
                    item.params[i].span,
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
          const ty = typeOfItem(cx, item.id, [], item.span);

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
          const ty = typeOfItem(cx, item.id, [], item.span);
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
            const initTy = init.value.type === "I32" ? TY_I32 : TY_INT;
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
    // Only the final id=0 crate needs and cares about main.
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

  constructor(public error: ErrorHandler) {}

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
    // the fancy occurs check as errs called it.
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

    if (lhs.kind === "error" || rhs.kind === "error") {
      // This Is Fine 🐶🔥.
      return;
    }

    if (rhs.kind === "never") {
      // not sure whether this is entirely correct wrt inference.. it will work out, probably.
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
        if (rhs.kind === "struct" && lhs.itemId === rhs.itemId) {
          return;
        }
        break;
      }
      case "rawptr": {
        if (rhs.kind === "rawptr") {
          this.assign(lhs.inner, rhs.inner, span);
          return;
        }
        break;
      }
    }

    this.error.emit(
      new CompilerError(
        `cannot assign ${printTy(rhs)} to ${printTy(lhs)}`,
        span,
      ),
    );
  }
}

type FuncCtx = {
  cx: TypeckCtx;
  infcx: InferContext;
  localTys: Ty[];
  loopState: LoopState[];
  checkExpr: (expr: Expr<Resolved>) => Expr<Typecked>;
};

type LoopState = { hasBreak: boolean; loopId: LoopId };

function typeOfValue(fcx: FuncCtx, res: Resolution, span: Span): Ty {
  switch (res.kind) {
    case "local": {
      const idx = fcx.localTys.length - 1 - res.index;
      return fcx.localTys[idx];
    }
    case "item": {
      return typeOfItem(fcx.cx, res.id, [], span);
    }
    case "builtin":
      return typeOfBuiltinValue(fcx, res.name, span);
    case "tyParam":
      return tyError(
        fcx.cx,
        new CompilerError(`type parameter cannot be used as value`, span),
      );
    case "error":
      return tyErrorFrom(res);
  }
}

function exprError(err: ErrorEmitted, span: Span): Expr<Typecked> {
  return {
    kind: "error",
    err,
    span,
    ty: tyErrorFrom({ err }),
  };
}

export function checkBody(
  cx: TypeckCtx,
  ast: Crate<Resolved>,
  body: Expr<Resolved>,
  fnTy: TyFn,
): Expr<Typecked> {
  const infcx = new InferContext(cx.gcx.error);

  const fcx: FuncCtx = {
    cx,
    infcx,
    localTys: [...fnTy.params],
    loopState: [],
    checkExpr: () => unreachable(),
  };

  const checker: Folder<Resolved, Typecked> = {
    ...mkDefaultFolder(),
    expr(expr): Expr<Typecked> {
      switch (expr.kind) {
        case "empty": {
          return { ...expr, ty: TY_UNIT };
        }
        case "let": {
          const loweredBindingTy = expr.type && lowerAstTy(cx, expr.type);
          const bindingTy = loweredBindingTy
            ? loweredBindingTy
            : infcx.newVar();

          const rhs = this.expr(expr.rhs);
          infcx.assign(bindingTy, rhs.ty, expr.span);

          // AST validation ensures that lets can only be in blocks, where
          // the types will be popped.
          fcx.localTys.push(bindingTy);

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
            case "ident":
            case "path": {
              const { res } = lhs.value;
              switch (res.kind) {
                case "local":
                  break;
                case "item": {
                  const item = cx.gcx.findItem(res.id, ast);
                  if (item.kind !== "global") {
                    emitError(
                      fcx.cx,
                      new CompilerError("cannot assign to item", expr.span),
                    );
                  }
                  break;
                }
                case "builtin":
                  emitError(
                    fcx.cx,
                    new CompilerError("cannot assign to builtins", expr.span),
                  );
              }
              break;
            }
            case "fieldAccess": {
              checkLValue(cx, lhs);
              break;
            }
            default: {
              emitError(
                fcx.cx,
                new CompilerError(
                  "invalid left-hand side of assignment",
                  lhs.span,
                ),
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
          const prevLocalTysLen = fcx.localTys.length;

          const exprs = expr.exprs.map((expr) => this.expr(expr));

          const ty = exprs.length > 0 ? exprs[exprs.length - 1].ty : TY_UNIT;

          fcx.localTys.length = prevLocalTysLen;

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
        case "ident":
        case "path": {
          const ty = typeOfValue(fcx, expr.value.res, expr.value.span);

          return { ...expr, ty };
        }
        case "binary": {
          return checkBinary(fcx, expr);
        }
        case "unary": {
          const rhs = this.expr(expr.rhs);
          rhs.ty = infcx.resolveIfPossible(rhs.ty);
          return checkUnary(fcx, expr, rhs);
        }
        case "call": {
          return checkCall(fcx, expr);
        }
        case "fieldAccess": {
          const lhs = this.expr(expr.lhs);
          lhs.ty = infcx.resolveIfPossible(lhs.ty);

          const { field } = expr;
          let ty: Ty;
          let fieldIdx: number | undefined;
          switch (lhs.ty.kind) {
            case "tuple": {
              const { elems } = lhs.ty;
              if (typeof field.value === "number") {
                if (elems.length > field.value) {
                  ty = elems[field.value];
                  fieldIdx = field.value;
                } else {
                  ty = tyError(
                    fcx.cx,
                    new CompilerError(
                      `tuple with ${elems.length} elements cannot be indexed with ${field.value}`,
                      field.span,
                    ),
                  );
                }
              } else {
                ty = tyError(
                  fcx.cx,
                  new CompilerError(
                    "tuple fields must be accessed with numbers",
                    field.span,
                  ),
                );
              }
              break;
            }
            case "struct":
            case "rawptr": {
              let fields: [string, Ty][];
              if (lhs.ty.kind === "struct") {
                fields = lhs.ty.fields_no_subst;
              } else if (lhs.ty.kind === "rawptr") {
                let inner = fcx.infcx.resolveIfPossible(lhs.ty.inner);
                if (inner.kind !== "struct") {
                  inner = tyError(
                    fcx.cx,
                    new CompilerError(
                      "fields can only be accessed on pointers pointing to a struct",
                      expr.lhs.span,
                    ),
                  );
                  ty = inner;
                  break;
                } else {
                  fields = inner.fields_no_subst;
                }
              } else {
                fields = [];
                unreachable("must be struct or rawptr here");
              }

              if (typeof field.value === "string") {
                const idx = fields.findIndex(([name]) => name === field.value);
                if (idx === -1) {
                  ty = tyError(
                    fcx.cx,
                    new CompilerError(
                      `field \`${field.value}\` does not exist on ${printTy(
                        lhs.ty,
                      )}`,
                      field.span,
                    ),
                  );
                  break;
                }

                ty = fields[idx][1];
                fieldIdx = idx;
              } else {
                ty = tyError(
                  fcx.cx,
                  new CompilerError(
                    "struct fields must be accessed with their name",
                    field.span,
                  ),
                );
              }
              break;
            }
            default: {
              ty = tyError(
                fcx.cx,
                new CompilerError(
                  `cannot access field \`${field.value}\` on type \`${printTy(
                    lhs.ty,
                  )}\``,
                  expr.span,
                ),
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
          fcx.loopState.push({
            hasBreak: false,
            loopId: expr.loopId,
          });

          const body = this.expr(expr.body);
          infcx.assign(TY_UNIT, body.ty, body.span);

          const hadBreak = fcx.loopState.pop();
          const ty = hadBreak ? TY_UNIT : TY_NEVER;

          return {
            ...expr,
            body,
            ty,
          };
        }
        case "break": {
          const loopStateLength = fcx.loopState.length;
          if (loopStateLength === 0) {
            const err: ErrorEmitted = emitError(
              fcx.cx,
              new CompilerError("break outside loop", expr.span),
            );
            return exprError(err, expr.span);
          }
          const target = fcx.loopState[loopStateLength - 1].loopId;
          fcx.loopState[loopStateLength - 1].hasBreak = true;

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

          const structTy = typeOfValue(fcx, expr.name.res, expr.name.span);

          if (structTy.kind !== "struct") {
            const err: ErrorEmitted = emitError(
              fcx.cx,
              new CompilerError(
                `struct literal is only allowed for struct types`,
                expr.span,
              ),
            );
            return exprError(err, expr.span);
          }

          const assignedFields = new Set();

          fields.forEach(({ name, expr: field }, i) => {
            const fieldIdx = structTy.fields_no_subst.findIndex(
              (def) => def[0] === name.name,
            );
            if (fieldIdx == -1) {
              emitError(
                fcx.cx,
                new CompilerError(
                  `field ${name.name} doesn't exist on type ${expr.name.name}`,
                  name.span,
                ),
              );
            }
            const fieldTy = structTy.fields_no_subst[fieldIdx];
            infcx.assign(fieldTy[1], field.ty, field.span);
            assignedFields.add(name.name);
            fields[i].fieldIdx = fieldIdx;
          });

          const missing: string[] = [];
          structTy.fields_no_subst.forEach(([name]) => {
            if (!assignedFields.has(name)) {
              missing.push(name);
            }
          });
          if (missing.length > 0) {
            emitError(
              fcx.cx,
              new CompilerError(
                `missing fields in literal: ${missing.join(", ")}`,
                expr.span,
              ),
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
        case "error": {
          return { ...expr, ty: tyErrorFrom(expr) };
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

  fcx.checkExpr = checker.expr.bind(checker);

  const checked = checker.expr(body);

  infcx.assign(fnTy.returnTy, checked.ty, body.span);

  const resolved = resolveBody(fcx, checked);

  return resolved;
}

function checkLValue(cx: TypeckCtx, expr: Expr<Typecked>) {
  switch (expr.kind) {
    case "ident":
    case "path":
      break;
    case "fieldAccess":
      checkLValue(cx, expr.lhs);
      break;
    default:
      emitError(
        cx,
        new CompilerError("invalid left-hand side of assignment", expr.span),
      );
  }
}

function checkBinary(
  fcx: FuncCtx,
  expr: Expr<Resolved> & ExprBinary<Resolved>,
): Expr<Typecked> {
  const lhs = fcx.checkExpr(expr.lhs);
  const rhs = fcx.checkExpr(expr.rhs);

  lhs.ty = fcx.infcx.resolveIfPossible(lhs.ty);
  rhs.ty = fcx.infcx.resolveIfPossible(rhs.ty);

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

    if (lhsTy.kind === "rawptr" && rhsTy.kind === "rawptr") {
      fcx.infcx.assign(lhsTy.inner, rhsTy.inner, expr.span);
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

  const ty = tyError(
    fcx.cx,
    new CompilerError(
      `invalid types for binary operation: ${printTy(lhs.ty)} ${
        expr.binaryKind
      } ${printTy(rhs.ty)}`,
      expr.span,
    ),
  );
  return { ...expr, lhs, rhs, ty };
}

function checkUnary(
  fcx: FuncCtx,
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

  const ty = tyError(
    fcx.cx,
    new CompilerError(
      `invalid types for unary operation: ${expr.unaryKind} ${printTy(rhs.ty)}`,
      expr.span,
    ),
  );
  return { ...expr, rhs, ty };
}

function checkCall(
  fcx: FuncCtx,
  expr: ExprCall<Resolved> & Expr<Resolved>,
): Expr<Typecked> {
  if (
    expr.lhs.kind === "ident" &&
    expr.lhs.value.res.kind === "builtin" &&
    expr.lhs.value.res.name === "___transmute"
  ) {
    const ty = fcx.infcx.newVar();
    const args = expr.args.map((arg) => fcx.checkExpr(arg));
    const ret: Expr<Typecked> = {
      ...expr,
      lhs: { ...expr.lhs, ty: TY_UNIT },
      args,
      ty,
    };

    return ret;
  }

  const lhs = fcx.checkExpr(expr.lhs);
  lhs.ty = fcx.infcx.resolveIfPossible(lhs.ty);

  // check args before checking the lhs.
  const args = expr.args.map((arg) => fcx.checkExpr(arg));

  const lhsTy = lhs.ty;
  if (lhsTy.kind !== "fn") {
    const ty = tyError(
      fcx.cx,
      new CompilerError(
        `expression of type ${printTy(lhsTy)} is not callable`,
        lhs.span,
      ),
    );
    return { ...expr, lhs, args, ty };
  }

  lhsTy.params.forEach((param, i) => {
    if (args.length <= i) {
      emitError(
        fcx.cx,
        new CompilerError(
          `missing argument of type ${printTy(param)}`,
          expr.span,
        ),
      );
      return;
    }

    fcx.infcx.assign(param, args[i].ty, args[i].span);
  });

  if (args.length > lhsTy.params.length) {
    emitError(
      fcx.cx,
      new CompilerError(
        `too many arguments passed, expected ${lhsTy.params.length}, found ${args.length}`,
        expr.span,
      ),
    );
  }

  return { ...expr, lhs, args, ty: lhsTy.returnTy };
}

function resolveBody(fcx: FuncCtx, checked: Expr<Typecked>): Expr<Typecked> {
  const resolveTy = (ty: Ty, span: Span) => {
    const resTy = fcx.infcx.resolveIfPossible(ty);
    // TODO: When doing deep resolution, we need to check for _any_ vars.
    if (resTy.kind === "var") {
      return tyError(fcx.cx, new CompilerError("cannot infer type", span));
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
